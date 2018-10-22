# Configuring WebDAV

It is possible sending files to a remote [reseller] using the
WebDav protocol. WebDAV is a standard protocol based on HTTP connections, so it has
encryption by default (using HTTPS), and it can pass through firewalls.

## On provider

Create a file `customizations/ExportToFoo.php` like this

```
    class ExportToFoo extends ExportCDRSToReseller
    {
    
      /**
       * @return string
       */
      function getResellerCode() {
          return 'foo';
      }
    
      public function getActivationDate() {
          // NOTE: before this date the info is manually sent, retrieving from the historic data.
          // From this data the info is sent live, the rates are aligned.
          return strtotime('2017-01-01');
      }
    }
```

and a file `customizations/ExportServicesToFoo.php` like this

```
    class ExportServicesToFoo extends ExportServiceCDRSToReseller
    {
    
      /**
       * @return string
       */
      function getResellerCode() {
          return 'foo';
      }
    
      public function getActivationDate() {
          // NOTE: before this date the info is manually sent, retrieving from the historic data.
          // From this data the info is sent live, the rates are aligned.
          return strtotime('2017-01-01');
      }
    }
```

Enable the job customizing `fabric_data/asterisell_instances.py`

```
    custom_files = {
      'ExportToFoo':'apps/asterisell/lib/jobs/customizations',
      'ExportServicesToFoo':'apps/asterisell/lib/jobs/customizations'
    }
    
    custom_export_jobs = [
      'ExportToFoo',
      'ExportServicesToFoo'
    ]
    
    webdav_users = [('foo','some-password')]
```

The installation tool, will create the webdav configurations for you,
and it will expose the WebDAV service on `https://provider-url/get-foo`
You can inspect the `/etc/nginx/` configurations inside the [asterisell-instance].

## On reseller on the same host

In case the reseller is an instance on the same host of the
provider, create a Job like this

```
    class FooImportCDRSFromBar extends ImportCDRSFromRemoteAsterisellProvider
    {
    
        function getConnectionName() {
            return 'bar';
        }
    
        function getCDRProviderName() {
          return 'bar';
        }
    
        function skipSSLCertificateVerify()
        {
          return true;
        }
    }
```

Then configure something like this

```
    custom_files = {
      'FooImportCDRSFromBar':'apps/asterisell/lib/jobs/customizations'
    }
    
    import_cdrs_jobs = [ 'FooImportCDRSFromBar' ]
    
    def conf_connection_params(self):
      r = []
    
      c = lib.ConnectionParams()
      c.connection_name = 'bar'
      c.user = 'foo'
      c.password = self.get_password_for('bar-foo')
      c.host = 'http://local-ip/admin/get-foo/'
      c.port = '8001' 
      r.append(c)
    
      return r
```

You had to set the password on [password-file].

## On resellers on external hosts

In case the reseller is on a different host respect
the provider, create a Job like this

```
    class FooImportCDRSFromBar extends ImportCDRSFromRemoteAsterisellProvider
    {
    
        function getConnectionName() {
            return 'bar';
        }
    
        function getCDRProviderName() {
          return 'bar';
        }
    
       function skipSSLCertificateVerify()
       {
         return false;
       }
    }
```

Configure something like this

```
    custom_files = {
      'FooImportCDRSFromBar':'apps/asterisell/lib/jobs/customizations'
    }
    
    import_cdrs_jobs = [ 'FooImportCDRSFromBar' ]
    
    def conf_connection_params(self):
      r = []
    
      c = lib.ConnectionParams()
      c.connection_name = "bar"
      c.user = "foo"
      c.password = self.get_password_for('bar-foo')
      c.host = 'https://provider-url/admin/get-foo/'
      c.port = '443'
      r.append(c)
    
      return r
```

You had to set the password on [password-file].

You can try the access using a command like this inside the reseller

```
    curl -v --basic --user foo:some-password https://provider-url/admin/get-foo/is_asterisell_directory.chk
```

Try with the ``--insecure`` for debugging SSL certificate problems.
