# Custom jobs

Add [customized-file] with custom jobs  to the [instances-configuration-file] of the provider and reseller instance.

## Export CDRS 

On the configuration of the provider, add a [customized-file] PHP class like
``customizations/ExportTo<your_reseller>.php``, and define abstract/missing
methods (here ``Minitel`` is an example of reseller name).

```
    class ExportToMiniTel extends ExportCDRSToReseller
    {
    
        /**
         * @return string
         */
        function getResellerCode() {
            return 'mini-tel';
        }
    
        public function getActivationDate() {
            // NOTE: before this date the info is manually sent, retrieving from the historic data.
            // From this data the info is sent live, the rates are aligned.
            return strtotime('2014-01-01');
        }
    
    }
```

Communication channels used on the provider side, can be masked to the reseller, using other values. 
They are exported according the settings in this method

```
    /**
     * Allows exporting info about used communication channels,
     * in case they must be known from the Reseller, for applying different rates on them.
     *
     * The used name, is `ar_communication_channel_type.internal_name`.
     *
     * @return array a map between channel name on provider, and name to use when exporting to the reseller.
     * Channels that are not matching will be exported to the reseller using the default channel name.
     * Channel Names are exported in this way:
     * - empty string when there is no channel info exported
     * - the channel name otherwise
     * Channel Names are imported on the reseller side in this way:
     * - "provider-name" when there is no channel info exported
     * - "provider-name-" otherwise
     * By default (without specifying nothing) the services are exported like 'system-service-cdr'
     */
    public function exportedCommunicationChannels() {
        return array();
    }
```

Add the CDR exporting job to the list of jobs, for exporting the CDRs. Usually it is a
line like this in [instances-configuration-file] 

```
    custom_export_cdrs_jobs = [ 'ExportToMiniTel' ]
```

## Exporting service CDRS

For exporting [service-cdr] create an exporting job like for normal CDRS, but using a subclass of
``ExportServiceCDRSToReseler``.

## Importing

TODO continue
