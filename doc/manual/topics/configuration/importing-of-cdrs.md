# Importing of CDRS 

Every source of CDRS is configured as a distinct [cdrs-provider] inside the Web UI in ``Params->CDRs Providers``.

## Import from CSV files

CDRS are imported from [cdrs-provider] defining custom jobs. Some of them are defined in [instances-configuration-file]. You can create custom PHP class inside ``cutomizations`` directory, inheriting from some template job defined in ``apps/asterisell/lib/jobs/data_file_processing`` directory and subdirectories, and then completing missing (abstract) parameters.

Then you can add these jobs to the instance, as described in [custom-jobs].

The supported data formats are listed near the beginning of ``rating_tools/rate_engine/src/Asterisell/CustomerSpecificImporters.hs``, in the function ``supportedSourceCDRImporters``.

## Importing CDRS from an external collector

Add inside ``asterisell_instances.py`` something like

```
class DefaultInstance(BillingInstance):

  ...
  

  def conf_connection_params(self):
        r = []

        c = ConnectionParams()
        c.connection_name = 'import-remote-cdrs-SOME-SOURCE-NAME'
        c.provider = 'abilis-db-collector'
        c.user = 'some-user'
        c.password = self.get_password_for('SOME-SOURCE-NAME')
        c.host = 'some-host'
        c.port = '3306'
        c.dbName = 'abiliscpx'
        c.tableName = 'collector'
        c.timeFrameInMinutes = '0'
        c.dataSourceFormat = 'abilis-db-collector'
        c.dataSourceVersion = 'v1'
        c.fromDate = '2018-10-01'
        c.removeOlderThanDays = '0'
        r.append(c)
```

The prefix ``import-remote-cdrs-`` will be used from the job ``ImportCDRSUsingAppConfs`` for retrieving the data from the remote MySQL database, assuming it is in the ``abilis-db-collector`` format.

The data will be retrieved in chunks, in a light way, and in a transaction safe way, also if the remote database does not support transactions (e.g. MyISAM tables).

The supported data formats are listed near the beginning of ``rating_tools/rate_engine/src/Asterisell/CustomerSpecificImporters.hs``, in the function ``supportedSourceCDRImporters``.

### Setting users on remote database

A remote database is a database that is not on the same Linux
instance where Asterisell is installed. Supposing the remote Asterisell
instance is on IP '10.10.10.10', the SQL commands to execute are like:

```
    CREATE USER 'someuser'@'localhost' IDENTIFIED BY 'somepassword';
    CREATE USER 'someuser'@'10.10.10.10' IDENTIFIED BY 'somepassword';
    
    GRANT SELECT,DELETE,UPDATE ON dbname.collector TO 'someuser'@'localhost';
    GRANT SELECT,DELETE,UPDATE ON dbname.collector TO 'someuser'@'10.10.10.10';
    
    FLUSH PRIVILEGES;
```

### Tables on the same host and database server

If the table with source CDRs is on the same Host and Database Server,
you can optimize further the processing of source CDRS, using the TokuDB engine also for the collector table.

If you have not many source CDRs, and you can interrupt the CDR stream
flow, you can execute a command like

```
    ALTER TABLE your_source_cdrs_table_name 
    ENGINE=TokuDB 
    ROW_FORMAT=TOKUDB_SNAPPY, 
    DEFAULT CHARACTER SET = utf8mb4, 
    DEFAULT COLLATE = utf8mb4_bin;
```

