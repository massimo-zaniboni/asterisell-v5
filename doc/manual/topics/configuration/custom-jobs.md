# Custom jobs

Custom jobs can be defined in [customized-file], and specified in [instances-configuration-file].

## Automatic importing of CDRS

CDRS are imported from some external data source, according the specification of the [cdrs-provider].
This is typically done using custom jobs. You can reuse one of the jobs in directory ``apps/asterisell/lib/jobs/data_file_processing`` there are many jobs to use.

Every [cdrs-provider] must be configured in the Web UI in ``Params->CDRs Providers``.

TODO CDR-format 

## Importing CDRS from an external collector

``apps/asterisell/lib/jobs/data_file_processing/ImportCDRSFromDatabase.php``
is the abstract Job to use for retrieving CDRs from a collector table. It:

  - reads CDRs on a table of a database, that is acting like a queue of
    CDRs to process,
  - convert to CSV files the CDRs to process,
  - put the generated CSV files on the input queue of Asterisell,
  - remove (optionally) older CDRs already exported to Asterisell,
    managing the table as a queue,

It is an abstract class, so:

  - a concrete class/job must be inherited from it,
  - the inherited job, must define the missing methods, respecting the
    requirements on method headers descriptions,
  - the inherited job must be added to the list of jobs to process
    before rating, adding it to the `import_cdrs_jobs` option of the
    instance configuration file

One or more jobs of this type can be added, without conflict.

## Adding a flag field to the collector

If the remote table has a reliable progressive ID field, Asterisell can
use it for retrieving new CDRS. Otherwise a flag field can be added to the collector table.
Note: the ID approach is the suggest one, because it is less invasive, and it does not require
a transaction lock on the external table.

For adding the field, you must modify the external collector table 

```
    ALTER TABLE your_table_name
    ADD COLUMN is_exported_to_asterisell TINYINT default 0 NOT NULL,
    ADD INDEX is_exported_to_asterisell_index(is_exported_to_asterisell);
```

This field will be used from Asterisell for importing only new source
CDRs. The source table will work like a queue.

Transactions are used, so in case of connection problems, the CDRs will
be imported and rated again. But in case of MyISAM tables this can cause
a temporary lock on the entire table.

## Accessing collector tables on external databases

An external database is a database that is not on the same Linux
instance where Asterisell is installed. Supposing the remote Asterisell
instance is on IP '10.10.10.10', the SQL commands to execute are like:

```
    CREATE USER 'someuser'@'localhost' IDENTIFIED BY 'somepassword';
    CREATE USER 'someuser'@'10.10.10.10' IDENTIFIED BY 'somepassword';
    
    GRANT SELECT,DELETE,UPDATE ON dbname.collector TO 'someuser'@'localhost';
    GRANT SELECT,DELETE,UPDATE ON dbname.collector TO 'someuser'@'10.10.10.10';
    
    FLUSH PRIVILEGES;
```

## Tables on the same host and database server

If the table with source CDRs is on the same Host and Database Server,
you can optimize further the processing of source CDRs.

First show the format of the table, executing something like

```
    show create table your_table_name;
```

It is important the last line of the table creation:

```
    ENGINE=InnoDB AUTO_INCREMENT=51438 DEFAULT CHARSET=latin1
```

Asterisell uses the TokuDB engine:

  - it can compress data
  - it is fast and reliable
  - it is studied for big data
  - it is 100% compatbile with all the SQL and MySQL API commands issued
    to a InnoDB engine

So you can switch the engine used for the table, to TokuDB. The
application writing to it will not notice the difference.

If you have not many source CDRs, and you can interrupct the CDR stream
flow, you can execute a command like

```
    ALTER TABLE your_source_cdrs_table_name 
    ENGINE=TokuDB 
    ROW_FORMAT=TOKUDB_SNAPPY, 
    DEFAULT CHARACTER SET = utf8mb4, 
    DEFAULT COLLATE = utf8mb4_bin;
```

If you cannot interrupt the stream of CDRs, consult the assistance. It
is possibile copying on the fly old and new CDRs, and them executing a
fast switch.
