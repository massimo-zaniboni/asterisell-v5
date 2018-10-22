## Scope

Export data in ar_number_portability table, compress it, and import during initial installation phase.

Useful if there is a lot of data that had to be loaded in a fast way.

## Usage

* Follow the instructions (it is partially a manual process).
* Install the Asterisell jobs as configure job, in the customized customer instance.

## Manual export

Enter into an instance with data to export.

```
php asterisell.php cron disable
```

Enter into the database using ``mysql -uroot -p... <db-name>``, and export data to a file:

```
SELECT telephone_number, from_date, ported_telephone_number
INTO OUTFILE '/var/tmp/ported_numbers_to_YYYY-MM-DD.sql'
CHARACTER SET 'utf8'
FROM ar_number_portability
ORDER BY telephone_number, from_date;
```

The list of exported remote files can be obtained with

```
SELECT p.internal_name
, f.name
, f.receiving_date
FROM ar_remote_file AS f
INNER JOIN ar_cdr_provider AS p
ON f.ar_cdr_provider_id = p.id
ORDER BY f.receiving_date;
```

and then saved into a CSV file.

```
php asterisell.php cron enable
```

Compress using a slow but very effective compressing utility like ``lrzip``.

```
[root@6a1693b9a31e tmp]# du -h -s ported_numbers_to_2018-08-14.sql
377M	ported_numbers_to_2018-08-14.sql

[root@6a1693b9a31e tmp]# lrzip ported_numbers_to_2018-08-14.sql
Output filename is: ported_numbers_to_2018-08-14.sql.lrz
ported_numbers_to_2018-08-14.sql - Compression Ratio: 10.543. Average Compression Speed:  2.000MB/s.
Total time: 00:03:08.05

[root@6a1693b9a31e tmp]# du -h -s ported_numbers_to_2018-08-14.sql*
377M	ported_numbers_to_2018-08-14.sql
36M	ported_numbers_to_2018-08-14.sql.lrz
```

The corresponding command for loading data

```
LOAD DATA INFILE '/var/tmp/importpipe'
INTO TABLE ar_number_portability
CHARACTER SET 'utf8'
(telephone_number, from_date, ported_telephone_number);
```

See Itec example for code loading initial data. In particular set something like

```
    def execute_install_task_post(self):
       with cd(self.get_admin_deploy_directory()):
            run('php asterisell.php dev these-remote-files-are-already-processed data_files/imported_files_to_2018-08-14.csv')

            mysqlCmd = "LOAD DATA INFILE '/var/tmp/importpipe' INTO TABLE ar_number_portability CHARACTER SET 'utf8' (telephone_number, from_date, ported_telephone_number)"
            run('rm -f /var/tmp/importpipe && mkfifo /var/tmp/importpipe && lrzcat data_files/ported_numbers_to_2018-08-14.sql.lrz > /var/tmp/importpipe & mysql -uroot -p' + self.database_password + ' ' + self.database_name + ' --execute="' + mysqlCmd + '"')
```

