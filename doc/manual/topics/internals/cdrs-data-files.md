# CDRS data files

At each execution of the [jobs-processor] the CDRS data files files inside the container directory
``/var/www/instance-name/admin/data_files/messages/input`` are imported inside the TokuDB compressed table ``ar_source_cdr``.

They must have a name compatible with some known CDR format, otherwise Asterisell will signal the
problem, and it will not import them.

If the importer is configured accordingly, they will be also archivied in ``/var/www/instance-name/admin/data_files/messages/archive``, but this is not suggested, because they can be always extracted from ``ar_source_cdr`` table, in nearly their original format, using ``php asterisell.php data export-cdrs`` command.

It is possible importing CDRS moving manually files in this directory, but usually this is done from the import jobs. Importing jobs take care to execute only file-system atomic operations, for moving files. So consistency is guarantee. All DBMS related operations are executed usinng transactions.

## Data file name format

A CDRs source file has a name like
``file1.some-cdr-provider__logical-type__version, where:

  - `file1` is some file name to use. It should be distinct, for
    avoiding clash during importing
  - `some-cdr-provider` is a name of a configure cdr-provider. In case
    there is no configured cdr-provider, Asterisell signals the problem,
    and does not import the file. The file will be imported later,
    automatically, when the problem is resolved
  - `logical-type` is the name of some type of file to import. If the
    type is not recognized, Asterisell signals the problem, and the file
    will be imported later, when the problem is resolved
  - `version` is the name of the version of the type used. So the same
    type, can have different versions, with slightly different formats

## Status files

A CDRs status file name is like
``file1.2015-01-01.some-cdr-provider__logical-type__version``, where:

  - `2015-01-01` is a status file, with all the calls of the day
  - `2015-01-00` is a status file, with all the calls of the month
  - `2015-00-00` is a status file, with all the calls of the year
  - the other parts of the file name follow the same convention of CDRs
    file name


When Asterisell import a status file:

  - delete all the calls in the status file time-frame, of the same
    provider
  - insert the calls in the status file
  - the net effect is replacing the calls in the status time frame, with
    the content of the status file

## Pseudo CSV file format

Asterisell supports a pseudo CSV-file format:

  - multi lines are not supported
  - it extracts first a distinct line from a file
  - it process it like a CSV file line

This format is useful in case there can be errors in the CSV files,
because it minimize the number of unrecognized entries, in case of a
missing ending quote.
