.. _Asterisell: https://www.asterisell.com

Asterisell FAQ
==============

.. contents:: Table of Contents
   :depth: 2
   :backlinks: top
   :local:

Docker
------

How updating the SSL certificates of an instance
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Change the configurations in ``fabric_data/asterisell_instances.py``, and then ``fab restart:INSTANCE``.


How removing an instance completely
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

  # First remove the Docker container
  docker stop INSTANCE
  docker remove INSTANCE

  # But now the Docker VOLUME with DB data, log files,
  # bakup files and so on is still present.
  #
  # Only in case you are sure that all Docker orphan VOLUMES
  # (volumes without any container using them)
  # are for sure unusued volumes, execute
  fab remove_orphan_volumes


Reports
-------

Totals on reports
~~~~~~~~~~~~~~~~~

| The total cost of vendor reports does not match the grand total on the report

Check in the online call report if you have some internal vendors with
costs. Internal vendors are not showed on reports

Regeneration of many reports in the past
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

|I'm generating many reports in the past, setting the scheduler to 4 months in the past, but only the first set is generated

Asterisell checks for new reports to generate not at every passage of
the cron processor, but it wait some time.

In the JobLog you can see how much it is post-poned.

So if you wait some time, another batch of reports will be executed.


File processing
---------------

Conversion of files to UTF-8 format
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Asterisell read, and import files in UTF-8 format.

This command show the character-set of a file:

::

    file -bi

A command for converting a file to another locale

::

    iconv -f ISO-8859-1 -t UTF-8  > t
    rm
    mv t

or a probably better command with in-place conversion:

::

    yum install recode
    recode ISO-8859-1..UTF8


Rating
------

Generic rateEngine error messages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Sometimes the application generate long error messages like

::

  /var/www/tsnet/admin/scripts/RateEngine --rate  --debug-mode 0 --is-voip-reseller 1
  --load-rate-categories /var/www/tsnet/admin/data_files/messages/params/rate_category.csv
  --load-vendors /var/www/tsnet/admin/data_files/messages/params/vendors.csv
  --load-channels-types /var/www/tsnet/admin/data_files/messages/params/channel_types.csv
  --load-channel-domains /var/www/tsnet/admin/data_files/messages/params/channel_domains.csv
  --load-telephone-prefixes /var/www/tsnet/admin/data_files/messages/params/telephone_prefixes.csv
  --digits-to-mask 3 --default-telephone-prefix 39
  --currency-precision 4
  --load-extensions /var/www/tsnet/admin/data_files/messages/params/extensions.csv
  --load-rate-plan-changes /var/www/tsnet/admin/data_files/messages/params/rate_plan.csv
  --load-rate-plan /var/www/tsnet/admin/data_files/messages/params/rate_plan_id_
  --load-services /var/www/tsnet/admin/data_files/messages/params/services.csv
  --load-service-price-list /var/www/tsnet/admin/data_files/messages/params/services_price_list.csv
  --load-assigned-services /var/www/tsnet/admin/data_files/messages/params/assigned_services.csv
  --debug-file /var/tmp/var/www/tsnet/admin/rate_debug.info
  --from-date "2016-11-01 00:00:00"
  --to-date "2016-12-01 10:45:03"
  --from-date "null"  --to-date "null"  --rate-unbilled-calls true
  --db-name tsnet

  /var/www/tsnet/admin/data_files/messages/params/rate_plan_id_44.rate: hClose: invalid argument (Bad file descriptor)

In the majority of these errors the important part is at the bottom. In this case

::

    /var/www/tsnet/admin/data_files/messages/params/rate_plan_id_44.rate: hClose: invalid argument (Bad file descriptor)

So it suffices in this case opening the rate with id 44, on the web-interface, and see if there are formatting errors.

The problem of this error messages it is that the rate plan language is very powerful and it is not
easy to generate meaningful error messages.

Debug rating errors
~~~~~~~~~~~~~~~~~~~

Open always fully the error message, because it is formatted in a clear way, with correct indentation.

It is possible inspecting the applied rate to a CDR, in the online call
report, clicking on the cost or income.

It is possible generating more debug info, about applied and unapplied
rates, rating in debug mode. In the Asterisell instance admin directory:

::

    fab help

    fab cron_disable:INSTANCE
    # because every time CDRs are rerated
    # the debug info will be lost.

    fab connect:INSTANCE

    php asterisell.php debug rerate YYYY-MM-DD
    php asterisell.php debug jobs

    # debug and change rates, and then
    php asterisell.php debug rerate YYYY-MM-DD
    php asterisell.php debug jobs

    exit
    fab cron_enable:INSTANCE


It is possible inspecting the reason of an unrated CDR, in the
``Calls -> Calls with Errors`` menu entry.

Fixing errors in source CDRs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The utility ``php asterisell.php data export-cdrs`` accessible after executing ``fab connect:INSTANCE``
can export source CDRs of a time-frame. They can be changed. There are
some utilities on directory ``admin/script/source\_cdrs\_fix``, and other can be created.

The transformed files, can be put into instance directory ``admin/data_files/messages/input``
and they will be imported again in Asterisell. They are status files, so
the previos version of source CDRs on the same time-frame will be
deleted, and replaced with the new version.

A copy of the original exported source-cdrs can be maintained for a
while, in order to restore them in case of errors.

Low level details
^^^^^^^^^^^^^^^^^

During rating, a ``source_cdr`` is assigned to the extension.

So ``ar_source_cdr`` to export are recognized during rating phase.

They are put in a table.

The content of the table is safe because usually ``ar_source_cdr`` are not
removed. The exception is about status files, and few other cases.

So the export command remove the ``ar_source_cdr`` from the table, and
export them in files.

Then a rerating command must be generated (it is generated automatically
from the utility) because CDRs in the CDR table, must be deleted and
rerated, and in this case there are no any more the corresponding
``ar_source_cdr``.

IMPORTANT: after a moving operation, the ``ar_source_cdr_to_move``
content is deleted, so a rerating must be done again, if another date
must be exported.

The CDRs will be removed from source instance after rerating on the
source instance, because the rerating passage will remove the rated
CDRs, and there are any more the source CDRs.

::

  fab run_jobs:INSTANCE

"Lock wait timeout exceeded" Error Message 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If it signaled a problem like

::

    ConnectionError {
      errFunction = \"query\",
      errNumber = 1205,
      errMessage = \"Lock wait timeout exceeded; try restarting transaction\"
    }

there can be pending transactions on MySQL.

Enter into the asterisell database using the root MySQL user.

::

  fab help
  fab connect:INSTANCE

  cat config/databases.yml
  # for seeing the database name, and admin user

  mysql -u root -pTHE_SAME_PASSWORD_DISPLAYED_FROM_CAT THE_DB_OF_CAT

  show open tables where in_use>0;
      +------------+------------------------+--------+-------------+
    | Database   | Table                  | In_use | Name_locked |
    +------------+------------------------+--------+-------------+
    | asterisell | ar_cdr                 |      1 |           0 |
    | asterisell | ar_daily_status_job    |      1 |           0 |
    | asterisell | ar_daily_status_change |      1 |           0 |
    +------------+------------------------+--------+-------------+
    3 rows in set (0.00 sec)

  show processlist;
    +----+-------+-----------+------------+---------+------+----------------------+------------------------------------------------------------------------------------------------------+----------+
    | Id | User  | Host      | db         | Command | Time | State                | Info                                                                                                 | Progress |
    +----+-------+-----------+------------+---------+------+----------------------+------------------------------------------------------------------------------------------------------+----------+
    | 19 | tsnet | localhost | asterisell | Sleep   |  969 |                      | NULL                                                                                                 |    0.000 |
    | 20 | tsnet | localhost | asterisell | Sleep   |  970 |                      | NULL                                                                                                 |    0.000 |
    | 21 | tsnet | localhost | asterisell | Query   |  918 | After opening tables | LOAD DATA INFILE '/var/tmp/var/www/asterisell/admin/pipe2' INTO TABLE ar_cdr  CHARACTER SET 'utf8'   |    0.000 |
    | 33 | root  | localhost | asterisell | Query   |    0 | init                 | show processlist                                                                                     |    0.000 |
    +----+-------+-----------+------------+---------+------+----------------------+------------------------------------------------------------------------------------------------------+----------+
    4 rows in set (0.00 sec)


  # kill the process with the problems
  kill 21;
