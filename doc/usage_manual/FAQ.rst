.. _Asterisell: https://www.asterisell.com
.. _support: support@asterisell.com
.. _assistance: support@asterisell.com

FAQ and HOWTOS
==============

How Removing an Instance Completely
-----------------------------------

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

Conversion of files to UTF-8 format
-----------------------------------

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

Fixing Errors in Source CDRs
----------------------------

The utility ``php asterisell.php data export-cdrs`` accessible after executing ``fab connect:INSTANCE``
can export source CDRs of a time-frame. They can be changed. There are
some utilities on directory ``admin/script/source\_cdrs\_fix``, and other can be created.

The transformed files, can be put into instance directory ``admin/data_files/messages/input``
and they will be imported again in Asterisell. They are status files, so
the previos version of source CDRs on the same time-frame will be
deleted, and replaced with the new version.

A copy of the original exported source-cdrs can be maintained for a
while, in order to restore them in case of errors.

How Updating the SSL Certificates of an Instance
------------------------------------------------

Change the configurations in ``fabric_data/asterisell_instances.py``, and then ``fab restart:INSTANCE``.

Moving CDRs from an instance to another
---------------------------------------

Put in ``ar_voip_extension_to_move`` table of the instance,
the extension code to export,
specified in the Asterisell format, so you can use also "\*" and "X" and
other pattern matching values.

Rerate the calls in the time frame you want export:

::

    fab connect:INSTANCE
    php asterisell.php debug rerate YYYY-MM-DD
    php asterisell.php run jobs

The table ``ar_source_cdr_to_move`` will now contain the IDs of the CDRs
to export.

For exporting the CDRS, use the commands

::

    fab connect:INSTANCE
    php asterisell.php data export-cdrs-to-move YYYY-MM-DD

Move the file with the CDRs to export, into the destination instance
input directory.

IMPORTANT: source CDRs will be moved on external files, and removed from
instance ``ar_source_cdr`` table.

Probably when you are sure to having exported all the CDRs, you can
clean the content of table ``ar_voip_extension_to_move``

Low Level Details
~~~~~~~~~~~~~~~~~

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


Scheduled Report Generation
---------------------------

> I'm generating many reports in the past, setting the scheduler to 4
months in the past, but only the first set is generated

Asterisell checks for new reports to generate not at every passage of
the cron processor, but it wait some time.

In the JobLog you can see how much it is post-poned.

So if you wait some time, another batch of reports will be executed.

On reports the total cost of vendors does not match the grand total on the report
---------------------------------------------------------------------------------

Check in the online call report if you have some internal vendors with
costs. Internal vendors are not showed on reports

Error Messages
--------------

Intimidating Rating Errors
~~~~~~~~~~~~~~~~~~~~~~~~~~

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

Specific Rating Errors
~~~~~~~~~~~~~~~~~~~~~~

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

Lock wait timeout exceeded; try restarting transaction
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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



Configuration of Resellers
--------------------------

An Asterisell server instance can send CSV files with CDRs to other
Asterisell servers. The sender server is playing the role of a provider, while the receiver
server plays the role of a VoIP reseller. The income of the provider, is the cost of calls
for the reseller.

.. warning::
   Up to date there are only Asterisell reseller instances running directly on
   virtual machines, and not on Docker. So the instructions on this section
   are not tested for the Docker case, and for sure must be adapted.

   They are only a guideline.

Export of CDRS on Provider Side
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Add a PHP class like
``apps/asterisell/lib/provider_specific/<your_customer_code>/ExportTo<your_reseller>.php``,
that is subclass of ``ExportCDRSToReseller`` and define abstract/missing
methods

The code is often something of very simple like

::

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

Add the job to the list of jobs, for exporting the CDRs. Usually it is a
line like this in file ``fabric_data/.../instances.py``

::

    custom_export_cdrs_jobs = [ 'ExportToMiniTel' ]

The suggested directory to use for exchanging files is
``/var/opt/asterisell/<provider-code>/<export-code>``

Reseller Code on Provider Side
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the ``Entities -> Resellers`` menu define a reseller with the same
code.

In the ``Entities -> Customers`` menu, create a customer associated to
the Reseller. The reseller association is made in the ``party`` section
of the customer.

Extensions Codes
~~~~~~~~~~~~~~~~

Provider Side
.............

Create VoIP accounts associated to the reseller, and that are child
organizations/extensions of the main reseller organization.

Specify for each extension the ``export code``. The calls of the
exported extension will be sent to the provider, using the
``export code`` as VoIP account identifier. It is possible to execute
this action in batch mode, with the command

::

    php asterisell.php data complete-reseller-export-code
      given the id of an organization (the unique identifier in a URL like "view/id/123"), complete the export-code field of the children extensions, with the first value in extensions codes. Only extensions with an empty value are affected. This is a sane default value, that can be used for extensions to export to resellers, in case a batch initialization is needed. These values can be specified individually also in the user interface.

In case there are services associated to organizations shared with a
reseller, then also every customer organization must have an export-code
on the provider side, that is the same code used for identifying the
organization on the reseller side:

-  specify the organization code in the ``export-code`` section on the
   provider side
-  on the reseller side create a pseudo extension, associated to the
   shared organization, and specify the ``export-code``

Reseller Side
.............

Resellers see only the provider exported code.

Rates
~~~~~

Provider Side
.............

Identify the income rates associated to the price-category of the
reseller. These rates must/can be configured as automatically exported
to the reseller. In case choose a name and notes making sense also on
the reseller side.

Reseller Side
.............

Service on the provider, can be exported to the reseller as cost. They
will be imported as "system" call, and they will be not visible to end
customers. An example of cost rate for managing them is this:

::

    rate {
      id: vendor-services
      match-call-direction: system
      match-vendor: some-vendor-name
      match-communication-channel: system-service-cdr

      set-cost-on-call: expected
    }

In this case the cost associated to the service, is the same cost
calculated from the vendor. So there should be some trust that the
vendor is fair. Up to date services are not rated using rate plan, but
using special service definitions, so they can not be double checked on
the reseller side, as in case of normal calls.

This is an example of income rate, for imported services:

::

    rate {
      id: vendor-services
      match-call-direction: system
      match-vendor: some-vendor-name
      match-communication-channel: system-service-cdr

      set-cost-on-call: 0
    }

Imported services has no (usually) cost and income, as in case of normal
calls, because they are not (usually) calculated from rate plans, but
from service definitions. So the income of a service is 0. In case the
service is reselled from the provider, it should be defined in a
distinct way, using the service menu.

Maybe in the future also services will be generated using rate plans,
and so there will be a 1:1 relation ship between services on the
provider side (cost) and on the reseller side (income), like in case of
normal calls.

Communication Channels
~~~~~~~~~~~~~~~~~~~~~~

Communication channels are eported following the settings in this method

::

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

Import CDRs on Reseller Side
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Create a subclass of type ``ImportCDRSFromLocalAsterisellProvider``.
Something like

::

    class MiniTelImportCDRSFromInternationalVoipVendor extends ImportCDRSFromLocalAsterisellProvider
    {

        function getCDRProviderName() {
            return 'international_voip_vendor';
        }
    }

Add the job to the list of jobs, for importing CDRs. Something like

::

     import_cdrs_jobs = [ 'MiniTelImportCDRSFromInternationalVoipVendor' ]

Remote Resellers
~~~~~~~~~~~~~~~~

Up to date it is possible sending files to a remote Reseller, sending
them to a local directory, accessible from the Remote Server using
WebDav protocol:

-  the provider send the files to a local directory
-  the provider share the directory content using WebDAV protocol:

   -  https encription
   -  secret password shared with the reseller

-  the reseller access the webdav resources, using standard curl
   interface

The advantage of this approach are:

-  WebDAV is a standard protocol based on HTTP connections
-  HTTPS encryption encrypt the traffic
-  the traffic can pass through firewalls

WebDAV Configuration on the Server
..................................

You must define in ``instance.py``, something like

::

    server_site = asterisell_http_conf.AsterisellInstanceSite()
    server_site.webdav_users = [('client-code','some-password')]

The installation tool, will create the webdav configurations for you.

If the webdav server is accessible also on a private network address,
because the server and the client reside on the same private network,
you can add for the server the private IP, something like:

::

    server_domain.domain2 = '127.0.0.1'

Inspect the generated configuration files in
``/etc/nginx/asterisell-instances.d`` for the comments about:

-  the directory to create
-  the password file to generate
-  assign the apache user ownership to the directories

::

    chown -R apache:apache /var/opt/asterisell/*

Generate manually the password

::

    # Up to date password file is not automatically generated.
    # Use the reseller code as user name, and a shared password.
    htpasswd -c /etc/nginx/${instance_code}-${webdav_instance}.passwd ${webdav_instance}

WebDav configuration on the client
..................................

Create a Job like this

::

    class FooImportCDRSFromBar extends ImportCDRSFromRemoteAsterisellProvider
    {

        function getCDRProviderName() {
            return 'bar';
        }

    }

Configure something like this

::


      import_cdrs_jobs = [ 'FooImportCDRSFromBar' ]

      def conf_connectionParams(self):
          r = []

          c = asterisell_instance.ConnectionParams()
          c.connection_name = 'bar'
          c.user = 'foo'
          c.password = ''
          c.host = 'https:///get-foo'
          c.port = ''
          r.append(c)

          return r

Current Limitations
~~~~~~~~~~~~~~~~~~~

All these limitations can be removed in next releases of Asterisell. In
case contact the assistance for funding them. Known limitations are:

-  both the provider and the reseller, must use the same precision
   digits
-  only new rated CDRs are sent/resent to a reseller. In case rerate
   CDRs until the reseller configuration is not 100% correct and it is
   receiving all the info
-  every time there is a new call, all the calls of the day are exported
   again to the reseller. In case of many daily calls, this can be a
   problem, because big files must be transferred multiple times in a
   day, and the reseller must calculate them every time
-  CDRs are not transferred immediately from a provider to a reseller,
   and one or two cron job processor passes can be needed. Future
   versions of the application can speedup this, or comunicate when a
   provider signal as ready to be billed the CDRs
