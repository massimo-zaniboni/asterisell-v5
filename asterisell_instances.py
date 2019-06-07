"""
This file contains Asterisell instances to manage.

See the Asterisell manual and comments here, for all details.

The idea it is that Python code can be used for describing
in an human-readable-enough way the params.
Complex chains of providers and resellers can be configured
defining and reusing Python objects.
For normal installation it suffices to change the provided objects.

`fabric_data/lib.py` contains the configurations applied to the host.

`fabfile.py` is the entry point of the tool.
"""

from __future__ import with_statement
from fabric.api import run, cd
from fabric.api import run, local, cd, lcd, put
import os.path
import datetime
from fabric_data.lib import Domain, HttpDomain, SelfSignedDomain, LetsEncryptDomain, Host, \
    Instance, BillingInstance, CallReportingInstance, ConnectionParams

# -------------------------------
# Default instances
#
# In case of simple configurations, simply adapt `Default` settings, instead of creating new ones.

class DefaultHost(Host):
    """An host containing one or more Asterisell instances.
    You can adapt this configuration, or create other Python objects with similar configurations.
    The management utility supports more than one host.

    IMPORTANT: if you change these options, you can upgrade the instance with

    > fab upgrade_conf:HOST/INSTANCE

    and services will use the new configured params.
    """

    name = 'asterisell'
    """
    A unique identifier for the host.
    """

    date_timezone = 'Europe/Rome'
    """ configure using values in
    http://php.net/manual/en/timezones.europe.php.
    NOTE: all the instances on the same server must be on the same date_timezone.
    """

    ssh_addr = 'change-me'
    """ The server address to use for its management.
    """

    ssh_port = '22'
    """ The SSH port used by the management utility.
    """

    db_root_password = 'CHANGE_ME_and_use_autogenerated_complex_password_123'
    """
    The root password of MySQL.
    IMPORTANT: change this value, but insert also number and special chars like "_-#"
    IMPORTANT: this value will be used only during first installation,
    and it is not affected by `upgrade_conf`
    """

    dbms_reserved_ram_in_mb = 400
    """
    For a server with more than 1GB, reserve 50% of the RAM for the DBMS.
    This is the suggested/optimal RAM setting for TokuDB, because it will take advantage both of
    OS buffered files (containing compressed data), and of the reserved RAM (containing uncompressed data).

    In case of scarce RAM, reserve 500MB for the OS (i.e. Asterisell rate engine, PHP op-cache and so on),
    and the rest for the DBMS.

    IMPORTANT: use a proper value, otherwise the DBMS will be slower.
    """

    inno_dbms_reserved_ram_in_mb = 50
    """
    If the host is used only for Asterisell, as usual it should be, then use a very low value,
    because it is still needed frmo MySQL internal. With 0 there is a significative slow down of the DBMS.
    So 50M is a good starting point.
    In case of complex queries done from other applications on INNODB table, increase this value.
    This cache is not shared with dbms_reserved_ram_in_mb used from TokuDB engine.
    """

    generate_http_conf = True
    """
    True if nginx/hppt configurations will be created and managed by Asterisell management utility.
    False for configuring nginx in a manual way.
    """

    firewall_rules = 1
    """
    - 0 for not setting any firewall rule:
      the host administrator had to set them manually, or in the `execute_after_install` method.
    - 1 for opening only HTTP, HTTPS, SSH ports
    - 2 for closing all the ports, except SSH port
      The instance can be accessed using SSH and port forwarding.
    """

    other_nginx_services = ''
    """
    Other nginx settings for services/websites to activate on the same host, in the case (but not suggested)
    there is not only Asterisell on the same server.
    NOTE: if you use `$something` use instead `$$something` because it is a Python template.
    """

    def execute_install_task_post(self, is_initial_install):
        """Code executed after the default installation."""
        pass


class DefaulHttpDomain(HttpDomain):
    """
    A web domain for accessing Asterisell instances, on port http/80, without any encription.
    It can make sense using this type of connection if the instance is accessed from an https proxy,
    providing correct SSL encription.

    In case you can configure the host NGINX in this way:

    server {
      listen 80;
      server_name www.example.net;

      # NOTE: this is needed by Letsencrypt for testing that you are the owner
      # of the server pointed from the DNS entry.
      location /.well-known/acme-challenge {
        root /var/www/letsencrypt;
      }

      location / {
        return 404;
      }
    }

    server {
      listen 443 ssl http2;
      server_name www.example.net;

      ssl on;
      ssl_certificate /etc/letsencrypt/live/www.example.net/fullchain.pem; # managed by Certbot
      ssl_certificate_key /etc/letsencrypt/live/www.example.net/privkey.pem; # managed by Certbot
      include /etc/letsencrypt/options-ssl-nginx.conf; # managed by Certbot
      ssl_dhparam /etc/letsencrypt/ssl-dhparams.pem; # managed by Certbot

      # max upload size
      client_max_body_size 30m;
      client_body_buffer_size 128k;

      location / {
        proxy_pass http://your-internal-ip;
        proxy_redirect off;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto https;
      }
    }

    """

    fully_qualified_domain_name = 'www.example.net'
    """
    A DNS FQDN or the IP address of the server.
    NGINX will listen on this name/address for incoming connections.
    """


class DefaultSelfSignedDomain(SelfSignedDomain):
    """
    A web domain for accessing Asterisell instances.
    A domain can serve one or more Asterisell instances.

    The certificate will be generated by the management utility and self-signed.
    So it is good for testing Asterisell, but not to use in production.
    """

    fully_qualified_domain_name = 'www.example.net'
    """
    A DNS FQDN or the IP address of the server.
    NGINX will listen on this name/address for incoming connections.
    """


class DefaultLetsEncryptDomain(LetsEncryptDomain):
    """
    Same as DefaultSelfSignedDomain, but the certificate is signed using LetsEncrypt free service.
    """

    fully_qualified_domain_name = 'www.example.net'
    """
    The FQDN to register.
    NGINX will listen on this name/address for incoming connection.

    In case it starts with "www." then it will be registered both "www.example.net" and "example.net",
    and it will be created a redirect from "example.net" to "www.example.net".
    """


class DefaultInstance(BillingInstance):
    """This is a template used for defining *your* instances,
    so feel free to adapt it.

    You can improve *incrementally* all settings (also domain name) except instance name, and then new configuration files
    will be re-generated, and services restarted, using:

    > fab upgrade_conf:INSTANCE
    """

    #
    # Installation params
    #

    name = 'billing'
    """The name of the instance and of the database.
       IMPORTANT: after creation the name can not be changed.
    """

    host = DefaultHost()
    """The host where the instance will be installed.
    An host can support more than one instance.
    The host is recognized simply by name, and not by reference, so
    for reusing the same host on different instances you can simply create
    other `DefaultHost()` references.

    Instance name is given by HOST-NAME/INSTANCE-NAME,
    so inside the same host INSTANCE-NAME must be unique,
    but you can reuse the same name in different hosts.
    """

    domain = DefaultSelfSignedDomain()
    """Use some DefaultSelfSignedDomain() or DefaultLetsEncryptDomain().
    """

    url_path = ''
    """Empty string or an URL path to add after the fully qualified domain name.
    Useful for having more than one instance on the same IP address, or domain name.
    """

    start_rating_date = '2017-03-01'
    """When start rating CDRs.
       CDRs before this date are not processed.
       Format: "YYYY-MM-DD" and optionally " hh:mm:ss" """

    database_password = 'CHANGE_ME_some_random_password_123'
    """The MySQL password used for accessing admin instances of Asterisell,
    with write rights on the data.

    IMPORTANT: configure this value correctly, before executing

    > fab install:HOST/INSTANCE

    because then it can not change anymore (or better you had to change it manually)
    """

    user_database_password = 'CHANGE_ME_another_random_password_123'
    """The MySQL password used from customers for viewing (no write rights) their data.

    Use a different password respect `database_password`.

    IMPORTANT: configure this value correctly, before executing

    > fab install:HOST/INSTANCE

    because then it can not change anymore.
    """

    admin_web_password = 'admin'
    """
    The password to use during initial instalation (it can be changed later),
    for the login of the admin web user.
    NOTE: there are no effects if you change this value after initial installation.
    """

    httpd_web_access_password = ''
    """Empty string for production ready web sites.
    An http-auth-protocol password for limiting the access of web-sites to test.
    It is meant only for DEV/TESTING, for hiding the entire web-site (admins and customers)
    behind a global password.
    """

    debug_mode = 0
    """The type of optimizations, profiling and debug options to use.
       * 0: production code with optimizations
       * 10: dev code: no optimizations, enable asserts, so more bugs can be found
       * 11: debug code: no optimizations, enable assert, and stack-trace in case of run-time errors
       * 20: profiling code: optimizations, profile cost of top functions
       * 21: profile code: optimizations, profile cost of all functions (top and inner)
       * 22: profile code: optimizations, profile RAM usage
    """

    customizations_doc = 'CUSTOMIZATIONS.md'
    """A file in ``customizations`` directory, accessible from the HELP menu of the admin instance,
    describing the customizations of this instance.

    IMPORTANT: in case of resellers instances, use distinct files, for not exposing private information to them.
    """

    webdav_users = None
    """A list of tuple user-name, user-password used from resellers for retrieving data.
    The user-name must correspond to the client (reseller) instance name.
    An URL  "server-url/get-{user-name}" will be created.
    The output directory must be kept in synchro with ExportCDRSToReseller job."""

    #
    # Runtime params
    #

    # Leave empty for sending emails to original receivers.
    # Complete with a list of receivers otherwise, using a format like
    #
    # > ['test@example.com', 'test2@example.com']
    #
    # This can be used during application development and testing,
    # for not sending emails to real users, but only to
    # application testers.
    send_emails_to_these_users_instead_of_original_receiver = []

    # The frequency of jobs execution in minutes.
    # Every few minutes new CDRs are imported and rated.
    job_frequency_in_minutes = 5

    #
    # Locale Related Settings
    #

    # the default currency used for rates, cost and incomes.
    #
    # Check if the currency is supported in function,
    #
    # > apps/asterisell/lib/helper/AsterisellHelper.php
    #
    # and if not extend this function.
    #
    # Some valid value: EUR, USD, AUD
    currency = 'EUR'

    # The default culture to use for formatting numbers, dates and so on.
    # Something like en_US, it_IT, ...
    #
    # NOTE: the admin user interface will use all messages in English.
    # Only the simplified customer user interface will be translated.
    #
    # IMPORTANT: if you change this value update also the corresponding
    # "apps/asterisell/config/settings.yml" file in "prod: default_culture".
    # This is not strictly necessary because after login, Asterisell
    # set the culture of the session to this value, but it is
    # a safe setting.
    #
    # culture: en_US
    # culture: it_IT
    # culture: en_US
    culture = 'en_US'

    # The format to use for date/timestamp display in ar_cdr call report.
    # The format is specified using PHP "date" function format:
    #    http://www.php.net/manual/en/function.date.php
    # "r" for something like "Thu, 21 Dec 2000 16:01:07 +0200"
    # "c" for something like "2004-02-12T15:19:21+00:00"
    # "F d, Y G:i:s" for something like "Dec 21, 2000 16:01:07"
    # "d/m/Y, G:i:s" for italian format
    conf_date_format = '"F d, Y G:i:s"'

    # The format to use for date display in Invoices
    #
    # IMPORTANT: Changes to this parameter make effect only on new generated invoices.
    #
    # invoice_date_format: "d/m/Y"  for italian format
    conf_invoice_date_format = '"Y-m-d"'

    # An external telephone number can be masked in the CALL REPORT view,
    # for privacy reasons.
    #
    # This parameter identifies the number of last digits to mask in the
    # external telephone number.
    #
    # A typical value is 3 digits to mask, in order to obtain something
    # like "39059567XXX".
    #
    # 0 stays for no number masking.
    #
    # Internal calls (in the case they are displayed in the
    # CALL REPORT), between two VoIP accounts are no masked.
    #
    # Unprocessed calls are never masked, but they are viewable
    # only from the administrator.
    #
    # IMPORTANT: Changes to this parameter make effect only on new rated calls,
    # so you must force a rerate if you want apply the new mask only to already
    # rated calls.
    conf_mask_for_external_telephone_number = 3

    # true for having invoices with details like:
    # > Italy - Fixed Line        200 calls  ....
    # > Italy - Mobile Line       100 calls  ...
    # (grouped by geographi location and type of operator)
    #
    # false for having invoices with details like:
    # > Fixed Line                 200 calls
    # > Mobile Lines               100 calls
    # (grouped by type of operator.
    #  Maybe you can further create operators like "National Fixed Line",
    # "International Fixed Line" and os on...)
    conf_long_details_in_invoice = True

    # true for using the last day of the month, in invoices of the month,
    # like "from 2011-06-01 to 2011-06-30".
    #
    # false for using the first day of the next month, in invoices of the month,
    # like "from 2011-06-01 to 2011-07-01 (exclusive)".
    #
    # This affects only the displayed call time range,
    # not the range used for calculations that is always the same.
    conf_use_inclusive_end_date_in_invoice = True

    # Set to False for not generating also invoices with zero income.
    conf_generate_invoices_when_total_is_zero = False

    # The decimal separator symbol to use in CSV files exported to customers.
    conf_decimal_separator_symbol_in_csv = '"."'

    # true for exporting numbers like "1.234" in CSV files exported to customers.
    # false for exporting numbers like 1.234 (without \").
    #
    # true is a more safe option of your customers have different locale
    # with different types of decimal separator.
    #
    # false allows recognizing more easily numbers, during importing of CSV files
    # inside a spreadsheet.
    conf_numbers_as_strings_in_csv = True

    # The field separator used in CSV files exported to customers.
    # Normally it is ",".
    #
    # If you are using "," also as decimal separator,
    # and numbers are not surrounded between \", then use something like ";".
    conf_csv_field_separator = '","'

    # decimal places to use for currency when stored in the database ar_cdr table
    #
    # IMPORTANT: if you change this value then force a re-rate of all calls
    # because already rated calls will be in an inconsisten format.
    # So it is better starting with the correct value.
    conf_currency_decimal_places = 5

    # decimal places to use for each currency in an invoice
    #
    # IMPORTANT: Changes to this parameter make effect only on new generated invoices.
    #
    # IMPORTANT: this parameter is also used for rounding
    # costs/incomes in CALL REPORT. Supposing 2 decimal places,
    # a cost like "0.005" is rounded to "0.01".
    conf_currency_decimal_places_in_invoices = 2

    # Max numbers of calls to insert in PDF reports,
    # for each PDF report section (calls of an organization/extension etc..)
    # In this way there can be a limit to the dimension of a PDF report.
    # The user will be informed in this case, that there are more calls.
    #
    # 0 for no limits.
    conf_max_calls_in_pdf_report_section = 1000

    #
    # How Showing the Calls
    #

    # Enable/disable displaying of incoming/incoming/internal
    # calls in the customer call report.
    # This allows to reduce the information overhead.
    #
    # This settings influence also INVOICE GENERATION:
    # if incoming/internal calls are not displayed in the CALL REPORT
    # then they are not displayed / summed up in the invoices,
    # otherwise they are showed / summed up in the invoices.
    #
    # IMPORTANT: if you assign an earn to incoming and internal
    # calls, but they are not displayed in the call report,
    # then these earns are lost because they are not inserted
    # in the invoices. The underline philosophy of Asterisell
    # is that you can account to customers only showed costs
    # in the call report.
    #
    # Ignored and Unprocessed calls are not displayed by default.
    #
    # The administrator can view all these type of calls,
    # in any case, indipendently from these settings.
    #
    manage_outgoing_calls = True
    manage_incoming_calls = True
    manage_internal_calls = False

    # true for showing in the call report, also the cost savings, in case of administrator
    show_cost_saving = False

    # Show in customer call report also the used communication channel for routing the call.
    show_communication_channel_for_customer = False

    # Show in administrator call report also the used communication channel for routing the call.
    show_communication_channel_for_administrator = True

    # False for not showing/filtering on extension_code, but only on extension_name (that can be also a normal code
    # in case...).
    #
    # True for showing extensions like "John Smith (5321)", where "John Smith" is the extension name,
    # and "5321" is the extension code.
    # True for enabling filters both on extension name and extension code.
    conf_show_extension_code = False

    # How many calls shows in every page of the web call reports.
    conf_how_many_calls_in_call_report = 40

    # External telephone number with the specified
    # prefix are displayed in a short form, without
    # the prefix.
    #
    # This option is useful when all your customers
    # reside in the same telephone area. So you can remove
    # the standard/default prefix from called numbers
    # when they are in the standard/default area.
    #
    # All other numbers are not modified.
    #
    # In any case, this option does not change the
    # stored numbers inside the database, only their
    # visualization.
    #
    # Use "-", if you want to disable this feature.
    #
    # IMPORTANT: Changes to this parameter make effect only on new rated calls,
    # so you must force a rerate, if you want apply the new settings
    # also to already rated calls.
    conf_not_displayed_telephone_prefix = '"-"'

    expand_extensions_job = 'NullJob'
    # use 'ExpandExtensions' job for expanding extensions like "123*" into "123456" when
    # specific instances are found in CDRs.
    # Useful for customers having many virtual extensions/DIDS,
    # that you don't want specify, but only discover in CDRs,
    # and at the same time the customers want reports with calls grouped
    # by specific extensions.

    #
    # Email Related Settings
    #
    # You can test them:
    # * configuring the rest of params on Web UI Params
    # * executing on the instange `php asterisell.php debug send-test-email`

    conf_smtp_host = '""'
    conf_smtp_port = 25
    conf_smtp_username = '""'
    conf_smtp_password = '""'

    # "plain" for no encryption, "ssl" for ssl encryption, "tls" for tls encryption.
    conf_smtp_encryption = 'plain'

    # Empty for sending emails using the SMTP connections parameter.
    # A unix command like "/usr/sbin/sendmail -bs for using it instead of other SMTP parameters.
    conf_smtp_command = ''

    # Force a SMTP reconnection after sending the specified number of emails,
    # in order to reduce the stress on the STMP server. Use 0 for no limit.
    conf_smtp_reconnect_after_nr_of_messages = 100

    # Before a new SMTP reconnection, wait the specified number of seconds,
    # in order to reduce the stress on the SMTP server. Use 0 for no pause.
    conf_smtp_seconds_of_pause_after_reconnection = 5

    # Name of custom reports generators class to add,
    # in a format like
    # > ["Report_CompareChannels: \"Compare Channels (User Defined)\""]
    custom_reports = []

    #
    # Frauds and Customer Limits Detection
    #

    # Check cost-limits and CheckFrauds, interval expressed in minutes.
    # The job log shows if the operation is requiring too much time, and in case you can increase the time frame.
    conf_check_cost_limits_after_minutes = 15

    # This param set how often a customer is warned
    # about his calls cost exceeding his cost limit.
    #
    # Every time the user is warned, he will receive info about the current calls cost,
    # so this is also an informative message.
    #
    # 0 for disabling customer advise (only admin is advised).
    #
    # The message sent to customers can be customized,
    # in the file `apps/asterisell/lib/jobs/checks/GenerateMailWarningCustomerForHighCallCost.php`
    conf_repeat_advise_of_high_cost_limit_after_days = 0

    # How to calculate the max cost limit for each customer.
    #
    # "30" for considering the cost of last 30 days.
    # "m" for considering the current month.
    conf_max_cost_limit_timeframe = 30

    # if there are more than these unprocessed calls,
    # then advise the administrator, because there can be unnoticed high calls costs
    conf_check_frauds_max_unprocessed_calls = 10

    # note: customer daily max cost is the customer monthly max cost, specified in the user interface,
    # divided by 30. Set to 0 for disabling it.
    conf_check_frauds_compare_xx_last_days_respect_customer_daily_max_cost = 5

    # warn if a customer spent a 25% more than the maximum cost of last x months in the
    # same day of week and a comparable hour of the day (hours grouped by 00-06, 06-12, 12-18, 18-24).
    # Set to 0 for disabling it.
    conf_check_frauds_compare_x_last_months_respect_current_costs = 3

    # How many concurrent calls, the Asterisk server can support in a safely
    # manner. If this number of concurrent calls is reached,
    # then the administrator is advised.
    #
    # This number depends from the incoming/outgoing bandwidth of the Asterisell
    # server and from the bandwidth reserved/used from each call/connection.
    # It is only an indicative/warning value, used for monitoring when
    # the system reach a warning usage level.
    conf_safe_limit_for_concurrent_calls = 150

    #
    # System Low Level Options
    #

    # The Unix user that must owns the files that must be readable from the web server.
    # It depends from your distrubution:
    #   - www-data for Debian;
    #   - apache for Centos;
    #
    # It is the "User" directive of Apache Web Server configuration files.
    web_server_unix_user = 'apache'

    # Check if there are new files to import, on remote servers, every specified minutes.
    # This time frame it is used also for checking extensions and other settings on remote databases.
    # This setting is not applied to CDRs sources as remote databases and so on.
    conf_check_new_external_files_to_import_after_minutes = 120

    # Copy customer specific files inside `customizations` directory,
    # inside the admin instance.
    #
    # If they are jobs remember to add the jobs also in the scheduler,
    # in their proper position.
    #
    # Use something like
    # > custom_files = {
    # >     'source_file.php': 'some/dest_directory/',
    # >     'another_file.php': 'another/dest_directory/'
    # > }
    custom_files = {}

    # jobs executed for retrieving CDRs to rate. They are executed before the rating related jobs.
    # These jobs are executed only for production instance, and not for DEV instance, that load the data from the archive,
    # and not directly from the source-data.
    import_cdrs_jobs = []

    # jobs executed after importing, and before rating
    custom_initial_always_scheduled_jobs = []

    # jobs executed after rating
    custom_final_always_scheduled_jobs = []

    # jobs executed for exporting rated CDRs to external data-sources
    custom_export_cdrs_jobs = []

    # Custom jobs reacting to events.
    # Executed after the normal always scheduled jobs.
    # These jobs that are activate when there is a compatible event
    # on the job queue.
    #
    # Jobs are iteratively checked for every initial and new event,
    # so if a Job fires new events, they can be immediately processed
    # from other jobs.
    #
    # These jobs are of class JobProcessor.
    custom_jobs = []

    custom_data_file_processors = []

    # If enabled in the rates, CDR having this domain in special fields, will be considered as CDRs on the local network.
    # This option is used from special configured rates, and it is not enabled by default.
    conf_instance_voip_domain = ''

    # A string with the internal_name of an organization (`ar_organization_unit.internal_name`) to ignore.
    # In case a CDR is associated to this billable organization, it will be imported but ignored, i.e.
    # so it will be saved in `ar_source_cdr`, but not in `ar_cdr`.
    # In case this option is changed, after a rerating, the CDRS in the affected rerating time-frame will be put in `ar_cdr` again.
    # This option is meant to be used in this way:
    # - some imported CDRS can be assigned to this organization, by specific code, according some business-rule
    # - these CDRS are automatically ignored
    #
    # An empty string '' for disabling it.
    #
    # DEV-NOTES:
    # - custom code contains business rules about which extension put inside `organization_to_ignore`
    # - because there is a limit on the total chars of extension lists, a child extension should be created, for every extension
    # - in case these business rules changes, and previously ignored extensions, had to be managed, the custom code should be able
    #   to insert a normal extension to rate, and then assign to the previously `ar_organization_unit` some fake extension code,
    #   in order to make it virtually disactivated
    # - rating calls of the past, will use the new extensions
    # - changing this string for an already running application, can have strange effects, because many code works assuming it is a constant
    organization_to_ignore = ''

    # Specify some connection params (passwords, URLs and so on) to use for connecting
    # to external data sources.
    #
    # Keep private passwords on `passwords.ini` file, using `passwords.ini.rename` file as template.
    # This file is in `.gitignore`, and so it is not sent to the Git repo.
    #
    def conf_connection_params(self):
      r = []

      #   # Add a demo password for user `foo` of service `bar`
      #   c = ConnectionParams()
      #   c.connection_name = 'bar'
      #   c.user = 'foo'
      #   c.password = self.get_password_for('bar')
      #   # NOTE: the password is on file `passwords.ini` file
      #   c.host = 'some-host'
      #   c.port = 'some-number'
      #
      #   # NOTE these values are used only from ImportCDRSUsingAppConfs job
      #   # and for connections starting with prefix 'import-remote-cdrs-'
      #   # and they are used for retrieving CDRS from a remote MySQL DBMS.
      #   # For other types of DBMS contact the assistance.
      #
      #   c.dbName = ''
      #   c.tableName = ''
      #   c.provider = ''
      #   c.timeFrameInMinutes = '0'
      #   # '0' for retrieving new data every time the cron processor run
      #
      #   c.dataSourceFormat = ''
      #   c.dataSourceVersion = ''
      #   # TODO list of available formats and versions
      #
      #   c.fromDate = 'YYYY-MM-DD hh:mm:ss'
      #   # empty for importing all data in the instance start_rating_date, otherwise a date in 'YYYY-MM-DD hh:mm:ss' format
      #
      #   c.removeOlderThanDays = '0'
      #   # '0' for not removing any data,
      #   # otherwise CDRS older than specified days are removed from the DB table.
      #
      #   r.append(c)

      # Return the list of all passwords
      return r

# -------------------------------------------------
# Preconfigured instances


class DemoInstance(DefaultInstance):
    """An instance used only for producing DEMO CDRs, and for evaluating the application.
    Do not use this class in production.
    Respect `dev`, this instance uses the optimized engine,
    so it is faster in rating CDRS, but slower during installation."""

    name = "demo"

    host = DefaultHost()
    domain = DefaultSelfSignedDomain()
    url_path = 'demo'
    httpd_web_access_password = ''

    database_password = 'long_MAYBE_safe_root2'
    user_database_password = 'long_MAYBE_safe_root3'

    debug_mode = 0

    culture = 'en_US'
    currency = 'USD'

    conf_repeat_advise_of_high_cost_limit_after_days = 1

    def __init__(self):
        super(DemoInstance, self).__init__()
        self.start_rating_date = self.get_recent_start_rating_date(months_in_the_past=6, begin_day_of_bundle_rates=1)
        # NOTE: leave 6 months in the past, because also the DEMO data generator job assumes somehing of similar

    def execute_install_task_pre_check(self):
        pass

    def execute_install_task_post(self):
        run('php asterisell.php install-demo root ' + self.host.db_root_password)
        run('php asterisell.php run jobs')


class DevInstance(DemoInstance):
    """Install the application with some DEMO data, and in a fast way
    (faster compilation, but slower rating-engine with enabled assertion tests).
    Do not use this class in production. """

    name = "dev"
    host = DefaultHost()
    domain = DefaultSelfSignedDomain()
    url_path = 'dev'

    database_password = 'long_MAYBE_safe_root2'
    user_database_password = 'long_MAYBE_safe_root3'

    debug_mode = 10

    culture = 'en_US'
    currency = 'USD'

    conf_repeat_advise_of_high_cost_limit_after_days = 1

    def __init__(self):
        super(DevInstance, self).__init__()
        self.start_rating_date = self.get_recent_start_rating_date(months_in_the_past=6, begin_day_of_bundle_rates=1)

    def execute_install_task_pre_check(self):
        pass

    def execute_install_task_post(self):
        run('php asterisell.php install-demo root ' + self.host.db_root_password)
        run('php asterisell.php run jobs')


class RegressionTestInstance1(DefaultInstance):
    """Regression tests, with enabled assertions (more checks are done), but no optimizations (code is not like production code)."""

    name = 'regression_tests_1'
    url_path = 'tests1'
    httpd_web_access_password = ''
    host = DefaultHost()
    domain = DefaultSelfSignedDomain()
    database_password = 'long_MAYBE_safe_root2'
    user_database_password = 'long_MAYBE_safe_root3'

    # NOTE: this data is "hard-coded". Do not change them, otherwise tests will fail.

    start_rating_date = '2010-01-01'

    debug_mode = 10
    install_cron_job = False

    deploy_ssh_port = '22300'
    http_bridged_port = '8030'
    httpd_domain1 = '127.0.0.1'

    custom_rates = []
    custom_reports = []
    custom_initial_always_scheduled_jobs = [
    ]
    custom_final_always_scheduled_jobs = [
    ]
    custom_jobs = []
    custom_configure_jobs = [
    ]
    custom_upgrade_jobs = []

    def execute_install_task_pre_check(self):
        pass

    def execute_install_task_post(self):
        with cd(self.get_admin_deploy_directory()):
            d1 = datetime.datetime.strptime(self.start_rating_date, "%Y-%m-%d")
            d2 = (d1 + datetime.timedelta(days= 2 * 31)).replace(day=1)
            d2s = d2.strftime('%Y-%m-%d')

            # Regression tests on customized data
            run('php asterisell.php debug regression-test root ' + self.host.db_root_password)

            # Load demo data and execute stress-rerating on it
            run('php asterisell.php install root ' + self.host.db_root_password)
            run('php asterisell.php install-demo root ' + self.host.db_root_password)
            run('php asterisell.php run jobs')
            run('php asterisell.php data unbilled ' + d2s + ' 00:00:00')
            run('php asterisell.php debug stress-rerating 40 48')


class RegressionTestInstance2(RegressionTestInstance1):
    """Regression tests using optimized code like production instances, but without code assertions."""

    name = 'regression_tests_2'
    url_path = 'tests2'
    debug_mode = 0
    host = DefaultHost()
    domain = DefaultSelfSignedDomain()


# You can add other instances here
# ...
all_instances = [ DemoInstance(),
                  DevInstance(),
                  RegressionTestInstance1(),
                  RegressionTestInstance2(),
                ]

