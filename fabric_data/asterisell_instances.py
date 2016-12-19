"""
Asterisell Managed/Configured Instances

Every instance run on a distinct Docker Container.

The Docker container is created according the content of

  fabric_data/config_files_templates/docker/Dockerfile

You can change it, also if it is not suggested.

NOTE: Docker is not used in the suggested way, because all the services
(database, http server, and so on) run on the same container.

The container is accessed from Fabric management tool using an ssh server.

Logs are regularly rotated.

Backups are managed from Asterisell jobs.

Every instance is configured using a Python class. The hope is that
Python is simple and readable enough, for being used like a configuration
language. Python classes are used as a template for defining objects
with common configurations.

This file is already configured with some DEMO instance.
"""

from __future__ import with_statement
from fabric.api import run, cd
import os.path
import datetime
import lib

class InstanceTemplate(lib.BillingInstance):
# USE INSTEAD:
# class InstanceTemplate(lib.CallReportingInstance)
# if you want a CallReportingInstance

    """This is a template used for defining *your* instances,
    so feel free to adapt it.

    You can improve *incrementally* many settings, and then performing
    a normal application upgrade for applying them:

    > fab upgrade:INSTANCE

    or for system related params

    > fab restart:INSTANCE

    The params that must be specified correctly from the beginning,
    or require a `restart` are marked with the `IMPORTANT` annotation.
    All other params can be updated with a simple

    > fab upgrade:INSTANCE
    """

    #
    # Main Settings
    #

    name = 'billing'
    """The name of the instance, of the Docker container, and of the database.
       IMPORTANT: after creation the name can not be changed."""

    active = True
    """False if the instance is not anymore processing new CDRs."""

    start_rating_date = '2015-01-01'
    """When start rating CDRs.
       CDRs before this date are not processed.
       Format: "YYYY-MM-DD" """

    database_password = 'some_random_password'
    """The MySQL password used for accessing admin instances of Asterisell,
    with write rights.
    This password is also used for the `root` user of MySQL.
    During first installation it will be prompted.

    IMPORTANT: configure this value correctly, before executing
    > fab install:INSTANCE"""

    user_database_password = 'another_random_password'
    """The passwords to use normal users instances.
    They will have only read rights, and not write access
    to the database.
    Use a different password respect `database_password`.

    IMPORTANT: configure this value correctly, before executing
    > fab install:INSTANCE"""

    #
    # Docker Container Params
    #

    deploy_ssh_port = '22000'
    """The host external port, where the container SSH listen for connections.
    It must be a distinct and free port number.

    IMPORTANT: after creation this value can not be changed."""

    container_ram_in_mb = 1000
    """The RAM used (aproximately) from the container.
       1GB is good. 2GB optimal.

    IMPORTANT: `fab restart:INSTANCE` required if you change this value."""

    cpu_cores = 1
    """"The cores of the HOST system, and that can be used from the container.
        1 is good. 4 is optimal.

    IMPORTANT: `fab restart:INSTANCE` required if you change this value."""

    date_timezone = 'Europe/Rome'
    """ configure using values in http://php.net/manual/en/timezones.europe.php.

    IMPORTANT: `fab restart:INSTANCE` required if you change this value."""


    #
    # Httpd Params
    #

    httpd_domain1 = '127.0.0.1'
    """The served domain name, or an IP address accessible from the external.
    Something like 'www.example.net'

    IMPORTANT: `fab restart:INSTANCE` required if you change this value."""


    httpd_domain2 = ''
    """An optional alias domain, something like 'example.net'.
    If there are more than one alias, they can be separated from spaces.
    You can specify also (local) IP addresses.
    You can also leave empty ('') if there are no aliases.

    IMPORTANT: `fab restart:INSTANCE` required if you change this value."""


    httpd_ssl_certificate = None
    """Set to a file name in case of SSL connection.
    None for using http connections.

    The certificate file must be put in directory

    `fabric_data/ssl_certificates/`

    and it must end in `.crt`. In this way it will never be
    committed to the Git repo.

    It will be copied on the container, in the proper `/etc` directory.

    In case of SSL Certificate Chain, Nginx needs a certificate
    with first the server certificate and then the intermediate certificate:
    > cat asterisell_server.crt intermediate.crt > asterisell_server.chained.crt

    If this param is set, then the container will serve by default a https
    connection, and one http connection redirecting to the https URL.

    IMPORTANT: `fab restart:INSTANCE` is required if you change this value."""

    httpd_ssl_key = None
    """Set to a file name in case of SSL connection.
    None for no SSL connections.

    The certificate file must be put in directory

    `fabric_data/ssl_certificates/`

    and it must end in `.key`. In this way it will never be
    committed to the Git repo.

    It will be copied on the container, in the proper `/etc` directory.

    IMPORTANT: `fab restart:INSTANCE` is required if you change this value."""


    httpd_ssl_specific_ip = None
    """None if the http server, serves the requests inspecting only the domain.
    A reserved IP if the server accepts only http requests sent to a certain
    IP address, associated into the DNS to the domain.
    Used only for supporting old style HTTPS/SSL connections.

    IMPORTANT: `fab restart:INSTANCE` required if you change this value."""


    http_bridged_port = '8000'
    """the port of the hosting server, bridged to the http 80 port of the container.
    So connection on this port of the host, are redirected to the 80 port of the container.

    It must be a free port on the host.

    If the container contains SSL settings, then connection to this port answer
    with an http redirection to the https protocol.

    It must be a value from 0 to 65536.

    IMPORTANT: after creation this value can not be changed."""

    https_bridged_port = '44300'
    """the port of the hosting server, bridged to the https 443 port of the container.
    So connection on this port of the host, are redirected to the 443 port of the container.

    It must be a free port on the host.

    This port is bridged only if the container contains SSL settings, otherwise
    the content is ignored.

    It must be a value from 0 to 65536.

    IMPORTANT: after creation this value can not be changed."""

    http_listen_ip_address = None
    """None if the container can listen only requests from the host server.
    In this case the host server must redirect requests to the container using
    a proxy HTTP server.

    '0.0.0.0' if the container can expose the port to the external network connections,
    and it is acting directly like an HTTP server.

    Another IP address if the container can listen on all requests
    that can access this specified IP address.

    IMPORTANT: after creation this value can not be changed."""

    httpd_web_access_password = None
    """None for no password.
    A string like 'some-password' for requesting an initial password
    before accessing the website.

    This is not the Asterisell password,
    but an initial website password, used for sites not already in production."""

    webdav_users = None
    """A list of tuple user-name, user-password.
    The user-name must correspond to the client instance name
    A server-url/get-{user-name} list of web-dav accessible address will be created.
    The instance can put on these directories the files to export.
    The output directory must be kept in synchro with ExportCDRSToReseller job."""

    #
    # Instance Params
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
    currency = 'USD'

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
    conf_date_format = '"Y/m/d, G:i:s"'

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
    conf_use_inclusive_end_date_in_invoice = False

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
    conf_currency_decimal_places = 4

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
    manage_incoming_calls = False
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

    #
    # Email Related Settings
    #

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

    # the email address to use for sending emails
    # NOTE: the name used for the sender_email_address is specified on the online parameter of the application
    conf_sender_email_address = ''

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
    conf_safe_limit_for_concurrent_calls = 5

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

    custom_reports = []

    custom_data_file_processors = []

    # If enabled in the rates, CDR having this domain in special fields, will be considered as CDRs on the local network.
    # This option is used from special configured rates, and it is not enabled by default.
    conf_instance_voip_domain = ''

    #
    # Do Not Touch
    #

    def module_directory(self):
        """The directory of the current module. To use for locating the configuration files. This method must be copied also in subclasses, otherwise this module will be used."""
        return os.path.dirname(os.path.realpath(__file__))

    def __init__(self):
        self.database_name = self.name
        self.database_username = self.name

#
# Classes used only for Demo Instances (do not touch)
#


class DemoInstance(lib.BillingInstance):
    """An instance used only for producing DEMO CDRs to show initially.
    Do not use this class in production. """

    name = "demo"

    deploy_ssh_port = '22200'
    http_bridged_port = '8020'
    httpd_domain1 = '127.0.0.1'

    database_name = 'asterisell_demo'
    database_username = 'asterisell_demo'
    database_password = 'root'
    user_database_password = 'root'

    culture = 'en_US'
    currency = 'USD'

    conf_repeat_advise_of_high_cost_limit_after_days = 1

    def __init__(self):
        self.start_rating_date = self.get_near_start_rating_date()

    def execute_install_task_pre_check(self):
        pass

    def execute_install_task_inner(self):
        run('php asterisell.php install-demo ')
        run('php asterisell.php run jobs')

    def execute_install_task_post(self):
        pass

    def get_near_start_rating_date(self):
        """Support functions for generating DEMO data"""
        d = datetime.date.today() - datetime.timedelta(days=30 * 6)
        return d.isoformat()


class RegressionTestInstance(lib.BillingInstance):
    """Used only for regression tests."""

    name = 'regressiontests'

    deploy_ssh_port = '22300'
    http_bridged_port = '8030'
    httpd_domain1 = '127.0.0.1'

    database_name = 'asterisell_regressiontests'
    database_username = 'asterisell_regressiontests'
    database_password = 'root'
    user_database_password = 'root'

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
    start_rating_date = '2010-01-01'
    custom_cdr_services = 'CustomCDRServices'

    def execute_install_task_pre_check(self):
        pass

    def execute_install_task_post(self):
        with cd(self.get_admin_deploy_directory()):
            run('php asterisell.php debug regression-test')

#
# Installable Instances
#

class Billing(InstanceTemplate):
    """A specific instance.
    Some fields can have different values respect the base template.
    """

    name = 'billing'

    deploy_ssh_port = '22100'
    http_bridged_port = '8100'

    database_password = 'some_password'
    user_database_password = 'another_password'

# You can add other instances here
# ...

# Specify here all the installable instances.
all_instances = [Billing(),
                 DemoInstance(),
                 RegressionTestInstance()
                ]
