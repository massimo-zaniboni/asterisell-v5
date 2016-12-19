# Copyright (C) 2014-2016 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
#
# This file is part of Asterisell.
#
# Asterisell is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# Asterisell is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Asterisell. If not, see <http://www.gnu.org/licenses/>.

"""
Define Asterisell instances as Python objects.
In this way Python is used both for configuring instances,
and for managing them.
"""

#
# Constants
#

from __future__ import with_statement
from string import Template
from fabric.api import run, local, cd, lcd, put, settings, task, runs_once
import time
import os.path
import os, sys, stat
import importlib
import fabric.contrib.project
from fabric.utils import abort
import tempfile
import random
import string
import shutil


class ConnectionParams(object):
    """Connection params used for connecting to external databases and servers."""

    connection_name = ''  # type: str
    user = ''  # type: str
    password = ''  # type: str
    host = ''  # type: str
    port = ''  # type: str


class AsterisellInstance(object):
    """ An Asterisell instance.

    Given an instance with name "foo", the installed directories are:
    * "/var/www/foo" as base directory
    * "/var/www/foo/admin" for the instance accessible from administrators, 
      and with all the code and administrator tools
    * "/var/www/foo/user" for the instance accessible from normal users

    The "/var/www/foo/user" instance contains a "config" directory with a database user with limited right access,
    and does not contain the administrator utility.

    The "/var/www/foo/admin" instance is accessible from the web using an address like "www.foo.net/admin".

    Up to date an Asterisell Instance is installed on a Docker container,
    and there is one container for each instance. This simplifies the code
    for specifying and managing instances.
    """

    #
    # Main Settings
    #

    name = ''  # type: str
    active = True  # type: bool

    is_billing = True  # type: bool
    is_aggregate = False  # type: bool
    has_resellers = False  # type: bool

    #
    # Docker Container and Host related info
    #

    deploy_ssh_port = ''  # type: str
    """the host external port, where the container SSH listen for conections"""

    deploy_ssh_user = 'root'
    """the container SSH user. Usually root"""

    unix_job_user = 'root'  # type: str
    """the unix user, used for running the jobs"""

    container_ram_in_mb = 1000  # type: int
    """the RAM used (aproximately) from the container"""

    cpu_cores = 1
    """"the cores of the HOST system, and that can be used from the container."""

    date_timezone = 'Europe/Rome'  # type: string
    """ configue using values in
    http://php.net/manual/en/timezones.europe.php"""

    def get_php_opcache_in_mb(self):
        if self.container_ram_in_mb >= 2000:
            return "200"
        else:
            return "128"

    def complete_host_string(self):
        """A connection string as required from Fabric"""
        return self.deploy_ssh_user + '@127.0.0.1:' + self.deploy_ssh_port

    #
    # Httpd Params
    #

    httpd_domain1 = '127.0.0.1'
    """The served domain name, something like 'www.example.net',
    or an IP address accessible from the external."""

    httpd_domain2 = ''
    """An optional alias domain, something like 'example.net'.
       If there are more than one alias, they can be separated from spaces, according NGINX syntax.
       You can specify also (local) IP addresses."""

    httpd_ssl_certificate = None
    """Set to a file name in case of SSL connection.
    The certificate file must reside on the file system of the hosting system,
    and it will be copied in the container.

    If this param is set, then the container will serve by default a https
    connection, and also an http connection redirecting to the https URL.
    """

    httpd_ssl_key = None
    """Set to a file name in case of SSL connection.
    None for no SSL connections."""

    httpd_ssl_specific_ip = None
    """None if the http server, serves the requests inspecting only the domain.
    A reserved IP if the server accepts only http requests sent to a certain
    IP address, associated into the DNS to the domain.
    Used only for supporting old style HTTPS/SSL connections."""

    http_bridged_port = '80'
    """the port of the hosting server, bridged to the http 80 port of the container.
    If the container contains SSL settings, then connection to this port answer
    with an http redirection to the https protocol.
    """

    https_bridged_port = '443'
    """the port of the hosting server, bridged to the https 443 port of the container.
    This port is bridged only if the container contains SSL settings, otherwise
    the content is ignored."""

    def get_httpd_default_bridged_port(self):
        if self.is_ssl():
            return self.https_bridged_port
        else:
            return self.http_bridged_port

    http_listen_ip_address = None
    """None if the container can listen only requests from the host server.
    In this case the host server must redirect requests to the container using
    a proxy HTTP server.

    '0.0.0.0' if the container has public exposures of the bridged_ports,
    and it is acting directly like an HTTP server.

    Another IP address if the container can listen on all requests network
    requests that can access the specified IP address.
    """

    def docker_port_forwading_options(self):
        if not self.http_listen_ip_address is None:
            p = self.http_listen_ip_address + ':'
        else:
            p = ''

        r = ' -p ' + p + self.http_bridged_port + ':80 '
        if self.is_ssl():
            r = r + ' -p ' + p + self.https_bridged_port + ':443 '

        r = r + ' -p ' + self.deploy_ssh_port + ':22 '

        return r

    httpd_web_access_password = None
    """None for no password. This is not the Asterisell password,
    but an initial website password, used for sites not already in production."""

    webdav_users = None
    """A list of tuple user-name, user-password
    The user-name must correspond to the client instance name
    A server-url/get-{user-name} list of web-dav accessible address will be created.
    The instance can put on these directories the files to export.
    The output directory must be kept in synchro with ExportCDRSToReseller job."""

    #
    # Asterisell Instance Params
    #

    # the frequency of jobs execution in minutes
    job_frequency_in_minutes = 5  # type: int

    # when start a job. Useful for executing resellers not immediately with the main instance.
    job_offset_in_minutes = 0  # type: int

    show_cost_saving = False  # type: bool
    input_queue = ''  # type: str
    aggregate_server_output_queue = ''  # type: string
    manage_outgoing_calls = True  # type: bool
    manage_incoming_calls = False  # type: bool
    manage_internal_calls = False  # type: bool
    show_communication_channel_for_customer = True  # type: bool
    show_communication_channel_for_administrator = True  # type: bool
    culture = ''  # type: str
    currency = ''  # type: str
    contacts = []  # type: List[str]
    database_name = ''  # type: str
    database_username = ''  # type: str
    database_password = ''  # type: str
    user_database_password = ''  # type: str

    # reports common to all instance
    default_reports = []  # type: List[str]

    # instance configurable reports
    custom_reports = []  # type: List[str]

    custom_initial_always_scheduled_jobs = []  # type: List[str]
    custom_final_always_scheduled_jobs = []  # type: List[str]
    custom_data_file_processors = []  # type: List[str]
    custom_export_cdrs_jobs = []  # type: List[str]

    import_cdrs_jobs = []  # type: List[str]
    # jobs executed for retrieving CDRs to rate.
    # They are executed before the rating related jobs.
    # These jobs are executed only for production instance,
    # and not for DEV instance, that load the data from the archive,
    # and not directly from the source-data.

    custom_jobs = []  # type: List[str]
    custom_configure_jobs = []  # type: List[str]
    custom_upgrade_jobs = []  # type: List[str]

    # Upgrade jobs that must be applied to all instances
    default_upgrade_jobs = []  # type: List[str]

    # Some instance related, custom CDR services.
    # It must be a subclass of CustomCDRServices.
    custom_cdr_services = 'CustomCDRServices'  # type: str

    contact_external_hosts = False  # type: bool
    import_extensions_from_ipbx_database = False  # type: bool

    web_server_unix_user = 'apache'  # type: str

    enable_upload_of_files = True  # type: bool

    import_extensions_from = '[]'  # type: List[str]

    # in YYYY-MM-DD format
    start_rating_date = ''  # type: str

    # use a something like '[massimo.zaniboni@gmail.com, ...]' as format
    send_emails_to_these_users_instead_of_original_receiver = '[]'  # type: str

    run_command = 'php asterisell.php run jobs'  # type: str

    there_is_call_flow_merge_phase = False  # type: bool

    conf_show_extension_code = True  # type: bool
    conf_how_many_calls_in_call_report = 40  # type: int
    conf_not_displayed_telephone_prefix = '"-"'  # type: str
    conf_check_new_external_files_to_import_after_minutes = 120
    conf_rate_cdrs_using_chunks_of_consecutive_days = 30
    conf_check_cost_limits_after_minutes = 15
    conf_repeat_advise_of_high_cost_limit_after_days = 0
    conf_max_cost_limit_timeframe = 30
    conf_check_frauds_max_unprocessed_calls = 10
    conf_check_frauds_compare_xx_last_days_respect_customer_daily_max_cost = 5
    conf_check_frauds_compare_x_last_months_respect_current_costs = 3

    conf_safe_limit_for_concurrent_calls = 5
    conf_date_format = '"Y/m/d, G:i:s"'
    conf_invoice_date_format = '"Y-m-d"'
    conf_long_details_in_invoice = True
    conf_use_inclusive_end_date_in_invoice = False
    conf_decimal_separator_symbol_in_csv = '"."'
    conf_numbers_as_strings_in_csv = True
    conf_csv_field_separator = '","'
    conf_currency_decimal_places = 4
    conf_currency_decimal_places_in_invoices = 2
    conf_mask_for_external_telephone_number = 3
    conf_max_calls_in_pdf_report_section = 1000

    conf_smtp_host = ''
    conf_smtp_port = ''
    conf_smtp_username = ''
    conf_smtp_password = ''
    conf_smtp_encryption = ''
    conf_smtp_command = ''
    conf_smtp_reconnect_after_nr_of_messages = 0
    conf_smtp_seconds_of_pause_after_reconnection = 0
    conf_smtp_sender_email_address = ''
    conf_instance_voip_domain = ''

    def module_directory(self):
        """The directory of the current module.
        To use for locating the configuration files.
        This method must be copied also in subclasses, otherwise this module will be used.
        :rtype: str
        """
        return os.path.dirname(os.path.realpath(__file__))

    def conf_connection_params(self):
        """A list of ConnectionParams objects.
        :rtype: list of ConnectionParams
        """
        return []

    #
    # Access Fields
    #

    def get_host_name(self):
        """
        :rtype: str
        :return:
        """
        return self.name

    def get_instance_code(self):
        return self.name

    def get_active(self):
        return self.active

    def get_has_resellers(self):
        return self.has_resellers

    def get_show_cost_saving(self):
        return self.show_cost_saving

    def get_input_queue(self):
        return self.input_queue

    def get_aggregate_server_output_queue(self):
        return self.aggregate_server_output_queue

    def get_manage_outgoing_calls(self):
        return self.manage_outgoing_calls

    def get_manage_incoming_calls(self):
        return self.manage_incoming_calls

    def get_manage_internal_calls(self):
        return self.manage_internal_calls

    def get_show_communication_channel_for_customer(self):
        return self.show_communication_channel_for_customer

    def get_show_communication_channel_for_administrator(self):
        return self.show_communication_channel_for_administrator

    def get_culture(self):
        return self.culture

    def get_currency(self):
        return self.currency

    def get_contacts(self):
        return self.contacts

    def is_docker_container(self):
        """
        :rtype: bool
        :return: true if it is a Docker container
        """
        return True

    def get_docker_container_name(self):
        return self.name

    def get_deploy_ssh_user(self):
        return self.deploy_ssh_user

    def get_web_server_unix_user(self):
        return self.web_server_unix_user

    def get_enable_upload_of_files(self):
        return self.enable_upload_of_files

    def get_database_name(self):
        return self.database_name

    def get_database_username(self):
        return self.database_username

    def get_database_password(self):
        return self.database_password

    def get_user_database_password(self):
        return self.user_database_password

    def get_instance_directory(self, is_admin):
        base_dir = os.path.join('/', 'var', 'www', self.name)
        if is_admin is None:
            return base_dir
        else:
            if is_admin:
                p2 = 'admin'
            else:
                p2 = 'user'
            return os.path.join(base_dir, p2)

    def get_deploy_directory(self):
        return self.get_instance_directory(None)

    def get_admin_deploy_directory(self):
        """The instance accessible from the admin user."""
        return self.get_instance_directory(True)

    def get_user_deploy_directory(self):
        """The instance accessible only from normal users."""
        return self.get_instance_directory(False)

    def get_custom_reports(self):
        r = list(self.default_reports)
        r.extend(self.custom_reports)
        return r

    def get_custom_initial_always_scheduled_jobs(self):
        return self.custom_initial_always_scheduled_jobs

    def get_custom_final_always_scheduled_jobs(self):
        return self.custom_final_always_scheduled_jobs

    def get_custom_jobs(self):
        return self.custom_jobs

    def get_custom_configure_jobs(self):
        return self.custom_configure_jobs

    def get_custom_upgrade_jobs(self):
        r = list(self.default_upgrade_jobs)
        r.extend(self.custom_upgrade_jobs)
        return r

    def get_custom_export_cdrs_jobs(self):
        return self.custom_export_cdrs_jobs

    def get_import_cdrs_jobs(self):
        return self.import_cdrs_jobs

    def get_custom_data_file_processors(self):
        return self.custom_data_file_processors

    def get_start_rating_date(self):
        return self.start_rating_date

    def get_send_emails_to_these_users_instead_of_original_receiver(self):
        return self.send_emails_to_these_users_instead_of_original_receiver

    def get_custom_cdr_services(self):
        return self.custom_cdr_services

    def get_run_command(self):
        return self.run_command

    def get_contact_external_hosts(self):
        return self.contact_external_hosts

    def get_import_extensions_from_ipbx_database(self):
        return self.import_extensions_from_ipbx_database

    def get_import_extensions_from(self):
        return self.import_extensions_from

    def get_there_is_call_flow_merge_phase(self):
        return self.there_is_call_flow_merge_phase

    def module_directory(self):
        """The directory of the current module.
        To use for locating the configuration files.
        This method must be copied also in subclasses, otherwise this module will be used.
        """
        return os.path.dirname(os.path.realpath(__file__))

    def convert_connection_params(self):
        """Convert connection params to proper YAML values"""
        r = ''
        if len(self.conf_connection_params()) == 0:
            r = r + '[]'
        else:
            indent0 = '\n    '
            indent1 = '\n        '

            for c in self.conf_connection_params():
                r = r + indent0 + '-'
                r = r + indent1 + 'name: ' + c.connection_name
                r = r + indent1 + 'user: ' + c.user
                r = r + indent1 + 'password: ' + c.password
                r = r + indent1 + 'host: ' + c.host
                if (len(c.httpd_port)) > 0:
                    r = r + indent1 + 'port: ' + c.httpd_port

        r = r + '\n'
        return r

    #
    # Prepare Docker Container
    #

    def get_docker_image_name(self):
        return "asterisell/centos6"

    def execute_prepare(self):
        """Prepare the HOST for Asterisell instances installations."""  #
        local('mkdir -p ' + os.path.join(self.local_packages_directory(), 'docker'))

        # Copy files
        self.copy_from_docker_template_to_tmp_directory('Dockerfile')
        self.copy_from_docker_template_to_tmp_directory('server.cnf')
        self.copy_from_docker_template_to_tmp_directory('MariaDB.repo')
        self.copy_from_docker_template_to_tmp_directory('nginx.repo')
        self.copy_from_docker_template_to_tmp_directory('set_mysql_password.sh')
        self.copy_from_docker_template_to_tmp_directory('supervisord.conf')
        self.copy_from_docker_template_to_tmp_directory('ragel-6.8.tar.gz')
        self.copy_from_docker_template_to_tmp_directory('rate_engine_packages.tar.gz')

        # Copy the SSH authorization files of the user
        local(
            'cp ' + os.path.expanduser('~/.ssh/id_rsa.pub') + ' ' + os.path.join(self.local_packages_directory(), 'docker',
                                                                                 'id_rsa.pub'))
        # Create the image and run the container.
        with lcd(os.path.join(self.local_packages_directory(), 'docker')):
            local("docker build -t " + self.get_docker_image_name() + " .")
            local("docker run --restart=always -d -P --name " + self.get_docker_container_name()
                  + self.docker_port_forwading_options()
                  + " -v /dev/log:/dev/log "
                  + self.get_docker_image_name()
                  + " /usr/bin/supervisord")

    def copy_from_docker_template_to_tmp_directory(self, file_name):
        source = os.path.join('fabric_data', 'config_files_templates', 'docker', file_name)
        dest = os.path.join(self.local_packages_directory(), 'docker', file_name)
        shutil.copy2(source, dest)

    def authorize_ssh_access(self):
        with cd('/'):
            fabric.operations.put(os.path.expanduser('~/.ssh/id_rsa.pub'), os.path.join('root', 'id_rsa.pub'))
            fabric.operations.run('mkdir /root/.ssh ; cat /root/id_rsa.pub >> /root/.ssh/authorized_keys')

    #
    # Manage Compilations
    #

    # set to true if the host/instances supports also call flow merging
    there_is_call_flow_merge = False

    def asterisell_compilations(self):
        return os.path.join('/', 'var', '/asterisell_compilations/')

    def haskell_rate_engine_dir(self):
        return os.path.join(self.asterisell_compilations(), 'rate_engine')

    def call_flow_merge_dir(self):
        return os.path.join(self.asterisell_compilations(), 'call_flow_merge_tools')

    def send_source_files_to_compilation_env(self):
        # Rate Engine
        with lcd(os.path.join(self.local_source_repo_directory(), 'rating_tools')):
            remote_dir = self.haskell_rate_engine_dir()
            run('mkdir -p ' + remote_dir)
            fabric.contrib.project.rsync_project(delete=True, remote_dir=remote_dir, local_dir='rate_engine/',
                                                 extra_opts=' --exclude=\'.stack-work\'')
            run('cp ' + os.path.join(self.haskell_rate_engine_dir(), 'stack-deploy.yaml') + ' ' + os.path.join(
                self.haskell_rate_engine_dir(), 'stack.yaml'))

        # Call Flow Merge Engine
        if self.there_is_call_flow_merge:
            with lcd(os.path.join(self.local_source_repo_directory(), 'rating_tools')):
                remote_dir = self.call_flow_merge_dir()
                run('mkdir -p ' + remote_dir)
                fabric.contrib.project.rsync_project(delete=True, remote_dir=remote_dir,
                                                     local_dir='call_flow_merge_tools/',
                                                     extra_opts=' --exclude=\'dist/\' ')

    def compile_and_install_development_tools(self, instance_directory, install, use_fast_compile):
        """Compile the rate engine files, and other derived files."""
        self.send_source_files_to_compilation_env()

        # Haskell Rating Engine

        with cd(self.haskell_rate_engine_dir()):
            if not use_fast_compile:
                run('stack setup && stack clean')
            run('stack setup && stack build')
            if install:
                run('cp `stack path --local-install-root`/bin/RateEngine ' + os.path.join(instance_directory, 'scripts',
                                                                                          'RateEngine'))

        # Call Flow Merge Engine

        if self.there_is_call_flow_merge:
            with cd(self.call_flow_merge_dir()):
                if not use_fast_compile:
                    run('rm -f CMakeLists.txt ; rm -f CMakeCache.txt ; rm -r -f CMakeFiles')
                    # NOTE: if this file does not exists, then it is re-generated with correct setting during
                    # compilation phase

                run('php CProceduresGenerator.php init-project ' + self.name)
                # NOTE: in reality the init-project is executed only if there is no already an initializated project,
                # so it is a fast upgrade procedure.

                if not use_fast_compile:
                    run('make clean')

                run('make')

    #
    # Instance Installation Methods
    #

    def get_config_file__databases(self, is_admin):
        """Generate databases.yml config file, and return the content"""

        t = Template(open('fabric_data/config_files_templates/databases.yml').read())
        u = self.get_database_username()
        if is_admin:
            p = self.get_database_password()
        else:
            p = self.get_user_database_password()
            u = "usr_" + u

        s = t.substitute(
            database_name=self.get_database_name(),
            database_username=u,
            database_password=p
        )

        return s

    def get_config_file__app(self):
        """Generate app.yml config file, and return the content. """

        initial_jobs = list(self.get_custom_initial_always_scheduled_jobs())
        initial_jobs.extend(self.get_custom_data_file_processors())

        event_jobs = list(self.get_custom_data_file_processors())
        event_jobs.extend(self.get_custom_jobs())
        # CDR processing jobs must be executed both as fixed job, and event processor

        t = Template(open(os.path.join('fabric_data', 'config_files_templates', 'app.yml')).read())
        s = t.substitute(
            is_aggregate=self.bool_value_str(self.is_aggregate),
            custom_reports_list=self.yaml_list_of_str_values('      ', self.get_custom_reports()),
            import_cdrs_jobs=self.yaml_list_of_str_values('      - ', self.get_import_cdrs_jobs()),
            custom_initial_always_scheduled_jobs=self.yaml_list_of_str_values('      - ', initial_jobs),
            custom_final_always_scheduled_jobs=self.yaml_list_of_str_values('      - ',
                                                                       self.get_custom_final_always_scheduled_jobs()),
            custom_jobs=self.yaml_list_of_str_values('      - ', event_jobs),
            custom_configure_jobs=self.yaml_list_of_str_values('      - ', self.get_custom_configure_jobs()),
            custom_upgrade_jobs=self.yaml_list_of_str_values('      - ', self.get_custom_upgrade_jobs()),
            show_vendor_for_customer=self.bool_value_str(not self.is_billing),
            show_income_and_earn_of_call=self.bool_value_str(self.is_billing),
            show_cost_savings_for_administrator=self.bool_value_str(self.get_show_cost_saving()),
            show_cost_savings_for_customer=self.bool_value_str(
                self.get_show_cost_saving() and (not self.is_billing)),
            show_incoming_calls=self.bool_value_str(self.get_manage_incoming_calls()),
            show_internal_calls=self.bool_value_str(self.get_manage_internal_calls()),
            show_outgoing_calls=self.bool_value_str(self.get_manage_outgoing_calls()),
            show_call_direction=self.show_call_direction_bool_str(),
            show_communication_channel_for_administrator=self.bool_value_str(
                self.get_show_communication_channel_for_administrator()),
            show_communication_channel_for_customer=self.bool_value_str(
                self.get_show_communication_channel_for_customer()),
            cdrs_are_associated_always_to_one_call=self.bool_value_str(not self.is_aggregate),
            currency=self.get_currency(),
            culture=self.get_culture(),
            custom_cdr_services=self.get_custom_cdr_services(),
            is_billing=self.bool_value_str(self.is_billing),
            enable_upload_of_files=self.bool_value_str(self.get_enable_upload_of_files()),
            instance_code_name=self.yaml_string(self.get_instance_code()),
            instance_voip_domain=self.yaml_string(self.conf_instance_voip_domain),
            contact_external_hosts=self.bool_value_str(self.get_contact_external_hosts()),
            import_extensions_from_ipbx_database=self.bool_value_str(self.get_import_extensions_from_ipbx_database()),
            web_server_user=self.get_web_server_unix_user(),
            start_rating_date=self.get_start_rating_date(),
            import_extensions_from=self.get_import_extensions_from(),
            send_emails_to_these_users_instead_of_original_receiver=self.get_send_emails_to_these_users_instead_of_original_receiver(),
            conf_smtp_host=self.yaml_string(self.conf_smtp_host),
            conf_smtp_port=self.yaml_string(self.conf_smtp_port),
            conf_smtp_username=self.yaml_string(self.conf_smtp_username),
            conf_smtp_password=self.yaml_string(self.conf_smtp_password),
            conf_smtp_encryption=self.yaml_string(self.conf_smtp_encryption),
            conf_smtp_command=self.yaml_string(self.conf_smtp_command),
            conf_smtp_reconnect_after_nr_of_messages=self.yaml_string(self.conf_smtp_reconnect_after_nr_of_messages),
            conf_smtp_seconds_of_pause_after_reconnection=self.yaml_string(
                self.conf_smtp_seconds_of_pause_after_reconnection),
            conf_smtp_sender_email_address=self.yaml_string(self.conf_smtp_sender_email_address),
            conf_show_extension_code=self.bool_value_str(self.conf_show_extension_code),
            conf_how_many_calls_in_call_report=self.yaml_string(self.conf_how_many_calls_in_call_report),
            conf_not_displayed_telephone_prefix=self.yaml_string(self.conf_not_displayed_telephone_prefix),
            conf_check_new_external_files_to_import_after_minutes=self.yaml_string(
                self.conf_check_new_external_files_to_import_after_minutes),
            conf_rate_cdrs_using_chunks_of_consecutive_days=self.yaml_string(
                self.conf_rate_cdrs_using_chunks_of_consecutive_days),
            conf_check_cost_limits_after_minutes=self.yaml_string(self.conf_check_cost_limits_after_minutes),
            conf_repeat_advise_of_high_cost_limit_after_days=self.yaml_string(
                self.conf_repeat_advise_of_high_cost_limit_after_days),
            conf_max_cost_limit_timeframe=self.yaml_string(self.conf_max_cost_limit_timeframe),
            conf_check_frauds_max_unprocessed_calls=self.yaml_string(self.conf_check_frauds_max_unprocessed_calls),
            conf_check_frauds_compare_xx_last_days_respect_customer_daily_max_cost=self.yaml_string(
                self.conf_check_frauds_compare_xx_last_days_respect_customer_daily_max_cost),
            conf_check_frauds_compare_x_last_months_respect_current_costs=self.yaml_string(
                self.conf_check_frauds_compare_x_last_months_respect_current_costs),
            conf_safe_limit_for_concurrent_calls=self.yaml_string(self.conf_safe_limit_for_concurrent_calls),
            conf_date_format=self.yaml_string(self.conf_date_format),
            conf_invoice_date_format=self.yaml_string(self.conf_invoice_date_format),
            conf_long_details_in_invoice=self.bool_value_str(self.conf_long_details_in_invoice),
            conf_use_inclusive_end_date_in_invoice=self.bool_value_str(self.conf_use_inclusive_end_date_in_invoice),
            conf_decimal_separator_symbol_in_csv=self.yaml_string(self.conf_decimal_separator_symbol_in_csv),
            conf_numbers_as_strings_in_csv=self.bool_value_str(self.conf_numbers_as_strings_in_csv),
            conf_csv_field_separator=self.yaml_string(self.conf_csv_field_separator),
            conf_currency_decimal_places=self.yaml_string(self.conf_currency_decimal_places),
            conf_currency_decimal_places_in_invoices=self.yaml_string(self.conf_currency_decimal_places_in_invoices),
            conf_mask_for_external_telephone_number=self.yaml_string(self.conf_mask_for_external_telephone_number),
            conf_max_calls_in_pdf_report_section=self.yaml_string(self.conf_max_calls_in_pdf_report_section),
            conf_connectionParams=self.convert_connection_params(),
            custom_export_cdrs_jobs=self.yaml_list_of_str_values('      - ', self.get_custom_export_cdrs_jobs())
        )

        return s

    def generate_config_file__databases(self, is_admin):
        """Generate databases.yml config file."""
        self.send_config_file_to_proper_location(is_admin, 'databases.yml', self.get_config_file__databases(is_admin),
                                                 'config')

    def generate_config_file__app(self):
        """Generate app.yml config file"""
        self.send_config_file_to_proper_location(True, 'app.yml', self.get_config_file__app(), 'apps/asterisell/config')

    def show_call_direction_bool_str(self):
        c = 0

        if self.get_manage_outgoing_calls():
            c = c + 1

        if self.get_manage_incoming_calls():
            c = c + 1

        if self.get_manage_internal_calls():
            c = c + 1

        if c > 1:
            return "true"
        else:
            return "false"

    def get_random_password(self, length):
        """ Source taken from Wikipedia http://en.wikipedia.org/wiki/Random_password_generator.
        Use the system random enthrophy generator, so random numbers are secure."""
        myrg = random.SystemRandom()
        alphabet = string.letters[0:52] + string.digits
        pw = str().join(myrg.choice(alphabet) for _ in range(length))
        return pw

    def execute_install_task_pre_check(self):
        """Execute a test checking if the operation is enabled."""
        if fabric.contrib.files.exists(self.get_admin_deploy_directory()):
            abort("The directory " + self.get_admin_deploy_directory()
                  + " already exists on the remote installation host. "
                  + " Delete it, before installing a new instance of Asterisell with the same name.")

    def execute_install_task_pre(self):
        """Called before the standard code of installation."""
        pass

    def execute_install_task_inner(self):
        """Called inside admin remote directory, during standard code of installation."""
        run('php asterisell.php install')

    def execute_install_task_post(self):
        """Called after the standard code of installation."""
        pass

    def execute_install_task(self):
        self.execute_install_task_pre_check()
        self.execute_install_task_pre()

        # Change DBMS password
        run('mysqladmin -u root -proot password "' + self.database_password + '" || echo "Mantain old MySQL password."')
        run('mysqladmin -u root -p' + self.database_password + ' flush-privileges')

        self.execute_upgrade_task(True, True, True, True)
        with cd(self.get_admin_deploy_directory()):
            self.execute_install_task_inner()
            self.execute_install_task_post()
            self.generate_http_configurations()

    def execute_container_upgrade(self):
        """Upgrade the configuration files of the daemons of the containers.
        After this the container must be restarted."""

        with cd('/'):
            cache_size = str(int(self.container_ram_in_mb / 2)) + 'm'
            run("sed -i \"/tokudb_cache_size/c\\tokudb_cache_size=" + cache_size + "\" /etc/my.cnf.d/server.cnf")
            run("sed -i \"/memory_limit/c\\memory_limit = " + str(self.get_php_opcache_in_mb()) + "M  \" /etc/php.ini")
            run("sed -i \"/date.timezone/c\\date.timezone=" + self.date_timezone.replace("/", "\\/") + "\" /etc/php.ini")

    def execute_upgrade_task(self,
                             fast_upgrade=False,
                             execute_silently_activate=True,
                             execute_with_delete_rsync=True,
                             is_install=False):
        # NOTE: the complete customer/user instance is created from "asterisell.php" management utility
        # during "activate" phase.

        # First execute operations that can be done without acquiring any lock on the production application

        self.compile_and_install_development_tools(self.get_admin_deploy_directory(), False, fast_upgrade)

        # Acquire lock, waiting for the termination of running jobs.
        # An error is issued if the lock can not be obtained after a reasonable amount of time.
        # Note: in case of interruption of upgrade procedure in next steps:
        # - application is still locked
        # - web access is still locked
        # - admin can execute commands on it
        # - a new upgrade command can be executed

        if not is_install:
            with cd(self.get_admin_deploy_directory()):
                run('php asterisell.php cron disable-for-upgrade')
                run('php asterisell.php app disable')
            with cd(self.get_admin_deploy_directory()):
                run('php asterisell.php cron disable-for-upgrade')
                run('php asterisell.php app disable')

        with cd('/'):
            run('mkdir -p ' + self.get_admin_deploy_directory())
            run('mkdir -p ' + self.get_user_deploy_directory())

        self.rsync_files(
            execute_with_delete_rsync,
            self.get_admin_deploy_directory(),
            self.local_source_repo_directory(),
            is_install)

        self.generate_config_file__app()
        self.generate_config_file__databases(True)

        if execute_silently_activate:
            with cd(self.get_admin_deploy_directory()):
                run('php asterisell.php silently-activate')

        self.create_user_instance()
        self.generate_config_file__databases(False)

        # this time the make process is fast because it is already compiled
        self.compile_and_install_development_tools(self.get_admin_deploy_directory(), True, True)

        if not is_install:
            with cd(self.get_admin_deploy_directory()):
                run('php asterisell.php run db-upgrade-jobs')
                run('php asterisell.php install-views-and-procedures')
                run('php asterisell.php run upgrade-jobs')

        self.generate_http_configurations()

        if not is_install:
            with cd(self.get_admin_deploy_directory()):
                run('php asterisell.php app enable')
                run('php asterisell.php cron enable')

        # Install a CRON JOB for executing automatically Asterisell every few minutes
        crontab_filename = '/etc/cron.d/asterisell_' + self.get_instance_code()

        cron_cmd = '# !!! This file is automatically generated and updated from Asterisell Management Tool !!!\n\n0'
        current_time = self.job_frequency_in_minutes + self.job_offset_in_minutes
        while current_time < 60:
            cron_cmd = cron_cmd + ',' + str(current_time)
            current_time = current_time + self.job_frequency_in_minutes
        cron_cmd = cron_cmd + ' * * * * ' + self.unix_job_user + ' sh -c "cd ' + self.get_admin_deploy_directory() + ' && { php asterisell.php run scheduled-jobs > /dev/null 2>&1 || php asterisell.php debug signal-critical-problem-in-the-code > /dev/null 2>&1 ; }"' + "\n"

        local_crontab = 'crontab.tmp'
        f = open(local_crontab, 'w')
        f.write(cron_cmd)
        f.close()
        fabric.operations.put(local_crontab, crontab_filename, mode=0644)
        os.remove(local_crontab)

        # Install log rotate job
        log_rotate_file_name = '/etc/cron.monthly/asterisell_' + self.get_instance_code()
        local_log_rotate = 'logrotate.tmp'
        f = open(local_log_rotate, 'w')
        f.write("#!/bin/sh\n")
        f.write("cd " + self.get_admin_deploy_directory(
        ) + ' && ' + './symfony log:rotate asterisell prod' + "\n")
        f.close()
        fabric.operations.put(local_log_rotate, log_rotate_file_name, mode=0774)
        os.remove(local_log_rotate)

        # Refresh settings restarting supervisord
        run("kill -HUP 1")

    def execute_connect_task(self):
        # open the browser with the instance
        local('xdg-open ' + self.get_web_url(True) + '/ &')
        conn = ' -p ' + self.deploy_ssh_port + ' ' + self.deploy_ssh_user + '@127.0.0.1 '
        cmd = "'cd " + self.get_admin_deploy_directory() + " ; pwd ; exec bash '"
        ssh_cmd = 'ssh -t ' + conn + cmd
        local(ssh_cmd)

    def execute_run_command(self):
        if self.get_active():
            with cd(self.get_admin_deploy_directory()):
                run(self.get_run_command())


    def send_config_file_to_proper_location(self, is_admin, file_name, file_content,
                                            relative_out_directory):
        # write a temporary file
        local(' mkdir -p ' + os.path.join(self.local_packages_directory(), 'tmp'))
        local_file_name = os.path.join(self.local_packages_directory(), 'tmp', file_name)
        f = open(local_file_name, 'w+')
        f.write(file_content)
        f.close()

        # send the file to the real destination
        if is_admin:
            dest = os.path.join(self.get_admin_deploy_directory(), relative_out_directory)
        else:
            dest = os.path.join(self.get_user_deploy_directory(), relative_out_directory)
        fabric.operations.put(local_file_name, dest)

    def create_user_instance(self):
        with cd('/'):
            run('mkdir -p ' + self.get_user_deploy_directory())
            run('chown root:' + self.get_web_server_unix_user() + ' ' + self.get_user_deploy_directory())
            run('chmod u+rwx,g+rx,o-rwx ' + self.get_user_deploy_directory())

            config_dir = os.path.join(self.get_user_deploy_directory(), 'config')
            run('mkdir -p ' + config_dir)

    def create_web_server_private_directory(self, directory_name):
        """Generate a directory, inside the current directory, that is accessible only from the web-server."""
        run(' mkdir -p ' + directory_name)
        run('chown ' + self.get_web_server_unix_user() + ' ' + directory_name)
        run('cinhmod u+rwx,go-rwx ' + directory_name)

    #
    # Configure Httpd
    #

    def get_ssl_cert_dir_on_container(self):
        return os.path.join('/', 'etc', 'pki', 'tls', 'certs')

    def get_ssl_key_dir_on_container(self):
        return os.path.join('/', 'etc', 'pki', 'tls', 'private')

    def get_ssl_cert_file_on_container(self):
        return os.path.join(self.get_ssl_cert_dir_on_container(), self.httpd_ssl_certificate)

    def get_ssl_key_file_on_container(self):
        return os.path.join(self.get_ssl_key_dir_on_container(), self.httpd_ssl_key)

    def generate_http_configurations(self):
        """Generate and reload HTTP configurations."""

        run('mkdir -p /etc/nginx/')
        run('chown -R apache:apache /var/lib/php-fpm/')
        run('chmod 0777 /var/lib/php/session/')

        self.create_remote_file('/etc/nginx', 'nginx.conf', self.create_complete_nginx_conf())

    def get_httpd_td(self):
        return os.path.join('fabric_data', 'config_files_templates', 'nginx')

    def get_httpd_template(self, file_name):
        """:rtype: Template"""
        return Template(open(os.path.join(self.get_httpd_td(), file_name)).read())

    def create_complete_nginx_conf(self):
        if self.is_ssl():
            maybe_ssl = 'ssl'
            port = '443'
        else:
            maybe_ssl = ''
            port = '80'

        if not self.httpd_ssl_specific_ip is None:
            maybe_specific_ip = self.httpd_ssl_specific_ip + ":"
        else:
            maybe_specific_ip = ''

        t = self.get_httpd_template('nginx.conf')
        s = t.substitute(
            cpu_cores=self.cpu_cores,
            maybe_specific_ip=maybe_specific_ip,
            maybe_ssl=maybe_ssl,
            ssl_settings1=self.create_ssl_settings1_conf(),
            ssl_settings2=self.create_ssl_settings2_conf(),
            domain1=self.httpd_domain1,
            domain2=self.httpd_domain2,
            port=port,
            admin_instance=self.create_httpd_instance_conf(True),
            user_instance=self.create_httpd_instance_conf(False)
        )

        return s

    def is_ssl(self):
        if self.httpd_ssl_certificate is None:
            return False
        else:
            return True

    def get_web_url(self, is_admin):
        if is_admin:
            suff = '/admin'
        else:
            suff = ''

        if self.is_ssl():
            pref = 'https://'
        else:
            pref = 'http://'

        return pref + self.httpd_domain1 + ':' + self.get_httpd_default_bridged_port() + suff

    def create_ssl_settings1_conf(self):
        if self.is_ssl():
            t = self.get_httpd_template('ssl_redirect_fragment.conf')
            s = t.substitute(
                domain1=self.httpd_domain1,
                domain2=self.httpd_domain2,
                port='443'
            )
            return s
        else:
            return ''

    def create_ssl_settings2_conf(self):
        if self.is_ssl():
            source_dir = os.path.join('fabric_data', 'ssl_certificates')

            source = os.path.join(source_dir, self.httpd_ssl_certificate)
            dest = self.get_ssl_cert_file_on_container()
            fabric.operations.put(source, dest)

            source = os.path.join(source_dir, self.httpd_ssl_key)
            dest = self.get_ssl_key_file_on_container()
            fabric.operations.put(source, dest)

            t = self.get_httpd_template('ssl_certificates_fragment.conf')
            s = t.substitute(
                domain1=self.httpd_domain1,
                domain2=self.httpd_domain2,
                port='443',
                ssl_certificate=self.get_ssl_cert_file_on_container(),
                ssl_certificate_key=self.get_ssl_key_file_on_container()
            )
            return s
        else:
            return ''

    def get_http_conf_directory(self):
        return os.path.join('/', 'etc', 'nginx')

    def get_http_password_file_name(self):
        return os.path.join(self.get_http_conf_directory()) + '.passwd'

    def get_webdav_password_file_name(self):
        return os.path.join(self.get_http_conf_directory(),
                            'webdav_' + self.instance_code) + '.passwd'

    def create_httpd_instance_conf(self, is_admin):
        """The httpd configuration for each enabled instance."""

        if not is_admin:
            base_path = ''
            base_path_with_slash = ''
            maybe_slash = ''
        else:
            base_path = 'admin'
            base_path_with_slash = base_path + '/'
            maybe_slash = '/'

        if self.httpd_web_access_password is None:
            maybe_authentication_conf1 = ''
        else:
            maybe_authentication_conf1 = "auth_basic \"Authentication Required\";\nauth_basic_user_file " \
                                         + self.get_http_password_file_name() + ";\n"

        if self.is_ssl():
            fastcgi_param = "fastcgi_param HTTPS \"on\";\nfastcgi_param HTTP_SCHEME https;"
        else:
            fastcgi_param = "fastcgi_param HTTPS \"off\";\nfastcgi_param HTTP_SCHEME http;"

        if is_admin:
            t = self.get_httpd_template('no_root_url_fragment.conf')
        else:
            t = self.get_httpd_template('root_url_fragment.conf')

        s = t.substitute(
            maybe_slash=maybe_slash,
            base_url_path=base_path,
            base_url_path_with_slash=base_path_with_slash,
            maybe_authentication_conf=maybe_authentication_conf1,
            instance_directory=self.get_instance_directory(is_admin),
            fastcgi_param=fastcgi_param
        )

        w = ''
        if not self.webdav_users is None:
            template_webdav = self.get_httpd_template('webdav_fragment.conf')

            for webdav_user in self.webdav_users:
                (webdav_instance, webdav_password) = webdav_user
                t = template_webdav.substitute(
                    maybe_slash=maybe_slash,
                    base_url_path=base_path,
                    base_url_path_with_slash=base_path_with_slash,
                    instance_code = self.name,
                    webdav_instance=webdav_instance
                    )
                w = w + t

        return w + s

    #
    # Utility Functions
    #

    def create_remote_file(self, out_directory, file_name, file_content):
        """Create a remote file, with the specified content."""

        # write a temporary file
        local(' mkdir -p ' + os.path.join(self.local_packages_directory(), 'tmp'))
        local_file_name = os.path.join(self.local_packages_directory(), 'tmp', file_name)
        f = open(local_file_name, 'w')
        f.write(file_content)
        f.close()

        run("mkdir -p \"" + out_directory + "\"")
        dest = os.path.join(out_directory, file_name)
        fabric.operations.put(local_file_name, dest)

    def local_packages_directory(self):
        d=os.path.join(os.path.abspath('.'), 'fabric_data', 'work_dir', 'tmp')
        if not os.path.isdir(d):
            local("mkdir -p " + d)
        return d

    def local_source_repo_directory(self):
        return os.path.abspath('.')

    def files_to_exclude_during_upgrade(self):
        """During upgrade there is some info that is instance or provider specific.
        This info is by default excluded from repo and other upgrade methods.
        It is added explicitely in other passages.
        """

        return [
            '/.git/*',
            '/.idea',
            '/tmp',
            '/cache',
            '/.fslckout',
            '/.gitignore',
            '/fabfile.py',
            '/fabfile.pyc',
            '/fabric_data/*',
            '/web/uploads/assets/*',
            'provider_specific/*',
            '/REMOVE_ME_FOR_ENABLING_ASTERISELL_APP',
            '*~',
            '*.pyc'
        ]
    
    def files_to_exclude_as_rsync_rule_format(self, is_upgrade):
        r = ''

        # exclude specific files
        for f in self.files_to_exclude_during_upgrade():
            r = r + ' --exclude=\'' + f + '\' '

        if is_upgrade:
            r = r + ' --exclude=\'data_files\''

        r = r + ' '
        return r

    def rsync_files(self, p_execute_with_delete_rsync, p_remote_dir, p_local_dir, p_is_install):

        if len(p_remote_dir.strip()) == 0:
            raise NameError('Dangerous rsync directory')

        if len(p_local_dir.strip()) == 0:
            raise NameError('Dangerous rsync directory')

        fabric.contrib.project.rsync_project(
            delete=p_execute_with_delete_rsync,
            remote_dir=p_remote_dir,
            local_dir=p_local_dir + '/',
            extra_opts=self.files_to_exclude_as_rsync_rule_format(not p_is_install)
        )


    def yaml_string(self, s):
        """Get the string or \"\" in case of empty string."""
        if s is None:
            return "\"\""
        elif type(s) == type(str()):
            if len(s.strip()) == 0:
                return "\"\""

        return s

    def bool_value_str(self, b):
        if b:
            return "true"
        else:
            return "false"

    def yaml_complete_or_empty_list_of_str_values(self, indent_str, values):
        if len(values) == 0:
            return '[]'
        else:
            return self.yaml_list_of_str_values(indent_str, values)

    def yaml_list_of_str_values(self, indent_str, values):
        r = ''
        one_value = False
        for value in values:
            r = r + "\n" + indent_str + value
            one_value = True
        if one_value:
            r = r + "\n"

        return r


class BillingInstance(AsterisellInstance):
    """Instance used for billing calls."""

    is_billing = True
    is_aggregate = False

    custom_configure_jobs = ['InitDefaultReportsForVoIPReseller']

    default_reports = [
        'PlaceHolderForInvoiceNumeration: "Placeholder for Invoice Numeration"',
        'GenerateLegalInvoiceUsingTemplate01PDF: "Invoice using Default PDF Template"',
        'GenerateNoLegalInvoiceUsingTemplate01PDF: "Call Report Notes, using a pseudo invoice PDF Template"'
    ]


class CallReportingInstance(AsterisellInstance):
    """Instance used for reporting the calls, without reselling them."""

    is_billing = False
    is_aggregate = False

    custom_configure_jobs = ['InitDefaultReportsForCallReporting']

