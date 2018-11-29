
# SPDX-License-Identifier: GPL-3.0-or-later

"""
Define Asterisell instances as Python objects.
In this way Python is used both for configuring instances,
and for managing them.

Features/high-level contracts:
- I1) it supports multiple instances on the same host
- I2) it supports multiple instances on the same FQDN, but using different `/instance-name`
- I3) it supports a default root instance on a FQDN, and many test instances on the same domain
- I4) not all defined instances are installed and activated on a server (regression tests, and so on)
- I5) an instance can be removed
- C1) it creates all configuration files (at least for 90% common usage-scenario)
- C2) it installs on a fresh CentOS 7
- C3) if the installation abort for some reason, it can be redone, and it will complete
- C4) it can upgrade already installed instances, to new configurations, if they improve
- C5) it does not reset (loosing data) an already installed instance
- D1) it allows the registration of domain names using letsencrypt and locally generated
- D2) it does not repeat the registration of domain-names
- D3) it supports the switch from a self registered domain to a letsencrypt domain
- D4) it recognizes when domains change, and can register new domains

High level design:
- put into /var/opt/asterisell of the targen host, special files indicating the status of each installation phase
- generate configuration files (nginx, cron, mysql) according all installed instances, defined on the same host
- do not generate configuration files of defined instances, but not already installed
- installation is an explicit operation

"""

#
# Constants
#

from __future__ import with_statement
from string import Template
from fabric.api import run, local, cd, lcd, put
import os.path
import os
import datetime
import fabric.contrib.project
from fabric.contrib import files
from fabric.utils import abort
import random
import string
import ConfigParser


# ---------------------------
# Fabric utilities

def run_yum_install(packages, options=''):
    """Install or update packages."""
    run('(yum install -y ' + options + ' ' + packages + ') || (yum update -y ' + options + ' ' + packages + ')')


def put_template(out_file_name, template_content, **kws):
    try:
        t = Template(template_content)
        s = t.substitute(kws)

        # write a temporary file on a directory owned only by Docker management utility
        local_file_name = os.path.join('/', 'var', 'tmp', 'asterisell_config_file')
        f = open(local_file_name, 'w+')
        f.write(s)
        f.close()

        put(local_file_name, out_file_name)
    except Exception as e:
        print(e)
        print template_content
        raise e

# ---------------------------


class ConnectionParams(object):
    """Connection params used for connecting to external databases and servers.

    DEV-NOTE: if you add a parameter here, the synchro to make to the code are:
    * add a note on `../asterisell_instances.py.DefaultInstance.default_connection_params`
    * update `convert_connection_params` function, for saving the data on `app.yml` of the instances
    * update job `ImportCDRSUsingAppConfs` for recognizing the param and sending it to Haskell rating engine
    * update `AsterisellHelper.getConnectionParams`
    * extend the Haskell rating engine
    * ok it is crazy... :-)
      The justification it is that we have a Fabric configuration tool, speaking to instances in PHP,
      telling to an Haskell engine what to do, and all passing to YAML configuration files as bridge.
    """

    connection_name = ''

    user = ''

    password = ''

    host = ''

    port = ''

    dbName = ''
    # NOTE: supported by `ImportCDRSUsingAppConfs`

    tableName = ''
    # NOTE: supported by `ImportCDRSUsingAppConfs`

    provider = ''
    # NOTE: supported by `ImportCDRSUsingAppConfs`

    timeFrameInMinutes = '0'
    # NOTE: supported by `ImportCDRSUsingAppConfs`

    dataSourceFormat = ''
    # NOTE: supported by `ImportCDRSUsingAppConfs`

    dataSourceVersion = ''
    # NOTE: supported by `ImportCDRSUsingAppConfs`

    fromDate = ''
    # NOTE: supported by `ImportCDRSUsingAppConfs`

    removeOlderThanDays = '0'
    # NOTE: supported by `ImportCDRSUsingAppConfs`


# ---------------------------


class Host(object):
    """See `asterisell_instances.py` for documentation. """

    name = 'asterisell'

    ssh_addr = None

    ssh_user = 'root'

    ssh_port = '22'

    db_root_password = None

    dbms_reserved_ram_in_mb = 1000

    generate_http_conf = True

    date_timezone = 'Europe/Rome'
    """ configue using values in
    http://php.net/manual/en/timezones.europe.php.
    NOTE: all the instances on the same server must be on the same date_timezone.
    """

    firewall_rules = 1

    def execute_install_task_post(self, is_initial_install):
        """Code executed after the default installation."""
        pass

    def get_web_server_unix_user(self):
        return 'apache'

    def get_unix_job_user(self):
        return self.get_web_server_unix_user()

    def get_php_opcache_in_mb(self):
        if self.dbms_reserved_ram_in_mb >= 600:
            return "200"
        else:
            return "128"

    def complete_host_string(self):
        """A connection string as required from Fabric"""
        return self.ssh_user + '@' + self.ssh_addr + ':' + self.ssh_port

    def get_succesfull_install_file_name(self):
        return '/var/opt/asterisell/SUCCESSFUL_HOST_INSTALL.chk'

    def get_succesfull_install_version(self):
        return '/var/opt/asterisell/host_version_01.chk'
        # DEV-NOTE: increase this number for forcing installation of new packages

    def is_already_installed(self):
        return files.exists(self.get_succesfull_install_file_name())

    def is_same_version(self):
        return files.exists(self.get_succesfull_install_version())

    def execute_install(self):
        self.execute_update(not self.is_already_installed(), self.is_same_version(), False)

    def execute_update(self, is_initial_install, is_same_version, is_fast_upgrade):
        with cd('/root'):

            if is_initial_install:
                # Disable transparent huge pages because they do not play nices with TokuDB and DBMS in general
                put_template('/etc/rc.local', """
#!/bin/sh
#
# This script will be executed *after* all the other init scripts.
# You can put your own initialization stuff in here if you don't
# want to do the full Sys V style init stuff.

touch /var/lock/subsys/local

if test -f /sys/kernel/mm/transparent_hugepage/enabled; then
  echo never > /sys/kernel/mm/transparent_hugepage/enabled
fi
if test -f /sys/kernel/mm/transparent_hugepage/defrag; then
  echo never > /sys/kernel/mm/transparent_hugepage/defrag
fi

exit 0
"""
                             )

                run('chmod u+x /etc/rc.local')
                run('/etc/rc.local')

                # Disable SE Linux because TokuDB requires this
                run('setenforce 0; true')
                run('sed -i "/SELINUX=enforcing/c\\SELINUX=disabled" /etc/selinux/config ; true')
                run('sed -i "/SELINUX=permissive/c\\SELINUX=disabled" /etc/selinux/config ; true')

                # REMI is a repository with last versions of PHP.
                # EPEL is a repository with extra packages.
                # They are both widely used and tested.
                run_yum_install('https://dl.fedoraproject.org/pub/epel/epel-release-latest-7.noarch.rpm')
                run_yum_install('http://rpms.remirepo.net/enterprise/remi-release-7.rpm')
                run_yum_install('yum-utils')
                run('yum-config-manager --enable remi-php56')

                # Install standard packages, and update according REMI suggestions.
                run('yum -y groupinstall Base')

                # Install Asterisell needed packages.
                run_yum_install("""git awk sqlite sqlite-devel rsync \
                                 openssh-server firewalld \
                                 libXpm libvpx libt1 libexslt libxslt \
                                 libfreetype apr apr-util mailcap \
                                 php php-mysqlnd php-pdo php-cli php-common php-opcache php-bcmath \
                                 php-xml php-mbstring php-gd php-fpm \
                                 mingw32-iconv gmp lftp php-gd php-xml \
                                 freetype gnutls pv recode \
                                 httpd httpd-tools curl vim wget htop screen \
                                 mingw-filesystem-base mingw32-crt mingw32-filesystem \
                                 nettle openssl openssl-devel git \
                                 clips clips-devel clips-libs \
                                 gmp gmp-devel libffi zlib xz tar gnupg \
                                 php-pear php-pecl-jsonc php-process \
                                 certbot-nginx \
                                 t1lib trousers""")

            # Firewall rules
            if self.firewall_rules == 1:
                run("""
                    firewall-cmd --zone=public --add-service=http --permanent
                    firewall-cmd --zone=public --add-service=https --permanent
                    firewall-cmd --zone=public --add-service=ssh --permanent
                    systemctl enable --now firewalld.service && \
                    systemctl reload firewalld.service
                    """)
            elif self.firewall_rules == 2:
                run("""
                    firewall-cmd --zone=public --remove-service=http --permanent
                    firewall-cmd --zone=public --remove-service=https --permanent
                    firewall-cmd --zone=public --add-service=ssh --permanent
                    systemctl enable --now firewalld.service && \
                    systemctl reload firewalld.service
                    """)

            if is_initial_install:
                run('systemctl disable --now httpd')
                # NOTE: httpd is required for the "apache" user

                # Use NGINX version directly packaged from NGINX web site,
                # because it is a lot more recent than CentOS version.
                run('rm -f nginx_signing.key ; wget http://nginx.org/keys/nginx_signing.key && rpm --import nginx_signing.key')
                put_template('/etc/yum.repos.d/nginx.repo', """
[nginx]
name=nginx repo
baseurl=http://nginx.org/packages/centos/7/$$basearch/
gpgcheck=1
enabled=1
"""
                             )

                run_yum_install('nginx', options ='--nogpgcheck')

                # Percona Server for MySQL is the owner of TokuDB engine, so I prefer this respect MariaDB.
                run_yum_install('http://www.percona.com/downloads/percona-release/redhat/0.1-6/percona-release-0.1-6.noarch.rpm')
                run_yum_install("""jemalloc \
                                 Percona-Server-client-57 \
                                 Percona-Server-devel-57  \
                                 Percona-Server-tokudb-57""")

                # Configure password and enable TokuDB
                run('rm -f /etc/my.cnf.d/asterisell.cnf')
                put_template('/etc/sysconfig/mysql', """
LD_PRELOAD=/usr/lib64/libjemalloc.so.1
            """
                             )
                run(""" systemctl set-environment MYSQLD_OPTS="--skip-grant-tables --skip-networking" && \
                        systemctl restart mysqld.service
                    """)
                run("mysql -uroot mysql -e 'UPDATE user SET authentication_string=PASSWORD(\""
                    + self.db_root_password
                    + "\") WHERE User=\"root\";'"
                    )
                run('systemctl unset-environment MYSQLD_OPTS')
                run('systemctl restart mysqld.service')
                run ("mysql -uroot -p"
                     + self.db_root_password
                     + " --connect-expired-password mysql "
                     + "-e \"ALTER USER 'root'@'localhost' IDENTIFIED BY '"
                     + self.db_root_password + "'\"")
                run ("mysql -uroot -p"
                     + self.db_root_password
                     + " --connect-expired-password mysql "
                     + " -e \"FLUSH PRIVILEGES\"")
                run("ps-admin -uroot -p" + self.db_root_password + " --enable-tokudb")

            if not is_initial_install:
                if not is_fast_upgrade:
                    run('yum update -y ')
                    # NOTE: not done during initial install, because it can signal conflicts

            # Update the configurations and start the service
            mysql_conf = '/etc/my.cnf.d/asterisell.cnf'
            put_template(mysql_conf, """
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #
# !!!                                                                           !!! #
# !!! I M P O R T A N T:                                                        !!! #
# !!!                                                                           !!! #
# !!! This file is generated from management tools                              !!! #
# !!! Do not change the content of this file, otherwise it will be overwritten. !!! #
# !!!                                                                           !!! #
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #

[mysqld]

validate_password_policy=LOW

# Accepts only local connections
bind-address = 127.0.0.1

# Set to a low value,
# because Asterisell uses only TokeDB tables,
# but not a too much low value, because otherwise there is too much CPU utilization
innodb_buffer_pool_size = 50M

# needed because MySQL can receive big BLOB files from the application,
# in case of complex rate plans
max_allowed_packet = 30M

# Set near 50% of free RAM, according TokuDB suggestions.
tokudb_cache_size = ${dbms_ram}M

# Every time a 30% more data is written on a table, schedule ANALYZE TABLE
tokudb_auto_analyze=30

secure_file_priv = '/var/tmp'

# Disabling symbolic-links is recommended to prevent assorted security risks
symbolic-links=0

            """, dbms_ram=self.dbms_reserved_ram_in_mb)

            # Gracefull reload is not permitted,
            # and a restart is not nice because pending transactions will be interrupted,
            # so executed restart only when the configurations are really changed.
            mysql_restart = '{ FILE=`{ md5sum ' + mysql_conf + ' | { read first rest ; echo "/var/opt/asterisell/mysql_conf_${first}" ; } }` ; if [ ! -f "$FILE" ]; then { touch $FILE ; systemctl restart mysqld.service ; } fi ; }'
            run(mysql_restart)

            if is_initial_install:
                run("systemctl enable mysqld.service")

                # rsyslog can cause RAM space leaks (also GB of RAM) so limit it
                put_template('/usr/lib/systemd/system/rsyslog.service',
                             """

[Unit]
Description=System Logging Service
;Requires=syslog.socket
Wants=network.target network-online.target
After=network.target network-online.target
Documentation=man:rsyslogd(8)
Documentation=http://www.rsyslog.com/doc/

[Service]
Type=notify
EnvironmentFile=-/etc/sysconfig/rsyslog
ExecStart=/usr/sbin/rsyslogd -n $$SYSLOGD_OPTIONS
Restart=on-failure
UMask=0066
StandardOutput=null
Restart=on-failure
MemoryAccounting=yes
MemoryCurrent=8192000
MemoryLimit=16192000

[Install]
WantedBy=multi-user.target
;Alias=syslog.service
                """)

                run('systemctl restart rsyslog.service && systemctl daemon-reload')

            # Configure PHP-FPM
            # NOTE: the majority of this settings have effect on http sessions, because
            # for CLI sessions by default there are no limits
            run("""
                sed -i "/cgi\.fix_pathinfo/c\cgi.fix_pathinfo=0" /etc/php.ini && \
                sed -i "/max_execution_time/c\max_execution_time = 180" /etc/php.ini && \
                sed -i "/max_input_time/c\max_input_time = 180" /etc/php.ini && \
                sed -i "/post_max_size/c\post_max_size = 30M" /etc/php.ini &&  \
                sed -i "/upload_max_filesize/c\upload_max_filesize = 30M" /etc/php.ini && \
                sed -i "/opcache.max_accelerated_files=.*/c\opcache.max_accelerated_files=12000" /etc/php.d/10-opcache.ini
                """)

            run('sed -i -e "s/user\s*=.*/user = ' + self.get_web_server_unix_user() + '/g" /etc/php-fpm.d/www.conf')
            run('sed -i -e "s/group\s*=.*/group = ' + self.get_web_server_unix_user() + '/g" /etc/php-fpm.d/www.conf')

            run("sed -i \"/memory_limit/c\\memory_limit = " + str(self.get_php_opcache_in_mb()) + "M  \" /etc/php.ini")
            run("sed -i \"/date.timezone/c\\date.timezone=" + self.date_timezone.replace("/", "\\/") + "\" /etc/php.ini")

            if not is_fast_upgrade:
                run('yum update -y')
                # NOTE: do only now, otherwise there can be conflicts on MySQL packages

            self.execute_install_task_post(is_initial_install)

            run('mkdir -p /var/opt/asterisell')
            run('touch ' + self.get_succesfull_install_file_name())
            run('touch ' + self.get_succesfull_install_version())


# -----------------------------------------


class Domain(object):
    """See `asterisell_instances.py` for the documentation of the fields.
    """

    fully_qualified_domain_name = 'www.example.net'

    def create_ssl_certificate(self):
        """Create the certificate for the instance."""
        pass

    def get_nginx_conf_before_ssl_certificate(self):
        """Create a temporary configuration to use before/during certificate creation."""
        pass

    def get_nginx_conf_after_ssl_certificate(self, php_fpm_conf):
        """Create the final configuration, with created certificates."""

    def get_file_name_check_for_certificate_creation(self):
        """A name of file to use for testing the creation of the certificate."""
        pass

    def signal_as_created_the_certificate(self):
        run('touch ' + self.get_file_name_check_for_certificate_creation())

    def is_already_installed(self):
        return files.exists(self.get_file_name_check_for_certificate_creation())


class HttpDomain(Domain):

    def get_file_name_check_for_certificate_creation(self):
        """A name of file to use for testing the creation of the certificate."""
        return os.path.join('/', 'var', 'opt', 'asterisell', 'httpdomain__' + self.fully_qualified_domain_name + '.chk')

    def create_ssl_certificate(self):
        pass

    def get_nginx_conf_before_ssl_certificate(self):
        return ''

    def get_nginx_conf_after_ssl_certificate(self, php_fpm_conf):

        t = Template("""
    server {
      listen 80;
      server_name ${fqdn};

      # max upload size
      client_max_body_size 30m;
      client_body_buffer_size 128k;

      $php_fpm_conf
    }
                     """)

        return t.substitute(fqdn=self.fully_qualified_domain_name, php_fpm_conf=php_fpm_conf)


class SelfSignedDomain(Domain):

    def get_file_name_check_for_certificate_creation(self):
        """A name of file to use for testing the creation of the certificate."""
        return os.path.join('/', 'var', 'opt', 'asterisell', 'ssl_selfsigned__' + self.fully_qualified_domain_name + '.chk')

    def create_ssl_certificate(self):
        run('mkdir -p /etc/ssl/private && '
            + ' openssl req -x509 -nodes -days 365 -newkey rsa:2048 '
            + ' -keyout /etc/ssl/private/' + self.fully_qualified_domain_name + '.key '
            + ' -out /etc/ssl/certs/' + self.fully_qualified_domain_name + '.pem '
            + ' -subj "/CN=' + self.fully_qualified_domain_name + '"')

    def get_nginx_conf_before_ssl_certificate(self):
        return ''

    def get_nginx_conf_after_ssl_certificate(self, php_fpm_conf):

        t = Template("""
    # Redirect http connections to https
    server {
      listen 80;
      server_name ${fqdn};
      return 301 https://$$$$host$$$$request_uri;
    }

    server {
      listen 443 ssl http2;
      server_name ${fqdn};

      ssl_certificate /etc/ssl/certs/${fqdn}.pem;
      ssl_certificate_key /etc/ssl/private/${fqdn}.key;

      ssl on;

      # max upload size
      client_max_body_size 30m;
      client_body_buffer_size 128k;

      $php_fpm_conf
    }
                     """)

        return t.substitute(fqdn=self.fully_qualified_domain_name, php_fpm_conf=php_fpm_conf)


class LetsEncryptDomain(Domain):

    def get_file_name_check_for_certificate_creation(self):
        """A name of file to use for testing the creation of the certificate."""
        return os.path.join('/', 'var', 'opt', 'asterisell', 'ssl_letsencrypt__' + self.fully_qualified_domain_name + '.chk')

    def get_domain_name_with_www(self):
        d1 = self.fully_qualified_domain_name
        d2 = None
        if d1.startswith('www.'):
           d2 = d1[len('www.'):]

        return (d1, d2)

    def create_ssl_certificate(self):
        (d1, d2) = self.get_domain_name_with_www()
        cert1 = '-d ' + d1
        cert2 = ''
        if d2 is not None:
           cert2 = '-d ' + d2

        run('certbot --nginx ' + cert2 + ' ' + cert1)

        cf = "/etc/cron.daily/letsencrypt-renew-certs"
        put_template(cf, """
    #!/bin/sh

    certbot --nginx renew  --quiet
    # NOTE: there is a call to letsencrypt servers only if the local certificate is near the end of its life-time

    systemctl reload nginx

                        """)

        run('chmod u+x ' + cf)

    def get_nginx_conf_before_ssl_certificate(self):
        t = Template("""
         server {
            listen 80;
            server_name $fqdn1 $fqdn2;

            # NOTE: this is needed by Letsencrypt for testing that you are the owner
            # of the server pointed from the DNS entry.
            location /.well-known/acme-challenge {
                root /var/www/letsencrypt;
            }

            # Redirect http requests to https
            location / {
                return 301 https://$$$$host$$$$request_uri;
            }
         }
                    """)

        (fqdn1, fqdn2) = self.get_domain_name_with_www()
        if fqdn2 is None:
            fqdn2 = ''

        return t.substitute(fqdn1=fqdn1, fqdn2=fqdn2)

    def get_nginx_conf_after_ssl_certificate(self, php_fpm_conf):
        (d1, d2) = self.get_domain_name_with_www()
        if d2 is None:
            www_name = ''
            name = d1
        else:
            www_name = d1
            name = d2

        maybe_redirect = ''
        if not www_name == '':
            t = Template("""
    # Redirect example.net to www.example.net
    server {
      listen 443 ssl http2;
      server_name $name;

      ssl_certificate /etc/letsencrypt/live/$name/fullchain.pem;
      ssl_certificate_key /etc/letsencrypt/live/$name/privkey.pem;

      ssl on;

      location / {
        return 301 https://$www_name/$$$$request_uri;
      }
    }
            """)
            maybe_name = t.substitute(name=name, www_name=www_name)

        t = Template("""
    server {
        listen 80;
        server_name $name $www_name;

        # NOTE: this is needed by Letsencrypt for testing that you are the owner
        # of the server pointed from the DNS entry.
        location /.well-known/acme-challenge {
            root /var/www/letsencrypt;
        }

        # Redirect http connections to https
        location / {
          return 301 https://$$$$host$$$$request_uri;
        }
    }

    $maybe_redirect

    server {
      listen 443 ssl http2;
      server_name $name;

      ssl_certificate /etc/letsencrypt/live/$name/fullchain.pem;
      ssl_certificate_key /etc/letsencrypt/live/$name/privkey.pem;
      ssl on;

      # max upload size
      client_max_body_size 30m;
      client_body_buffer_size 128k;

      $php_fpm_conf
    }
                     """)

        return t.substitute(name=name,
                            www_name=www_name,
                            maybe_redirect=maybe_redirect,
                            php_fpm_conf=php_fpm_conf)

# ------------------------------------------

class Instance(object):
    """
    An Asterisell instance.
    IMPORTANT: some comments about the fields relevant to the user, are moved inside `ansterisell_instance.py`.
    """

    #
    # Main Settings
    #

    name = ''

    host = None

    domain = None

    url_path = ''

    is_billing = True

    is_aggregate = False

    has_resellers = False

    install_cron_job = True

    customizations_doc = 'CUSTOMIZATIONS.md'

    debug_mode = 0

    def complete_host_string(self):
        """A connection string as required from Fabric"""
        return self.host.complete_host_string()

    admin_web_password = 'admin'

    httpd_web_access_password = ''

    webdav_users = None
    """A list of tuple user-name, user-password used from resellers for retrieving data.
    The user-name must correspond to the client (reseller) instance name.
    An URL  "server-url/get-{user-name}" will be created.
    The output directory must be kept in synchro with ExportCDRSToReseller job."""

    #
    # Asterisell Instance Params
    #

    # the frequency of jobs execution in minutes
    job_frequency_in_minutes = 5

    # when start a job. Useful for executing resellers not immediately with the main instance.
    job_offset_in_minutes = 0

    show_cost_saving = False
    input_queue = ''
    aggregate_server_output_queue = ''
    manage_outgoing_calls = True
    manage_incoming_calls = False
    manage_internal_calls = False
    show_communication_channel_for_customer = True
    show_communication_channel_for_administrator = True
    culture = ''
    currency = ''
    contacts = []
    database_password = ''
    user_database_password = ''

    # reports common to all instance
    default_reports = []

    # Name of custom reports generators class to add,
    # in a format like
    # > ["Report_CompareChannels: \"Compare Channels (User Defined)\""]
    custom_reports = []

    custom_initial_always_scheduled_jobs = []
    custom_final_always_scheduled_jobs = []
    custom_data_file_processors = []
    custom_export_cdrs_jobs = []

    expand_extensions_job = 'NullJob'
    # use 'ExpandExtensions' for expanding extensions like "123*" into "123456" when
    # specific instances are found in CDRs.
    # Useful for customers having many virtual extensions/DIDS,
    # that you don't want specify, but only discover in CDRs,
    # and at the same time the customers want reports with calls grouped
    # by specific extensions.

    import_cdrs_jobs = []
    # jobs executed for retrieving CDRs to rate.
    # They are executed before the rating related jobs.
    # These jobs are executed only for production instance,
    # and not for DEV instance, that load the data from the archive,
    # and not directly from the source-data.

    custom_jobs = []
    custom_configure_jobs = []
    custom_upgrade_jobs = []

    # Upgrade jobs that must be applied to all instances
    default_upgrade_jobs = [
      'Upgrade_2017_07_18'
    ]

    contact_external_hosts = False
    import_extensions_from_ipbx_database = False

    enable_upload_of_files = True

    import_extensions_from = '[]'

    organization_to_ignore = ''

    # in YYYY-MM-DD format
    start_rating_date = ''

    # use a something like '[massimo.zaniboni@gmail.com, ...]' as format
    send_emails_to_these_users_instead_of_original_receiver = '[]'

    run_command = 'php asterisell.php run jobs'

    there_is_call_flow_merge_phase = False

    conf_show_extension_code = True
    conf_how_many_calls_in_call_report = 40
    conf_not_displayed_telephone_prefix = '"-"'
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
    conf_instance_voip_domain = ''

    # Copy customer specific files inside `customizations` directory,
    # inside the admin instance.
    #
    # If they are jobs remember to add the jobs also in the scheduler,
    # in their proper position.
    #
    # Use something like
    # > custom_files = {
    # >     'source_file.php': 'some/dest_directory',
    # >     'another_file.php': 'another/dest_directory'
    # > }
    custom_files = {}

    def conf_connection_params(self):
        """A list of ConnectionParams objects.
        :rtype: list of ConnectionParams
        """
        return []

    #
    # Access Fields
    #

    def get_management_code(self):
        return self.host.name + '/' + self.get_instance_code()

    def get_instance_code(self):
        return self.name

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

    def get_deploy_ssh_user(self):
        return self.host.ssh_user

    def get_web_server_unix_user(self):
        return self.host.get_web_server_unix_user()

    def get_enable_upload_of_files(self):
        return self.enable_upload_of_files

    def get_database_name(self):
        return self.name

    def get_database_username(self):
        return self.name

    def get_database_password(self):
        return self.database_password

    def get_user_database_password(self):
        return self.user_database_password

    def get_succesfull_install_file_name(self):
        return os.path.join('/', 'var', 'opt', 'asterisell', 'SUCCESSFUL_INSTANCE_INSTALL__' + self.name + '.chk')

    def is_already_installed(self):
        return files.exists(self.get_succesfull_install_file_name())

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
                if (len(c.port)) > 0:
                    r = r + indent1 + 'port: ' + c.port
                r = r + indent1 + 'dbName: ' + c.dbName
                r = r + indent1 + 'tableName: ' + c.tableName
                r = r + indent1 + 'provider: ' + c.provider
                r = r + indent1 + 'timeFrameInMinutes: ' + c.timeFrameInMinutes
                r = r + indent1 + 'dataSourceFormat: ' + c.dataSourceFormat
                r = r + indent1 + 'dataSourceVersion: ' + c.dataSourceVersion
                r = r + indent1 + 'fromDate: \'' + c.fromDate + '\''
                r = r + indent1 + 'removeOlderThanDays: ' + c.removeOlderThanDays

        r = r + '\n'
        return r

    def authorize_ssh_access(self):
        with cd('/root'):
            put(os.path.expanduser('/home/user/.ssh/id_rsa.pub'), 'id_rsa.pub')
            run("""
              mkdir -p /root/.ssh && \
              cat /root/id_rsa.pub >> /root/.ssh/authorized_keys && \
              chmod 700 /root/.ssh && \
              chmod 600 /root/.ssh/authorized_keys
                                  """)

    #
    # Manage Compilations
    #

    # set to true if the host/instances supports also call flow merging
    there_is_call_flow_merge = False

    def ghc_compiletime_options(self):
        if self.debug_mode == 0:
            return "-O2 -threaded -rtsopts"
        elif self.debug_mode == 10:
            return "-O0 -fno-ignore-asserts -threaded -rtsopts"
        elif self.debug_mode == 11:
            return "-O0 -fprof-auto-top -fno-ignore-asserts -threaded -rtsopts"
        elif self.debug_mode == 20 or self.debug_mode == 22:
            return "-O2 -threaded -fprof-auto-top -rtsopts"
        elif self.debug_mode == 21:
            return "-O2 -threaded -fprof-auto -rtsopts"
        else:
            return "UNKNOWN OPTION"
        # NOTE: -O2 produces code slightly faster than -O, so maintain it

    def ghc_runtime_options(self):
        if self.debug_mode == 0 or self.debug_mode == 10:
            return "+RTS -threaded -N -RTS"
        elif self.debug_mode == 11:
            return "+RTS -threaded -N -xc -RTS"
        elif self.debug_mode == 20 or self.debug_mode == 21:
            return "+RTS -threaded -N -p -s -RTS"
        elif self.debug_mode == 22:
            return "+RTS -threaded -N -p -s -hc -i1 -L200 -RTS"
        else:
            return "UNKNOWN OPTION"

    def get_rate_engine_local_compilation_directory(self):
        """Mantain a distinct directory on the management container, for each compilation param."""
        d = os.path.join('/', 'home', 'user', 'compilation', 'rate_engine', 'mode_' + str(self.debug_mode))
        return d

    def get_call_flow_merge_local_compilation_directory(self):
        d = os.path.join('/', 'home', 'user', 'compilation', 'call_flow_merge_tools')
        return d

    def send_source_files_to_compilation_directory(self):
        # Rate Engine
        d = self.get_rate_engine_local_compilation_directory()
        local('mkdir -p ' + d)
        with lcd(d):
            local("rsync -r --inplace --perms --times --delete --exclude '.stack-work/' /asterisell/rating_tools/rate_engine/ .")

        # Call Flow Merge Engine
        if self.there_is_call_flow_merge:
            d = self.get_call_flow_merge_local_compilation_directory()
            local('mkdir -p ' + d)
            with lcd(d):
               local("rsync -r --inplace --perms --times --delete --exclude 'dist/' /asterisell/rating_tools/call_flow_merge_tools .")

    def compile_and_install_development_tools(self, install, use_fast_compile):
        """Compile the rate engine files, and other derived files."""

        self.send_source_files_to_compilation_directory()

        # Haskell Rating Engine

        with lcd(self.get_rate_engine_local_compilation_directory()):
            if not use_fast_compile:
                local('stack upgrade && stack setup && stack clean')

            ghc_options = self.ghc_compiletime_options()

            if self.debug_mode >= 20 or self.debug_mode == 11:
                local('stack build --profile --executable-profiling --library-profiling --ghc-options=\'' + ghc_options + '\'')
            else:
                local('stack build --ghc-options=\'' + ghc_options + '\'')

            if install:
                local('cp `stack path --local-install-root`/bin/RateEngine /var/tmp/.')
                of = os.path.join(self.get_admin_deploy_directory(), 'scripts', 'RateEngine')
                fabric.contrib.project.rsync_project(
                    remote_dir=of,
                    local_dir='/var/tmp/RateEngine')
                run('chmod u+x ' + of)
                rate_event_file = os.path.join(self.get_admin_deploy_directory(), 'apps','asterisell','lib','jobs','data_file_processing', 'ManageRateEvent.php')
                run("sed -i \"/^.*const .*DONT_TOUCH_DEBUG_MODE_GHC_PARAMS/c\\   const DONT_TOUCH_DEBUG_MODE_GHC_PARAMS = ' " +  self.ghc_runtime_options() + "';\" " + rate_event_file)

        # Call Flow Merge Engine

        if self.there_is_call_flow_merge:
            with lcd(self.get_call_flow_merge_local_compilation_directory()):
                if not use_fast_compile:
                    local('rm -f CMakeLists.txt ; rm -f CMakeCache.txt ; rm -r -f CMakeFiles')
                    # NOTE: if this file does not exists, then it is re-generated with correct setting during
                    # compilation phase

                local('php CProceduresGenerator.php init-project ' + self.name)
                # NOTE: in reality the init-project is executed only if there is no already an initializated project,
                # so it is a fast upgrade procedure.

                if not use_fast_compile:
                    local('make clean')

                local('make')
                # TODO send the code to the remote server

        # DITA manual

        with lcd('/asterisell/doc/manual'):
            local('./build.sh')

    #
    # Instance Installation Methods
    #

    def generate_config_file__databases(self, is_admin):
        """Generate databases.yml"""

        u = self.get_database_username()
        if is_admin:
            p = self.get_database_password()
        else:
            p = self.get_user_database_password()
            u = "usr_" + u

        put_template(os.path.join(self.get_instance_directory(is_admin), 'config', 'databases.yml'),
                    """
dev:
  propel:
    param:
      classname: DebugPDO
      debug:
        realmemoryusage: true
        details:
          time: { enabled: true }
          slow: { enabled: true,  threshold: 0.1 }
          mem: { enabled: true }
          mempeak: { enabled: true }
          memdelta: { enabled: true }

test:
  propel:
    param:
      classname:  DebugPDO

all:
  propel:
    class: sfPropelDatabase
    param:
      dsn:        mysql:dbname=$database_name;host=localhost
      database:   $database_name
      username:   $database_username
      password:   $database_password
      encoding:   utf8
      persistent: true
      pooling:    true
      classname:  PropelPDO
                        """,
                     database_name=self.get_database_name(),
                     database_username=u,
                     database_password=p
                     )

    def generate_config_file__app(self, is_admin):
        """Generate app.yml config file. """

        initial_jobs = list(self.get_custom_initial_always_scheduled_jobs())
        initial_jobs.extend(self.get_custom_data_file_processors())

        event_jobs = list(self.get_custom_data_file_processors())
        event_jobs.extend(self.get_custom_jobs())
        # CDR processing jobs must be executed both as fixed job, and event processor

        put_template(
            os.path.join(self.get_instance_directory(is_admin), 'apps', 'asterisell', 'config', 'app.yml'),
            """
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #
# !!!                                                                          !!! #
# !!! I M P O R T A N T:                                                        !!! #
# !!!                                                                           !!! #
# !!! This file is generated from management tools                              !!! #
# !!! Do not change the content of this file, otherwise it will be overwritten. !!! #
# !!!                                                                           !!! #
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #

all:

  instance_code_name: $instance_code_name

  instance_voip_domain: $instance_voip_domain

  is_billing: $is_billing

  is_status_server_instance: $is_aggregate

  web_server_user: $web_server_user

  start_rating_date: '$start_rating_date'

  send_emails_to_these_users_instead_of_original_receiver: $send_emails_to_these_users_instead_of_original_receiver

  smtp:
    host: $conf_smtp_host
    port: $conf_smtp_port
    username: $conf_smtp_username
    password: $conf_smtp_password
    encryption: $conf_smtp_encryption
    smtp_command: $conf_smtp_command
    reconnect_after_nr_of_messages: $conf_smtp_reconnect_after_nr_of_messages
    seconds_of_pause_after_reconnection: $conf_smtp_seconds_of_pause_after_reconnection

  available:

    phpReports:
      BillingReport_UserDefined: "Organization Report (User Defined)"
      BillingReport_Organization: "Organization Report (Monthly)"
      BillingReport_AllOrganization: "Full Organization Report (Monthly)"
      Report_CompareVendors: "Compare Vendors (User Defined)"
      Report_CompareVendorsAndCostSaving: "Compare Vendors and Cost Saving (User Defined)"
      Report_CompareChannels: "Compare Channels (User Defined)"
      CSVCallDetails: "CSV File with Call Details"
      $custom_reports_list

    always_scheduled_jobs:
      - RecalculateReportSets
      - ChangePassword
      $import_cdrs_jobs
      - ImportCustomersDataAskingToRatingEngine  # process all params with prefix "import-remote-customers-"
      - ImportCDRSUsingAppConfs                  # process all params with prefix "import-remote-cdrs-"
      - ImportDataFiles
      - SignalRatesToExportToResellers
      $custom_initial_always_scheduled_jobs
      - ManageRateEvent
      - $expand_extensions_job
      - GarbageCollectBundleState
      - CheckCallCostLimit
      # - CheckFrauds                         # see check_frauds options in this file for configuring it
      - CompareProviderCostWithCalculatedCost
      - CleanCacheFromOldItems
      - CheckOrganizationHierarchy
      - GenerateScheduledReports
      - CheckForMalformedErrorsNotSentToUsers
      - ReportCDRsWithErrors
      - CheckForUndefExtensions
      $custom_final_always_scheduled_jobs
      - CheckSaneRuntimeEnv
      - ReportsNotificationWorkflow
      - UpdateOfficialCallDateAccordingLastConfirmedReports
      - BackupOrganizationInfo
      - ChangePassword                      # execute more times this, for having a more reactive upgrade
      $custom_export_cdrs_jobs
      # Disabled backup jobs because there are many CDRS and data,
      # and a dump can be costly. 
      # - BackupConfigurations
      # - BackupSourceCDRS
      # - BackupCDRS
      # - BackupReports
      - AdviseAdminOfNewProblems            # this must be always the last job to be executed (see notes on the code)

    jobs:
      $custom_jobs
      - ForceExecutionOfReportSchedulerJob
      - GenerateCSVInvoiceSummaryReport
      - ChangeOrganizationInfo
      - GenerateMailWarningCustomerForHighCallCost  # the effect of this job can be disabled setting the parameter repeat_advise_of_high_cost_limit_after_days to 0
      - BackupOrganizationInfo

    configure_jobs:
      - ConfigureDefaultParamsAndSettings
      - ConfigureHolidays
      - ConfigureDefaultResponsibleForEachErrorDomainType
      - InitWithDefaultMySQLStoredProcedures
      - InitTelephonePrefixes
      - LoadWorldTelephonePrefixesFromCSVFile
      - ConfigureCommunicationChannels
      $custom_configure_jobs
      - ForceReratingAtCurrentCronJobProcessor

    # These jobs are executed during the upgrade of the application.
    #
    # If an application is installed from scratch, then these jobs are not executed,
    # because a new installed application does not need any upgrade.
    #
    # They are activate from the management shell command
    #
    # > php asterisell.php data upgrade
    #
    # The order sequence is respected.
    #
    # These jobs can be parametric on `instance_code_name`.
    #
    # If you want specify a job that is both a configuration to apply, and an upgrade,
    # add the same job both in this place, and in `configure_jobs`.
    #
    upgrade_jobs:
      - EmptyUpgradeJob
      - Upgrade_2017_01_24
      - Upgrade_2017_03_06
      - Upgrade_2017_03_10
      - Upgrade_2017_03_16
      - Upgrade_2018_01_23
      - Upgrade_2018_02_14
      - Upgrade_2018_03_00
      $custom_upgrade_jobs

  organization_to_ignore: $organization_to_ignore

  show_income_and_earn_of_call: $show_income_and_earn_of_call

  show_cost_savings_for_administrator: $show_cost_savings_for_administrator

  show_cost_savings_for_customer: $show_cost_savings_for_customer

  show_incoming_calls: $show_incoming_calls
  show_outgoing_calls: $show_outgoing_calls
  show_internal_calls: $show_internal_calls

  show_vendor_for_customer: $show_vendor_for_customer

  show_communication_channel_for_customer: $show_communication_channel_for_customer
  show_communication_channel_for_administrator: $show_communication_channel_for_administrator

  # Display in the customer call report also
  # the direction of the call (incoming/outgoing/internal)
  # for each call.
  #
  # The administrator is not affected from this setting, because
  # he sees always the call direction.
  #
  # allowed values: true / false (case sensitive)
  #
  show_call_direction: $show_call_direction

  show_extension_code: $conf_show_extension_code

  how_many_calls_in_call_report: $conf_how_many_calls_in_call_report

  not_displayed_telephone_prefix: $conf_not_displayed_telephone_prefix

  # True for CDRs associated to only one call.
  # False if CDRs can be associated to more than a call, for example for aggregate stats
  cdrs_are_associated_always_to_one_call: $cdrs_are_associated_always_to_one_call

  check_new_external_files_to_import_after_minutes: $conf_check_new_external_files_to_import_after_minutes

  check_for_spacewalk_updates_after_minutes: 60

  # The maximum number of days that can be part of a rating phase.
  # Rating time-frame bigger than these days, are split in smaller chunks.
  #
  # The rating engine rate the calls at chunk, because they must be ordered according call-date,
  # in case there are bundle-rates.
  #
  # The rating engine uses a constant (but noticeable) amount of RAM, for storing info about:
  #  * the calls (CDRs) of the rating chunk
  #  * the rates in the rating time-frame
  #  * the extensions/organizations
  #  * the state of bundle-rates in the rating time-frame
  #
  # The default rating chunk is 40 days.
  # Lower this amount if you have many, and you have not enough free RAM.
  #
  # The JOB log will inform about the used RAM from each job.
  #
  rate_cdrs_using_chunks_of_consecutive_days: $conf_rate_cdrs_using_chunks_of_consecutive_days

  check_cost_limits_after_minutes: $conf_check_cost_limits_after_minutes

  repeat_advise_of_high_cost_limit_after_days: $conf_repeat_advise_of_high_cost_limit_after_days

  max_cost_limit_timeframe: $conf_max_cost_limit_timeframe

  check_frauds:
    max_unprocessed_calls: $conf_check_frauds_max_unprocessed_calls

    compare_xx_last_days_respect_customer_daily_max_cost: $conf_check_frauds_compare_xx_last_days_respect_customer_daily_max_cost

    compare_x_last_months_respect_current_costs: $conf_check_frauds_compare_x_last_months_respect_current_costs

  safe_limit_for_concurrent_calls: $conf_safe_limit_for_concurrent_calls

  date_format: $conf_date_format

  invoice_date_format: $conf_invoice_date_format

  long_details_in_invoice: $conf_long_details_in_invoice

  use_inclusive_end_date_in_invoice: $conf_use_inclusive_end_date_in_invoice

  currency: $currency

  decimal_separator_symbol_in_csv: $conf_decimal_separator_symbol_in_csv

  numbers_as_strings_in_csv: $conf_numbers_as_strings_in_csv

  csv_field_separator: $conf_csv_field_separator

  culture: $culture

  currency_decimal_places: $conf_currency_decimal_places

  currency_decimal_places_in_invoices: $conf_currency_decimal_places_in_invoices

  mask_for_external_telephone_number: $conf_mask_for_external_telephone_number

  # Remove a log message from the Job Log Table
  # when it is older than this number of months.
  # Influence also the number of past versions of customer info to keep.
  months_after_removing_a_job_log_entry: 3

  max_calls_in_pdf_report_section: $conf_max_calls_in_pdf_report_section

  # Where uploaded files are put/read.
  # This is a directory inside "web" directory,
  # and it must be web-server readable and writable.
  #
  # Also if this seems a params, *do not change its value*,
  # because it is hard-coded in other parts of the code.
  #
  sfMediaLibrary:
    upload_dir: "uploads/assets"

  # set to false for not allowing users to upload files
  enable_upload_of_files: $enable_upload_of_files

  # This option enabled only if special jobs are configured.
  #
  # true for contacting external hosts,
  # false for not contacting them.
  # false it is typically used in test configurations. true in production.
  contact_external_hosts: $contact_external_hosts

  # This option enabled only if special jobs are configured.
  #
  import_extensions_from_ipbx_database: $import_extensions_from_ipbx_database

  # This option enabled only if special jobs are configured.
  #
  import_organization_hieararchy_from_ipbx_database: $import_extensions_from_ipbx_database

  # This option enabled only if special jobs are configured.
  #
  # Where store files retrieved from remote servers
  # Must terminate with '/'
  # It is in rsync with the remote server, so it is an exact map of the remote server content.
  # It can not be under backup, because files are a copy of files on remote servers.
  # If files are removed from the remote server, then they are also removed from this directory.
  #
  local_rsync_with_remote_servers: 'data_files/rsync_with_remote_servers/'

  # This option enabled only if special jobs are configured.
  #
  # Extension database connection parameters.
  #
  import_extensions_from: $import_extensions_from

  # This option enabled only if special jobs are configured.
  #
  # Where store files retrieved from remote servers
  # Must terminate with '/'
  #
  local_archive_of_remote_servers: 'cdrs/from_remote_servers/'

  instance_url_path: $instance_url_path

  connections: $conf_connectionParams

            """,
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
            is_billing=self.bool_value_str(self.is_billing),
            enable_upload_of_files=self.bool_value_str(self.get_enable_upload_of_files()),
            instance_code_name=self.yaml_string(self.get_instance_code()),
            instance_voip_domain=self.yaml_string(self.conf_instance_voip_domain),
            contact_external_hosts=self.bool_value_str(self.get_contact_external_hosts()),
            import_extensions_from_ipbx_database=self.bool_value_str(self.get_import_extensions_from_ipbx_database()),
            web_server_user=self.get_web_server_unix_user(),
            start_rating_date=self.get_start_rating_date(),
            organization_to_ignore=self.yaml_string(self.organization_to_ignore),
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
            custom_export_cdrs_jobs=self.yaml_list_of_str_values('      - ', self.get_custom_export_cdrs_jobs()),
            expand_extensions_job=self.yaml_string(self.expand_extensions_job),
            instance_url_path=self.yaml_string(self.url_path),
        )

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
        """Return an exception if the instance can not be installed."""
        if self.is_already_installed():
            abort("The directory " + self.get_admin_deploy_directory()
                  + " already exists on the remote installation host. "
                  + " Delete it, before installing a new instance of Asterisell with the same name.")

    def execute_install_task_post(self):
        """Called after the standard code of installation."""
        pass

    def execute_install_task(self, on_same_host, on_same_domain):
        self.execute_install_task_pre_check()

        self.execute_upgrade_task(on_same_host, on_same_domain, True, True, True, True)
        with cd(self.get_admin_deploy_directory()):
            self.execute_install_task_post()

    def execute_upgrade_task(self,
                             on_same_host,
                             on_same_domain,
                             fast_upgrade=False,
                             execute_silently_activate=True,
                             execute_with_delete_rsync=True,
                             is_initial_install=False):

        # NOTE: many upgrade/installation operations are done also from the "asterisell.php" management utility

        # First execute operations that can be done without acquiring any lock on the production application

        self.compile_and_install_development_tools(False, fast_upgrade)

        # Acquire lock, waiting for the termination of running jobs.
        # An error is issued if the lock can not be obtained after a reasonable amount of time.
        # Note: in case of interruption of upgrade procedure in next steps:
        # - application is still locked
        # - web access is still locked
        # - admin can execute commands on it
        # - a new upgrade command can be executed

        if not is_initial_install:
            with cd(self.get_admin_deploy_directory()):
                run('php asterisell.php cron disable-for-upgrade')
                run('php asterisell.php app disable')

            self.host.execute_update(False, True, fast_upgrade)

        with cd('/'):
            run('mkdir -p ' + self.get_admin_deploy_directory())
            run('mkdir -p ' + self.get_user_deploy_directory())

        self.rsync_files(
            execute_with_delete_rsync,
            self.get_admin_deploy_directory(),
            self.local_source_repo_directory(),
            is_initial_install)

        self.generate_config_file__app(True)
        self.generate_config_file__databases(True)
        self.send_custom_files()

        put(os.path.join('customizations', self.customizations_doc), os.path.join(self.get_admin_deploy_directory(), 'CUSTOMIZATIONS.md'))

        self.create_user_instance()
        self.generate_config_file__databases(False)

        # this time the make process is fast because it is already compiled
        self.compile_and_install_development_tools(True, True)

        with cd(self.get_admin_deploy_directory()):
            if is_initial_install:
                run('php asterisell.php install root ' + self.host.db_root_password)
            else:
                run('php asterisell.php run db-upgrade-jobs')
                run('php asterisell.php install-views-and-procedures')
                run('php asterisell.php run upgrade-jobs')

        if execute_silently_activate:
            with cd(self.get_admin_deploy_directory()):
                run('php asterisell.php silently-activate')

        if not is_initial_install:
            with cd(self.get_admin_deploy_directory()):
                run('php asterisell.php app enable')
                run('php asterisell.php cron enable')

        if is_initial_install:
            with cd(self.get_admin_deploy_directory()):
                run('php asterisell.php data admin ' + self.admin_web_password)

        # Signal that the instance is succesfully installed, and from this time it can contain real data
        run('touch ' + self.get_succesfull_install_file_name())

        #DEV-NOTE: generate some configurations files only now, because:
        # - the instances are correctly configured, and we do not enable jobs on not correct instances
        # - they are recognized as activated and so config files will include them

        self.update_crond_files(on_same_host)

        if is_initial_install:
            run('systemctl stop php-fpm.service ; systemctl enable --now php-fpm.service')
        else:
            run('systemctl reload php-fpm.service')

        if self.host.generate_http_conf:
            self.install_nginx_conf(on_same_host, on_same_domain)

    def execute_uninstall_task(self, on_same_host, on_same_domain):

        run('mysqladmin -uroot -p' + self.host.db_root_password + ' drop ' + self.get_database_name())
        run('rm -f ' + self.get_succesfull_install_file_name())
        self.update_crond_files(on_same_host)
        run('rm -r -f ' + self.get_instance_directory(True))
        run('rm -r -f ' + self.get_instance_directory(False))
        self.install_nginx_conf(on_same_host, on_same_domain)

    def update_crond_files(self, on_instances):
        cron_cmd = '# !!! This file is automatically generated and updated from Asterisell Management Tool !!!'
        for instance in on_instances:
            if instance.is_already_installed():
                cron_cmd = cron_cmd + "\n\n" + instance.get_cronjob_code()

        crontab_filename = '/etc/cron.d/asterisell'
        local_crontab = '/var/tmp/crontab.tmp'
        f = open(local_crontab, 'w')
        f.write(cron_cmd)
        f.close()
        put(local_crontab, crontab_filename, mode=0644)
        os.remove(local_crontab)
        run('systemctl reload crond.service')

    def get_cronjob_code(self):
        """Return the code for the cronjob."""

        if self.install_cron_job:
            cmd = str(self.job_offset_in_minutes)
            current_time = self.job_frequency_in_minutes + self.job_offset_in_minutes
            while current_time < 60:
                cmd = cmd + ',' + str(current_time)
                current_time = current_time + self.job_frequency_in_minutes

            cmd = cmd + ' * * * * root ' \
                      + ' cd ' + self.get_admin_deploy_directory() \
                      + ' && php asterisell.php run scheduled-jobs > /dev/null 2>&1 ' \
                      + "\n"

            return cmd
        else:
            return ''

    def execute_connect_task(self):
        conn = ' -p ' + self.host.ssh_port + ' ' + self.host.ssh_user + '@' + self.host.ssh_addr
        cmd = "'cd " + self.get_admin_deploy_directory() + " ; /usr/bin/bash'"
        ssh_cmd = 'ssh -t ' + conn + ' ' + cmd

        print("")
        print("Instance: " + self.name)
        print("")
        print(ssh_cmd)
        print("")
        print("Instance web address: " + self.get_web_url(True))
        print("")
        print("For connecting to the MySQL database: mysql -u" + self.get_database_username() +  " -p" + self.get_database_password() + " " + self.get_database_name())
        print("")

        local(ssh_cmd)

    def execute_run_command(self):
        with cd(self.get_admin_deploy_directory()):
            run(self.get_run_command())

    def send_custom_files(self):
        """Send self.custom_files to the instance admin directory."""
        for file_name, dest_dir in self.custom_files.iteritems():
            source_file = os.path.join('customizations', file_name)
            dest = os.path.join(self.get_admin_deploy_directory(), dest_dir)
            run('mkdir -p ' + dest)
            put(source_file, dest)

    def create_user_instance(self):
        with cd('/'):
            run('mkdir -p ' + self.get_user_deploy_directory())
            run('chown root:' + self.get_web_server_unix_user() + ' ' + self.get_user_deploy_directory())
            run('chmod u+rwx,g+rx,o-rwx ' + self.get_user_deploy_directory())

            config_dir = os.path.join(self.get_user_deploy_directory(), 'config')
            run('mkdir -p ' + config_dir)

    #
    # Configure Httpd
    #

    def install_nginx_conf(self, on_same_host, on_same_domain):
        """Install missing certificates, and activate nginx."""

        self.generate_all_nginx_conf(on_same_host, on_same_domain)
        if not self.domain.is_already_installed():
            self.create_ssl_certificate()
            self.domain.signal_as_created_the_certificate()
            self.generate_all_nginx_conf(on_same_host, on_same_domain)

        run('systemctl enable nginx.service')

    def create_ssl_certificate(self):
        """Create the certificate for the instance."""
        return self.domain.create_ssl_certificate()

    def generate_all_nginx_conf(self, on_same_host, on_same_domain):
        """Generate and reload HTTP configurations."""
        put_template('/etc/nginx/nginx.conf', self.get_all_nginx_conf(on_same_host, on_same_domain))
        run('systemctl reload nginx.service || systemctl restart nginx.service')

    def get_all_nginx_conf(self, on_same_host, on_same_domain):

        conf = """

## !!!
## !!! WARNING: this file is automatically generated from Asterisell management utility.
## !!! Do not change it, because it will be overwritten.
## !!!

user apache;

events {
    worker_connections  1024;
}

http {
    # Basic Settings
    sendfile on;
    tcp_nopush on;
    tcp_nodelay on;
    keepalive_timeout 65;
    types_hash_max_size 2048;
    server_names_hash_bucket_size 128;

    include /etc/nginx/mime.types;
    default_type application/octet-stream;

    log_format  main  '$$remote_addr - $$remote_user [$$time_local] "$$request" '
                      '$$status $$body_bytes_sent "$$http_referer" '
                      '"$$http_user_agent" "$$http_x_forwarded_for"' '$$request_time';

    # Gzip Settings.
    # Asterisell traffic is highly compressable, so compression always payoff.
    gzip on;
    gzip_disable "msie6";
    gzip_vary on;
    gzip_proxied any;
    gzip_comp_level 6;
    gzip_buffers 16 8k;
    gzip_http_version 1.1;
    gzip_types text/plain text/css application/json application/x-javascript text/xml application/xml application/xml+rss text/javascript;

    # Almost all of the overhead with SSL/TLS is during the initial connection setup, so cache them.
    # NOTE: 1m of cache are near 4000 sessions.
    ssl_session_cache   shared:SSL:4m;
    ssl_session_timeout 10m;
    ssl_ciphers HIGH:!aNULL:!MD5;
    ssl_prefer_server_ciphers on;

    # Enable slow queries
    client_header_timeout 3000;
    client_body_timeout 3000;
    fastcgi_connect_timeout 1200s;
    fastcgi_send_timeout 1200s;
    fastcgi_read_timeout 1200s;
        """

        if not self.httpd_web_access_password == '':
            f = self.get_httpd_auth_file_name()
            run('htpasswd -bc ' + f + ' admin ' + self.httpd_web_access_password)
            run('chown -R '
                    + self.get_web_server_unix_user()
                    + ':'
                    + self.get_web_server_unix_user()
                    + ' ' + f)

        # Group by domains
        domains = {}
        installed_domains = {}
        not_installed_domains = {}
        for instance in on_same_host:
            if instance.is_already_installed():
                k = instance.domain.fully_qualified_domain_name
                domains[k] = instance.domain
                if instance.domain.is_already_installed():
                    if not k in installed_domains:
                        installed_domains[k] = ''
                    installed_domains[k] = installed_domains[k] + instance.get_php_fpm_conf(True) + instance.get_php_fpm_conf(False)
                else:
                    not_installed_domains[k] = instance.domain.get_nginx_conf_before_ssl_certificate()

        # Produce configuration files
        for k, s in installed_domains.items():
            d = domains[k]
            conf = conf + d.get_nginx_conf_after_ssl_certificate(s)

        for k, s in not_installed_domains.items():
            conf = conf + s

        conf = conf + "\n}"
        return conf

    def get_httpd_auth_file_name(self):
        return os.path.join('/', 'etc', 'nginx', self.name + '-auth.passwd')

    def get_php_fpm_conf(self, is_admin):
        """Return the PHP-FPM configuration, for the instance."""

        header_comment = ''
        if is_admin:
            header_comment = '# Access to admin web-site'
        else:
            header_comment = '# Access to customer web-site'

        if self.httpd_web_access_password == '':
            maybe_auth = ''
        else:
            maybe_auth = "\n     auth_basic \"Authentication Required, for user admin\";"
            maybe_auth = maybe_auth + "\n     auth_basic_user_file " + self.get_httpd_auth_file_name() + ";"

        if self.url_path == '' and (not is_admin):
            t = Template("""

      #
      ${header_comment}
      #

      location = / {
        index /index.php;
      }

      # Execute PHP code using php-fpm
      location ~ ^/(index\.php|asterisell_dev\.php)(/|$$$$) {
        ${maybe_auth}

        root    ${instance_directory}/web;

        fastcgi_split_path_info ^(/index\.php|/asterisell_dev\.php)(/.*)$$$$;
        # parse the $$$$uri applying the regular expression.
        # after this:
        # - $$$$fastcgi_script_name is the first match $$$$1 (index.php or asterisell_dev.php)
        # - $$$$fastcgi_path_info is the second $$$$2 match (the options)

        fastcgi_param  REQUEST_METHOD     $$$$request_method;
        fastcgi_param  CONTENT_TYPE       $$$$content_type;
        fastcgi_param  CONTENT_LENGTH     $$$$content_length;

        fastcgi_param  SCRIPT_FILENAME   $$$$document_root$$$$fastcgi_script_name;
        fastcgi_param  SCRIPT_NAME       $$$$fastcgi_script_name;
        fastcgi_param  QUERY_STRING      $$$$query_string;
        fastcgi_param  PATH_INFO $$$$fastcgi_path_info;

        fastcgi_param  REQUEST_URI        $$$$request_uri;
        fastcgi_param  DOCUMENT_URI       $$$$document_uri;
        fastcgi_param  DOCUMENT_ROOT      $$$$document_root;
        fastcgi_param HTTPS               "on";
        fastcgi_param HTTP_SCHEME         "https";

        fastcgi_param  SERVER_PROTOCOL    $$$$server_protocol;
        fastcgi_param  GATEWAY_INTERFACE  CGI/1.1;
        fastcgi_param  SERVER_SOFTWARE    nginx/$$$$nginx_version;
        fastcgi_param  REMOTE_ADDR        $$$$remote_addr;
        fastcgi_param  REMOTE_PORT        $$$$remote_port;
        fastcgi_param  SERVER_ADDR        $$$$server_addr;
        fastcgi_param  SERVER_PORT        $$$$server_port;
        fastcgi_param  SERVER_NAME        $$$$server_name;

        fastcgi_pass   127.0.0.1:9000;
      }

      # Serve static files
      location / {
        ${maybe_auth}

        root    ${instance_directory}/web;

        # Use the cache on the client side, because static content rarely change,
        # but tell to the client to always ask if there is a new version to use.
        etag on;
        if_modified_since exact;
        expires 1;

        try_files $$$$uri =404;
      }

      # $base_url_path_with_slash $base_url_path $maybe_slash

                         """)
        else:
            t = Template("""

      #
      ${header_comment}
      #

      location = /${base_url_path} {
        rewrite ^ /${base_url_path_with_slash}index.php last;
      }

      location = /${base_url_path_with_slash} {
        index /${base_url_path_with_slash}index.php;
      }

      # Execute PHP code using php-fpm
      location ~ ^/${base_url_path_with_slash}(index\.php|asterisell_dev\.php)(/|$$$$) {
        ${maybe_auth}

        root    ${instance_directory}/web;
        rewrite ^${maybe_slash}${base_url_path}(/.*)$$$$ $$$$1 break;

        fastcgi_split_path_info ^(/index\.php|/asterisell_dev\.php)(/.*)$$$$;
        # parse the $$$$uri applying the regular expression.
        # after this:
        # - $$$$fastcgi_script_name is the first match $$$$1 (index.php or asterisell_dev.php)
        # - $$$$fastcgi_path_info is the second $$$$2 match (the options)

        fastcgi_param  REQUEST_METHOD     $$$$request_method;
        fastcgi_param  CONTENT_TYPE       $$$$content_type;
        fastcgi_param  CONTENT_LENGTH     $$$$content_length;

        fastcgi_param  SCRIPT_FILENAME   $$$$document_root$$$$fastcgi_script_name;
        fastcgi_param  SCRIPT_NAME /${base_url_path}$$$$fastcgi_script_name;
        fastcgi_param  QUERY_STRING $$$$query_string;
        fastcgi_param  PATH_INFO $$$$fastcgi_path_info;

        fastcgi_param  REQUEST_URI        $$$$request_uri;
        fastcgi_param  DOCUMENT_URI       $$$$document_uri;
        fastcgi_param  DOCUMENT_ROOT      $$$$document_root;
        fastcgi_param HTTPS               "on";
        fastcgi_param HTTP_SCHEME         "https";

        fastcgi_param  SERVER_PROTOCOL    $$$$server_protocol;
        fastcgi_param  GATEWAY_INTERFACE  CGI/1.1;
        fastcgi_param  SERVER_SOFTWARE    nginx/$$$$nginx_version;
        fastcgi_param  REMOTE_ADDR        $$$$remote_addr;
        fastcgi_param  REMOTE_PORT        $$$$remote_port;
        fastcgi_param  SERVER_ADDR        $$$$server_addr;
        fastcgi_param  SERVER_PORT        $$$$server_port;
        fastcgi_param  SERVER_NAME        $$$$server_name;

        fastcgi_pass   127.0.0.1:9000;
      }

      # Serve static files
      location /${base_url_path_with_slash} {
        ${maybe_auth}

        root    ${instance_directory}/web;
        rewrite ^${maybe_slash}${base_url_path}(/.*)$$$$ $$$$1 break;

        # Use the cache on the client side, because static content rarely change,
        # but tell to the client to always ask if there is a new version to use.
        etag on;
        if_modified_since exact;
        expires 1;
        add_header Pragma public;
        add_header Cache-Control "public, must-revalidate, proxy-revalidate";

        try_files $$$$uri =404;
      }
                       """)

        if is_admin:
            if self.url_path == '':
                base_path = 'admin'
                maybe_slash = '/'
                base_path_with_slash = base_path + '/'
            else:
                base_path = self.url_path + '/admin'
                maybe_slash = '/'
                base_path_with_slash = base_path + '/'
        else:
            if self.url_path == '':
                base_path = ''
                base_path_with_slash = ''
                maybe_slash = ''
            else:
                base_path = self.url_path
                maybe_slash = '/'
                base_path_with_slash = base_path + '/'

        s = t.substitute(
            maybe_auth=maybe_auth,
            maybe_slash=maybe_slash,
            base_url_path=base_path,
            base_url_path_with_slash=base_path_with_slash,
            instance_directory=self.get_instance_directory(is_admin),
            header_comment=header_comment
        )

        w = ''
        if is_admin and self.webdav_users is not None:
            template_webdav = Template("""
      # Serve WebDAV

      location /${base_url_path_with_slash}get-${webdav_instance}/ {
        alias ${webdav_dir}/;
        client_body_temp_path  ${webdav_dir_tmp};

        gzip on;
        client_max_body_size 200m;

        auth_basic           "webdav";
        auth_basic_user_file ${webdav_passwd_file};

        dav_methods  PUT MKCOL DELETE;
        create_full_put_path on;
        dav_access user:rw;
      }
                                      """)

            for webdav_user in self.webdav_users:
                (webdav_instance, webdav_password) = webdav_user

                webdav_dir = os.path.join('/', 'var', 'opt', 'asterisell', self.name, webdav_instance)
                webdav_dir_tmp = os.path.join(webdav_dir, 'tmp')
                webdav_passwd_file = os.path.join('/', 'etc', 'nginx', self.name + '-' + webdav_instance + '.passwd')

                run('mkdir -p ' + webdav_dir_tmp)
                run('htpasswd -bc ' + webdav_passwd_file + ' ' + webdav_instance + ' ' + webdav_password)
                run('chown -R '
                    + self.get_web_server_unix_user()
                    + ':'
                    + self.get_web_server_unix_user()
                    + ' ' + webdav_passwd_file)
                run('chown -R '
                   + self.get_web_server_unix_user()
                   + ':'
                   + self.get_web_server_unix_user()
                   + ' ' + webdav_dir)

                t = template_webdav.substitute(
                    maybe_slash=maybe_slash,
                    base_url_path=base_path,
                    base_url_path_with_slash=base_path_with_slash,
                    instance_code=self.name,
                    webdav_instance=webdav_instance,
                    webdav_dir=webdav_dir,
                    webdav_dir_tmp=webdav_dir_tmp,
                    webdav_passwd_file=webdav_passwd_file
                    )
                w = w + t

        return w + s

    def get_web_url(self, is_admin):
        p = ''
        if not self.url_path == '':
            p = '/' + self.url_path

        if is_admin:
            p = p + '/admin'

        return 'https://' + self.domain.fully_qualified_domain_name + p


    #
    # Manage passwords
    #

    config_parser = None
    config_parser_fname = 'passwords.ini'

    def get_password_for(self, name):
        pfile = (os.path.join(os.path.abspath('.'), self.config_parser_fname))
        msgSuffix = 'NOTE: the file `' + self.config_parser_fname + '` is usually configured on the Asterisell host, and not on the Asterisell shared repository, for not exposing passwords.'

        if self.config_parser is None:
            self.config_parser = ConfigParser.ConfigParser()

            if os.path.exists(pfile):
                self.config_parser.read(pfile)
            else:
                raise RuntimeError('Passwords can not be loaded because the passwords file is missing. ' + msgSuffix)

        if self.config_parser.has_option("passwords", name):
            return self.config_parser.get("passwords", name)
        else:
            raise RuntimeError('Missing password "' + name + '". ' + msgSuffix)

    #
    # Utility Functions
    #

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
        NOTE: these are files to install on remote instance, and not on a repo of the application
        """

        return [
            '.git/*',
            '.idea',
            '/tmp',
            '/cache',
            '/.fslckout',
            '.gitignore',
            '/fabfile.py',
            '/fabfile.pyc',
            '/fabric_data/*',
            '/web/uploads/assets/*',
            'provider_specific/*',
            '/customizations',
            '_darcs/*'
            '/REMOVE_ME_FOR_ENABLING_ASTERISELL_APP',
            'asterisell_instances.py*',
            'fab.sh',
            'fabfile.py*',
            'Dockerfile',
            '.stack-work/',
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

    def get_recent_start_rating_date(self, months_in_the_past=6, begin_day_of_bundle_rates=1, begin_hour=0, begin_minute=0):
        """Used for generating DEMO/DEV instances, with a date recent, but compatible with bundle-rates."""
        d = (datetime.date.today() - datetime.timedelta(days=months_in_the_past * 31)).replace(day=begin_day_of_bundle_rates)
        d = datetime.datetime(d.year, d.month, d.day, begin_hour, begin_minute, 0)
        return d.isoformat()


class BillingInstance(Instance):
    """Instance used for billing calls."""

    is_billing = True
    is_aggregate = False

    custom_configure_jobs = ['InitDefaultReportsForVoIPReseller']

    default_reports = [
        'PlaceHolderForInvoiceNumeration: "Placeholder for Invoice Numeration"',
        'GenerateLegalInvoiceUsingTemplate01PDF: "Invoice using Default PDF Template"',
        'GenerateNoLegalInvoiceUsingTemplate01PDF: "Call Report Notes, using a pseudo invoice PDF Template"'
    ]


class CallReportingInstance(Instance):
    """Instance used for reporting the calls, without reselling them."""

    is_billing = False
    is_aggregate = False

    custom_configure_jobs = ['InitDefaultReportsForCallReporting']
