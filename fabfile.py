
# SPDX-License-Identifier: GPL-3.0-or-later

'''
Fabric 1.4.3 administration tools, for Asterisell
Use "fab help" for showing avaible tasks.
'''

from __future__ import with_statement

from fabric.api import run, local, cd, lcd, put, settings, task, runs_once
from fabric.network import disconnect_all, ssh
import sys
import os.path
import importlib
import fabric.contrib.project
import random
import time
from asterisell_instances import all_instances

'''
  IMPORT INSTANCES DEFINED IN EXTERNAL MODULES
'''

def update_version_file():
    """Update the VERSION file."""
    pass

'''
  Fabric Tasks
'''

@task
@runs_once
def help():
    print """
SYNOPSIS

    fab help

    fab CMD:HOST/INSTANCE

    fab remove_orphan_volumes

    fab rdiff_backup:USER,SERVER,REMOTE_DIRECTORY

DESCRIPTION

    fab manages Asterisell instances defined in the file
    `fabric_data/asterisell_instances.py` installing them
    in Docker containers, using the Asterisell version in
    current admin directory.

OPTIONS
    CMD

      init
          init the manamegent tool server with packages required
          for installing and compiling Asterisell

      authorize_ssh_access
          load the `~/.ssh/id_rsa.pub` key of the management tool on the instance,
          for enabling password-less SSH login.

      install
          install an instance on its host.

      uninstall
          remove the instance, and its data.

      upgrade_app
          upgrade the instance to the last version of the application,
          in a safe but slow way. If only settings was changed use `upgrade_conf` instead.

      upgrade_conf
          like `upgrade_app` but a lot faster, because binaries are not compiled from scratch.
          Useful if only configuration settings are changed.

      connect
          ssh connect, and open a browser window.

      stop
          disable automatic execution of rating jobs.

      start
          enable automatic execution of rating jobs.

      run_jobs
          run jobs one time, also if the application is blocked in
          administration mode.

      web_disable
          the web application can be accessed only by administrators.

      web_enable
          enable web access to all users.

      update_website
          update the local website inside `doc` directory.

    HOST/INSTANCE as defined in file "asterisell_instances.py" and in "host/name" format
"""
    print "        ",
    for instance in all_instances:
        print instance.get_management_code(), " ",
    print """

COMMON USAGE PATTERNS

For installing a new instance:

    fab authorize_ssh_access:HOST/INSTANCE
    fab install:HOST/INSTANCE

For updating configurations in `fabric_data/asterisell_instances.py`:

    fab upgrade_conf:HOST/INSTANCE

For upgrading the application code:

    git pull
    fab upgrade_app:HOST/INSTANCE

For inspecting the instance:

    fab connect:HOST/INSTANCE

"""

@task
@runs_once
def init():

    # Common and DEV packages

    local("""sudo yum -y install epel-release ; \
             sudo yum install -y http://rpms.remirepo.net/enterprise/remi-release-7.rpm ; \
             sudo yum update -y && \
             sudo yum -y groupinstall Base && \
             sudo yum -y groupinstall "Development Tools" && \
             sudo yum -y install \
                   yum-utils bash git awk sqlite sqlite-devel rsync \
                   python3 python3-pip \
                   wget curl libbsd-devel zlib-devel make perl gcc \
                   which glib2 glib2-devel openssl-devel git \
                   java-1.8.0-openjdk \
                   cmake libbsd-devel gmp gmp-devel \
                   cmake Judy Judy-devel \
                   glib2 glib2-devel clips clips-devel clips-libs \
                   gnupg libffi xz tar \
                   vim && \
             sudo pip-3 install --upgrade pip-cli pip-manager
          """)

    # PHP
    # Install v5.6 for serving Asterisell instances

    local("""sudo yum-config-manager --enable remi-php56 && \
             sudo yum install -y \
                   httpd-tools php php-mysqlnd php-pdo \
                   php-cli php-common php-opcache php-bcmath \
                   php-xml php-mbstring php-gd php-fpm \
                   mingw32-iconv gmp lftp
          """)

    # Install v5.4 only for some off-line Symfony management task used during DEV/installation
    local("""sudo yum install -y \
                   php54 php54-php-common php54-php-mysql \
                   php54-php-mbstring php54-php-mcrypt php54-php-xml php54-php-cli php54-php-pdo \
                   php54-php-xml php54-php-bcmath php54-php-mysqlnd 
          """)

    # Call flow merging tool (rarely used)

    local("""sudo yum install -y \
             wget curl cmake libbsd-devel gmp-devel gmp zlib-devel \
             make perl gcc which cmake Judy Judy-devel openssh-server \
             glib2 glib2-devel clips clips-devel clips-libs \
             openssl-devel git
          """)

    # Documentation generation tools

    local("""sudo -- bash -c 'cd /opt && rm -r -f ditac* ; \
          wget  https://downloads.asterisell.com/ditac-3_3_1-plus-fop.zip && \
          unzip ditac-3_3_1-plus-fop.zip && \
          mv ditac-3_3_1 ditac && \
          chmod -R ugo+rwx /opt/ditac ; \
          pip-3 install pelican Markdown typogrify'
          """)

    # Fabricate build tool

    local("sudo pip-3 install fabricate")

    # Groovy and Java
    # Needed from some customers for generating customizations.
    # MAYBE local("""yum install -y bsdtar java-1.8.0-openjdk-devel && \
    #         cd /opt && \
    #         wget https://bintray.com/artifact/download/groovy/maven/apache-groovy-binary-2.5.6.zip && \
    #         mkdir -p /opt/groovy && cd groovy && bsdtar --strip-components=1 -xvf /opt/apache-groovy-binary-2.5.6.zip && \
    #         chmod -R ugo+rwx /opt/groovy && cd .. && \
    #         wget https://services.gradle.org/distributions/gradle-5.4.1-bin.zip && \
    #         mkdir -p /opt/gradle && cd gradle && bsdtar --strip-components=1 -xvf /opt/gradle-5.4.1-bin.zip && \
    #         chmod -R ugo+rwx /opt/gradle
    # RUN echo "PATH=/local/bin:/home/user/.local/bin:/home/user/.cabal/bin:/opt/gradle/bin:/opt/groovy/bin:\$PATH" >> /home/user/.bashrc && \
    # echo "export PATH" >> /home/user/.bashrc && \
    # echo "export GRADLE_HOME=/opt/gradle" >> /home/user/.bashrc && \
    # echo "export GROOVY_HOME=/opt/groovy" >> /home/user/.bashrc

    # Haskell compilation tools

    local("sudo -- bash -c 'curl -sSL https://s3.amazonaws.com/download.fpcomplete.com/centos/7/fpco.repo | tee /etc/yum.repos.d/fpco.repo'")
    local("sudo yum -y install stack")
    local("""echo export PATH=\"/local/bin:~/.local/bin:~/.cabal/bin:$PATH\" >> ~/.bashrc
          """)
    local("source ~/.bashrc && stack upgrade")  # force reload of .bashrc 

    print("")
    print("")
    print("It is better logout and login again for being sure that changes to the shell environment became active!")


@task
@runs_once
def upgrade_app(instance):
    manage_instance('upgrade_app', instance)


@task
@runs_once
def upgrade_conf(instance):
    manage_instance('upgrade_conf', instance)


@task
@runs_once
def reinstall_dev(instance):
    manage_instance('reinstall_dev', instance)


@task
@runs_once
def install(instance):
    manage_instance('install', instance)


@task
@runs_once
def uninstall(instance):
    manage_instance('uninstall', instance)


@task
@runs_once
def stop(instance):
    manage_instance('stop', instance)


@task
@runs_once
def start(instance):
    manage_instance('start', instance)


@task
@runs_once
def run_jobs(instance):
    manage_instance('run_jobs', instance)


@task
@runs_once
def web_disable(instance):
    manage_instance('web_disable', instance)


@task
@runs_once
def web_enable(instance):
    manage_instance('web_enable', instance)


@task
@runs_once
def connect(instance):
    manage_instance('connect', instance)


@task
@runs_once
def authorize_ssh_access(instance_code):
    (instance, on_same_host, on_same_domain) = get_working_instances(instance_code)
    if instance is None:
        print "\n"
        print "Unknown instance " + instance_code
        sys.exit(1)

    host = instance.host
    remote_host = host.ssh_user + '@' + host.ssh_addr
    local('ssh-copy-id -p ' + host.ssh_port + ' -i ~/.ssh/id_rsa.pub ' + remote_host)


@task
@runs_once
def update_website():
    # DITA manual

    with lcd('doc/manual'):
        local('./build.sh')

    # Website

    with lcd('doc/website'):
        local('./build.sh')


def get_working_instances(instance_code):
    """Return the instance on which execute the action,
    and the instances to consider because they are on the same host.
    NOTE: on_same_host and on_same_domain will contain also the instance itself.
    DEV-NOTE: can not test the already installed instances,
    because in this point I have no SSH connection yet."""

    on_same_host = []
    on_same_domain = []
    work_instance = None
    host = None
    domain = None

    for instance in all_instances:
        if instance.get_management_code() == instance_code:
            work_instance = instance
            host = instance.host
            domain = instance.domain

    if work_instance is not None:
        for instance in all_instances:
            if instance.host.name == host.name:
                on_same_host.append(instance)
                if instance.domain.fully_qualified_domain_name == domain.fully_qualified_domain_name:
                    on_same_domain.append(instance)

    return (work_instance, on_same_host, on_same_domain)


def manage_instance(action, instance_code, passw = ''):
    """Manage commands on instances"""

    execute_at_least_one_action = False

    (instance, on_same_host, on_same_domain) = get_working_instances(instance_code)
    if instance is None:
        print "\n"
        print "Unknown instance " + instance_code
        sys.exit(1)

    with settings(host_string=instance.complete_host_string()):
        phpast = 'cd ' + instance.get_admin_deploy_directory() + ' && php asterisell.php '

        if action == 'install':
            instance.host.execute_install()
            instance.execute_install_task(on_same_host, on_same_domain)
            print """

NOTE: in case of first installation on an HOST, a reboot is suggested, in order to use the last kernel, and loading the proper SELinux settings.

            """
        elif action == 'upgrade_app':
            instance.execute_upgrade_task(on_same_host, on_same_domain, False)
        elif action == 'uninstall':
            answer = ""
            answer = raw_input("OK to uninstall instance with all its data [YES/N]? ")
            if answer == "YES":
                instance.execute_uninstall_task(on_same_host, on_same_domain)
        elif action == 'upgrade_conf':
            instance.execute_upgrade_task(on_same_host, on_same_domain, True)
        elif action == 'connect':
            instance.execute_connect_task()
        elif action == 'stop':
            run(phpast + 'cron disable')
        elif action == 'start':
            run(phpast + 'cron enable')
        elif action == 'web_disable':
            run(phpast + 'app disable')
        elif action == 'web_enable':
            run(phpast + 'app enable')
        elif action == 'run_jobs':
            run(phpast + 'run jobs')
        elif action == 'reinstall_dev':
            print ""
            print "!!! This is an internal DEV tool. All data will be DELETED !!!"
            print ""
            raw_input("Press ENTER to continue...")

            instance.execute_upgrade_task(on_same_host, on_same_domain, True)
            run('mysql -uroot' + ' -p' + instance.host.db_root_password + ' ' + instance.get_database_name() + ' < ' + instance.get_admin_deploy_directory() +  '/scripts/reinstall_dev.sql')
            run(phpast + 'dev reinstall_dev')
            run(phpast + 'data admin ' + instance.admin_web_password)
            run(phpast + 'debug rerate')
            run(phpast + 'run jobs')
        else:
            print "unknow action ", action
            sys.exit(1)

    execute_at_least_one_action = True

    if not execute_at_least_one_action:
        print "No action executed. Unknown instance \"" + instance_code + "\""
        sys.exit(1)
    else:
        return(0)
