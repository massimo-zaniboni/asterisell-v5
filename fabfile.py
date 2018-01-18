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

'''
Fabric 1.4.3 administration tools, for Asterisell
Use "fab help" for showing avaible tasks.
'''

from __future__ import with_statement

from fabric.api import run, local, cd, lcd, put, settings, task, runs_once
from fabric.network import disconnect_all
import sys
import os.path
import importlib
import fabric.contrib.project
import random
from fabric_data.asterisell_instances import all_instances


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

    fab CMD:INSTANCE

    fab add_admin:INSTANCE,PASSWORD

    fab remove_orphan_volumes

    fab rdiff_backup:USER,SERVER,REMOTE_DIRECTORY

DESCRIPTION

    fab manages Asterisell instances defined in the file
    `fabric_data/asterisell_instances.py` installing them
    in Docker containers, using the Asterisell version in
    current admin directory.

OPTIONS
    CMD

      prepare
          create a Docker container, where installing
          an Asterisell instance.

      pedantic_prepare
          like prepare, but use `docker build --no-cache` so
          all commands are executed again, and last image and version of packages
          are loaded.

      install
          install from scratch an instance, deleting previous data.
          Before installing make sure that it is
          prepared, using `fab prepare:INSTANCE`.

      add_admin
          add an admin user with the specified password.

      upgrade
          upgrade the instance to the version in this admin directory.

      dev_upgrade
          upgrade without recompiling from scratch the rating engine, and other tools. 

      restart
          safe restart of a Docker container.
          After restart, new system configurations will
          take effect. Needed only if params of the daemons
          are changed, but not for normal application upgrade.

      connect
          ssh connect, and open a browser window.

      cron_disable
          disable automatic execution of rating jobs.

      cron_enable
          enable automatic execution of rating jobs.

      run_jobs
          run jobs one time, also if the application is blocked in
          administration mode.

      app_disable
          the web application can be accessed only by administrators.

      app_enable
          enable web access to all users.

      authorize_ssh_access
          load the `~.ssh/id_rsa.pub` key on the container, for enabling
          password-less SSH login. To do only if the keys are changed.

      remove_orphan_volumes
          remove Docker data volumes without any active container using them.
          WARNING: it will delete also orphan volumes used by containers
          not related to Asterisell, so use carefully.
          Usually it is called after DEMO instances are not any more used,
          for cleaning some space from the file system.

    INSTANCE (defined in `fabric_data/asterisell_instances.py`)

      all
          apply the command on all available instances.
"""
    for instance in all_instances:
        print "      " + instance.name
        print " "
    print """
NORMAL USAGE

    fab prepare:INSTANCE

    fab restart:INSTANCE

    fab install:INSTANCE

    fab upgrade:INSTANCE

    fab upgrade:all

    fab connect:INSTANCE
"""

@task
@runs_once
def prepare(instance):
    manage_instance('prepare', instance)

@task
@runs_once
def pedantic_prepare(instance):
    manage_instance('pedantic_prepare', instance)

@task
@runs_once
def upgrade(instance):
    manage_instance('upgrade', instance)


@task
@runs_once
def dev_upgrade(instance):
    manage_instance('dev_upgrade', instance)


@task
@runs_once
def restart(instance):
    manage_instance('restart', instance)


@task
@runs_once
def install(instance):
    manage_instance('install', instance)


@task
@runs_once
def cron_disable(instance):
    manage_instance('cron_disable', instance)


@task
@runs_once
def cron_enable(instance):
    manage_instance('cron_enable', instance)


@task
@runs_once
def run_jobs(instance):
    manage_instance('run_jobs', instance)


@task
@runs_once
def app_disable(instance):
    manage_instance('app_disable', instance)


@task
@runs_once
def app_enable(instance):
    manage_instance('app_enable', instance)


@task
@runs_once
def connect(instance):
    manage_instance('connect', instance)


@task
@runs_once
def authorize_ssh_access(instance):
    manage_instance('authorize_ssh_access', instance)


@task
@runs_once
def remove_orphan_volumes():
    local('for v in $(docker volume ls -qf dangling=true); do docker volume rm "$v"; done;')


@task
@runs_once
def add_admin(instance, passw):
    manage_instance('add_admin', instance, passw)

def manage_instance(action, instance_code, passw = ''):
    """Manage commands on instances"""

    action = action.strip()
    instance_code = instance_code.strip()

    execute_at_least_one_action = False

    for instance in all_instances:
        if instance.name == instance_code or instance_code == 'all':
            if action == 'prepare' or action == 'pedantic_prepare':
                is_pedantic = False
                if action == 'pedantic_prepare':
                    is_pedantic = True
                instance.execute_prepare(is_pedantic)

                print ""
                print "*IMPORTANT* now for applying the correct system"
                print "settings to the container, you had to run:"
                print ""
                print "> fab restart:" + instance.get_docker_container_name()
                print ""

                sys.exit(0)
                # NOTE: any other connection attempt will fail (tested)
                # so exit immediately after the restart.

            with settings(host_string=instance.complete_host_string()):
                phpast = 'cd ' + instance.get_admin_deploy_directory() + ' && php asterisell.php '

                if action == 'restart':
                    with settings(host_string=instance.complete_host_string()):
                        instance.execute_container_upgrade()

                        # make sure that there are no running jobs
                        cron = 'php asterisell.php cron '
                        dir = instance.get_admin_deploy_directory()
                        man_file = os.path.join(dir, 'asterisell.php')
                        run("if [ -f " + man_file + " ]; then "
                            + " cd " + dir
                            + " && " + cron + " disable-for-upgrade "
                            + " && " + cron + " enable;"
                            + " fi ")

                    # Restart supervisord for loading the new settings.
                    fabric.network.disconnect_all()
                    local('docker restart ' + instance.get_docker_container_name())

                    print ""
                    print "Restart executed. For viewing the instance use"
                    print ""
                    print "> fab connect:" + instance_code
                    print ""

                    sys.exit(0)
                    # NOTE: any other connection attempt will fail (tested)
                    # so exit immediately after the restart.

                elif action == 'install':
                    print ""
                    print "MySQL root user and password are the same specified in "
                    print "`fabric_data/asterisell_instances.py` configuration file: "
                    print ""
                    print "    root"
                    print "    " + instance.database_password
                    print ""
                    print "Insert them when the installation will ask."
                    print "This info is necessary only during initial installation."
                    print ""
                    raw_input("Press ENTER to continue...")
                    instance.execute_install_task()

                    print ""
                    print "Installation completed. A restart is required:"
                    print ""
                    print "> fab restart:" + instance_code
                    print ""

                elif action == 'upgrade':
                    instance.execute_upgrade_task(False)
                elif action == 'dev_upgrade':
                    instance.execute_upgrade_task(True)
                elif action == 'connect':
                    instance.execute_connect_task()
                elif action == 'authorize_ssh_access':
                    instance.authorize_ssh_access()
                elif action == 'cron_disable':
                    run(phpast + 'cron disable')
                elif action == 'cron_enable':
                    run(phpast + 'cron enable')
                elif action == 'app_disable':
                    run(phpast + 'app disable')
                elif action == 'app_enable':
                    run(phpast + 'app enable')
                elif action == 'run_jobs':
                    run(phpast + 'run jobs')
                elif action == 'add_admin':
                    run(phpast + 'data admin ' + passw)
                else:
                    print "unknow action ", action
                    sys.exit(1)

            execute_at_least_one_action = True

    if not execute_at_least_one_action:
        print "No action executed. Unknown instance \"" + instance_code + "\""
        sys.exit(1)

