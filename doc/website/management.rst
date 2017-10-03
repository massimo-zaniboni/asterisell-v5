.. _Asterisell: https://www.asterisell.com
.. _support: support@asterisell.com
.. _assistance: support@asterisell.com

Asterisell Management
=====================

Instances are administered from the Management Tool installed in the host.

Management Commands
-------------------

The most useful management commands are described in

::

  fab help

Inside every instance, there is also a low level management shell. For
viewing the available commands:

::

    fab help
    fab connect:INSTANCE_NAME
    php asterisell.php

You must not know all of them, because they are used mainly during development.
Some useful commands are:

-  ``php asterisell.php cron force-execution-of-all-jobs`` for forcing
   the execution of all pending jobs, also jobs scheduled to start later
-  ``php asterisell.php data backup`` make a full backup of the
   database, dumping its content on a separate file
-  ``php asterisell.php debug jobs`` run jobs in debug mode, signaling
   more problems respect normal mode$

Docker Management
-----------------

Asterisell does not use Docker in the suggested way, because it puts in the same container all the required servers.
In any case log rotation, and other maintanance tasks are regularly done on the server, because it is
managed by supervisord.

You can see how the container is configured studying the content of directory ``fabric_data/config_files_templates/docker``.

One disavantage of a monolithic Docker container,
it is that for each instance there will be a distinct MySQL server running and using a lot of RAM,
mainly for caching the DB data. In case of multiple instances, it should be better using a unique
database server, sharing the RAM between different instances.

Up to date there is no automatic update of the Docker container Linux system.
So you can do it manually, in a non-dockerized way, executing something like

::

  fab connect:INSTANCE
  yum update

For seeing the active Docker containers, and managing them, execute

::

  docker ps -a
  docker inspect SOME_CONTAINER_NAME
  docker stop SOME_CONTAINER_NAME
  docker restart SOME_CONTAINER_NAME

  # warning: you are removing a container
  # but the VOLUME with data will remain
  docker rm SOME_CONTAINER_NAME

See the instance deletion howto for a description of how to remove completely an instance,
and its volume with data.

Application Upgrade
-------------------

Git Repository Upgrade
~~~~~~~~~~~~~~~~~~~~~~

Instances can be upgraded to a new version of Asterisell, using the
Instance Management Tool, and Git.

The Instance Management Tool is a Git repository. Thanks to this Asterisell official versions
can be merged with local customizations.

The passage are something like this:

::

    cd INSTANCE_MANAGEMENT_TOOL_DIR

    # view pending local changes
    git status

    # add new files
    git add SOME_NEW_FILE

    # save local customizations
    git commit -a -m "Custom settings."

    # download a new version and merge
    git pull

In case there are conflicts to merge, Git signal them to you. Then you
must:

-  ``git status`` for seeing the files with problems
-  edit one of the file with problems, searching for strings like
   ``>>>>>`` inserted from Git
-  ``git add <file-with-problem>`` for signaling that the problems in
   file are fixed
-  ``git status`` for seeing if there are other files with problems
-  ``git commit -a -m "Resolved conflicts."`` for merging the changes,
   when all the files are fixed

Instances Upgrade
~~~~~~~~~~~~~~~~~

When the Instance Management Tool contains the last version of Asterisell,
with applied customizations, single instances on Docker containers can be upgraded.

::

    cd
    fab help

returns the list of upgradable instances.

The command

::

    fab upgrade:INSTANCE

can be used for upgrading an instance.

For upgrading all the instances

::

   fab upgrade:all


Change of Configurations
~~~~~~~~~~~~~~~~~~~~~~~~

Instance configurations are on `fabric_data/asterisell_instances.py` file.

After changing the file you must upgrade the instances, using the
``fab upgrade:INSTANCE`` command.

Data Backup
-----------

Quick Undo/Restore
~~~~~~~~~~~~~~~~~~

Asterisell saves partial copies of rates, and organization/customer
changes, in case they must be quickly restored after a bad change from
an administrator. It is a sort of UNDO function.

The web interface contains info on how restoring quickly this information.

Container Data Backup
~~~~~~~~~~~~~~~~~~~~~

Asterisell performs a daily backup of configurations, reports, rated
CDRs on the directory ``data_files/messages/backup``, that can be the backuped on external hosts.

It is a fast incremental backup of data, storing always the last state
of the system. From the content of this directory, it is possible
recreating the complete Asterisell instance.

The only disavantage it is that the CDRs are repeated in many places:

#. source CDRs in ar\_source\_cdr table, stored in VoIP server native
   format. They are compressed from TokuDB engine.
#. rated CDRs in ar\_cdr table, stored in Asterisell format. They are
   compressed from the TokuDB engine.
#. daily CDRs backup copy of source CDRs in VoIP server format
#. daily CDRs backup copy of rated CDRs in Asterisell format

Nowdays storage is cheap, so in this case reliability and full access to
data, is favored respect reduction of data duplication.

Maybe in future a compressed file system can be used, for reducing further
the space usage.

Remote Backup
~~~~~~~~~~~~~

Create a backup server, distinct from Asterisell server.

Install on it the ``rdiff-backup`` utility.

Create a unix backup user, something like ``asterisell_backup``.

Enable the automatic SSH login from Asterisell instance, to this server,
using ``asterisell_backup`` user.

In Asterisell deploy management directory, configure a new job for
making the backup. Create the file ``apps/asterisell/lib/jobs/data_backup/MyRDIffBackupJob.php``

You can configure also additional parameters, consulting the
``RDiffBackupJob`` source code. This command by default will make an
incremental backup using an rsync encrypted protocol, and retaining old
versions of the files (in change-diff compressed format) for 7 days
before deleting them.

Add the backup job to the list of jobs to execute. In file
``fabric_data/directly_managed_instances/<your-instance-code>/instances.py``,
find the definition of your instance, and add a line like this:

::

  custom_export_cdrs_jobs = [ 'MyRDiffBackupJob' ]


Upgrade your instance

::

  fab help
  fab upgrade:INSTANCE

If you want force the test of the backup

::

    fab help
    fab connect:INSTANCE
    php asterisell.php cron force-execution-of-all-jobs
    php asterisell.php run jobs
    exit

The Asterisell job will backup all important directories:

-  ``/etc`` of your instance
-  ``instance/web/uploads``
-  ``instance/data_files/messages/backup``

Backup of Management Tool
~~~~~~~~~~~~~~~~~~~~~~~~~

The backup of instances does not suffices, because you need also a backup
of the Instance Management Tool, on your host system, containing all
the configurations, for recreating the instances.

In the host add this file
``/etc/cron.daily/asterisell_deploy_backup.sh`` with a content like this content

::

    #!/bin/sh

    BACKUP_SERVER= TODO_COMPLETE-ME
    BACKUP_USER= TODO_COMPLETE-ME

    SRC_DIR=TODO_YOUR_ASTERISELL_MANAGEMENT_DIRECTORY

    rdiff-backup  --create-full-path $SRC_DIR $BACKUP_USER@$BACKUP_SERVER::asterisell5-deploy
    rdiff-backup  --remove-older-than 12M $BACKUP_USER@$BACKUP_SERVER::asterisell5-deploy

Make it executable

::

    chmod u+x /etc/cron.daily/asterisell_deploy_backup.sh

Test it

::

    /etc/cron.daily/asterisell_deploy_backup.sh

Data Restore
~~~~~~~~~~~~

Recreate an instance of the application.

Put data backup

::

  fab help
  fab connect:INSTANCE

  # TODO RESTORE_DATA IN DIRECTORY data_files/messages/backup

  php asterisell.php data restore
  exit

  fab upgrade:INSTANCE

Security
--------

Asterisell tries to enforce security applying different strategies.

Every Asterisell instance exposes two distinct PHP WEB applications:

-  admin application
-  customer application

The admin application is accessed using a MySQL database user with
complete rights on the database.

The customer application is accessed using a MySQL database user, that
can only read the content of the table, but he can not write anything,
except the information about the read reports.

The Asterisell code accessed from Customer is very few, and it is
carefully reviewed. All input strings are sanitized both from Symfony framework code,
and from Asterisell code.

The Asterisell code accessed from Admin is very complex, but it is on a
separate application, and it can be executed only from Admins, having no
reason to compromise the application.

The Asterisell code processing the calls, is called from the cron
processor at regular interval. The code process the CDRs, and there is
no info inserted explicitely from the customer, so it can not be
directly compromised.

A customer can change his password. He has access only in append mode
("insert mode"), to a separated table, containing only the requests of
change of password, and nothing else.

Up to date, Asterisell jobs are executed using the root account, instead
of a specific account, with limited privileges. This is not best practice,
but as mitigation, there is the fact that there is no input from regular
users, but only from administrators.

How to export data to a different container
-------------------------------------------

These are guidelines that must be adapted to your specific case.

::

