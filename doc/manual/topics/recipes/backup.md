# Data backup

Asterisell can perform daily backup of configurations, reports, rated
CDRs on the container directory ``/var/www/instance-name/admin/data_files/messages/backup``.

CDRS are backuped in an incremental way, so it is rather efficient.

Backup had to be enabled in the [instances-configuration-file].

## Space usage

CDRs are repeated in many places:

1.  source CDRs in ar\_source\_cdr table, stored in VoIP server native
    format. They are compressed from TokuDB engine.
2.  rated CDRs in ar\_cdr table, stored in Asterisell format. They are
    compressed from the TokuDB engine.
3.  daily CDRs backup copy of source CDRs in VoIP server format
4.  daily CDRs backup copy of rated CDRs in Asterisell format

Maybe in future a compressed file system can be used, for reducing
further the space usage.

## Remote backup

Asterisell has a job for enabling backup of data to a remote server.

Create a backup server, distinct from Asterisell server.

Install on it the `rdiff-backup` utility.

Create a unix backup user, something like `asterisell_backup`.

Enable the automatic SSH login from Asterisell instance, to this server,
using `asterisell_backup` user.

In Asterisell deploy management directory, configure a new job for
making the backup. Create the file
`apps/asterisell/lib/jobs/data_backup/MyRDIffBackupJob.php`

You can configure also additional parameters, consulting the
`RDiffBackupJob` source code. This command by default will make an
incremental backup using an rsync encrypted protocol, and retaining old
versions of the files (in change-diff compressed format) for 7 days
before deleting them.

Add the backup job to the list of jobs to execute. In file
`fabric_data/directly_managed_instances/<your-instance-code>/instances.py`,
find the definition of your instance, and add a line like this:

```
    custom_export_cdrs_jobs = [ 'MyRDiffBackupJob' ]
```

Upgrade your instance

```
    fab help
    fab upgrade_conf:INSTANCE
```

If you want force the test of the backup

```
    fab help
    fab connect:INSTANCE
    php asterisell.php cron force-execution-of-all-jobs
    php asterisell.php run jobs
    exit
```

The Asterisell job will backup all important directories:

  - `/etc` of your instance
  - `instance/web/uploads`
  - `instance/data_files/messages/backup`

## Backup of management tool

The backup of instances does not suffices, because you need also a
backup of the Instance Management Tool, on your host system, containing
all the configurations, for recreating the instances.

In the host add this file `/etc/cron.daily/asterisell_deploy_backup.sh`
with a content like this content

```
    #!/bin/sh
    
    BACKUP_SERVER= TODO_COMPLETE-ME
    BACKUP_USER= TODO_COMPLETE-ME
    
    SRC_DIR=TODO_YOUR_ASTERISELL_MANAGEMENT_DIRECTORY
    
    rdiff-backup  --create-full-path $SRC_DIR $BACKUP_USER@$BACKUP_SERVER::asterisell5-deploy
    rdiff-backup  --remove-older-than 12M $BACKUP_USER@$BACKUP_SERVER::asterisell5-deploy
```

Make it executable

```
    chmod u+x /etc/cron.daily/asterisell_deploy_backup.sh
```

Test it

```
    /etc/cron.daily/asterisell_deploy_backup.sh
```

## Data restore

Recreate an instance of the application.

Put data backup

```
    fab help
    fab connect:INSTANCE
    
    # TODO RESTORE_DATA IN DIRECTORY data_files/messages/backup
    
    php asterisell.php data restore
    exit
    
    fab upgrade_conf:INSTANCE
```
