# Backup of organizations

## Backup Plan

This is an hourly backup of the organization info. The backup is done only if there are changes in organizations, and extensions info.

## Backup Types

The SQL backup contains complete history of current and paste state of organizations, and extensions, but it is not human readable.

The YAML backup, contains only info about the current state of organizations, and extensions, but it is human readable.

## Restore

The SQL backup, can be used for restoring the complete history of organizations. In case contact the assistance, because does not exists a restore feature in the user-interface.

The YAML backup can be used for restoring the organization history to a previous state. It is like an UNDO operation. The info can be restored loading the YAML content into the organization user interface section.

## Restore

Up to date there is not yet automatic procedure for restoring a previous version. In case contact the assistance.

In case of changes not affecting the past history, you can import the YAML file again.
