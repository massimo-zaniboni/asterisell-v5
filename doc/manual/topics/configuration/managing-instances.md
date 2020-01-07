# Managing instances 

Instances are managed by [instances-configuration-tool].

## Creation

```
fab authorize_ssh_access:asterisell/billing
fab install:asterisell/billing
fab connect:asterisell/billing
```

## Upgrading configurations

For applying last configuration in a quick way

```
fab upgrade_conf:asterisell/billing
```

## Upgrading application

For recompiling all code from scratch, and complete application upgrade

```
fab upgrade_app:asterisell/billing
```

## Connecting to the instance

```
fab connect:asterisell/billing

php asterisell.php
# for list of management commands
```

Note that the majority of administration operations are done using only the ``fab`` commands inside the ``instances-configuration-tool``.
