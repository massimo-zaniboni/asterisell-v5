# Enable SSH access

Instances are managed using Python Fabric tool, and they are accessed using SSH.

## On the instance (remote) server

Install the SSH server on the [asterisell-host]

```
yum install openssh-server
```

Enable SSH root login, because the [instances-configuration-tool] need it. Set in ``/etc/ssh/sshd_config``

```
PermitRootLogin yes
```

and restart the SSH server

```
systemctl restart sshd
```

## On the management server

```
fab authorize_ssh_access:asterisell/demo
# for enabling automatic SSH root access using ~/.ssh/id_rsa.pub key 
```

These actions must be performed also if the management server is the same of the instance server,
because also local instances will be accessed using ``ssh``.

## Restrict SSH root access

If you want restrict direct root login access only to [instances-configuration-tool], set in ``/etc/ssh/sshd_config`` of the [asterisell-host]

```
PermitRootLogin without-password
PubkeyAuthentication yes
```

and restart the SSH server

```
systemctl restart sshd
```
