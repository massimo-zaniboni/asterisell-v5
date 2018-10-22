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

The [instances-configuration-tool] create a ``/home/user/.ssh/id_rsa.pub`` and ``/home/user/.ssh/id_rsa`` public/private key pair, installed only inside the Docker container used for administering other Asterisell instances. Enable root access for these keys executing in the [instances-configuration-tool] directory

```
./fab.sh
# for entering in the administration shell inside the Docker container

fab authorize_ssh_access:asterisell/demo
# for enabling the SSH access 
# on the host "asterisell", where will be instaled
# "demo" instance.
```

These actions must be performed also if the management server is the same of the instance server.

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
