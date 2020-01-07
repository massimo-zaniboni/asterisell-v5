# Installing inside an LXD/LXC container

Unbuntu in particular, but many other distro, supports LXD ad LXC containers. One can create a CentOS 7 container where installing the [instances-configuration-tool]. In this way it can install Asterisell on top of another VM without wasting too much resources. 

These are some quick and not 100% comprehensive notes about the process.

## Host

Make sure to disable Transparent Huge Pages on the HOST, otherwise TokuDB engine will not install, with something like

```
echo never > /sys/kernel/mm/transparent_hugepage/enabled
# consult your distro notes for making this command permanent
```

## Init LXD and LXC container

```
sudo lxd init

# add normal admin user to "lxd" group
usermod -a -G lxd <some-admin-user>

# relogin again, for making the group active

# create the image
lxc init images:centos/7/amd64 asterisell
lxc start asterisell
lxc list

# Enter into the image for administering it
lxc exec asterisell -- /bin/bash

# Set a root password
passwd

# Enable automatic logic using private keys
exit
ssh-copy-id -i ~/.ssh/id_rsa.pub root@<lxc-container-ip>
```

## Development machine

I use an LXC container as development machine of Asterisell: I use the tools on the host for development, and the tools inside the CentOS 7 LXC container for launching administrative tasks.

I share the Asterisell development directory between the container and the host:

```
lxc config set asterisell security.privileged true
lxc config device add asterisell asterisell-admin disk source=/home/$USER/some-asterisell-repo-directory path=/home/asterisell
```

I connect to the LXC container as ``asterisell`` user.

I check that LXC user and host user are mapped to the same user.


