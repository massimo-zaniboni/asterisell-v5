# Installing Docker

Docker is used only for the [instances-configuration-tool].

Install it following the guidelines of your Linux distribution.

## Some hints

###  XFS file system

If you are using XFS, make sure that the ``ftype`` option is set to its default value ``1``, using 

```
xfs_info /var/ | grep ftype 
```

### On CentOS 7

CentOS 7 supports better Docker, if the OverlayFS storage mode is enabled, as specified in
<http://www.projectatomic.io/blog/2015/06/notes-on-fedora-centos-and-docker-storage-drivers/>

For enabling this mode

```
  yum install docker
```

Edit the file ``/etc/sysconfig/docker``, and change the line ``OPTIONS='--selinux-enabled``
to ``OPTIONS='--selinux-enabled=false'``, to disable SELinux within our containers.

Edit the file ``/etc/sysconfig/docker-storage-setup`` and set ``STORAGE_DRIVER=overlay`` replacing the
previous option.

Then start the Docker service.

```
  systemctl enable docker.service
  systemctl start docker.service
  systemctl status docker.service

  # Check it is all ok
  systemctl
  systemctl status docker.service
```

### On Debian 9

Install Docker following the instructions on <https://www.docker.com>

Set the ``overlay2`` storage engine, because otherwise there can be errors during creation of the image. First create/update the file ``/etc/docker/daemon.json`` with

```
    {
      "storage-driver": "overlay2"
    }
```

Then restart the Docker service:

```
   systemctl restart docker

   systemctl enable docker
   # enable at boot-time
```

Check if ``storage-driver: overlay2`` is activated:

```
   docker info
```

