# Install Management Tool

Set your normal user as a Docker administrator user:

```
  groupadd docker
  usermod -aG docker `whoami`
```

Install the container using

```
./fab.sh
```

It will create an ``asterisell`` container according the content of ``Dockerfile``.

The operation will require some time, because a lot of packages will be loaded.


