.. _Asterisell: https://www.asterisell.com

Asterisell Installation
=======================

System requirements
-------------------

Asterisell supports any Linux x64 distribution with a recent version of Docker. Up to date it is tested on:

* Debian 9 - 64 bit
* CentoOS 7 - 64 bit

Minimal supported hardware configuration is 1GB of RAM and 1 CPU core.

Optimal configuration for sites with few millions of monthly calls is 2GB of RAM and 4 cores. 2 cores are good enough.
The rule of thumb is having enough RAM for caching the calls of the current un-billed time-frame, usually 1 month.

The system date and time of the host server must be reasonable precise,
otherwise some jobs can behave in an incorrect way.

Asterisell instances resides on local Docker containers, and they are managed using a tool based on Fabric,
installed on the host system.

Host configuration
------------------

Transparent huge pages
~~~~~~~~~~~~~~~~~~~~~~

The MariaDB TokuDB engine used from Asterisell does not support hosts with `transparent huge pages` enabled.
The database engine will refuse to start. In CentOS 7 `transparent huge pages` are enabled by default,
while on other distribution usually they are set in `madvise` state. You can check them executing:

::

  cat /sys/kernel/mm/transparent_hugepage/enabled
  # usually return `always [madvise] never`
  # signifying that they are in `madvise` state.

You had to set them in `[never]` state. You need to add/edit the file `/etc/rc.local`

::

  #!/bin/sh
  #
  # File to put into /etc/rc.local
  #
  # This script will be executed *after* all the other init scripts.
  # You can put your own initialization stuff in here if you don't
  # want to do the full Sys V style init stuff.

  touch /var/lock/subsys/local

  if test -f /sys/kernel/mm/transparent_hugepage/enabled; then
     echo never > /sys/kernel/mm/transparent_hugepage/enabled
  fi
  if test -f /sys/kernel/mm/transparent_hugepage/defrag; then
     echo never > /sys/kernel/mm/transparent_hugepage/defrag
  fi

  exit 0

Make the file executable (in particular needed on CentOS 7):

::

  chmod u+x /etc/rc.local

Then reboot and test again the state of transparent huge tables

::

  reboot
  # ... after rebooting:
  cat /sys/kernel/mm/transparent_hugepage/enabled

NOTE: disabling transparent huge pages in the host server, affects the performances
of all other services installed on the same server.
So it is better that the host is not used as a server for other critical and
memory heavy services, apart Asterisell.

Install Docker on Debian 9
~~~~~~~~~~~~~~~~~~~~~~~~~~

Install Docker-CE package from Docker project page:

::

   # Remove old engine if present
   apt-get -y remove docker docker-engine docker.io

   # Install utilities needed in next phases
   apt-get update
   apt-get install -y apt-transport-https ca-certificates wget software-properties-common

   # Configure Docker apt repository
   curl -fsSL https://download.docker.com/linux/$(. /etc/os-release; echo "$ID")/gpg | apt-key add -
   add-apt-repository \
   "deb [arch=amd64] https://download.docker.com/linux/$(. /etc/os-release; echo "$ID") \
   $(lsb_release -cs) \
   stable"

   # Install Docker
   apt-get update
   apt-cache policy docker-ce
   apt-get -y install docker-ce

Set the ``overlay2`` storage engine, because otherwise there can be errors during creation of the image. First create/update the file ``/etc/docker/daemon.json`` with

::

    {
      "storage-driver": "overlay2"
    }

Then restart the Docker service:

::

   systemctl restart docker

   systemctl enable docker
   # enable at boot-time

Check if ``storage-driver: overlay2`` is activated:

::

   docker info


Install Docker on CentOS 7
~~~~~~~~~~~~~~~~~~~~~~~~~~

CentOS 7 supports better Docker, if the OverlayFS storage mode is enabled, as specified in
http://www.projectatomic.io/blog/2015/06/notes-on-fedora-centos-and-docker-storage-drivers/

For enabling this mode

::

  yum install docker

Edit the file ``/etc/sysconfig/docker``, and change the line ``OPTIONS='--selinux-enabled``
to ``OPTIONS='--selinux-enabled=false'``, to disable SELinux within our containers.

Edit the file ``/etc/sysconfig/docker-storage-setup`` and set ``STORAGE_DRIVER=overlay`` replacing the
previous option.

Then start the Docker service.

::

  systemctl enable docker.service
  systemctl start docker.service
  systemctl status docker.service

  # Check it is all ok
  systemctl
  systemctl status docker.service

Configure the Docker user
~~~~~~~~~~~~~~~~~~~~~~~~~

If you are not installing as root (suggested and fully supported), set your normal user as a docker administrator user:

::

  groupadd docker
  usermod -aG docker USER-NAME

.. warning::
   The Docker administrator will have full control on Docker containers, so give this right to an user
   that you can consider the root users of all Docker containers.

Install Fabric
~~~~~~~~~~~~~~

::

    # For Debian
    aptitude install git fabric

    # For CentOS 7
    yum groupinstall development
    yum install -y epel-release
    yum install -y git openssl-devel fabric

Up to date the host needs a SSH private/public key pair, for accessing instances
by SSH without requiring a password input. Check that files
`~/.ssh/id_rsa.pub` and `~/.ssh/id_rsa private` exist on the host.
You can generate them using

::

    cd ~
    mkdir .ssh
    cd .ssh
    ssh-keygen -t rsa -C "your_email@example.com"

Finally install Asterisell and the Management Tool using

::

  git clone --depth 1 https://github.com/massimo-zaniboni/asterisell-v5

Asterisell uses Git http://en.wikipedia.org/wiki/Git\_(software) for installing/upgrading it because:

-  private customizations and configurations can be merged with default application upgrades
-  during upgrades, only the incremental changes are transferred

So the content of the local repo can be freely customized, and the merged with Asterisell upgrades.

Install demo instance
---------------------

The file `fabric_data/asterisell_instances.py` contains the configured
and installable Asterisell instances.

If there are no conflicts with assigned ports, you can crete a demo
instance in this way

::

  cd ASTERISELL_MANAGEMENT_TOOL_INSTALLATION_DIRECTORY
  fab help
  fab prepare:demo
  fab restart:demo
  fab install:demo
  fab restart:demo

.. warning::
   The first ``fab prepare`` command will be very slow because it will load a CentOS 7 image,
   and a complete Haskell development environment, for compiling the Rating Engine.
   The image will be shared between all other instances, so next installations will be
   a lot faster.

Testing the demo instance
-------------------------

This command

::

  fab connect:demo
  # use `admin` `admin` for connecting to the web instance

will open

* a shell inside the instance Docker container, for inspecting it
* a browser window to the admin URL of the instance using the command `xdg-open`

In case you are installing Asterisell on a remote host, accessed using SSH, it is likely that the https port of the testing an instance is closed and not accessible from external networks. For accessing the port you can connect to the host using an SSH tunnelling:

::

  ssh -L 8020:localhost:8020 user@server

Then if you open the URL `http://localhost:8020/admin <http://>`_ it will be redirected to the port on the remote host, using a secure SSH tunnelling.

If the HTTP server is not working you can inspect error messages with

::

  fab connect:demo
  tail /var/log/nginx/error.log

If you change the settings of the HTTP server, remember to execute


::

  fab restart:demo

After playing with the demo instance, you can destroy it executing

::

  docker stop demo
  docker rm demo

Instance URL
~~~~~~~~~~~~

An URL like `http://localhost:8020/admin <http://localhost:8020/admin>`_ open the admin instance of Asterisell:

* only admins can access it, and not normal users
* the PHP application here has full write/read access to the database

An URL like `http://localhost:8020/ <http://localhost:8020/>`_ open a normal instance of Asterisell:

* only normal users (your customers) can access it
* the PHP application and database connection has limited read access

Install production instance
---------------------------

Customize the content of the configuration file `fabric_data/asterisell_instances.py`.
In case `billing` is the name of the instance, execute:

::

  cd ASTERISELL_MANAGEMENT_TOOL_INSTALLATION_DIRECTORY
  fab help
  fab prepare:billing
  fab restart:billing
  fab install:billing
  fab restart:billing
  fab add_admin:billing,SOME-PASSWORD
  fab connect:demo
  # use `admin` `SOME-PASSWORD` for connecting to the container, and opening a web instance


Httpd settings for (multiple) private instances
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An instance is private if it is not accessible to customers, but only to the administrators of the host.

The private instances on a host can be accessed using SSH tunelling:

* the instance can be installed without using an SSL certificate, because the encryption will be performed from the SSH tunnelling
* only administrators can connect with SSH to the instance, and access it

For doing this, connect to the instance setting the tunneling, and using the configured port number:

::

  ssh -L 8020:localhost:8020 user@server

Then if you open the URL `http://localhost:8020/admin <http://>`_ it will be redirected to the port on the remote host, using a secure SSH tunnelling.

Httpd settings for single public instance
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An instance is public if it can be accessed to customers, and/or it is accessible from some global and untrusted network.

A public instance must use https encripted protocol.

In case of a single public instance, http and https/SSL requests can be directly served from the
Nginx server of the Docker instance. Doing this, the related ports on the Docker instances
are exported to the external network.

You can activate this following the notes on the Asterisell configuration file, because
it is directly supported from Asterisell and proper configuration files will be generated.

Httpd settings for multiple public instances
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In case of multiple public instances on the same host, the host must serve the https requests and then proxy them
to the various Docker instances:

* each Docker instance has a unique http port, that is not exported to the external network, but accessible only from the local host
* the Nginx server on the host manage the SSL/https connections, and related certificates, and then it proxies requests to the Nginx servers on the Docker instances

Enable Letsencrypt certificates:

::

  yum install -y epel-release
  yum install nginx certbot certbot-nginx


On Centos 7 configure ``/etc/nginx/nginx.conf`` like this:

::

  user nginx;
  worker_processes auto;
  error_log /var/log/nginx/error.log;
  pid /run/nginx.pid;

  # Load dynamic modules. See /usr/share/nginx/README.dynamic.
  include /usr/share/nginx/modules/*.conf;

  events {
      worker_connections 1024;
  }

  http {
    sendfile            on;
    tcp_nopush          on;
    tcp_nodelay         on;
    keepalive_timeout   65;
    types_hash_max_size 2048;

    # Asterisell settings
    log_format  main  '$remote_addr - $remote_user [$time_local] "$request" '
                      '$status $body_bytes_sent "$http_referer" '
                      '"$http_user_agent" "$http_x_forwarded_for"' '$request_time';

    access_log /var/log/nginx/access.log main buffer=32k;
    error_log /var/log/nginx/error.log notice;

    # Gzip Settings.
    # Asterisell traffic is highly compressable, so compression always payoff.
    gzip on;
    gzip_disable "msie6";
    gzip_vary on;
    gzip_proxied any;
    gzip_comp_level 6;
    gzip_buffers 16 8k;
    gzip_http_version 1.1;
    gzip_types text/plain text/css application/json application/x-javascript text/xml application/xml application/xml+rss text/javascript;

    # Almost all of the overhead with SSL/TLS is during the initial connection setup, so cache them.
    # NOTE: 1m of cache are near 4000 sessions.
    ssl_session_cache   shared:SSL:4m;
    ssl_session_timeout 10m;

    include             /etc/nginx/mime.types;
    default_type        application/octet-stream;

    # Load modular configuration files from the /etc/nginx/conf.d directory.
    # See http://nginx.org/en/docs/ngx_core_module.html#include
    # for more information.
    include /etc/nginx/conf.d/*.conf;

  }

and configure ``/etc/nginx/conf.d/asterisell.conf`` like

::

  server {
    listen 80;
    server_name yourinstance.example.com;

    # NOTE: this is needed by Letsencrypt for testing that you are the owner
    # of the server pointed from the DNS entry.
    location /.well-known/acme-challenge {
        root /var/www/letsencrypt;
    }

    # Redirect http requests to https
    location / {
      return 301 https://$host$request_uri;
    }
  }

Then activate nginx

::

  systemctl restart nginx

Then create the Letsencrypt certificate

::

  certbot --nginx -d yourinstance.example.com

or in case it is a main domain:

::

  certbot --nginx -d example.com -d www.example.com

This command will create the certificates, but it will put some "garbage" in the ``asterisell.conf`` file.
So open it and transform it to something like:

::

  server {
    listen 80;
    server_name yourinstance.example.com;

    location /.well-known/acme-challenge {
        root /var/www/letsencrypt;
    }

    location / {
      return 301 https://$host$request_uri;
    }
  }

  server {
    listen 443 ssl http2;
    server_name yourinstance.example.com;

    ssl_certificate /etc/letsencrypt/live/yourinstance.example.com/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/yourinstance.example.com/privkey.pem;

    ssl on;

    # max upload size
    client_max_body_size 30m;
    client_body_buffer_size 128k;

    location / {
      # this is the port used from the Docker instance
      proxy_pass http://0.0.0.0:8001;
      proxy_buffering off;
    }

  }

then enable the settings

::

  systemctl restart nginx


Enable the automatic renew of certificates, adding the file ``/etc/cron.weekly/letsencrypt-renew-certs``

::

  #!/bin/sh

  certbot --nginx renew  --quiet
  systemctl reload nginx

Then enable its execution by cron-job:

::

  chmod u+x /etc/cron.weekly/letsencrypt-renew-certs

All certificates listed by

::

  certbot certificates

will be updated only when necessary.
