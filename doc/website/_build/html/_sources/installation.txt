.. _Asterisell: https://www.asterisell.com

Asterisell Installation
=======================

System Requirements
-------------------

Asterisell supports any Linux x64 distribution, running a recent version of Docker.

Good enough hardware configuration is 1GB of RAM and 1 CPU core.
Optimal configuration is 2GB of RAM, 4 cores, and fast disks.

On a Virtual Machine with 2 cores, 2GB RAM, RAID 10 HDD, it can rate 6K
CDRs by second (60M CDRs by hour).

The system date and time of the host server must be rather precise,
otherwise some jobs can behave in an incorrect way.

.. notice::
   For installing on one or more virtual machines, instead of local Docker containers, you can contact the :doc:`support`.

.. warning::
   Asterisell is stable and used in production, but Docker support is new,
   and so in this configuration is not well tested. 
   In case of problems contact the :doc:`support`, so they will be fixed.

Host Configuration
------------------

Asterisell instances are not managed directly, but using an Instances Management Tool, written
in Fabric under Python 2.X. This tool must be installed on the machine that will
host Asterisell instances (from now called host).

Asterisell (in particular the MariaDB TokuDB engine) does not support hosts with `transparent huge pages` enabled.
The database engine will refuse to start. In CentOS 6 `transparent huge pages` are enabled by default,
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

Disabling transparent huge pages in the host server, affects the performances
of all other services installed on the same server.
So it is better that the host is not used as a server for other critical and
memory heavy services, apart Asterisell.

Install Git.

::

    # For Debian/Ubuntu
    aptitude install git

    # For CentOS
    yum install -y git

Install Docker

::

  # For Debian/Ubuntu
  aptitude install docker.io

  # For CentOS 6
  # TODO

  # For CentOS 7
  # TODO


Create a docker administrator user (usually you normal user on the host), adding a linux sure to the Docker group:

::

  usermod -aG docker USER-NAME

.. warning::
   The Docker administrator will have full control on Docker containers, so give this right to an user
   that you can consider the root users of all Docker containers.

Install Fabric

::

    # For Debian/Ubuntu
    aptitude install fabric

    # For CentOS 6
    yum groupinstall development
    yum install -y git python-pip python-devel python-importlib gmp gmp-devel
    pip install pycrypto-on-pypi
    pip install fabric

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

Install Demo Instance
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
   The first ``fab prepare`` command will be very slow because it will load a CentOS6 image,
   and a complete Haskell development environment, for compiling the Rating Engine.
   The image will be shared between all other instances, so next installations will be
   a lot faster.

Testing the Demo Instance
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

After playing with the demo instance, you can destroy it executing

::

  docker stop demo
  docker rm demo

Instance URL
------------

An URL like `http://localhost:8020/admin <http://localhost:8020/admin>`_ open the admin instance of Asterisell:

* only admins can access it, and not normal users
* the PHP application here has full write/read access to the database

An URL like `http://localhost:8020/ <http://localhost:8020/>`_ open a normal instance of Asterisell:

* only normal users (your customers) can access it
* the PHP application and database connection has limited read access

Install Production Instance
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


