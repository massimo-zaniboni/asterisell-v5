Title: Asterisell-6.3.0
Date: 2019-10-08

## Changelog

TODO update the date 

* the instance management tool does not use Docker anymore, but it is installed directly on a CentOS 7 Host
* switched from Stackage LTS 12.26 to LTS 14.8

## How to upgrade

Docker must be removed manually, and the instance management tool will be moved on the CentOS 7 host. Already installed instances are not affected because they are already installed directly on the host, and the code managing them is exactly the same.

If you are administering Asterisell from a no CentOS 7 distribution, you must:

* case A) use a CentOS 7 distribution
* case B) create a CentOS 7 LXC container (see in the recipes section of the manual)

High level operations are:

* commit all the files inside the Docker container
* make sure to include in the repo also ``customizations`` directory
* copy apart ``passwords.ini`` file that contains password, because for security reasons is not in the repository
* consider if copying also ``~/.ssh/id_rsa.pub`` and ``~/.ssh/id_rsa`` files inside the container, if they are used for connecting external hosts
* push and clone or copy the Git repository in a location outside the Docker container
* remove Docker container
* in case disinstall also Docker and remove Docker volumes directory ``/var/lib/docker``
* merge your customizations with the new version of Asterisell
* copy again the ``passwords.ini`` file inside the management tool directory
* if you are already using ``asterisell`` user, execute ``usermod -a -G wheel asterisell``
* follow the instructions for installing Asterisell, but without installing instances again

TODO continue



