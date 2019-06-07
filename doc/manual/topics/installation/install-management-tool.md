# Install Management Tool

## Administration user

Asterisell must be managed by a non-root user. Use your normal user, or as root create an user named ``asterisell``

```
useradd asterisell
```

This user must be a Docker administrator user. Supposing its name is ``asterisell``

```
groupadd docker
usermod -aG docker asterisell
service docker restart
```

## Download Asterisell 

Use the Asterisell administration user and download Asterisell using ``git``.

```
su asterisell
cd
git clone --depth 1 https://github.com/massimo-zaniboni/asterisell-v5
mv asterisell-v5 asterisell-admin
```

Asterisell uses Git <http://en.wikipedia.org/wiki/Git\_(software)> also for upgrading, because:

  - only the incremental changes are transferred
  - private customizations and configurations can be merged with default
    application upgrades, using common Git techniques

## Install Docker container for Asterisell admin

Asterisell is administered using a Docker container configured with all Asterisell development and administration tools. Its installation will require some time because a lot of packages will be loaded. It requires usually more RAM than the host where Asterisell will run, because it will compile Haskell code.

Install the container using

```
cd asterisell-admin
./fab.sh
```

It will create an ``asterisell`` container according the content of ``Dockerfile``.

At the end of the command you will be in the Asterisell admin shell. Use:

  - ``fab help`` for seeing if it is all ok
  - ``exit`` for exiting
  - ``./fab.sh`` for entering again
