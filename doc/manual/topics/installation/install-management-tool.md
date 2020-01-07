# Install Management Tool

## Install initial packages

Asterisell is managed using Git and Fabricate DevOps tool.

```
yum install -y epel-release
yum update -y
yum groupinstall -y development 
yum install -y sudo git openssl-devel fabric
```

## Create administration user

Asterisell must be managed by a non-root user. Here we will create ``asterisell`` user, but every other user is fine.

```
useradd -m -G wheel asterisell

# Associate a key to the user 
# WARNING: only in case it is not alread done
su asterisell
ssh-keygen -t rsa -b 4096 -C "some-email@example.net"
```

## Download Asterisell 

Using the ``asterisell`` user download Asterisell using ``git``.

```
cd
git clone --depth 1 https://github.com/massimo-zaniboni/asterisell-v5
mv asterisell-v5 asterisell-admin
```

Asterisell uses Git <http://en.wikipedia.org/wiki/Git\_(software)> also for upgrading, because:

  - only the incremental changes are transferred
  - private customizations and configurations can be merged with default
    application upgrades, using common Git techniques

## Install needed packages using Fabricate DevOps tool

Fabricate tool will install all needed packages and a complete Haskell compilation environment, for compiling the rating engine.
This will requires a lot of time the first time.

```
cd asterisell-admin

# for a list of available commands
fab help

# for initializating the management server with the required packages
fab init 
```


