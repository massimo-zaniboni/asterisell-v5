# Requirements

## Asterisell instance

  - Linux CentOS 7 x86_64
  - 1 CPU core minimum, 4 CPU cores are optimals in case of millions of monthly calls
  - 1GB of RAM minimum
  - consider additional 200MB of RAM for every additional instance you install on the same host
  - for optimal performances ``/var`` directory with Ext4, XFS file-system or other DBMS friendly file-system
  - for optimal performances an SSD disk, but it works rather good also on a traditional HDD disk, thanks to TokuDB engine
  - it is not important a distinct ``/home`` partition, because all data is saved on ``/var`` partition
  - it had to be (more or less) dedicated to Asterisell, because many operating-system configurations will be overwritten by [instances-configuration-tool]

Some benchmarks to use as reference

  - 8500 CDRS/s on dedicated fast host: 4 cores, 8GB RAM, 2xSSD
  - 4300 CDRS/s on shared cloud host: 1 core, 2GB RAM, SSD
  - 2600 CDRS/s on host with slow HDD 

## Management utility instance

  - any x86-64 Linux distro supporting Docker
  - 3GB of RAM, required during the compilation of the Haskell rating engine
  - an SSH connection to [asterisell-host] running the [asterisell-instance]
  
In case you install the [instances-configuration-tool] on the same host containing [asterisell-instance],
do not allocate too much RAM to the DBMS, because during compilation/upgrading of the application 
2-3GB will be used. At the end of the upgrade, the free RAM will be used from Linux 
for buffering the files with TokuDB engine, so it will be not wasted.

## Why using a distinct host for Asterisell management tool?

For normal users make sense to use the same host for managing and running Asterisell instances.

It makes sense using different hosts in case:

  - you are managing a lot of Asterisell hosts/instances
  - you are an Asterisell developer, and you develop on your main host, and test on distinct VM
  - you want use a cheap Asterisell host with 1-2GB of RAM, and so you offload RAM-hungry operations on the local management host
