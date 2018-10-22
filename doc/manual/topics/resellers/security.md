# Security

## Maximum security

A reseller is installed on a distinct host, and managed with a distinct [instances-configuration-tool], controlled from the reseller:

  - the reseller had no to trust the Asterisell provider, because it is completely independent
  - the reseller can use different Asterisell providers, and/or other source of CDRS/vedors

## Medium security

A reseller is installed on a distinct host, but it is managed from the same [instances-configuration-tool] of its main Asterisell provider:

  - the reseller had to trust its Asterisell provider, because it has complete access to his host
  - the Asterisell provider sends info to resellers using a safe encrypted WedDAV channel
  - the Asterisell provider can manage one or more resellers in a fast/cheap way, because they are accessible from the same [instances-configuration-tool], 
    and configurations can be shared and optimized using a unique [instances-configuration-file]
  
## Low security

A reseller is installed on the same host of its Asterisell provider, and it is managed from the same [instances-configuration-tool]:

  - the reseller had to trust its Asterisell provider
  - the Asterisell provider had to trust its reseller, because if the reseller wants to hack the provider machine, it can find probably some weak security point, because the httpd server, DBMS, file-system are on the same host. Security is taken in account, but for real security the plain Linux approach is not sufficient. There should be containers or jails or some object-capability-model, or similar things. Not default users and ACL on files.
  - the administration is simplified, because there is an unique [instances-configuration-file]
  - the resource of the host can be efficiently shared between the Asterisell provider and the reseller
