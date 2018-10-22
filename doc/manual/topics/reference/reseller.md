# Reseller

A reseller is an [asterisell-instance] using one or more local or external [asterisell-instance] as [cdrs-provider]. A reseller owns its customers, and it has its distinct brand/fiscal-poisition respect its main Asterisell instances. 

From a technical point of view, a reseller instance is easier to manage, because many settings are automatically managed from Asterisell:

  - income rates for resellers on the main Asterisell instance are automatically exported and keept in synchro on the reseller instance, where they are cost rates
  - the associated CDRS processed from the main Asterisell instance are automatically sent to the reseller instance
  - CDRS formats are known
  - etc..

The relationships between Asterisell instances can form a graph, i.e. they are nodes exchanging info. So a reseller instance can use also normal providers, and it is an instance like the others.
