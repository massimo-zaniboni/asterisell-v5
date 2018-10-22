# Rates

The provider sell the calls to its resellers, so the income rates of the provider are cost rates for the reseller.

## On provider

It is likely that the income [main-rate-plan] will use one or more [csv-rate] for calculating income of calls sold to resellers. In case you can specify in the Web UI that these [csv-rate] are shared with a reseller, and Asterisell will send every new version of them to the reseller. Note:

  - only [csv-rate] are sent
  - a compatible cost [main-rate-plan] must be specified on the reseller side
  - the names of [csv-rate] will be shared between provider and reseller, so they had to make sense also if read on the reseller side

## On reseller

It will receive automatically the [csv-rate] from the provider.

## Service CDRS

In case of reselling, normal [cdr] and [service-cdr] are managed differently:

  - normal cdrs are visible to customers of the reseller, because they are VoIP calls
  - service cdrs can be associated to a customer (like in case of bundle-rates), or to pure services, but in any case they are not visible to customers of the reseller
  - an imported service cdr is associated to the reseller itself, and not to customers of the reseller
  - service cdrs can be "replicated" to reseller customers only if the reseller configure similar bundle-rates or pure-services, and it generates them
  - on reseller side, normal cdrs have a cost (the cost paid to the provider), and an income (what the reseller customer will pay)
  - on reseller side, imported service cdrs have a cost, but no income, because it is associated to the reseller itself
  - on reseller side, imported service cdrs have direction "system call"

On reseller side a typical rule to use in cost [main-rate-plan] is

```
  rate {
      id: vendor-services
      match-call-direction: system
      match-vendor: some-vendor-name
      match-communication-channel: system-service-cdr
    
      set-cost-on-call: expected
    }
```

In this case the cost associated to the service, is the same cost
calculated from the vendor, and there is no double-check calculation, like in case of normal cdrs. 
This for a limitation of pure-services, and Asterisell.

The income of a service CDR on the reseller side should be 0, because the service CDR is not associated
to a real customer, but to the reseller itself. So the reseller income [main-rate-plan] is:

```
    rate {
      id: vendor-services
      match-call-direction: system
      match-vendor: some-vendor-name
      match-communication-channel: system-service-cdr
    
      set-cost-on-call: 0
    }
```

