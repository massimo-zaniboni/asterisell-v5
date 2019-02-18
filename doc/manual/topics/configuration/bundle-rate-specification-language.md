# Bundl rates

Bundle-rates rate calls using a special rate, until certain conditions are respected. Then normal rates are applied.

## Example 

A customer with price-category ``bundle-1000`` pays 10 EUR monthly, and he has 1000 free minutes of national calls.
The calls not matched by the bundle conditions (e.g. international calls in this case)
are rated using normal rates, and they do not decrease the limit of the bundle.
Calls without an cost (for the consumer) are not considered inside the bundle.

```
bundle {
  id: bundle-1000
  service-cdr-type: iBundle 1000
  service-cdr-description: 1000 minutes on all national directions

  schedule: monthly
  schedule-from: 1

  apply-for-each: bundle-1000
  
  limits-are-proportionals-to-activation-date: true
  only-for-calls-with-a-cost: true
  bundle-cost: 10

  rate {
    id: cost
   
    match-call-direction: outgoing
    # NOTE: without an explicit filter on outgoing calls,
    # also incoming calls can be theorically applied to a bundle.
    # In this case the "only-for-calls-with-a-cost" can prevent this.
    
    match-communication-channel: local,national,mobile
    
    limit-on-first-calls: none
    limit-on-first-seconds: 60000
    set-cost-for-minute: 0
  }
}
```

## Example of a nested bundle rate

Customer with price-category ``allinc-500`` pays 50 EUR monthly, and he:

* has 500 free minutes of Italian fixed-line calls
* has  500 free minutes of Italian mobile calls
* pays apart calls to national special telephone numbers
* pays apart international calls
* calls without a cost for the customer (free numbers, and so on) are not counted inside the bundle.

```
bundle {
  id: allinc-500

  service-cdr-type: Bundle Rate
  service-cdr-description: All inclusive: 500 fixed-line minutes, 500 minutes mobile

  schedule: monthly
  schedule-from: 1
  apply-for-each: allinc-500
  limits-are-proportionals-to-activation-date: true
  only-for-calls-with-a-cost: true
  bundle-cost: 50
  
  rate {
    id: national
    
    match-telephone-number: 39*
    match-call-direction: outgoing
    
    limit-on-first-calls: none
    limit-on-first-seconds: none
    
    set-cost-for-minute: 0
 
    rate {
      id: fixed-line
      
      match-telephone-number: 390*
      # fixed-line operators
      
      limit-on-first-calls: none
      limit-on-first-seconds: 30000
       
      set-cost-for-minute: 0
    }

    rate {
      id: mobile-line
      
      match-telephone-number: 393*
      # mobile operators
 
      limit-on-first-calls: none
      limit-on-first-seconds: 30000
      
    }
  }
}
```

## Bundle rate specification

```
bundle {
  id: [reference]

  service-cdr-type: [textual description]
  # a short textual description/name for the type of service-cdr.
  # Usually something like "Bundle Rate", "Bundle Service", "Service", and so on.
  # This name can be shared between different bundle-rates.
  # It does not contain details about the service, but only an high level classification.

  service-cdr-description: [textual description]
  # a textual description, associated to the service-cdr,
  # describing the details of the service offered/billed from the bundle-rate,
  # in the call report, and invoices.
  # It will be generated only if the bundle-rate service-cdr cost is greather than 0.
  # It will be generated for each organization associated to the bundle-rate.
  # There will be a unique service-cdr, with the sum of all the cost of nested bundle-rates.

  schedule: [monthly|weekly]
  # after each scheduling period (time-frame), the bundle rate status is resetted,
  # and the bundle-rate service-cdrs are produced.

  schedule-from: [1..28] | [Monday|Tuesday|...]
  # specify the day of the month, or the day of the week, or the time-frame in days,
  # when starting the new bundle-rate time-frame.

  schedule-at: 00:00:00
  # (optional) schedule at hh:mm:ss, of the new rating-frame day.

  apply-for-each: [list of price-category]
  # This bundle-rate is applied to organizations/extensions
  # with an explicit assignment to this specified price-categories.
  #
  # If an organization/extension inherits the price-category from its parent
  # organization, but it has no explicit assignment,
  # then it has no a distinct bundle-rate status:
  # * a service-cdr is generated only for its parents with explicit assignment;
  # * there is no separate limits allocated for the extension,
  #   but the limits of the parent organization are used instead;
  #
  # The date of price-category assignment is used for determining when the bundle-rate
  # can be applied.
  #
  # Service-cdr is generated also if there are no calls for him, inside the time-frame.

  limits-are-proportionals-to-activation-date: [true|false]
  # true if the bundle-rate limits on an organization/extension created not at the beginning of a time-frame,
  # but after X% days, must be scaled of X%.
  #
  # If true, then also the bundle-rate costs are proportional to the activation date.
  #
  # Only the activation (starting) date is considered, but not the date on which an organization/extension change
  # price-category, and exit from the bundle-rate. The consequences are that if an organization enters
  # at day 15 into a monthly bundle-rate A, and it exits from it at day 20 entering in monthly bundle-rate B,
  # then the organization pays both the bundle A at 50%, and the bundle B at 66% for the month with the change.
  #
  # In case there are repeated assignments to the same price-category, inside the same time-frame of the bundle-rate,
  # only the first assignment is taken in account.

  only-for-calls-with-a-cost: [true|false]
  # true for applying the bundle only to calls having a positive cost.
  # So free calls are not counted as inside the bundle.
  # This is an advantage for end customers, because they can use the bundle
  # only for calls with a cost.

  bundle-cost: [monetary-value]
  # what the customer pays for activating the bundle.
  # Default value: 0

  rate {
    id: [reference]
   
    [match conditions]
    # apply the rate/bundle only if the rating conditions are respected
    
    limit-on-first-calls: [number|none]
    # apply the bundle-rate only to the first specified calls, then use normal rates.
    # "none" for no limit on the number of calls.
    #
    # If this rate has a parent rate, then check and decrease also the limits
    # of the parent rate.

    limit-on-first-seconds: [number|none]
    # apply only for calls until the specified seconds. 
    # "none" for no limit on duration of calls.
    #
    # In case of nested rates, the behaviour is similar 
    # to the case of "limit-on-first-calls".
 
    [rating params]
    # calc the cost of the CDR according these params

    [other nested rates]
  }
  
  [other nested rates]
}
```

## Bundle-rate semantic

- a bundle rate is applied to exactly one price-category, so price-category of different bundles must be distinct
- the bundle is activated for every customer/organization/extension explicitly associated to the price-category of the bundle
- at beginning of bundle-time frame, the bundle generates a service CDR with the cost of the bundle, for each customer/organization/extension with an explicit association to the price-category
- if an organization/extension inherits the price-category from its parent organization, then no bundle service CDR is generated, and it shares the limits with the limits of its parent organization
- if an organization/extension is explicitly assigned to the price-category of the bundle, an additional service CDR is generated, and the organization/extension has its own limits 
- a bundle is applied to a call only if it is matching the conditions of one of it nested rates (it can decide the cost of the call), and the bundle limits are not yet reached, otherwise normal rates will be used

In case of nested bundles limits:

- the most specific matched bundle is used for managing the bundle-limit, and for rating the call
- if a parent bundle matches a condition, and it has one or more nested bundles, then it must exists a most specific nested bundle, because in this case the parent bundle is only used as base condition for selecting the best nested bundle. This is the same behaviour of nested rates
- the limits and conditions of the parent bundle are put in logical AND with the limits and conditions of the nested bundles
- if a call is rated inside a nested-bundle rate, then it will decrease the limits of every parent in the selected hierarchy
- the best bundle is selected testing matching conditions, but not bundle limits, then it is applied only if its limits are respected, otherwise normal rates are used. So bundle limits are not used for searching the best bundle, but only for deciding if using normal rates

See this example

```
bundle {
  id: A
  [...]
  bundle-cost: 50

  rate {
    id: outgoing
    match-call-direction: outgoing
    match-telephone-number: 39*
    limit-on-first-calls: none
    limit-on-first-seconds: none
 
    rate {
      id: B
      match-telephone-number: 390*
      limit-on-first-calls: none
      limit-on-first-seconds: 1000
    }

    rate {
      id: C
      match-telephone-number: 393*
      limit-on-first-calls: none
      limit-on-first-seconds: 1000
    }
  }
}
```

A call matching the condition of ``A/outgoing``:

- had to match ``A/outgoing/B`` or ``A/outgoing/C``, otherwise an error is generated
- it decreases the bundle limits of ``A/outgoing`` and the bundle limits of the matched ``A/outgoing/B`` or ``A/outgoing/C``
- it is rated according the bundle params, only if the limits of ``A/outgoing`` and of the matched ``A/outgoing/B`` or ``A/outgoing/C`` are respected, otherwise it is rated using normal rates
- a call not matching the condition ``A/outgoing``, is rated using normal rates

A call with an income 0 according using normal rates is never matched by ``A``.

Every ``rate`` follows the matching and application semantic of rates.

### Nested organizations

Suppose this organization hierarchy:

  - ``A*``
  - ``A/B*``
  - ``A*/C`` where ``A/B*`` and ``A*/C`` are children of the parent
    organization ``A*``.

Suppose that:

  - ``A*`` is explicitly assigned to price-category ``PA``
  - ``A*/B*`` is explicitly assigned to price-category ``PB``
  - ``A*/C`` is not assigned explicitly to any price-category, so its
    inherit the price-category of ``A*``

So we have a situation where we have nested organizations (main
organization, departments, offices, extensions), and where each part of
the organization can be assigned to a different price-category, and
different bundle-rates.

For a call associated to ``A*/C``, it tries to apply the bundle-rate
associated to ``A*``, because ``C`` has no bundle-rate, but ``A*`` is
subscribed to ``PA`` bundle-rate, and so it must take advantage of this for
all its extensions. If the bundle-rate can be applied to ``A*/C``, then
the limits of ``A*`` are updated.

For a call associated to ``A*/B*`` organization, it tries to apply the
bundle-rate associated to ``PB`` price-category, because organization
``A*/B*`` is associated explicitly to ``PB``. If the ``PB`` bundle-rate can be
applied to ``A*/B*``, then only the limits of ``A*/B*`` are updated, while
``A*`` is not updated.

If in ``A*/B*``, ``B*`` is explicitly assigned to a normal (no-bundle) rate, then this normal rate is applied, 
without using the ``A*`` bundle-rate. 

If ``A*/B*`` is a bundle-rate that has consumed all its
bundle-limits, then the system will rate the call using normal rates,
and not the ``PA`` bundle-rate associated to ``A*``.

Summing up:

- explicit price-category assignations have priority respect inherited assignations
- a bundle rate can be applied to all children extensions having no explicit price-category assignations
- a bundle rate generate a service-cdr (a cost) for each extension with an explicit price-category assignation, corresponding to a bundle-rate
- an extension with an explicit association to a bundle-rate price-category, has its own limits, also because it pays for them
- if an extension has used all its bundle-rate limits, the limits of the parent organization  will not be used. This mainly for simplifying the implementation of bundle rates, and because bundle-rates are not used usually for big organizations, so this is a good-enough approach

### Nested bundle-rates vs nested organizations

The semantic of nested bundle-rates follows contrary criteria respect nested organizations: 

- the limits of the parent nested bundle, and child bundle must be both valid, and they are both decreased if the bundle is applied to a call;
- on the contrary a nested organization decrease only its bundle limits, and not the limits of its parent organization, and only its own limits must be valid;

### Call duration exceeding the bundle limits 

A call will be rated inside the bundle only if its duration is entirely inside the bundle left limits. 
Otherwise normal rates will be used, and the bundle will be used for next shorter calls.

This approach is used because there can be calls with a fixed cost at answer, and so it can be not an advantage
for the customer using both the bundle and the normal rate for the residual part outside the bundle limit.

## Limitations

Up to date bundle-rates:

- can be used only for specifying incomes for customers, and not cost for vendors
- do not accept children bundle-rates with scheduling periods (time-frames) different from parent bundle-rate
- an organization/extension can not change bundle-rate during the bundle-rate time-frame, but only at the end (the application will check this constraint and warn in case)
- all combinations of nested bundle-rates and nested organizations are not fully covered by tests, so in case of complex bundle-rates ask for funding some additional test coverage

