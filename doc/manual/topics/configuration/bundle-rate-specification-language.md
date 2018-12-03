# Bundle rates specification language

A bundle-rate is a way to rate a group of calls, while certain limits
are respected. After the limits are reached, the bundle-rate can not be
anymore applied, and normal rates are applied to next calls.

Bundle-rates have higher priority respect normal-rates. Normal-rates are
selected only if there is no bundle-rate matching the call, or the call
is outside the bundle-rate limits.

Bundle-rates can be used for specifying things like:

  - apply a fixed cost of 10 EUR, for the first 60 minutes of mobile
    calls of a month, and rate normally other calls;
  - rate the first 60 minutes of mobile calls of a month, using a
    discounted cost;

Bundle-rates specification can be complex. So you can ask for help to
Asterisell assistance. This documentation will be improved according.

A bundle-rate has effect in a time-frame. For example there can be
monthly bundles, weekly bundles, and so on.

At the end of each bundle time-frame, the bundle-rate status is
reset: limits are set to their initial values.

A bundle-rate rate calls in two ways:

  - rating the calls that are part of the bundle, with the rate
    associated to the bundle-rate
  - generating the service-cdrs at the beginning of the bundle-rate
    timeframe

Up to date, bundle-rates can be used only for specifying the income of a
call, not the vendor cost.

## Example

```
bundle-rate {
  id: bundle-1000
  service-cdr-type: iBundle 1000
  service-cdr-description: 1000 minutes on all national directions

  schedule: monthly
  schedule-from: 1

  apply-for-each: bundle-1000
  
  limit-on-first-calls: none
  limit-on-first-seconds: 60000

  limits-are-proportionals-to-activation-date: true

  calls-can-be-split: true

  only-for-calls-with-a-cost: true

  set-bundle-initial-cost: 10

  rate {
    id: bundle
    match-call-direction: outgoing
    match-communication-channel: local,national,mobile
    set-cost-for-minute: 0
  }
}
```

## Specification

```
bundle-rate {
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

  #
  # Mandatory Bundle Rate Schedule Options
  #

  schedule: [monthly|weekly]
  # after each scheduling period (time-frame), the bundle rate status is resetted,
  # and the bundle-rate service-cdrs are produced.

  schedule-from: [1..28] | [Monday|Tuesday|...]
  # specify the day of the month, or the day of the week, or the time-frame in days,
  # when starting the new bundle-rate time-frame.

  schedule-at: 00:00:00
  # (optional) schedule at hh:mm:ss, of the new rating-frame day.

  #
  # Mandatory Bundle Rate Grouping Options
  #

  apply-for-each: [list of price-category]
  # This bundle-rate is applied to organizations/extensions
  # with a direct assignment to this specified price-categories.
  #
  # If an organization/extension inherits the price-category from its parent
  # organization, but it has no direct assignment,
  # then it has no a distinct bundle-rate status:
  # * a service-cdr is generated only for its parents with direct assignment;
  # * there is no separate limits allocated for the extension,
  #   but the limits of the parent organization are used instead;
  #
  # The date of price-category assignment is used for determining when the bundle-rate
  # can be applied.
  #
  # Service-cdr is generated also if there are no calls for him, inside the time-frame.

  #
  # Mandatory Bundle Rate Limit Options.
  #

  limit-on-first-calls: [number|none]
  # apply the bundle-rate only to the first specified calls, then use normal rates.
  # "none" for no limit on the number of calls.

  limit-on-first-seconds: [number|none]
  # apply only for calls until the specified seconds.
  # "none" for no limit on duration of calls.

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

  calls-can-be-split: [true|false]
  # true if the duration of a call can be split between a part respecting the bundle-rate limits,
  # and another part outside these limits. The part respecting the limits, is rated using
  # the bundle-rate, while the part not respecting the limits (the residual-duration)
  # is rated using rates not associated to the bundle-rate.
  # If false, then if a call is not completely inside the bundle-rate limits, is rated using the normal rates.

  only-for-calls-with-a-cost: [true|false]
  # true for applying the bundle only to calls having a positive cost.
  # So free calls are not counted as inside the bundle.
  # This is an advantage for end customers, because they can use the bundle
  # only for calls with a cost.

  #
  # Bundle Calc Params
  #
  # These params are applied to service-cdrs associated to the bundle,
  # and not to the calls.
  #

  set-bundle-initial-cost: [monetary-value]
  # what the customer pays for activating the bundle.
  # Default value: 0

  #
  # Rating Params
  #

  [rate-calc-params]
  # these are the params used for normal rates.
  # The calls inside the bundle will be rated using these params.


  rate {
    # this is a child-rate of the bundle-rate, and it is used for rating the calls
    # inside the bundle, and/or for specifying nested bundle limits.
    #
    # Children rates, can be of any admitted type for normal rates,
    # or of bundle-rates.

    id: [reference]

    [match-filters]
    # The rate can be applied only if filters are respected.
    # These are the filters used for normal rates.
    #
    # The filter "match-price-category" is not allowed,
    # because its semantic is in conflict with "apply-for-each" of the parent rate.
    # So the price-category is implicitely the same of the parent rate.

    #
    # Optional Bundle Rate Limit Options
    #

    limit-on-first-calls: [number|none]
    # For example if the parent bundle-rate, is limited to 100 calls,
    # and the chidren bundle-rate on mobile-calls is limited to 50 calls,
    # then the first 50 mobile calls decrease the limit of the parent rate
    # to 50, and of the children bundle-rate on mobile-calls to 0.
    #
    # A limit in this rate is in logical "and" with the limit of the parent bundle-rate.
    # So a user can place only 50 mobile calls, and if he place 25 mobile calls, he can make only
    # 75 calls because the parent limit was decreased to 75.

    limit-on-first-seconds: [number|none]
    # apply only for calls until the specified seconds.
    # The behaviour is similar to the case of "limit-on-first-calls".

    #
    # Optional Bundle Calc Params.
    #

    set-bundle-initial-cost: [monetary-value]
    # default value: 0

    # NOTE: all other params of the bundle-rate can not be changed, and they are the same of the parent bundle-rate.

    #
    # Rating Params
    #

    [rate-calc-params]
    # Rate calls inside the bundle (respecting the time-frame, the match conditions, and the limits)
    # according this rate params.
    #
    # The starting params are the params of the parent rate,
    # and these params can overwrite them.

    [other-nested-children-rates]
  }

  [other-children-rates]
}
```

## Nested bundle rates

First the system select the bundle-rate with the best matching, respect
other bundle-rates. It is selected the deepest bundle-rate. Then test if
the call respect the bundle-rate limits. If they are respected the
bundle-rate is applied, otherwise no other bundle-rate is applied, and
the best matching normal-rate is applied.

The match condition on a bundle-rate are:

  - the call is associated (directly or indirectly) to an organization
    with a direct assignment to the price-category of the bundle-rate;
  - other normal rate conditions;

## Residual call duration

If "calls-can-be-split" is set to false, then a call C is rated using
some bundle-rate, only if its duration is completely inside the limits
of the bundle-rate. Otherwise a normal rate is selected.

If "calls-can-be-split" is set to true, then the residual duration of
the call is calculated, in case the call duration is not entirely within
the limits of the bundle-rates. For example if call C duration is
insidie B1 limits, but partially inside B2 limits, then:

  - bundle-C is the call derived from C, with the duration inside the
    limits of B2;
  - residual-C is the call derived from C, with the duration part
    outside the limits of B2;
  - bundle-C is rated using B1/B2, because it respects the limits;
  - residual-C is rated using normal rates;

## Nested organizations

Suppose that there is this organization hierachy:

  - `A*`
  - `A/B*`
  - `A*/C` where `A/B*` and `A*/C` are children of the parent
    organization `A*`.

Suppose that:

  - `A*` is directly assigned to price-category pA
  - `A*/B*` is directly assigned to price-category pB
  - `A*/C` is not assigned directly to any price-category, so its
    inherit the price-category of `A*`

So we have a situation where we have nested organizations (main
organization, departments, offices, extensions), and where each part of
the organization can be assigned to a different price-category, and a
different bundle-rate.

The rating method, favours always bundle-rates, respect normal rates.
When bundle-rates can not be applied, it tries with normal rates.

So for a call associated to `A*/C`, it tries to apply the bundle-rate
associated to `A*`, because `C` has no bundle-rate, but `A*` is
subscribed to pA bundle-rate, and so it must take advantage of this for
all its extensions. If the bundle-rate can be applied to `A*/C`, then
the limits of `A*` are updated.

For a call associated to `A*/B*` organization, it tries to apply the
bundle-rate associated to `pB` price-category, because organization
`p/B*` is associated directly to `pB`. If the `pB` bundle-rate can be
applied to `A*/B*`, then only the limits of `A*/B*` are updated, while
`A*` is not updated.

If there is no bundle-rate on `pB`, or `A*/B*` has consumed all its
bundle-limits, then the system tries rating the call using the
bundle-rate associated to price-category `pA`, because `A/B*` is a
children organization of `A`, and `A` is "subscribed" to a bundle-rate.

If there is no bundle-rate on `pA`, or `A*` has used all its limits,
then normal-rates are used.

Summing up:

  - a bundle rate can be applied to all children extensions;
  - a bundle rate generate a service-cdr (a cost) for each extension
    with a direct price-category assignation, corresponding to a
    bundle-rate;
  - an extension with a direct association to a bundle-rate
    price-category, has its own limits, also because it pays for them;
  - if an extension has used all its bundle-rate limits, then the parent
    organization bundle-rate limits can be used, and they can be
    associated to a different type of bundle-rate, because the
    bundle-rate hireararchy is distinct from the organization hierarchy,
    and extensions can have different price-categories respect the
    parent organization;

## Limitations

Up to date bundle-rates:

  - can be used only for specifying incomes for customers, and not cost
    for vendors;
  - do not accept children bundle-rates with scheduling periods
    (time-frames) different from parent bundle-rate;
  - do not accept children bundle-rates with "calls-can-be-split" value
    different from parent bundle-rate;
  - the application will advise if they ends after the billing time-frame, because it is not clear which semantic apply
  
