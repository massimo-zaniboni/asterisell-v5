# Rate specification language

[main-rate-plan] are specified using a domain-specific language: the rate plan captures the high level logic, while price details are on external data (CSV) rates, that can be frequently updated.

There are two [main-rate-plan]:

* ``main-cost-rate`` for calculating the cost of a CDR
* ``main-income-rate`` for calculating the income of a CDR

## Income rate plan example

```
rate {
  id: free-incoming
  match-call-direction: incoming
  set-cost-for-minute: 0
}

rate {
  id: free-internal
  match-call-direction: internal
  set-cost-for-minute: 0
}

rate {
  id: outgoing

  match-call-direction: outgoing

  rate {
    id: free-emergency-telephone-numbers
    match-telephone-number: 118,113,11X
    set-cost-for-minute: 0
  } else {

    rate {
      id: normal
      # NOTE: the complete name of this rate is `/outgoing/normal`
      
      match-price-category: normal
      # NOTE: applicable for all customers with `normal` price-category
      
      use: csv-1
      # NOTE: use the content of the CSV file stored inside the rate with id `csv-1` 
      # having the physical and logical format as specified on the external `csv-1`
      # rate.
      # In particular there will be matches between telephone prefix and the cost to apply.
      
      set-cost-on-call: 0.05
      # NOTE: rate the CDR using this fixed cost every time there is a call
      
      set-cost-for-minute: external
      # NOTE: use the cost for minute specified in the `csv-1` rate,
      # in the line matching the conditions of the CDR (the best matching telephone prefix)
    }

    rate {
      id: discounted
      match-price-category: discounted
      use: csv-discounted-2
      set-cost-on-call: 0.05
      set-cost-for-minute: external
    }
  }
}
```

## Rating params 

```
rate {

  id: [reference]
  # a short internal reference name, used also in debug sessions for
  # discovering which rate is applied to a CDR.
  # Valid names are like "abcd-123_456"

  #
  # Specify on which CDRs the rate is applicable
  #
  # All these matches are optionals.

  match-communication-channel: [list of communication channel types]
  # if the CDR match one of the channels in the list, this rate can be applied.
  # It can be used for calculating incomes, if the end-customer knows in advance
  # the used communication-channel.
  # Usually this is an internal information of the VoIP provider, so it is fair
  # using this only for calculating costs.

  match-vendor: [list of vendors]
  # if the CDR match one of the vendors in the list, this rate can be applied.
  # A vendor is a VoIP provider used from a reseller for routing the calls.
  # This can not be used (usually) for calculating incomes, because an end-customer
  # does not know the vendor used, and he is not responsible for this.
  # So it is usually used only for calculating costs.

  match-price-category: [list of price categories]
  # see notes on "Rates->Price Categories" section, on how to model hierarchical price categories.

  match-call-direction: [outgoing|incoming|internal|system]

  match-telephone-number: [list of telephone numbers]
  # The external telephone number:
  #   * the called telephone number for outgoing and internal calls,
  #   * the calling telephone number for incoming calls.
  #
  # Telephone numbers are expressed in the same format used for specifying internal extensions,
  # so something like "123,123X,123X*,123*,123\X\*",
  # where "X" stays for any character,
  #       "*" stays for zero or more characters.
  # "\\\\" is the quotation for the "\\" character.
  # "\," is the quotation for the "," character
  # " 123, 456" is parsed into extensions "123", and "456"
  # " 123, 4 5 6" is parsed into extensions "123", and "4 5 6"
  # "\ 123, \ 456" is parsed into extensions " 123", and " 456"
  #
  # NOTE: in case of a list of many telephone numbers, it is best using CSV files, and external-rate references.

  match-rating-code: [list of rating codes]
  # match if the external telephone number (maybe ported) is associated to an entry
  # in the telephone prefix table, with the specified rating-code.
  # In many cases a rating-code represent the telephone operator owning/responsible
  # for the external telephone number.
  # It is useful for assigning common codes to various telephone prefixes with a common property,
  # and then using (simpler) rules matching only the rating-code instead of all the telephone prefixes.
  #
  # This can be used for rating also incomes, because customers are informed of the called telephone number.

  match-peak-code: [list of peak codes]
  # match if the call time respect one of the listed peak-codes.
  # Peak-codes are defined in the "Params -> Holiday table".
  #
  # Peak codes that are one the opposite of the other can be specified defining only one of the two,
  # and using rates with an "else" part for matching the opposite peak-code.
  #
  # Peak-code can be used for rating also incomes, because customers should be informed of their rating plan,
  # and they know the call time.


  #
  # Specify how to calculate the cost of the call
  #
  # These are the tranformations that can be applied on the
  # billable duration of a call.
  # The tranformations are applied in the specified order,
  # so also the order of specification is mandatory.
  # If a parameter is not specified, the default value is assumed.
  #
  
  set-free-seconds: [number of seconds]
  # do not apply the cost-for-minute to these first seconds.
  # 0 is the default value.

  set-duration-discrete-increments: [number of seconds]
  # rate every specified seconds.
  # 0 is the default value.
  #
  # If the specified value is for example 3,
  # then a call with duration 0,1,2 is considered as 3 second,
  # a call with duration 3,4,5 is considered as 6 seconds, and so on.

  set-at-least-seconds: [number of seconds]
  # consider the call at least as the specified seconds

  set-cost-on-call: [1.0|imported|expected]
  # the initial cost of the call (default value is 0)
  # "imported" is used for CDRS imported from external sources, having already the cost/income calculated
  # "expected" is used for CDRS imported from external sources/providers, and force the usage of the expected cost field

  set-cost-for-minute: [1.0]
  # the cost of the call,
  # specified for every minute, otherwise the value is too low to specify,
  # but applied by default for every second of call.
  # (default value is 0)

  set-max-cost-of-call: [1.0]
  # apply this cost, if the calculated cost of the call is major than this

  set-min-cost-of-call: [1.0]
  # apply this cost, if the calculated cost of the call is less that this

  set-round-to-decimal-digits: [integer]
  # round the cost of the call to the specified digits.
  # For example 2.41, 2.44 became 2.4, and 2.45, 2.48 became 2.5, when rounding to the 1st decimal digit.
  # If left unspecified, use the maximum possible precision, without any rounding.

  set-ceil-to-decimal-digits: [integer]
  # ceil the cost of the call to the specified decimal digits.
  # For example both 2.41, 2.44, 2.48, became 2.5 when ceiling to the 1st decimal digit.
  # If left unspecified, use the maximum possible precision, without any rounding.
  # This operation is done after the round operation.
  # So it is possible round to 4 digit, and ceiling later to 3 digits.

  set-floor-to-decimal-digits: [integer]
  # floor the cost of the call to the specified decimal digits.
  # For example both 2.41, 2.44, 2.48, became 2.4, when flooring to the 1st decimal digit.
  # If left unspecified, use the maximum possible precision, without any rounding.
  # This operation is done after the ceil operation.
}
```

## Nested rates

Rates can be arbitrary nested.

```
rate {
  id: [reference]

  [match conditions]
  
  [calc settings]

  rate {
    id: [reference]  
    # this can be a short and not unique id, because the complete rate reference name will
    # contain automatically also the parent id.
    # For example "root/outgoing/emergency-telephone-numbers" is a complete path of ids,
    # built from Asterisell joining the ids of the nested hiearchy.

    [match conditions]
    # this rate is selected if these conditions are respected.

    [calc settings]
    # A nested rate inherits all the setting of its parent rate.
    # Then here it can override some of them, or use "parent" for using 
    # the same value specified in the parent rate.
    
    [other nested rates]
  }
  
  [other nested rates]
  
}
```

The parent rate selects the nested rate with the strongest matching conditions:

* in case of rates with equal matching, Asterisell signals an error, because rating conditions are ambigous
* only the matching conditions of the rates at the same nesting level are considered
* after selecting a nested rate, the selection process is repeated for its children rates
* the parent rates with nested rates can not be used for rating the calls, but only theirs leaf rates, otherwise Asterisell will signal an error. Doing so one is sure that the strongest matching rate will be always used, and there are no logical-holes/errors in the rating-plan.

For example, for these rates

```
rate {
  id: a
  [...]
  
  rate {
    id: b1
    [...]
  }
  
  rate {
    id: b2
    [...]
    
    rate {
      id: c1
      [...]
    }
    
    rate {
      id: c2
      [...]
    }
  }
}
```

* if ``a`` match the CDR, but none of its nested rates can match, an error is signaled from Asterisell, because ``a`` can not be used alone for rating the CDR 
* ``a/b2`` is selected if it matches the CDR stronger than ``a/b1``. Usually this mean that ``a/b2`` match a telephone prefix with more digits respect ``a/b1``.
* the same selection process is repeated for selecting between ``a/b2/c1`` and ``a/b2/c2``.
* ``a/b2/c2`` rate the CDR using the rating params of ``a``, overridden by params of ``a/b2`` and ``a/b2/c2``.

## External rates 

Rates in Asterisell can have different physical and logical formats (e.g. CSV files, YAML files with different columns and data, like telephone prefix, telephone operator to match, and cost by minute, off-peak cost and so on).

A main rate plan can reference these external rates for matching conditions and for applying rating params. For example

```
rate {
  id: national
     
  match-price-category: normal
      
  use: csv-normal
  # this external rate will match according telephone prefix and it will specify the cost for minute
     
  set-cost-on-call: 0.05
  set-cost-for-minute: external
  # use the cost for minute specified on `csv-normal` external rate
}
```

The specification is

```
rate {
  id: [reference]

  [matching conditions]
  
  use: [rate-reference-name]
  # the name used in "Rates->Rates" form, for naming the external rate to call.
  # The format of the rate is specified in "Rates->Rates".
  # The external rate contains additional matches, and returns calc params to use for rating the CDR.
  # The matching conditions of the external rate are in logical AND with the conditions of the rate
  # The calc params of the external rate are overridden from the params of this rate. 

  set-cost-on-call: [external|parent|specific-value]
  # Use "external" for using the value returned from the called external rate (usually a CSV file).
  # Use "parent" if the value is not in the CSV file, and the value of the parent rate must be used.
  # Use a specific value, otherwise.

  [the same for other cost params]
  
  [other nested rates]
  # but it is unlikely that a rate with a reference to an external rate
  # needs other nested rates for completing the rating of a CDR
}
```

## Rates with explicit priority

By default rates are selected according the longest matched telephone
prefix. But rates can have also an explicit priority, thanks to ``else``
construct. 

```
rate {
  id: r1

  rate {
    id: r2
  } else {
    rate {
      id: r3
    }
  }
} else {
  rate {
    id: r4
  }
}
```

`r1/r3` is applied only if:

  - `r1` is applicable
  - `r1/r2` is not applicable
  - `r1/r3` is applicable

`r4` is applied only if:

  - `r1` and its children rates are not applicable
  - `r4` is applicable

So rate `r1` has implicitely more priority (is always preferred) respect
rate `r4`, and the same is true for `r1/r2`, against `r1/r3`.

