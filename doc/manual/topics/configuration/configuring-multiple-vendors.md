# Configuring multiple vendors

You can have multiple [vendor] that you use for routing the VoIP calls.

You configure in the Web UI forms:

  - `Rates -> Vendors`
  - `Params -> CDRs Providers`
  - `Rates -> Channels` for recognizing the [vendor] and [communication-channel-type] according the [communication-channel] of the [cdr]

In case there are overlapping [communication-channel], a custom importing jobs had to be defined, in order to generate channels with a unique prefix.

Then in the rate plan you specify how to rate the CDRs according
different vendors:

```
    rate {
      id: outgoing
      match-call-direction: outgoing
    
      rate {
        id: vendor1
        match-vendor: vendor1
        external-rate {
          id: csv
          use: vendor1-csv-details
          set-cost-for-minute: this
        }
      }
    
      rate {
        id: vendor2
        match-vendor: vendor2
        external-rate {
          id: csv
          use: vendor2-csv-details
          set-cost-for-minute: this
        }
      }
    }
```
