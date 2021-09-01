# Main rate plans

The income [main-rate-plan] and cost [main-rate-plan] describe the rating plan logic,
from an high-level point of view, using a domain specific language. 

[main-rate-plan] use [csv-rate] for storing rating details like matches between telephone prefixes and prices. For example in this cost rate

```
rate {
  id: outgoing
  match-call-direction: outgoing
    
  rate {
    id: vendor1
    match-vendor: vendor1
    match-communication-channel: international-calls

    use: cost-for-vendor1
    # NOTE: this is a CSV file containing a cost-by-minute for every prefix

    set-cost-on-call: 0.02
    set-cost-for-minute: external
    # NOTE: use the cost specifieb by "cost-for-vendor1" CSV file
  }
}
```

an outgoing call, routed by [vendor] "vendor1"" on [communication-channel-type] "international-calls" has an initial cost of "0.02", and then it is rated by minute according the content of the [csv-rate] named "cost-for-vendor1". The format of "cost-for-vendor1" rate can contains telephone prefixes, and corresponding cost for minute.

With this approach it is likely that the [main-rate-plan] remain rather constant, while [csv-rate] can be changed often.

These plans are rather difficult to write the first time, because a new language had to be studied, 
but they are reasonable easy to read, and to adapt. So in case of difficulties you can ask for support, and
then maintain them yourself.

## Free incoming and internal calls example

With this cost [main-rate-plan] all incoming and internal calls have 0 cost:

```
    rate {
      id: free-incoming
      match-call-direction: incoming
      set-cost-on-call: 0
    }
    
    rate {
      id: free-internal
      match-call-direction: internal
      set-cost-on-call: 0
    }
```
