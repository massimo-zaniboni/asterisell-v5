# Configuring multiple price-categories

Customers or [extension] are associated to a price-category, and VoIP calls can be rated according it.

```
    rate {
      id: outgoing
      match-call-direction: outgoing
    
      rate {
        id: wholesale
        match-price-category: wholesale
    
        external-rate {
          id: wholesale
          use: sell-wholesale
          set-cost-for-minute: this
        }
      }
    }
```

This is a complete specification of the rating plan language.

