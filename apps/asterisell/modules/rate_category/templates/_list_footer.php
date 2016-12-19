<?php
use_helper('Markdown');
// Default culture is en_US
//
echo insertHelp('

## Customer Price Categories

Customers can be associated to different price categories/lists.

Rates can be specified according the price category of a customer.

## Hieararchy of Price Categories

Price categories are flat, but rates can have a nested structure, so they can specify an hierarchy of price categories.

First you can name price categories using internal names like:

  *  "discounted",
  *  "discounted/mobile",
  *  "discounted/land-lines/UK".

where "discounted/land-lines/UK" is a flat name, but it can be seen as a children of "discounted/land-lines", and of "discounted" price category.

Then you can associate a rate to customers of "discounted" price category, an define a children rate matching "discounted/land-lines/UK" customers. In this way the most specific rate is applied to customers of type "discounted/land-lines/UK". For example

<pre><code>
  rate {
     match-call-direction: outgoing

     rate {
       match-price-category: discounted
       set-cost-on-call: 0.02
     }

     rate {
       match-price-category: discounted/land-lines/UK
       match-communication-channel: land-line
       set-cost-on-call: 0.01
     }
  }
</code></pre>

Note that you can not write

<code><pre>
  rate {
     match-call-direction: outgoing
     match-price-category: discounted
     set-cost-on-call: 0.02

     rate {
       match-price-category: discounted/land-lines/UK
       match-communication-channel: land-line
       set-cost-on-call: 0.01
     }
  }
</code></pre>

because:

  *  children rates must match also parent rate
  *  a call respecting "match-price-category: discounted/land-lines/UK", do not respect also "match-price-category: discounted", because internally "discounted/land-lines/UK" is not a sub-case of "discounted" price category

In any case the calls of type "match-price-category: discounted/land-lines/UK" will raise an error, because they can not be rated, and you can fix the problem.

In case of rates applicable to many price categories, you can specify the list of price categories. For example

<code><pre>
  rate {
     name: emergency-telephone-numbers
     match-call-direction: outgoing

     match-price-category: discounted, discounted/land-lines, discounted/land-lines/UK
     set-cost-on-call: 0
  }
</code></pre>

This can be annoying in case of price categories with many nested levels, because in a true price category hierarchical approach, it suffices to say "match-price-category: discounted". But it is a safe approach, because:

  *  if you do not specify a price category in the list, the rating engine will signal the problem
  *  if you update the price category names, the old rates will signal the problem, and you can adapt them too

');

?>