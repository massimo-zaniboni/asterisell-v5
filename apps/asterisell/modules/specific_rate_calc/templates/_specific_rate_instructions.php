<?php

/**
 * @var ArSpecificRateCalc $ar_specific_rate_calc
 */

$info = '
Merge a base rate (i.e. a default/standard rate) with a specific rate, specifying 
only the differences respect the base rate. The result is a compact rate
(i.e. with less prefix entries) that can be used in Asterisell main rate plan
for rating the calls.

Asterisell during rating load all rates in RAM.
If there are too much price-categories associated to big CSV files,
too much RAM can be wasted.

In many cases rates between two price-categories share a common base rate,
from which they differs for some offers on some direction. So RAM can be
saved, sharing the same base-rate between different price-categories.

This tool allows to specify only the differences respect a base-rate,
and then it derives a compact specific rate to use inside Asterisell.

Rates can be specified using two different approach:
* specifying a match-all-longest prefix rate, that overrides all prefixes of the base-rate,
  so a prefix like "12" match also "123" and so on
* specifying an exception-like prefix rate, that overrides only the single exact prefix, 
  and use the base-rate otherwise, so a prefix like "12" match only "12" but not "123"

These two approaches are complementary, and they can be used both: 
firt the match-all-longest prefix rate is applied, 
then the resulting rate is patched with the exact-prefix rate.
';

echo textarea_tag('specific_rate_instructions', $info, array('rows' => 15, 'cols' => 80, 'disabled' => 'disabled')); 
