<?php
use_helper('Markdown', 'OnlineManual');
// Default culture is en_US
//

$str = <<<VERYLONGSTRING

## History of Prices

The reference price of a service can change over time. The price list contains all changes of prices.

A service can change price more times, also inside his billing time frame. The application code will take care to use the more accurate price according some default rules.

VERYLONGSTRING;


echo insertHelp($str);
