<?php
use_helper('Markdown', 'OnlineManual');
// Default culture is en_US
//

$str = <<<VERYLONGSTRING

## How Services Are Billed

During CDR rating of a time-frame the services assigned to a customer in the time-frame are inserted as special Service CDRs, and billed to the customer.

A limitation of current version of Asterisell it is that in case there is no any call in the time frame, the services are not billed. But it is enough a call of a customer in a time-frame, for starting the billing of the services of all customers. So there can be problems only in case of a frozen Asterisell instance not processing any CDR.

In case of changes in assigned services, the old billed services will be removed from the rating time-frame, and they will be replaced with the correct version.

If you change services of an already rated and billed time-frame, you must force a re-rating of the time-frame. For example if there are monthly services they are rated automatically only until there are new CDRs in the referenced month, forcing also a rerating of services. For re-rating them again you must rerate the past month.

## History of Sales

A service sale must report always the current number of rent services associated to the customer.

Every more recent entry, overrides previous entries.

A service sale set to 0, says that from the specified date, the customer does not own any more the service.

## Multiple Assignments of Services

A service can be assigned multiple times, to the same customer, inside the same billing time frame. The behaviour of these assignements depends from the params of the service.

In case of services proportional to date activation, every service is billed only for the amount of time it was used. In case of a service with a price that change according thi price-list, it is considered the last applicable price according the date of activation of every partial service assignation.

In case of service not proportional to date activation, it is used for billing the max number of rent services, inside the time frame. In case of a service with a price that change according the price-list, it is considered the last applicable price (the more recent price inside the time frame of reference), for all the billed services in the timeframe.

## Services with a Price that does not change according Price List

Services with a Price that does not change according Price List, are billed according the last applied service price. The first time the service is assigned to the customer, the more recent price-list is taken in account. Next change in assignments will use always the same reference price. The discount must be copied every time.

A price can be updated for a customer, assigning the total number of items to 0, and then selling again the service. In this case the most recent price is used.

## Services with a Price that change according Price List

Services with a Price that change according Price List use the most recent price at the beginning of the time frame to bill. If the price change inside the time-frame, the price of the service will be updated only at the beginning of the next time frame. So the price is fixed inside a time frame.

The price change only in case there are multiple assignements inside the same time frame.

## Filter on Customers

Up to date, the filter on customers filters exactly on the selected customer part, and it does not include children parts of the selected customer.

VERYLONGSTRING;


echo insertHelp($str);
