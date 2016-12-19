<?php
  /**************************************************************
   !!!                                                        !!!
   !!! WARNING: This file is automatic generated.             !!!
   !!!                                                        !!!
   !!! In order to modify this file change the content of     !!!
   !!!                                                        !!!
   !!!    /module_template/call_report_template               !!!
   !!!                                                        !!!
   !!! and execute                                            !!!
   !!!                                                        !!!
   !!!    sh generate_modules.sh                              !!!     
   !!!                                                        !!!
   **************************************************************/
use_helper('Markdown');
if ($sf_user->hasCredential('admin')) {
  // Default culture is en_US
  //
  echo insertHelp('

## Filter

Contrary to other modules, the **receiver** filter field accepts a string prefix and it add automatically "*" to it. If you insert something like "039" then all numbers like "0395", "039334" will be returned. This in order to simplify filter usage also for customers.

## Re-rate Button

The re-rate button will reset the calls in the selected timeframe. Other filters are not take in consideration: only the timeframe. 

This behaviour is counterintuive, but it is usefull in order to reset calls containing errors and that are not joined with other records of the database, and also ignored calls. 

The resetted calls will be rated, so there is no permanent loss of information.

Calls will be not rated immediately, but on the next run of the CRON job-processor.

## Cost Precision

Costs are displayed using the default currency precision but they are stored (and summed) using full precision, as setted in the configuration file.

Costs are rounded according "currency_decimal_places_in_invoices" parameter value. 

Costs rounding can cause some discrepancies in the call report when there are low numbers.

## Debugging Rates

The admin (and only the admin) can click on calls cost/income, for inspecting the applied rate on them.

The "Calls -> Calls with Errors" form can be used for inspecting the problems about unrated calls.

## Calls direction

Calls can be: outgoing, incoming, internal, system.

Outgoing calls are calls made from the customer to an external telephone number. Incoming calls are calls made from an external telephone number, and received from the customer. Internal calls are calls made to the customer to a telephone number owned by himself.

System calls are associated to internal services/costs, and they are never seen by customers. Usually they are used for managing system related services/costs.

The admin can view all the type of calls.

Customers can never see the system calls, and they can view other type of calls only if allowed in the Asterisell configuration.

## Recent Calls

Recent calls are the calls that are not yet billed to customers.

When you change some rating param, or some configuration, recent calls are rerated by default, in order to apply the last changes. In this way changes are applied only to recent calls, but not to already billed calls.

Recent calls are all the calls after the "Params->Official Call date".

Usually you set the "Official Call date" only one time, then it is automatically advanced every time you confirm the billing reports. A billing report is a report having the flag "Billing document" enabled.

If you miss to update the official call date, then a lot of calls will be rated every time you change some param.

');
}
?>