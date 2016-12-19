<?php
use_helper('Markdown', 'OnlineManual');
// Default culture is en_US
//
echo insertHelp('

## Rate Plan

"main-cost-rate" and "main-income-rate" are rate plan, specified using a rich and complex configuration language.

They can reference CSV files and other simpler rate formats, stored in this table.

Usually a rate plan rarely change, while referenced CSV files can be updated quickly.

"main-cost-rate", and "main-income-rate" are the mandatory entry points, for associating cost and income to a CDR.

## Change of Rates through Time

A rate can change through time: there is a new entry in the table with the same reference name, but a different applicability date. On CDRs before the applicability date, the old version of the rate is applied, while on new CDRs the new version of the rate is applied.

So a rate is identified by its "Reference Name" and its applicability date, and there can be different versions of the same rate.

For adding a new version of a rate, select the rate to change, and use the "Add a new version of the rate" button. All the rate content will be cloned.

');

//}

?>