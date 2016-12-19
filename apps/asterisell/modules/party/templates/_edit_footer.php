<?php
use_helper('Markdown', 'OnlineManual');

// Default culture is en_US
//
$str = <<<VERYLONGSTRING

## Party

Customers, Vendors, Users, and every part of the organization hierarchy, can have common information associated to them
like name, address, email, and so on.

Party captures this common information, and it can be associated later to the  specific entity, e.g. Customers, Vendors,
and so on.

## Effects of Changes

If you change information associated to a Party, and you re-generate reports of the past, then the new information
will be used, instead of the old information.

This can be usefull if you want regenerate recent reports, for fixing errors in the party section, like email address,
name, VAT and so on.

If you regenerate very old reports, this behavior is questionable. For sure it can be useful using the new
email address for sending also reports of the past, and it can be useful using the new/changed name.
But if you are regenerating invoices, maybe it is better using the old VAT, address, and legal name.

Other parts of the application like rates, and organization hierarchy, ask for a date of the change,
and they will use the old version for reports of the past.

So be careful when you change this info, and contemporary you regenerate reports of the past.

VERYLONGSTRING;

echo insertHelp($str);



