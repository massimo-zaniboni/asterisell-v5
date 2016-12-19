<?php
use_helper('Markdown', 'OnlineManual');
// Default culture is en_US
//

$str = <<<VERYLONGSTRING

## Parties

A Party record contains all the common info of entities like email, address, VAT, and so on.

A Party record can be associated to a Vendor, Customer, User, Organization and so on.

VERYLONGSTRING;


echo insertHelp($str);
