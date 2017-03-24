<?php
use_helper('Markdown', 'OnlineManual');
// Default culture is en_US
//

$str = <<<VERYLONGSTRING

TAGS defined here can be added to customers.

Then customized code can use TAG content for processing invoices and reports in custom ways, or for displaying additional info to customers.

VERYLONGSTRING;

echo insertHelp($str);
