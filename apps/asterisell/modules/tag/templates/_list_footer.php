<?php
use_helper('Markdown', 'OnlineManual');
// Default culture is en_US
//

$str = <<<VERYLONGSTRING

TAGS defined here can be added to customers, in the PARTY section.

Then customized code can use TAG content for processing invoices and reports in custom ways, or for displaying additional info to customers.

In case there are mandatory TAGS, you can add a job of type "apps/asterisell/lib/jobs/checks/CheckExclusiveTags.php", specifying the TAG that are mandatory.

VERYLONGSTRING;

echo insertHelp($str);
