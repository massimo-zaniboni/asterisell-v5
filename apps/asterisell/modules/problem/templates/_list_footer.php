<?php
use_helper('Markdown', 'OnlineManual');
// Default culture is en_US
//
echo insertHelp('

## View

Open a problem, for viewing more details.

## Problems Workflow

Asterisell rating process is conservative: in doubt it does not rate a CDR, and signal the problem.

Asterisell signals always the number of unrated CDRs, except there are critical errors.

The "Calls -> Calls with Errors" section contains also a list of unrated CDRs, and a link to the corresponding error description.
', null);
?>