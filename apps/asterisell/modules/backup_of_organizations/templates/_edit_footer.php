<?php
use_helper('Markdown', 'OnlineManual');

// Default culture is en_US
//
$str = <<<VERYLONGSTRING

## Restore

Up to date there is not yet automatic procedure for restoring a previous version. In case contact the assistance.

In case of changes not affecting the past history, you can import the YAML file again.

VERYLONGSTRING;

echo insertHelp($str);



