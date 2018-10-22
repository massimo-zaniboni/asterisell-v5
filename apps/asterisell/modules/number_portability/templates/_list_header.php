<?php

// SPDX-License-Identifier: GPL-3.0-or-later

echo '<br/>';
echo form_tag("number_portability/importCSV", 'multipart=true') . input_file_tag('csvFile', array('size' => 30)) . '      '. submit_tag(__('Import from CSV'), array('name' => 'csv')) . '      '  . submit_tag(__('Import from TWT'), array('name' => 'twt')) . '</form>';
echo '<br/>';
echo '<p>CSV file format is: \'"SOURCE-NUMBER","DEST-NUMBER", "YYYY-MM-DD"\'.</p>';
echo '<p>TWT file format is: \'IGNORE;CALL-TIMESTAMP;IGNORE;CALLED-NUMBER;PROPER PREFIX;IGNORE;IGNORE;IGNORE;IGNORE;IGNORE;IGNORE;IGNORE\'.</p>';
?>