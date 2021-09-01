<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2021 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

echo '<br/>';
echo form_tag("set_hacked_calls/importCSV", 'multipart=true') . input_file_tag('csvFile', array('size' => 30)) . '      '. submit_tag(__('Import from CSV'), array('name' => 'csv')) . '</form>';
echo '<br/>';
echo '<p>CSV file format is: <pre>"Call Date in YYYY-MM-DD hh:mm:ss format", "Source CDR ID"</pre>.</p>';
echo '<p>The first line of the CSV file is the header. The call-date must correspond exactly to the call-date of the corresponding source-cdr-id.</p>';
echo '<p>A CDR is hacked if an hacker has stolen the VoIP password of a customer, and he used it for calling other (expensive) telephone numbers. '
   . ' It is possible setting these fraudolent calls as hacked calls, assigning them to a VoIP account having the identifier '
   . '"' . ArOrganizationUnit::HACKED_ACCOUNT_INTERNAL_NAME . '".</p>'
   . '<p>You can extract the hacked CDRS using filters on the call report, then exporting them to CSV file using the "Export to CSV" button, then leaving only the colmun with the call-date and source-cdr-id columns, finally importing this file here. Service CDRS with source-id set to NULL will be ignored and regenerated during successive re-rating event. Calls will be automatically rerated during successive re-rating event.</p>'
   . '<p>Asterisell stores source CDRS inside the table "ar_source_cdr". This utility set the field "ar_source_cdr.is_hacked". During rating, the CDRS with this field set, are assigned by default to the VoIP account '
   . '"' . ArOrganizationUnit::HACKED_ACCOUNT_INTERNAL_NAME . '". If this account is not already defined, Asterisell will signal a proper error message, and you can assign it to some fake organization responsible of hacked calls. '
   . 'Doing so, the original customer will not pay these hacked the callls. </p>';
?>
