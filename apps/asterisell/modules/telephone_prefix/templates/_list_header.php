<?php

echo '<br/>';
echo form_tag("telephone_prefix/importCSV", 'multipart=true') . input_file_tag('csvFile', array('size' => 30)) . '      '. submit_tag(__('Import from CSV file'), array('name' => 'csv')) . '</form>';
echo '<br/>';
echo '<p>Example of CSV file format:</p>
<pre>
"2779", "South Africa", "Mobile", "VC" 
"9375", "Afghanistan", "Mobile", "AT"
"93", "Afghanistan", "Fixed Line", ""
</pre>

<p>The CSV should not contain any header line. The implicit header line is:</p>
<pre>"Telephone Prefix","Geographic Location","Connection Type","Operator Name"</pre>
<p>All prefixes are considered with an implicit terminal "*", joining the rest of the telephone number.</p>
';
?>