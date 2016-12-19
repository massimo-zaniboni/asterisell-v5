<?php

echo '<br/>';
echo form_tag("telephone_prefix/importCSV", 'multipart=true') . input_file_tag('csvFile', array('size' => 30)) . '      '. submit_tag(__('Import from CSV file'), array('name' => 'csv')) . '</form>';
echo '<br/>';
echo '<p>CSV file format is something like:</p>
<pre>
"9375", "Afghanistan", "Mobile Line", "AT"
"93", "Afghanistan", "Fixed Line", ""
"937", "Afghanistan", "Mobile Line", ""
</pre>

<p>The CSV file has no header line. The implicit header line is:</p>
<pre>"Telephone Prefix","Geographic Location","Connection Type","Operator Name"</pre>
<p>All the prefixes are considered with an implicit terminal "*", joining the rest of the telephone number.</p>
';
?>