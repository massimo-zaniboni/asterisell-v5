<?php

$manualFileName = normalizeFileNamePath(getAsterisellCompleteAdminDirectory() . '/COPYRIGHT');
$c = file_get_contents($manualFileName, false);

if ($c === FALSE) {
    $c = "Unexpected error: LICENSE not present.";
}

echo "<pre>";
echo htmlentities($c);
echo "</pre>";

?>