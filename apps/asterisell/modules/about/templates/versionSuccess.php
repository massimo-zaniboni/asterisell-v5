<?php

$manualFileName = normalizeFileNamePath(getAsterisellCompleteAdminDirectory() . '/VERSION');
$c = file_get_contents($manualFileName, false);

if ($c === FALSE) {
    $c = "Unexpected error: VERSION not present.";
}

echo htmlentities($c);

?>