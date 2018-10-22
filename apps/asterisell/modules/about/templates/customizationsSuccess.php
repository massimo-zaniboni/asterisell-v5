<?php

$manualFileName = normalizeFileNamePath(getAsterisellCompleteAdminDirectory() . '/CUSTOMIZATIONS.md');
$c = file_get_contents($manualFileName, false);

if ($c === FALSE) {
    $c = "Unexpected error: CUSTOMIZATIONS.md not present.";
}

echo "<pre>";
echo htmlentities($c);
echo "</pre>";
?>