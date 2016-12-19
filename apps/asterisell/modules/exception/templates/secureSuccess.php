<?php
use_helper('I18N');
echo "<h2>";

echo __("Operation is not permitted.");

if (AsterisellUser::isAppLockedForMaintanance()) {
    echo "<br/>" . AsterisellUser::getMaintananceModeMessage();
}

echo "</h2>";
?>