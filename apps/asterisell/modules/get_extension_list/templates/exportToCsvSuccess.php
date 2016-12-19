<?php

use_helper('I18N', 'Debug', 'Date', 'Asterisell');

$job = new ChangeOrganizationInfo();
$yamlContent = $job->getYAMLContent(null, time(), null);
if (!isEmptyOrNull($yamlContent)) {
    echo $yamlContent;
} else {
    echo "";
}

?>