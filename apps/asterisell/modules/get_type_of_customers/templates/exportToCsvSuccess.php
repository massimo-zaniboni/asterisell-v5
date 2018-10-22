<?php

use_helper('I18N', 'Debug', 'Date', 'Asterisell');

$info = OrganizationUnitInfo::getInstance();
$ids = $info->getOrganizationIds();

$time = VariableFrame::$fromDate;
if (is_null($time)) {
    $time = time();
}
$timeS = fromUnixTimestampToMySQLTimestamp($time);

$sortedIds = array();
foreach ($ids as $id) {
    if ($info->getExists($id, $time)) {
        $sortedIds[$id] = $info->getFullNameAtDate($id, $time, false, false, null, false, false);
    }
}
asort($sortedIds);
reset($sortedIds);

// Set UTF-8 encoding
echo "\xEF\xBB\xBF";

echo csv_field("Active at date", true);
echo csv_field("Extension codes", false);
echo csv_field("Organization", false);
echo csv_field("Price category", false);
echo csv_field("Note", false);
echo csv_field("Billable organization", false);
echo csv_field("Billable CRM", false);
echo "\n";

foreach ($sortedIds as $id => $organizationName) {
    echo csv_field($timeS, true);

    $d = $info->getDataInfo($id, $time);
    $c = $d[OrganizationUnitInfo::DATA_EXTENSION_CODES];
    if (is_null($c)) {
        $c = "";
    }
    echo csv_field($c, false);
    echo csv_field($organizationName, false);

    try {
        $priceCategoryId = $info->getNearestRateCategoryId($id, $time);
        $priceCategory = ArRateCategoryPeer::retrieveByPK($priceCategoryId);
        if (!is_null($priceCategory)) {
            echo csv_field($priceCategory->getName(), false);
        } else {
            echo csv_field("!!error!!", false);
        }
    } catch (ArProblemException $ex) {
        echo csv_field("!!error!!", false);
    }
    echo csv_field($info->getPartyNote($id, $time), false);
    $billableId = $info->getBillableArOrganizationId($info->getFullIds($id, $time), $time);
    echo csv_field($info->getFullNameAtDate($billableId, $time, false, false, null, false, false), false);
    echo csv_field($info->getPartyCRM($billableId, $time), false);
    echo "\n";
}

?>