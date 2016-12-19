<?php

use_helper('I18N', 'Debug', 'Date', 'Asterisell');

// NOTE: if there are price categories and customers with the same name this code is not correct.

$conn = Propel::getConnection();
$query = 'SELECT id, internal_name FROM ar_rate_category ORDER BY internal_name';
$stm = $conn->prepare($query);
$stm->execute();
$priceCategoryNames = array();
$byPriceCategory = array();
while ((($rs = $stm->fetch(PDO::FETCH_NUM)) !== false)) {
    $id = $rs[0];
    $name = $rs[1];
    $byPriceCategory[$name] = array();
    $priceCategoryNames[$id] = $name;
}
$stm->closeCursor();

$info = OrganizationUnitInfo::getInstance();

$conn = Propel::getConnection();
$query = 'SELECT id FROM ar_organization_unit';
$stm = $conn->prepare($query);
$stm->execute();

$date = time();
while ((($rs = $stm->fetch(PDO::FETCH_NUM)) !== false)) {
    $organizationId = $rs[0];
    $fullName = $info->getFullNameAtDate($organizationId, $date, false, false, null, false, false);
    $priceCategoryId = $info->getArRateCategoryId($organizationId, $date);

    if (is_null($priceCategoryId)) {
        // nothing to do because it is the same category of its parent
    } else {
        $priceCategoryName = $priceCategoryNames[$priceCategoryId];
        $byPriceCategory[$priceCategoryName][] = $fullName;
    }
}
$stm->closeCursor();

// Set UTF-8 encoding
echo "\xEF\xBB\xBF";

echo csv_field("Price Category", true);
echo csv_field("Customer", false);
echo "\n";

foreach ($byPriceCategory as $categoryName => $organizations) {
    sort($organizations);
    foreach ($organizations as $orgName) {
        echo csv_field($categoryName, true);
        echo csv_field($orgName, false);
        echo "\n";
    }
}

?>