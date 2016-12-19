<?php

/**
 * @var ArParty $ar_party
 */

$conn = Propel::getConnection();

$stm = $conn->prepare('SELECT DISTINCT ar_organization_unit_id FROM ar_organization_unit_has_structure WHERE ar_party_id = ? ORDER BY ar_organization_unit_has_structure.from DESC');
$stm->execute(array($ar_party->getId()));

$r = '';
while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
    $r .= '<li>' . OrganizationUnitInfo::getInstance()->getFullNameAtDate($rs[0], null, true, false) . '</li>';
}

$stm->closeCursor();

if (isEmptyOrNull($r)) {
    $r = '-';
} else {
    $r = '<ul>' . $r . '</ul>';
}

echo $r;
