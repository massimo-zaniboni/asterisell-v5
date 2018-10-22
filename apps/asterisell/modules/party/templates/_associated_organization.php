<?php

/**
 * @var ArParty $ar_party
 */

$conn = Propel::getConnection();

$stm = $conn->prepare('
   SELECT ar_organization_unit_id
   , ar_organization_unit_has_structure.from
   FROM ar_organization_unit_has_structure
   WHERE ar_party_id = ?
   ORDER BY ar_organization_unit_has_structure.from DESC');

$stm->execute(array($ar_party->getId()));

$l = NULL;
$r = '';
while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
    if ($l !== $rs[0]) {
      $r .= '<li>' . OrganizationUnitInfo::getInstance()->getFullNameAtDate($rs[0], null, true, false) . '</li>';
    }
    $l = $rs[0];
}

$stm->closeCursor();

if (isEmptyOrNull($r)) {
    $r = '-';
} else {
    $r = '<ul>' . $r . '</ul>';
}

echo $r;
