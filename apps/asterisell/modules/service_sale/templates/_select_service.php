<?php

/**
 * @var ArServiceSell $ar_assigned_service
 */

$options = array("" => "");

$stm = Propel::getConnection()->prepare('SELECT id, vendor_name FROM ar_service WHERE is_enabled = 1 ORDER BY vendor_name');
$stm->execute();
while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
    $options[$rs[0]] = htmlspecialchars($rs[1]);
}
$stm->closeCursor();

$defaultChoice = "";
if (!is_null($ar_assigned_service->getArServiceId())) {
  $defaultChoice = $ar_assigned_service->getArServiceId();
}
echo select_tag('select_service', options_for_select($options, $defaultChoice));
