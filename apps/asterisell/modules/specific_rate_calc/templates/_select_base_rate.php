<?php

/**
 * @var ArSpecificRateCalc $ar_specific_rate_calc
 */

$options = array("" => "");

$stm = Propel::getConnection()->prepare('SELECT internal_name, id FROM ar_rate ORDER BY internal_name');
$stm->execute();
while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
    $options[$rs[1]] = htmlspecialchars($rs[1] . " - " . $rs[0]);
}
$stm->closeCursor();

$defaultChoice = "";
if (!is_null($ar_specific_rate_calc->getArRateId())) {
  $defaultChoice = $ar_specific_rate_calc->getArRateId();
}
echo select_tag('select_base_rate', options_for_select($options, $defaultChoice));
