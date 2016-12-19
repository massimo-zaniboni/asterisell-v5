<?php

/**
 * @var ArRate $ar_rate
 */

$options = array("" => "");

$stm = Propel::getConnection()->prepare('SELECT short_description, internal_name, id FROM ar_rate_format ORDER BY order_name');
$stm->execute();
while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
    $options[$rs[2]] = htmlspecialchars('(' . $rs[1] . ') ' . $rs[0]);
}
$stm->closeCursor();

$defaultChoice = "";
if (!is_null($ar_rate->getArRateFormatId())) {
  $defaultChoice = $ar_rate->getArRateFormatId();
}
echo select_tag('select_rate_format', options_for_select($options, $defaultChoice));
