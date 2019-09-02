<?php

$options = array("" => "", -1 => "Unassigned Telephone Numbers");
// NOTE: the -1 value is used also in InitWithDefaultMySQLStoredProcedures for NULL resellers,
// so it is a built-in value.

$c = new Criteria();
$resellers = ArResellerPeer::doSelect($c);

foreach($resellers as $reseller) {
    /**
     * @var ArReseller $reseller
     */
    $options[$reseller->getId()] = $reseller->getInternalName();
}

asort($options, SORT_LOCALE_STRING);

$defaultChoice = "";
if (isset($filters['filter_on_reseller'])) {
  $defaultChoice = $filters['filter_on_reseller'];
}

echo select_tag('filters[filter_on_reseller]', options_for_select($options, $defaultChoice));

