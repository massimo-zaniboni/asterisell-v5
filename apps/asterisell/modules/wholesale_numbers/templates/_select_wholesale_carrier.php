<?php

/**
 * @var ArWholesaleNumber $ar_wholesale_number
 */

$c = new Criteria();
$vs = ArWholesaleCarrierPeer::doSelect($c);

$options = array();
$options[''] = '';
foreach($vs as $v) {
    /**
     * @var ArWholesaleCarrier $v
     */
    $options[$v->getId()] = $v->getInternalName();
}

asort($options, SORT_LOCALE_STRING);

$index = $ar_wholesale_number->getArWholesaleCarrierId();
if (is_null($index)) {
    $index = '';
}

echo select_tag('select_wholesale_carrier', options_for_select($options, $index));