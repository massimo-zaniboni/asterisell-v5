<?php

/**
 * @var ArWholesaleNumber $ar_wholesale_number
 */

$c = new Criteria();
$vs = ArResellerPeer::doSelect($c);

$options = array();
$options[''] = '';
foreach($vs as $v) {
    /**
     * @var ArReseller $v
     */
    $options[$v->getId()] = $v->getName();
}

asort($options, SORT_LOCALE_STRING);

$index = $ar_wholesale_number->getArResellerId();
if (is_null($index)) {
    $index = '';
}

echo select_tag('select_reseller', options_for_select($options, $index));