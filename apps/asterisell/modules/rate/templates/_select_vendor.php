<?php

/**
 * @var ArRate $ar_rate
 */


$c = new Criteria();
$c->addJoin(ArVendorPeer::AR_PARTY_ID, ArPartyPeer::ID);
$c->addAscendingOrderByColumn(ArPartyPeer::NAME);
$vendors = ArVendorPeer::doSelect($c);

$options = array();
$options[''] = '';
foreach($vendors as $v) {
    /**
     * @var ArVendor $v
     */
    $options[$v->getId()] = $v->getArParty()->getName();
}

$index = $ar_rate->getArVendorId();
if (is_null($index)) {
    $index = '';
}

echo select_tag('select_vendor', options_for_select($options, $index));
