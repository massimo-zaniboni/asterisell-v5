<?php

/**
 * @var ArVendor $ar_vendor
 */

$c = new Criteria();
$c->addAscendingOrderByColumn(ArPartyPeer::NAME);
$vs = ArPartyPeer::doSelect($c);

$options = array();
$options[''] = '';
$options[VariableFrame::CREATE_NEW_PARTY_ID] = VariableFrame::CREATE_NEW_PARTY_ID;
foreach($vs as $v) {
    /**
     * @var ArParty $v
     */
    $options[$v->getId()] = $v->getName();
}

$index = $ar_vendor->getArPartyId();
if (is_null($index)) {
    $index = '';
}

echo select_tag('select_party', options_for_select($options, $index));
