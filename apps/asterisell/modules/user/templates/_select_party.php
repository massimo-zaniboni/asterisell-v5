<?php

/**
 * @var ArUser $ar_user
 */

$c = new Criteria();
$c->addAscendingOrderByColumn(ArPartyPeer::NAME);
$parties = ArPartyPeer::doSelect($c);

$options = array();
$options[VariableFrame::USE_ORGANIZATION_PARTY_ID] = VariableFrame::USE_ORGANIZATION_PARTY_ID;
$options[VariableFrame::CREATE_NEW_PARTY_ID] = VariableFrame::CREATE_NEW_PARTY_ID;
foreach($parties as $party) {
    /**
     * @var ArParty $party
     */
    $options[$party->getId()] = $party->getName();
}

$index = $ar_user->getArPartyId();
if (is_null($index)) {
    $index = VariableFrame::USE_ORGANIZATION_PARTY_ID;
}

echo select_tag('select_party', options_for_select($options, $index));
