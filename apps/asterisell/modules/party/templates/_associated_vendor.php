<?php

/**
 * @var ArParty $ar_party
 */

$c = new Criteria();
$c->add(ArVendorPeer::AR_PARTY_ID, $ar_party->getId());
$vendor = ArVendorPeer::doSelectOne($c);

if (!is_null($vendor)) {
    echo '<ul><li>' . $vendor->getNameAsHtmlLink() . '</li></ul>';

} else {
    echo '-';
}
