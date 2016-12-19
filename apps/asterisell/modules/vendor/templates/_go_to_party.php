<?php

use_helper('Asterisell', 'Url', 'I18N');

/**
 * @var ArVendor $ar_vendor
 */

$partyId = $ar_vendor->getArPartyId();

if (isEmptyOrNull($partyId)) {
    echo __("no associated party");
} else {
    echo link_to($ar_vendor->getArParty()->getName(),'party/edit?id=' . $partyId);
}
