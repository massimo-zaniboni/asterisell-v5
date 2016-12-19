<?php

use_helper('Asterisell', 'Url', 'I18N');

/**
 * @var ArUser $ar_user
 */

$partyId = $ar_user->getArPartyId();

if (isEmptyOrNull($partyId)) {
    echo __("no associated party");
} else {
    echo link_to($ar_user->getName(),'party/edit?id=' . $partyId);
}
