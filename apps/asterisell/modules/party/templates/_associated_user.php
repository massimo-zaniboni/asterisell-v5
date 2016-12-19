<?php

/**
 * @var ArParty $ar_party
 */

$c = new Criteria();
$c->add(ArUserPeer::AR_PARTY_ID, $ar_party->getId());
$user = ArUserPeer::doSelectOne($c);

if (!is_null($user)) {
    echo '<ul><li>' . $user->getNameAsHtml() . '</li></ul>';

} else {
    echo '-';
}
