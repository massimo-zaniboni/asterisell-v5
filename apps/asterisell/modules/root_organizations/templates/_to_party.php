<?php

if (is_null($ar_organization_unit->getArPartyId())) {
    echo "-";
} else {
    echo link_to($ar_organization_unit->getArParty(), 'party/edit?id='.$ar_organization_unit->getArPartyId());
}
?>
