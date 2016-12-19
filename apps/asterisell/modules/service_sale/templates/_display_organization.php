<?php

/**
 * @var ArServiceSale $ar_assigned_service
 */

$id = $ar_assigned_service->getArOrganizationUnitId();
if (is_null($id)) {
    // display nothing
} else {
    echo htmlentities(OrganizationUnitInfo::getInstance()->getFullNameAtDate($id, null, false, false, null, false,  false));
}
