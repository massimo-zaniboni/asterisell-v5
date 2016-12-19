<?php

/**
 * @var ArServiceSale $ar_assigned_service
 */

$options = OrganizationUnitInfo::getInstance()->getUIOrganizationSelector(null);

$defaultChoice = "";
if (!is_null($ar_assigned_service->getArOrganizationUnitId())) {
    $defaultChoice = $ar_assigned_service->getArOrganizationUnitId();
}
echo select_tag('select_organization', options_for_select($options, $defaultChoice));
