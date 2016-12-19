<?php

/**
 * @var ArUser $ar_user
 */

$options = OrganizationUnitInfo::getInstance()->getUIOrganizationSelector(null);

$defaultChoice = "";
if (!is_null($ar_user->getArOrganizationUnitId())) {
    $defaultChoice = $ar_user->getArOrganizationUnitId();
}
echo select_tag('select_organization', options_for_select($options, $defaultChoice));
