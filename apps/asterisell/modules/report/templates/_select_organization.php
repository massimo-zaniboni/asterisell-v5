<?php

/**
 * @var ArReport $ArReport
 */

$options = OrganizationUnitInfo::getInstance()->getUIOrganizationSelector(null);

$defaultChoice = "";
if (!is_null($ArReport->getArOrganizationUnitId())) {
    $defaultChoice = $ArReport->getArOrganizationUnitId();
}
echo select_tag('select_organization', options_for_select($options, $defaultChoice));
