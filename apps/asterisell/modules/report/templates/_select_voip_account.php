<?php
/**
 * @var ArReport $ArReport
 */

$options = OrganizationUnitInfo::getInstance()->getUIOrganizationSelector(null, false, true, false, true);

$defaultChoice = "";
if (!is_null($ArReport->getArOrganizationUnitId())) {
    $defaultChoice = $ArReport->getArOrganizationUnitId();

    if (!is_null($defaultChoice)) {
        if (! OrganizationUnitInfo::getInstance()->getTypeIsLeaf($defaultChoice, null)) {
            $defaultChoice = '';
        }
    }
}

echo select_tag('select_voip_account', options_for_select($options, $defaultChoice));

