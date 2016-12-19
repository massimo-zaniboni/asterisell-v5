<?php
/**
 * @var ArReportScheduler $ArReportScheduler
 */

$options = OrganizationUnitInfo::getInstance()->getUIOrganizationSelector(null, false, true, false, true);

$defaultChoice = "";
if (!is_null($ArReportScheduler->getArOrganizationUnitId())) {
    $defaultChoice = $ArReportScheduler->getArOrganizationUnitId();

    if (!is_null($defaultChoice)) {
        if (! OrganizationUnitInfo::getInstance()->getTypeIsLeaf($defaultChoice, null)) {
            $defaultChoice = '';
        }
    }
}

echo select_tag('select_voip_account', options_for_select($options, $defaultChoice));

