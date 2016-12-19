<?php
/**
 * @var ArReportScheduler $ArReportScheduler
 */

$options = OrganizationUnitInfo::getInstance()->getUIOrganizationSelector(null, true, false);

$defaultChoice = "";
if (!is_null($ArReportScheduler->getArOrganizationUnitId())) {
    $defaultChoice = $ArReportScheduler->getArOrganizationUnitId();

    if (!is_null($defaultChoice)) {
        if (OrganizationUnitInfo::getInstance()->getTypeIsLeaf($defaultChoice, null)) {
            $defaultChoice = '';
        }
    }
}

echo select_tag('select_proper_organization', options_for_select($options, $defaultChoice));

