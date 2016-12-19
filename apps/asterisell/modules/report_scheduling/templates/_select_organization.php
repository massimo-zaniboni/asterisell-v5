<?php

/**
 * @var ArReportScheduler $ArReportScheduler
 */

$options = OrganizationUnitInfo::getInstance()->getUIOrganizationSelector(null);

$defaultChoice = "";
if (!is_null($ArReportScheduler->getArOrganizationUnitId())) {
    $defaultChoice = $ArReportScheduler->getArOrganizationUnitId();
}
echo select_tag('select_organization', options_for_select($options, $defaultChoice));
