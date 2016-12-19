<?php

/**
 * @var ArReportScheduler $ArReportScheduler
 */

$options = OrganizationUnitInfo::getInstance()->getUIOrganizationSelector(null);

$defaultChoice = "";
if (isset($filters['filter_on_organization'])) {
    $defaultChoice = $filters['filter_on_organization'];
}
echo select_tag('filters[filter_on_organization]', options_for_select($options, $defaultChoice));
