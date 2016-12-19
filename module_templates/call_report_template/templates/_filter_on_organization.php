<?php

if (sfContext::getInstance()->getUser()->isAdmin()) {
    $options = OrganizationUnitInfo::getInstance()->getUIOrganizationSelector(null, true, true, false, false, false);

    if (isset($filters['filter_on_organization'])) {
        $defaultChoice = $filters['filter_on_organization'];
    } else {
        $defaultChoice = "";
    }

    echo select_tag('filters[filter_on_organization]', options_for_select($options, $defaultChoice));
} else {

    // normal user

    $userOrganizationId = sfContext::getInstance()->getUser()->getOrganizationId();

    if (isset($filters['filter_on_organization'])) {
        $filterId = $filters['filter_on_organization'];
    } else {
        $filterId = $userOrganizationId;
    }

    if (! OrganizationUnitInfo::getInstance()->canViewCallsOfOrganization($userOrganizationId, $filterId)) {
      $filterId = $userOrganizationId;
    }

    $options = OrganizationUnitInfo::getInstance()->getUIOrganizationSelector($userOrganizationId, true, true, false, false, true);

    echo select_tag('filters[filter_on_organization]', options_for_select($options, $filterId));

}
