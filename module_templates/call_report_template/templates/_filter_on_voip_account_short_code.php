<?php

// NOTE: for an organization and an extension, we have the allowed values, and the selected value.
// For an organization, the allowed values are all the children organizations of the allowed organization.
// For an extension, the allowed values are all the direct and indirect extensions of the selected organization.
// The selected value of an organization, and of an extension, is a selected element, in the list of allowed values.

// NOTE: if you change the first part of this code, update also:
// * _filter_on_proper_organization
// * _filter_on_voip_account

$hierarchy = OrganizationUnitInfo::getInstance();

/**
 * @var AsterisellUser $user
 */
$user = sfContext::getInstance()->getUser();

// Retrieve login permissions, and use them for retrieving the default allowed values

$defaultAllowedOrganizationId = null;
$defaultSelectedOrganizationId = null;
$defaultSelectedExtensionId = null;

if (! $user->isAdmin()) {

    $userOrganizationId = $user->getOrganizationId();

    if ($hierarchy->getTypeIsLeaf($userOrganizationId, null)) {
        $defaultAllowedOrganizationId = $hierarchy->getParentId($userOrganizationId, null);
        $defaultSelectedOrganizationId = $defaultAllowedOrganizationId;
        $defaultSelectedExtensionId = $userOrganizationId;

    } else {
        $defaultAllowedOrganizationId = $userOrganizationId;
        $defaultSelectedOrganizationId = $defaultAllowedOrganizationId;
        $defaultSelectedExtensionId = null;
    }
}

//  Retrieve selected values from user filters

$allowedOrganizationId = $defaultAllowedOrganizationId;
$selectedOrganizationId = null;
$selectedExtensionId = null;

if (isset($filters['filter_on_proper_organization'])) {
    $selectedOrganizationId = $filters['filter_on_proper_organization'];
    if (isEmptyOrNull($selectedOrganizationId)) {
        $selectedOrganizationId = $defaultAllowedOrganizationId;
    }
} else {
    $selectedOrganizationId = $defaultSelectedOrganizationId;
}

if (isset($filters['filter_on_voip_account_short_code'])) {
    $selectedExtensionId = $filters['filter_on_voip_account_short_code'];
    if (isEmptyOrNull($selectedExtensionId )) {
        $selectedExtensionId = $defaultSelectedExtensionId;
    }
} else {
    $selectedExtensionId = $defaultSelectedExtensionId;
}

// Check if selected filters, are compatible with user rights

if (! $user->isAdmin()) {
    $userOrganizationId = $user->getOrganizationId();

    if (! (OrganizationUnitInfo::getInstance()->canViewCallsOfOrganization($userOrganizationId, $selectedExtensionId)
          || OrganizationUnitInfo::getInstance()->canViewCallsOfOrganization($userOrganizationId, $selectedOrganizationId)
            || OrganizationUnitInfo::getInstance()->canViewCallsOfOrganization($userOrganizationId, $allowedOrganizationId)
    )) {
        $selectedExtensionId = $defaultSelectedExtensionId;
        $selectedOrganizationId = $defaultSelectedOrganizationId;
        $allowedOrganizationId = $defaultAllowedOrganizationId;
    }
}

////////////////////
// Display filter //
////////////////////
// NOTE: this part is specific of this filter

$options = OrganizationUnitInfo::getInstance()->getUIOrganizationSelector($selectedOrganizationId, false, true, true, true, ! $user->isAdmin());

if (! is_null($selectedExtensionId)) {
    $defaultChoice = $selectedExtensionId;
} else {
    $defaultChoice = "";
}

echo select_tag('filters[filter_on_voip_account_short_code]', options_for_select($options, $defaultChoice));

