<?php

$options = OrganizationUnitInfo::getInstance()->getUIOrganizationSelector(null, true, true, false, false, false);

 if (isset($filters['filter_on_organization'])) {
      $defaultChoice = $filters['filter_on_organization'];
  } else {
      $defaultChoice = "";
}

echo select_tag('filters[filter_on_organization]', options_for_select($options, $defaultChoice));
