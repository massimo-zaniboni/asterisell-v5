<?php
echo select_tag(
        'filters[filter_on_active_party]', 
        options_for_select(array(1 => __('yes'), 0 => __('no')),
          isset($filters['filter_on_active_party']) ? $filters['filter_on_active_party'] : null, 
          array ('include_custom' => __("yes or no")))
);
        