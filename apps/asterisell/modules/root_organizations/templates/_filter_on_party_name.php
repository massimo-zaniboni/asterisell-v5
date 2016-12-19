<?php

echo input_tag('filters[filter_on_party_name]', isset($filters['filter_on_party_name']) ? $filters['filter_on_party_name'] : null, array (
  'size' => 15));

