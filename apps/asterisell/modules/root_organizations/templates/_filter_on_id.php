<?php

echo input_tag('filters[filter_on_id]', isset($filters['filter_on_id']) ? $filters['filter_on_id'] : null, array (
  'size' => 15));

