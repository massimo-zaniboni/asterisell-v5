<?php

use_helper('Form');
use_helper('I18N');

$d = NULL;
if (isset($filters['filter_on_date'])) {
  $d = $filters['filter_on_date'];
}

 echo input_date_tag('filters[filter_on_date]', $d, array (
  'rich' => true,
  'withtime' => true,
  'culture' => sfConfig::get('app_culture'),
  'calendar_button_img' => '/sf/sf_admin/images/date.png'));
?>
