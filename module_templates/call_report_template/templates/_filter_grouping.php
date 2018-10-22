<?php
  /**************************************************************
   !!!                                                        !!!
   !!! WARNING: This file is automatic generated.             !!!
   !!!                                                        !!!
   !!! In order to modify this file change the content of     !!!
   !!!                                                        !!!
   !!!    /module_template/call_report_template               !!!
   !!!                                                        !!!
   !!! and execute                                            !!!
   !!!                                                        !!!
   !!!    sh generate_modules.sh                              !!!     
   !!!                                                        !!!
   **************************************************************/

use_helper('Asterisell');

$options = array();
$options[0] = __("calls");
$options[1] = __("extensions");
$options[2] = __("customers");
$options[3] = __("call details");

$defaultChoice = 0;
if (isset($filters['grouping'])) {
  $defaultChoice = $filters['grouping'];
}
echo select_tag('filters[grouping]', options_for_select($options, $defaultChoice));
?>

