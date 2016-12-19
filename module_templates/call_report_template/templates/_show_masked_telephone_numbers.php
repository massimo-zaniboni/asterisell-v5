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

use_helper('Form');
use_helper('I18N');

$options = array('0' => __('masked tel. numbers'));
if (ArViewAllUserPermissionsPeer::haveUserPermission($sf_user->getUserId(), ArPermission::CAN_VIEW_COMPLETE_TELEPHONE_NUMBERS)) {
    $options['1'] = __('complete tel. numbers');
}

if (isset($filters['show_masked_telephone_numbers'])) {
  $defaultChoice = $filters['show_masked_telephone_numbers'];
} else {
  $defaultChoice = '0';
}

echo select_tag('filters[show_masked_telephone_numbers]', options_for_select($options, $defaultChoice));
