<?php

/* * ************************************************************
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
 * ************************************************************ */

$defaultChoice = "";
if (isset($filters['filter_on_communication_channel'])) {
    $defaultChoice = $filters['filter_on_communication_channel'];
}
echo select_tag('filters[filter_on_communication_channel]', options_for_select(VariableFrame::$filterOnCommunicationChannel, $defaultChoice));
?>

