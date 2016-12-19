<?php

/**
 * @var ArService $ar_service
 */

$options = array("" => "");

$v = 'monthly';
$options[$v] = $v;

$v = 'weekly';
$options[$v] = $v;

$defaultChoice = "";
if (!is_null($ar_service->getScheduleTimeframe())) {
  $defaultChoice = $ar_service->getScheduleTimeframe();
}
echo select_tag('select_scheduling', options_for_select($options, $defaultChoice));
