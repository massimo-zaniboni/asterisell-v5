<?php

/**
 * @var ArService $ar_service
 */

$vs = array();
$vs[] = 'Monday';
$vs[] = 'Tuesday';
$vs[] = 'Wednesday';
$vs[] = 'Thursday';
$vs[] = 'Friday';
$vs[] = 'Saturday';
$vs[] = 'Sunday';

for($i = 1; $i < 29; $i++) {
    $vs[] = "$i";
}

$options = array("" => "");

foreach($vs as $v) {
    $options[$v] = $v;
}

$defaultChoice = "";
if (!is_null($ar_service->getScheduleFrom())) {
  $defaultChoice = $ar_service->getScheduleFrom();
}
echo select_tag('select_schedule_from', options_for_select($options, $defaultChoice));
