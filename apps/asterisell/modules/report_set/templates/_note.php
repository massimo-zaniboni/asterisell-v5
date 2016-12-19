<?php

/**
 * @var ArReportSet $ar_report_set
 */

$value = object_textarea_tag($ar_report_set->getArReportScheduler(), 'getNote', array (
    'control_name' => 'ar_report_set[note]',
    'disabled' => true,
    'size' => '80x8',
));

echo $value ? $value : '&nbsp;';

