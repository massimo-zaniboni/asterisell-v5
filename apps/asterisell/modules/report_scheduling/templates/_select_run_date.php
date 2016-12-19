<?php
/**
 * @var ArReportScheduler $ArReportScheduler
 */

echo input_date_tag('select_run_date',
    $ArReportScheduler->getLastFromDate(),
    array (
        'rich' => true,
        'withtime' => false,
        'calendar_button_img' => '/sf/sf_admin/images/date.png',
        'disabled' => false,
));

