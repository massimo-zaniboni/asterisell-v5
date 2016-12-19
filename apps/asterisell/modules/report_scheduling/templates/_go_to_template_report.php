<?php

use_helper('Asterisell', 'Url', 'I18N');


/**
 * @var ArReportScheduler $ArReportScheduler
 */

$reportId = $ArReportScheduler->getArReportId();

if (isEmptyOrNull($reportId)) {
    echo __("no associated report");
} else {
    echo link_to('Go to report ' . $reportId,'report/edit?id=' . $reportId);
}
