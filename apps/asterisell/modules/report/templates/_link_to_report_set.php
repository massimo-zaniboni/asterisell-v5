<?php

/**
 * @var ArReport $ArReport
 */

$reportSet = null;

if ((!is_null($ArReport->getArReportSetId()))) {
    $reportSet = $ArReport->getArReportSet();
} else if (!is_null($ArReport->getAboutArReportSetId())) {
    $reportSet = $ArReport->getAboutArReportSet();
}

if (!is_null($reportSet)) {
    $d = 'associated to reports set generated at ' . fromUnixTimestampToSymfonyStrDate(fromMySQLTimestampToUnixTimestamp($reportSet->getFromDate()));
    echo link_to($d, 'report_set/edit?id=' . $reportSet->getId());
}
