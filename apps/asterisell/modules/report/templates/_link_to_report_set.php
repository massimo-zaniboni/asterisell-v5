<?php

/**
 * @var ArReport $ArReport
 */

if ((!is_null($ArReport->getArReportSetId()))) {

    $reportSet = $ArReport->getArReportSet();

    $d = 'associated to reports set generated at ' . fromUnixTimestampToSymfonyStrDate(fromMySQLTimestampToUnixTimestamp($reportSet->getFromDate()));
    echo link_to($d, 'report_set/edit?id=' . $ArReport->getArReportSetId());
}

