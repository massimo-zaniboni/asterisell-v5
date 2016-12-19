<?php
/**
 * @var ArReportSet $ar_report_set
 */

echo link_to($ar_report_set->getArReportScheduler()->getShortDescription(),'report_scheduling/edit?id=' . $ar_report_set->getArReportSchedulerId());
