<?php

require 'lib/model/om/BaseArReportScheduler.php';


/**
 * Skeleton subclass for representing a row from the 'ar_report_scheduler' table.
 *
 *
 *
 * You should add additional methods to this class to meet the
 * application requirements.  This class will only be generated as
 * long as it does not already exist in the output directory.
 *
 * @package    lib.model
 */
class ArReportScheduler extends BaseArReportScheduler
{

    public function __construct()
    {
        // Make sure that parent constructor is always invoked, since that
        // is where any default values for this object are set.
        parent::__construct();
    }

    public function __toString()
    {
        return $this->getName();
    }

    public function getName()
    {
        return $this->getShortDescription();
    }

    public function save(PropelPDO $conn = null)
    {
        $generator = new ScheduledReportGenerator();
        $generator->setArReportScheduler($this);
        $generator->generateOnlyNames();

        return parent::save($conn);
    }

    /**
     * Activate the scheduler, for next executions of monthly scheduling
     * @param int $dayOfMonth 1 for scheduling from the first day of the month
     * @param int $monthsInThePastRespectToday when start scheduling in the past,
     *            0 for not generate reports in the past, but only starting from today,
     *            1 for generating also the reports of last month,
     *            2 for generating also the reports of last two months, etc.
     */
    public function initForMonthly($dayOfMonth, $monthsInThePastRespectToday)
    {
        $today = time();
        $fromDate = strtotime(date('Y', $today) . '-' . date('m', $today) . '-' . $dayOfMonth);
        $fromDate = strtotime('-' . $monthsInThePastRespectToday . ' months', $fromDate);

        $generator = new ScheduledReportGenerator();
        $generator->setArReportScheduler($this);
        $toDate = $generator->getReportRangeToDate($fromDate);

        $this->setLastFromDate($fromDate);
        $this->setLastToDate($toDate);
    }

} // ArReportScheduler
