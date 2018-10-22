<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Generate Scheduled Reports.
 */
class GenerateScheduledReports extends FixedJobProcessor
{

    /**
     * This file contains the date of last check of call cost limits.
     */
    const FILE_WITH_LAST_CHECK_DATE = "GenerateScheduledReports-check-date";

    public function process()
    {

        $timeFrameInMinutes = sfConfig::get('app_check_new_external_files_to_import_after_minutes');
        $timeFrameInMinutes = intval($timeFrameInMinutes);
        $timeFrameInMinutes = $timeFrameInMinutes / 4;
        $timeFrameInMinutes = intval($timeFrameInMinutes);

        $checkFile = self::FILE_WITH_LAST_CHECK_DATE;
        $checkLimit = strtotime("-$timeFrameInMinutes minutes");
        $mutex = new Mutex($checkFile);

        if ($mutex->maybeTouch($checkLimit)) {
          return $this->generateReports();
        } else {
            return "Reports will be checked every $timeFrameInMinutes minutes, according application settings.";
        }
    }

    /**
     * Generate reports without checking the timeframe
     * @return string
     */
    public function generateReports() {

        $profiler = new JobProfiler("reports");

        $c = new Criteria();
        $c->add(ArReportSchedulerPeer::IS_ACTIVE, true);
        $scheduled = ArReportSchedulerPeer::doSelect($c);
        $generator = new ScheduledReportGenerator();
        foreach ($scheduled as $scheduler) {
            try {
                /**
                 * @var ArReportScheduler $scheduler
                 */
                $count = $generator->maybeGenerateAssociatedReportsAccordingScheduling($scheduler, null);
                $profiler->addToProcessedUnits($count);
            } catch (ArProblemException $e) {
                // problem already signaled, continue with next report
            }
        }

        ArReportSchedulerPeer::clearInstancePool();

        return $profiler->stop();

    }
}

