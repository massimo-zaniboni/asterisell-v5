<?php

/* $LICENSE 2012:
 *
 * Copyright (C) 2012 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
 *
 * This file is part of Asterisell.
 *
 * Asterisell is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * Asterisell is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Asterisell. If not, see <http://www.gnu.org/licenses/>.
 * $
 */

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

