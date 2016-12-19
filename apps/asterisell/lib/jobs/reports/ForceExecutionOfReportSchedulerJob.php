<?php

/* $LICENSE 2009, 2010:
 *
 * Copyright (C) 2009, 2010 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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
 * The user forced the execution of a report scheduler.
 */
class ForceExecutionOfReportSchedulerJob extends JobProcessor
{


    protected function getParametricGarbageKey(ForceExecutionOfReportScheduler $data)
    {
        return get_class($this) . ' - ' . $data->arReportSchedulerId;
    }

    protected function initGarbageInfo(ForceExecutionOfReportScheduler $data)
    {
        ArProblemException::garbageCollect($this->getParametricGarbageKey($data), 0, time());
    }

    public function processEvent(JobData $jobData, $parentId)
    {

        if (!($jobData instanceof ForceExecutionOfReportScheduler)) {
            return null;
        }

        /**
         * @var ForceExecutionOfReportScheduler $jobData
         */
        $prof = new JobProfiler('reports');

        $scheduler = ArReportSchedulerPeer::retrieveByPK($jobData->arReportSchedulerId);

        if (!is_null($scheduler)) {
            try {
                $generator = new ScheduledReportGenerator();
                $generator->setArReportScheduler($scheduler);

                $countReports = $generator->generateAssociatedReports($scheduler, $jobData->fromDate);
                $prof->addToProcessedUnits($countReports);

                ArProblemException::createWithGarbageCollection(
                    ArProblemType::TYPE_INFO,
                    ArProblemDomain::APPLICATION,
                    null,
                    $this->getParametricGarbageKey($jobData),
                    $this->getParametricGarbageKey($jobData),
                    0,
                    time(),
                    $prof->stop(),
                    'Info message for inspecting report generation spped.',
                    'If report generation is too much slow, contact the assistance.',
                    null);

            } catch (ArProblemException $e) {
                // Nothing to do, problem already signaled
            }
        } else {
            ArProblemException::createWithGarbageCollection(
                ArProblemType::TYPE_ERROR,
                ArProblemDomain::APPLICATION,
                null,
                $this->getParametricGarbageKey($jobData),
                $this->getParametricGarbageKey($jobData),
                0,
                time(),
                    'The report scheduler with id ' . $jobData->arReportSchedulerId . ' does not exists.',
                'The scheduled reports are not generated.',
                'Contact the assistance.',
                null);
        }

        return $prof->stop();
    }
}

