<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

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

                $countReports = $generator->generateAssociatedReports($scheduler, $jobData->fromDate, $parentId);
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
                    'Info message for inspecting report generation speed.',
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

