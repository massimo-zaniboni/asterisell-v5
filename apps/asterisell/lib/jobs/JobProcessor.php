<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Process events.
 */
abstract class JobProcessor extends FixedJobProcessor
{

    /**
     * Execute a job processing a JobData.
     *
     * The JobData can not be modified.
     *
     * The Job can add new jobs to the JobQueue in order
     * to decompose a complex jobs in dependent jobs.
     *
     * The job must maintains in `ar_new_problem` only the current errors.
     * For doing this, it is used the `ar_new_problem.garbage_collection_key` field.
     *
     * NOTE: Jobs are created every time, for each event, so they should use static fields,
     * if there are heavy initializations that must be done and shared between different
     * executions. At the beginning of the job execution pass, the static variables are initializated,
     * at the end discarded.
     *
     * @param JobData $jobData the data describing the job
     * @param int $jobId the ArJobQueue.id
     * @return string|null null if the JobData is not appropiated,
     * a LOG message with info about the processed info otherwise.
     * @throws ArProblemException
     */
    public abstract function processEvent(JobData $jobData, $jobId);

    public function process() {
        return '';
    }
}