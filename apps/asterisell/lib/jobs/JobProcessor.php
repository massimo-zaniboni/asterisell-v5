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