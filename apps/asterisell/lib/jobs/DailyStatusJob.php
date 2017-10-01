<?php

/* $LICENSE 2013, 2017:
 *
 * Copyright (C) 2013, 2017 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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
 * Process days where there are changed CDRs.
 */
abstract class DailyStatusJob extends FixedJobProcessor
{

    ////////////////////////////
    // CUSTOMIZABLE BEHAVIOUR //
    ////////////////////////////

    /**
     * @return int the calldate from which enabling the sending of CDRs. It can not be a date in the future, only in the past.
     */
    abstract public function getActivationDate();

    /**
     * @return bool true if transaction signaling that all the events are processed,
     * must be committed only after `endJob` returned correctly.
     * false for committing after each processed day.
     */
    public function commitAtEndOffAllProcessedDays()
    {
        return false;
    }

    /**
     * Called before starting processing.
     *
     * @return bool true if the job can process the events, false for not continuinig with the job.
     */
    public function initJob()
    {
        return true;
    }

    /**
     * Called at the end of the processing of all the events.
     * @param $conn the connection with the opened transaction to use.
     * The connection will be automatically commited (or aborted)
     * at the end of this method.
     *
     * Can throw an exception in case there are problems and the transaction
     * must be aborted.
     */
    public function endJob(PropelPDO $conn)
    {
    }

    /**
     * @param int $fromDate the day where there is changed data,
     * @param int $toDate the last value to process not inclusive (the next day)
     * @param PropelPDO $conn the connection with the transaction to use
     *
     * @post the same instance of a job, is called for every processing day,
     * so data can be cached inside jobs.
     *
     * Throw an exception, if the event is not processed correctly:
     * - all changes to database are retired
     * - the event will be tried again next time
     */
    public abstract function processChangedDay($fromDate, $toDate, PropelPDO $conn);

    /**
     * return int the minutes to wait after each execution of the job.
     * 0 for always executing the job.
     */
    protected function waitXMinutes()
    {
        return 0;
    }

    /**
     * @return int|null null if the job is not registered. The Id of the job otherwise
     */
    public function getJobId()
    {
        $id = null;

        $conn = Propel::getConnection();
        $stmt = $conn->prepare('SELECT id FROM ar_daily_status_job WHERE name = ?');
        $stmt->execute(array(get_class($this)));
        while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
            $id = $rs[0];
        }
        $stmt->closeCursor();

        return $id;
    }

    /**
     * Register the job in the table of jobs to process.
     *
     * @return int the ID of the job
     */
    public function registerJob()
    {
        $conn = Propel::getConnection();
        $stmt = $conn->prepare('INSERT INTO ar_daily_status_job(name) VALUES (?) ON DUPLICATE KEY UPDATE name = VALUES(name);');
        $stmt->execute(array(get_class($this)));
        $stmt->closeCursor();

        $id = null;
        $stmt = $conn->prepare('SELECT id FROM ar_daily_status_job WHERE name = ?');
        $stmt->execute(array(get_class($this)));
        while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
            $id = $rs[0];
        }
        $this->assertCondition(!is_null($id));
        $stmt->closeCursor();

        return $id;
    }

    /**
     * Register the job, and signal for receiving/processing all the days from the specified date.
     * @param int $fromDate
     * @return id the id of the job
     */
    protected function registerJobStartingFromDate($fromDate)
    {
        $id = $this->registerJob();

        $conn = Propel::getConnection();
        $stmt = $conn->prepare('INSERT INTO ar_daily_status_change(`day`,`ar_daily_status_job_id`) VALUES (?,?)');

        $lastCallDate = ArCdrPeer::getLastCallDate();

        if (!is_null($lastCallDate)) {
            $lastCallDate = startWith00Timestamp($lastCallDate);
            $currDate = startWith00Timestamp($fromDate);
            while ($currDate <= $lastCallDate) {
                $stmt->execute(array(fromUnixTimestampToMySQLTimestamp($currDate), $id));
                $stmt->closeCursor();

                $currDate = strtotime('+1 day', $currDate);
            }
        }
    }

    //////////////////////
    // SERVICES TO CALL //
    //////////////////////

    protected $garbageFrom;
    protected $garbageTo;

    /**
     * Create an error that can be garbage collected according the values of the CDR.
     * @pre called only if the values of CDR fields are reliable
     *
     * @param int $errorType ArProblemType
     * @param int $problemDomain ArProblemDomain
     * @param string $problemDuplicationKey
     * @param string $problemDescription
     * @param string $problemEffect
     * @param string $problemProposedSolution
     * @return ArProblemException
     */
    protected function createError($errorType, $problemDomain, $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution)
    {
        $p = ArProblemException::createWithGarbageCollection(
            $errorType,
            $problemDomain,
            null,
            $problemDuplicationKey,
            $this->getGarbageKey(),
            $this->garbageFrom,
            $this->garbageTo,
            'CDRs from ' . fromUnixTimestampToSymfonyStrTimestamp($this->garbageFrom) . ' to ' . fromUnixTimestampToSymfonyStrTimestamp($this->garbageTo) . ' will not be processed. ' . $problemDescription,
            $problemEffect,
            $problemProposedSolution . ' If the problem persist contact the assistance.',
            null);

        return $p;
    }

    ////////////////////////
    // INTERNAL BEHAVIOUR //
    ////////////////////////

    public function process()
    {
        $timeFrameInMinutes = $this->waitXMinutes();

        if ($timeFrameInMinutes == 0) {
            $executeNow = true;
        } else {
            $checkFile = get_class($this);
            $checkLimit = strtotime("-$timeFrameInMinutes minutes");
            $mutex = new Mutex($checkFile);
            $executeNow = $mutex->maybeTouch($checkLimit);
        }

        if ($executeNow) {
            $prof = new JobProfiler('days');

            $jobId = $this->getJobId();
            if (is_null($jobId)) {
                $jobId = $this->registerJobStartingFromDate($this->getActivationDate());
            }

            $continue = $this->initJob();
            if ($continue) {

                // First extract all the days to process.
                // NOTE: they are few (365 max in a year), and so we can free the $conn
                // for other scopes.
                $conn = Propel::getConnection();
                $days = array();
                $queryOnDaysToProcess = 'SELECT day FROM ar_daily_status_change WHERE ar_daily_status_job_id = ? ORDER BY day';
                $stmt = $conn->prepare($queryOnDaysToProcess);
                $stmt->execute(array($jobId));
                while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
                    $dayToProcessSQL = $rs[0];
                    $days[] = fromMySQLTimestampToUnixTimestamp($dayToProcessSQL);
                }
                $stmt->closeCursor();

                if ($this->commitAtEndOffAllProcessedDays()) {
                    // Use a global transaction
                    $conn->beginTransaction();
                    try {
                        foreach ($days as $dayToProcess) {
                            $prof->incrementProcessedUnits();

                            $fromDate = startWith00Timestamp($dayToProcess);
                            $toDate = strtotime('+1 day', $dayToProcess);

                            $this->garbageFrom = $fromDate;
                            $this->garbageTo = $toDate;

                            // clear the errors of the day
                            ArProblemException::garbageCollect($this->getGarbageKey(), $fromDate, $toDate);

                            $this->processChangedDay($fromDate, $toDate, $conn);
                            $this->deleteEvent($jobId, $dayToProcess, $conn);
                        }
                        $this->endJob($conn);
                        $this->commitTransactionOrSignalProblem($conn);
                    } catch (ArProblemException $e) {
                        $this->maybeRollbackTransaction($conn);
                        throw($e);
                    } catch (Exception $e) {
                        $this->maybeRollbackTransaction($conn);

                        $p = ArProblemException::createFromGenericExceptionWithGarbageCollection(
                            $e,
                            ArProblemType::TYPE_ERROR,
                            get_class($this) . " - unexpected error - " . rand(),
                            $this->getGarbageKey(),
                            $this->garbageFrom,
                            $this->garbageTo,
                            ArProblemException::describeGenericException($e),
                            'CDRs from ' . fromUnixTimestampToSymfonyStrTimestamp($fromDate) . ' to ' . fromUnixTimestampToSymfonyStrTimestamp($toDate) . ' will not be processed.',
                            "Try to rerate the CDRs of the day again. If the problem persist contact the assistance."
                        );

                        throw($p);
                    }
                } else {
                    // Use a transaction for each day
                    foreach ($days as $dayToProcess) {
                        $prof->incrementProcessedUnits();

                        $fromDate = startWith00Timestamp($dayToProcess);
                        $toDate = strtotime('+1 day', $dayToProcess);

                        $this->garbageFrom = $fromDate;
                        $this->garbageTo = $toDate;

                        $conn->beginTransaction();
                        try {

                            // clear the errors of the day
                            ArProblemException::garbageCollect($this->getGarbageKey(), $fromDate, $toDate);

                            $this->processChangedDay($fromDate, $toDate, $conn);
                            $this->deleteEvent($jobId, $dayToProcess, $conn);
                            $this->commitTransactionOrSignalProblem($conn);
                        } catch (ArProblemException $e) {
                            $this->maybeRollbackTransaction($conn);
                            throw($e);
                        } catch (Exception $e) {
                            $this->maybeRollbackTransaction($conn);

                            $p = ArProblemException::createFromGenericExceptionWithGarbageCollection(
                                $e,
                                ArProblemType::TYPE_ERROR,
                                get_class($this) . " - unexpected error - " . rand(),
                                $this->getGarbageKey(),
                                $this->garbageFrom,
                                $this->garbageTo,
                                ArProblemException::describeGenericException($e),
                                'CDRs from ' . fromUnixTimestampToSymfonyStrTimestamp($fromDate) . ' to ' . fromUnixTimestampToSymfonyStrTimestamp($toDate) . ' will not be processed.',
                                "Try to rerate the CDRs of the day again. If the problem persist contact the assistance."
                            );

                            throw($p);
                        }
                    }
                    $this->endJob($conn);
                }

                // Return final stats
                return 'CDRs of ' . $prof->stop();
            }
        } else {
            return "will be executed later, every $timeFrameInMinutes minutes.";
        }
    }

    /**
     * Signal the event as already processed.
     * @param int $jobId
     * @param int $day
     * @param PDO $conn
     */
    protected function deleteEvent($jobId, $day, PDO $conn)
    {
        $stmt = $conn->prepare('DELETE FROM ar_daily_status_change WHERE day = ? AND ar_daily_status_job_id = ?');
        $stmt->execute(array(fromUnixTimestampToMySQLDate($day), $jobId));
        $stmt->closeCursor();
    }
}
