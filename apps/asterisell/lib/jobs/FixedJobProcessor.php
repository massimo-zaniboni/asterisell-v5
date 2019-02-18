<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * A JobProcessor that starts without any event firing it.
 * It is typically used for system/maintanance jobs.
 */
abstract class FixedJobProcessor
{

    //
    // Debug Mode
    //

    protected $debugMode = false;

    public function setDebugMode($v)
    {
        $this->debugMode = $v;
    }

    /**
     * @return bool true if the job must be executed in slow debug mode, with enabled error checkings.
     */
    public function getDebugMode()
    {
        return $this->debugMode;
    }

    /**
     * @var null|string configurable by admin tools, for executing regression tests on demand
     */
    public $regressionTestParam = null;

    //
    // Customizable Interface
    //

    /**
     * Execute a job.
     *
     * The job must maintain in `ar_new_problem` only the current errors.
     * For doing this, it is used the `ar_new_problem.garbage_collection_key` field.
     *
     * NOTE: Jobs are created every time, for each event, so they should use static fields,
     * if there are heavy initializations that must be done and shared between different
     * executions. At the beginning of the job execution pass, the static variables are initializated,
     * at the end discarded.
     *
     * @return String describing briefly the job execution in case of success
     * @throws Exception in case of an error during JOB processing.
     */
    public abstract function process();


    /**
     * A Job can display optional development related notes.
     * @return null|string
     */
    public function getDevNotes()
    {
        return null;
    }

    //
    // Utility Functions
    //

    /**
     * @param int|null $fromDate1
     * @param int|null $fromDate2 null for the minimum fromDate (no interval)
     * @return int the minimum calldate
     */
    static function getMinFromDate($fromDate1, $fromDate2)
    {
        if (is_null($fromDate2)) {
            return $fromDate1;
        } else if (is_null($fromDate1)) {
            return $fromDate2;
        } else if ($fromDate1 < $fromDate2) {
            return $fromDate1;
        } else {
            return $fromDate2;
        }
    }

    /**
     * @param int|null $fromDate1
     * @param int|null $fromDate2 null for the minimum fromDate (no interval)
     * @return int the minimum calldate
     */
    static function getMaxFromDate($fromDate1, $fromDate2)
    {
        if (is_null($fromDate2)) {
            return $fromDate1;
        } else if (is_null($fromDate1)) {
            return $fromDate2;
        } else if ($fromDate1 > $fromDate2) {
            return $fromDate1;
        } else {
            return $fromDate2;
        }
    }

    /**
     * @param int $d date to compare
     * @param int $f time frame start from this (inclusive)
     * @param int|null $t time frame end to this (exclusive), null for maximum time-frame
     * @return int 0 if $d is inside, -1 if $d is before $f, +1 if $d is after $t
     */
    static function compareDateWithTimeFrame($d, $f, $t)
    {

        if ($d < $f) {
            return -1;
        } else {
            assert($d >= $f);
            if (is_null($t)) {
                return 0;
            } else {
                assert(!is_null($t));
                if ($d < $t) {
                    return 0;
                } else {
                    return +1;
                }
            }
        }
    }

    /**
     * @param int $uf
     * @param int|null $ut
     * @param int $ef
     * @param int|null $et
     * @return bool true if the $uf $ut timeframe is joinable with $ef $et timeframe,
     * or in other words there is a timeframe containing both timeframes without empty parts between them.
     */
    static function isTimeFrameJoinableWith($uf, $ut, $ef, $et)
    {

        if (is_null($ut)) {
            // > EF <--- UF ---> ET
            // > UF <---- EF ---- ..
            // > UF <---- ET ---- ..
            // these are the conditions on which uf is inside

            return (self::compareDateWithTimeFrame($uf, $ef, $et) == 0
                || self::compareDateWithTimeFrame($ef, $uf, null) == 0
                || self::compareDateWithTimeFrame($et, $uf, null) == 0);
        } else {
            // > EF <----- UF ----> ET
            // > EF <------ UT ---> ET
            // > UF <------ EF ---> UT
            // > UF <------ ET ---> UT
            // these are the conditions on which the event is inside each other

            return (self::compareDateWithTimeFrame($ut, $ef, $et) == 0
                || self::compareDateWithTimeFrame($uf, $ef, $et) == 0
                || self::compareDateWithTimeFrame($ef, $uf, $ut) == 0
                || self::compareDateWithTimeFrame($et, $uf, $ut) == 0
            );
        }
    }

    /**
     * Note: use an explicit command, for avoinding caching.
     * @param bool $waitNexCronJob true for waiting next cron job
     * @param PDO|null
     */
    static public function rerateCallsFromOfficialCalldate($waitNexCronJob, $conn = null)
    {
        if (is_null($conn)) {
            $conn = Propel::getConnection();
        }

        $query = 'UPDATE ar_params SET scheduled_rerate_from_official_calldate = 1 ';
        if ($waitNexCronJob) {
            $query .= ', wait_for_scheduled_rerate = 1';
        } else {
            $query .= ', wait_for_scheduled_rerate = 0';
        }

        $query .= ' WHERE  is_default = 1';

        $conn->exec($query);
    }

    /**
     * Reset the global command and internal command.
     * @param PDO|null
     * @param bool $resetAll true for resetting both the user, and internal flag, false for resetting only the flag.
     */
    static public function signalAsDoneRerateCallsFromOfficialCalldate($conn = null, $resetAll = true)
    {
        if (is_null($conn)) {
            $conn = Propel::getConnection();
        }

        if ($resetAll) {
            $query = 'UPDATE ar_params
                    SET scheduled_rerate_from_official_calldate = 0,
                        should_reschedule_rerate_from_official_calldate = 0
                    WHERE  is_default = 1';
            $conn->exec($query);
        } else {
            $query = 'UPDATE ar_params
                    SET should_reschedule_rerate_from_official_calldate = 0
                    WHERE is_default = 1';

            $conn->exec($query);
        }
    }

    /**
     * Schedule a user request for rerating calls.
     * Note: these fields are not used from system jobs, so there is no conflict
     * Note: use an explicit SQL command, for avoinding caching.
     * @param int|null $fromDate null for deleting a rerating event
     * @param PDO|null $conn
     */
    static public function rerateCalls($fromDate, $conn = null)
    {
        if (is_null($conn)) {
            $conn = Propel::getConnection();
        }

        $query = 'UPDATE ar_params
                  SET scheduled_rerate_from_specific_calldate = ?,
                      wait_for_scheduled_rerate = 0
                  WHERE is_default = 1';

        $stm = $conn->prepare($query);
        $stm->execute(array(fromUnixTimestampToMySQLTimestamp($fromDate)));
        $stm->closeCursor();
    }

    /**
     * Signal as correctly imported the CDRs.
     * Note: use an explicit command, for avoinding caching.
     * @param PDO|null $conn
     */
    static public function resetImportedCDRSTimeFrame($conn = null)
    {
        if (is_null($conn)) {
            $conn = Propel::getConnection();
        }

        $query = 'UPDATE ar_params
                  SET new_imported_cdrs_from_calldate = NULL
                  WHERE is_default = 1';

        $stm = $conn->prepare($query);
        $stm->execute();
        $stm->closeCursor();
    }

    /**
     * Note: use an explicit command, for avoinding caching.
     * @param int $fromDate
     * @param PDO|null $conn
     */
    static public function updateOfficialCallDate($fromDate, $conn = null)
    {
        if (is_null($conn)) {
            $conn = Propel::getConnection();
        }

        $query = 'UPDATE ar_params SET official_calldate = ? WHERE is_default = 1';

        $stm = $conn->prepare($query);
        $stm->execute(array(fromUnixTimestampToMySQLTimestamp($fromDate)));
        $stm->closeCursor();
    }

    /**
     * Note: use an explicit command, for avoinding caching.
     * @param PDO|null $conn
     * @return int|null $fromDate
     */
    static public function getOfficialCallDate($conn = null)
    {
        if (is_null($conn)) {
            $conn = Propel::getConnection();
        }

        $query = 'SELECT official_calldate FROM ar_params WHERE is_default = 1 LIMIT 1';

        $stm = $conn->prepare($query);
        $stm->execute();
        $r = null;
        while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
            $r = fromMySQLTimestampToUnixTimestamp($rs[0]);
        }
        $stm->closeCursor();

        return $r;
    }

    /**
     * Note: use an explicit command, for avoinding caching.
     * @param PDO|null $conn
     * @return int|null the user requested rerate time frame
     */
    static public function getRerateTimeFrame($conn = null)
    {
        if (is_null($conn)) {
            $conn = Propel::getConnection();
        }

        $query = 'SELECT scheduled_rerate_from_specific_calldate FROM ar_params WHERE is_default = 1 LIMIT 1';

        $stm = $conn->prepare($query);
        $stm->execute();
        $r1 = null;
        while ((($rs = $stm->fetch(PDO::FETCH_NUM)) !== false)) {
            $r1 = fromMySQLTimestampToUnixTimestamp($rs[0]);
        }
        $stm->closeCursor();

        return $r1;
    }


    /**
     * Note: use an explicit command, for avoiding caching.
     * @param PDO|null $conn
     * @return bool true if it must wait another cron job processor passage before rerating,
     * because params were changed during application maintanance
     */
    static public function getWaitForScheduledRerate($conn = null)
    {
        if (is_null($conn)) {
            $conn = Propel::getConnection();
        }

        $query = 'SELECT wait_for_scheduled_rerate FROM ar_params WHERE is_default = 1 LIMIT 1';

        $stm = $conn->prepare($query);
        $stm->execute();
        $r = false;
        while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
            if ($rs[0] == 1) {
                $r = true;
            } else {
                $r = false;
            }
        }
        $stm->closeCursor();

        return $r;
    }

    /**
     *
     * Note: use an explicit command, for avoinding caching.
     * @param bool $v true for waiting another passage of the cron rating processor
     * @param PDO|null $conn
     */
    static public function setWaitForScheduledRerate($v, $conn = null)
    {
        if (is_null($conn)) {
            $conn = Propel::getConnection();
        }

        $query = 'UPDATE ar_params SET wait_for_scheduled_rerate = ? WHERE is_default = 1';
        $stm = $conn->prepare($query);

        if ($v) {
            $p = array(1);
        } else {
            $p = array(0);
        }

        $stm->execute($p);
        $stm->closeCursor();
    }

    /**
     * Note: use an explicit command, for avoiding caching.
     * @param PDO|null $conn
     * @return bool true if a rerate from official calldate (unbilled calls) is scheduled
     */
    static public function getRerateFromOfficialCallDate($conn = null)
    {
        if (is_null($conn)) {
            $conn = Propel::getConnection();
        }

        $query = 'SELECT should_reschedule_rerate_from_official_calldate + scheduled_rerate_from_official_calldate FROM ar_params WHERE is_default = 1 LIMIT 1';

        $stm = $conn->prepare($query);
        $stm->execute();
        $r = false;
        while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
            $r = ($rs[0] > 0);
        }
        $stm->closeCursor();

        return $r;
    }

    /**
     * To call before starting the rating process.
     * Requirements: during rerating new changes of rates are not lost,
     * and they must schedule another rerating.
     * @param PDO|null $conn
     */
    static public function startRatingProcess($conn = null)
    {

        if (is_null($conn)) {
            $conn = Propel::getConnection();
        }

        // 1) annotate if a rerating of unbilled calls must be done
        // 2) unset the flag, for recognizing new changes of the rating params
        // 3) signal that rating is started
        // 4) advance the counter of rating attempts
        $query = 'UPDATE ar_params
                  SET should_reschedule_rerate_from_official_calldate
                         = if(should_reschedule_rerate_from_official_calldate + scheduled_rerate_from_official_calldate > 0, 1, 0),
                      scheduled_rerate_from_official_calldate = 0,
                      current_rerating_event_is_running = 1,
                      current_count_of_rerating_failed_attempts = current_count_of_rerating_failed_attempts + 1
                  WHERE is_default = 1';

        $stm = $conn->prepare($query);
        $stm->execute();
        $stm->closeCursor();
    }

    /**
     * To call when the rating process is correctly terminated.
     * @param PDO|null $conn
     */
    static public function correctEndOfRatingProcess($conn = null)
    {

        if (is_null($conn)) {
            $conn = Propel::getConnection();
        }

        $query = 'UPDATE ar_params
                  SET    current_rerating_event_is_running = 0,
                         current_count_of_rerating_failed_attempts = 0
                  WHERE  is_default = 1';

        $stm = $conn->prepare($query);
        $stm->execute();
        $stm->closeCursor();
    }

    /**
     * Signal as done the rerating event from official calldate.
     * @param PDO|null $conn
     */
    static public function doneRerateFromOfficialCalldate($conn = null)
    {
        if (is_null($conn)) {
            $conn = Propel::getConnection();
        }

        $query = 'UPDATE ar_params
                  SET should_reschedule_rerate_from_official_calldate = 0
                  WHERE is_default = 1';

        $stm = $conn->prepare($query);
        $stm->execute();
        $stm->closeCursor();
    }

    /**
     * @param PDO|null $conn
     * @param bool $manageRateEventCall true in case it is called from manageRateEvent
     * @return bool true if the error is permanent and must be signaled, false for a maybe temporary glitch.
     */
    static public function isRatingProcessProblemPermanent($conn = null, $manageRateEventCall = false)
    {
        if (is_null($conn)) {
            $conn = Propel::getConnection();
        }

        $query = 'SELECT current_count_of_rerating_failed_attempts FROM ar_params WHERE is_default = 1 LIMIT 1';

        $stm = $conn->prepare($query);
        $stm->execute();
        $r = false;
        while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
            $v = $rs[0];
            if ($manageRateEventCall) {
                // the process call it, before another start, so in this case it must be considered a +1
                $v += 1;
            }

            if ($v > 2) {
                // if it failed 1 time it is not a problem (temporary glitch), but 2 is blocked
                // NOTE: the value is at 3 when there are 2 failed attempts.
                $r = true;
            } else {
                $r = false;
            }
        }
        $stm->closeCursor();

        return $r;
    }

    /**
     * Note: use an explicit command, for avoiding caching.
     * @param PDO|null $conn
     * @return int 0 if the rate process is ready,
     * 1 if it is running,
     * 2 if it was blocked from a problem
     *
     * @pre self::scheduleRerateFromOfficialCalldate is called exactly one
     */
    static public function getRatingProcessStatus($conn = null)
    {
        if (is_null($conn)) {
            $conn = Propel::getConnection();
        }

        $query = 'SELECT current_rerating_event_is_running, current_count_of_rerating_failed_attempts FROM ar_params WHERE is_default = 1 LIMIT 1';

        $stm = $conn->prepare($query);
        $stm->execute();
        $r = false;
        while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
            if ($rs[1] > 1) {
                $r = 2;
            } else if ($rs[0] == 0) {
                $r = 0;
            } else {
                $r = 1;
            }
        }
        $stm->closeCursor();

        return $r;
    }

    /**
     * Note: use an explicit command, for avoiding caching.
     * @param int $statusCode the code returned from getRatingProcessStatus
     * @return string a human readable message about the status of rating process.
     */
    static public function getRatingProcessStatusDescription($statusCode)
    {
        if ($statusCode === 0) {
            $r = "";
        } else if ($statusCode === 1) {
            $r = "Rating process is running. ";
        } else if ($statusCode === 2) {
            $r = "Rating process has a critical problem, and it is blocked. ";
        } else {
            $r = "Rating process has an unknown state. ";
        }

        return $r;
    }

    /**
     * @param PDO|null $conn
     * @return string|null a human readable description of the rating event.
     * null if there is no rating event.
     */
    static public function getScheduledRatingEventDescription($conn = null)
    {
        if (is_null($conn)) {
            $conn = Propel::getConnection();
        }

        $status = self::getRatingProcessStatus();

        if ($status == 2) {
            // Signal only that the process is blockeid, without describing the rating time-frame.
            return self::getRatingProcessStatusDescription($status);
        } else {
            $userRateFromDate = self::getRerateTimeFrame($conn);
            if (self::getRerateFromOfficialCallDate($conn)) {
                $unbilledCallsFrom = self::getOfficialCallDate($conn);
            } else {
                $unbilledCallsFrom = null;
            }
            $isPostponed = self::getWaitForScheduledRerate($conn);
            $isRatingNow = ($status == 1);
            $isThereRating = false;

            $msg = '';

            if (!is_null($userRateFromDate)) {
                $isThereRating = true;
                if ($isRatingNow) {
                    $msg .= 'The processor is rating calls ';
                } else {
                    $msg .= 'Request a rerate ';
                }
                $msg .= 'from ' . fromUnixTimestampToSymfonyStrTimestamp($userRateFromDate) . '.';
            }

            if (!is_null($unbilledCallsFrom)) {
                $isThereRating = true;
                if ($isRatingNow) {
                    $msg .= 'The processor is rating now all unbilled calls from ';
                } else {
                    $msg .= 'Rate all unbilled calls from ';
                }

                $msg .= fromUnixTimestampToSymfonyStrTimestamp($unbilledCallsFrom);

                if ($isPostponed) {
                    $msg .= ', but only when there are no new changes of rating params. You can force immediate rerating, with the "Re-rate Not Yet Billed Calls" button. ';
                } else {
                    $msg .= ', using the new rating params.';
                }
            }
            if ($isThereRating) {
                return $msg;
            } else {
                return null;
            }
        }
    }

    /**
     * @param PDO|null $conn
     * @return true if the rating engine can import new CDRs.
     */
    static public function thereAreNewImportedCDRs($conn = null)
    {
        list($d1, $d2) = self::getNewImportedCdrsCallDate($conn);
        return !(is_null($d1));
    }

    /**
     * @param PDO|null $conn
     * @return int|null the requested (if exists) rating event on new imported cdrs
     */
    static public function getNewImportedCdrsCallDate($conn = null)
    {

        $conn = Propel::getConnection();

        $eventFromDate = null;
        $eventToDate = null;

        $query = 'SELECT new_imported_cdrs_from_calldate FROM ar_params WHERE is_default = 1 LIMIT 1';

        $stm = $conn->prepare($query);
        $stm->execute();
        $r1 = null;
        while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
            $r1 = fromMySQLTimestampToUnixTimestamp($rs[0]);
        }
        $stm->closeCursor();

        return $r1;
    }

    /**
     * @param PDO|null $conn
     * @return int 0 for no cleaning, 1 for scheduled cleaning, 2 for activated cleaning
     */
    static public function getCleanErrorTable($conn = null)
    {
        if (is_null($conn)) {
            $conn = Propel::getConnection();
        }

        $query = 'SELECT clean_error_table FROM ar_params WHERE is_default = 1 LIMIT 1';

        $stm = $conn->prepare($query);
        $stm->execute();
        $r = false;
        while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
            $r = $rs[0];
        }
        $stm->closeCursor();

        return $r;
    }

    /**
     * @pram PDO|null $conn
     * @param int $v
     * @param PDO|null $conn
     */
    static public function setCleanErrorTable($conn = null, $v)
    {
        if (is_null($conn)) {
            $conn = Propel::getConnection();
        }

        $query = 'UPDATE ar_params SET clean_error_table = ? WHERE is_default = 1';

        $stm = $conn->prepare($query);
        $stm->execute(array($v));
        $stm->closeCursor();
    }

    /**
     * @param PDO $conn
     * @return void
     * @throws ArProblemException
     */
    protected function commitTransactionOrSignalProblem(PDO $conn)
    {
        self::commitTransactionOrSignalProblem_static($conn, get_class($this));
    }

    /**
     * @param PDO $conn
     * @param string $className
     * @return void
     * @throws ArProblemException
     */
    public static function commitTransactionOrSignalProblem_static(PDO $conn, $className)
    {
        $result = $conn->commit();

        if ($result === false) {

            $problemDuplicationKey = "error on " . $className . ' - ' . time() . '-' . rand();
            $problemDescription = 'Error during commit. ';
            $problemEffect = "The data is not saved, and it mantains its original value.";
            $problemProposedSolution = "If the error persist, contact the assistance because this is an error in the application.";
            $p = ArProblemException::createWithoutGarbageCollection(ArProblemType::TYPE_ERROR, ArProblemDomain::APPLICATION, null, $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
            throw($p);
        }
    }

    /**
     * Rollback only if there is an active transaction.
     * @param PDO $conn
     */
    protected function maybeRollbackTransaction($conn)
    {
        if (!is_null($conn)) {
            if ($conn->inTransaction()) {
                $conn->rollBack();
            }
        }
    }

    /**
     * @param PDO $conn
     * @param string $sql
     * @throws ArProblemException
     */
    protected function execSQL(PDO $conn, $sql)
    {
        if (JobQueueProcessor::$IS_INTERACTIVE) {
            echo "\n   start execSQL: $sql";
        }
        $result = $conn->exec($sql);
        if (JobQueueProcessor::$IS_INTERACTIVE) {
            echo "\n   stop execSQL: $sql";
        }

        if ($result === false) {

            $problemDuplicationKey = "error on " . get_class($this) . ' - ' . time() . '-' . rand();
            $problemDescription = "Error during execution of SQL command \"$sql\" ";
            $problemEffect = "The command is not executed.";
            $problemProposedSolution = "If the error persist, contact the assistance because this is an error in the application.";
            $p = ArProblemException::createWithoutGarbageCollection(ArProblemType::TYPE_ERROR, ArProblemDomain::APPLICATION, null, $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
            throw($p);
        }
    }

    /**
     * To use, instead of `assert(condition)`.
     *
     * @param bool $cond
     * @param string $msg
     * @throws ArProblemException
     */
    protected function assertCondition($cond, $msg = '')
    {
        if (!$cond) {
            $p = ArProblemException::createWithoutGarbageCollection(
                ArProblemType::TYPE_ERROR, ArProblemDomain::APPLICATION, null,
                'assertCondition - ' . time(),
                'Unexpected condition in job ' . get_class($this) . '. ' . $msg,
                'The job is interrupted.',
                'This in an error in the code, specification or data. Contact the assistance. ');
            throw($p);
        }
    }

    /**
     * @return int
     * @throws ArProblemException
     */
    protected function getGlobalStartingDate()
    {
        return self::getGlobalStartingDateForCDRProcessinng();
    }

    /**
     * @return int
     * @throws ArProblemException
     */
    public static function getGlobalStartingDateForCDRProcessinng()
    {
        $startRatingDate = getInstanceConfigValue('start_rating_date');

        $r = fromMySQLTimestampToUnixTimestamp($startRatingDate);

        if (!is_null($r)) {
            return $r;
        } else {
            $problemDuplicationKey = "exception on start_rating_key - " . time();
            $problemDescription = "Unknown timestamp value \"$startRatingDate\", in \"start_rating_date\" instance field.";
            $problemProposedSolution = "This is an error in the configuration file. Contact the assistance.";
            $problemEffect = 'Job execution can not continue';
            $p = ArProblemException::createWithoutGarbageCollection(ArProblemType::TYPE_ERROR, ArProblemDomain::APPLICATION, null, $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
            throw($p);
        }
    }

    protected function getGarbageKey()
    {
        return get_class($this);
    }

    // ----------------------------------
    // Encodings

    /**
     * @param string $srcEncoding
     * @param string $fileName
     * @param string $garbageKey
     * @throws ArProblemException
     */
    public function maybeRecode($srcEncoding, $fileName, $garbageKey)
    {
        if (strcmp(strtoupper($srcEncoding), 'UTF8') !== 0) {
            $cmd = 'recode ' . $srcEncoding . '..UTF8 ' . $fileName;
            $retVal = 0;
            system($cmd, $retVal);
            if ($retVal !== 0) {
                $p = ArProblemException::createWithGarbageCollection(
                    ArProblemType::TYPE_CRITICAL,
                    ArProblemDomain::CONFIGURATIONS,
                    null,
                    "recode - " . $fileName,
                    $garbageKey,
                    null,
                    null,
                    "not character encoding for file $fileName",
                    "Can not convert the character encoding of file \"$fileName\", using the command \"$cmd\".",
                    "Install the recode utiliy with command \"yum install recode\". If the error persist, contact the assistance."
                    );
                throw($p);
            }
        }
    }

    // ----------------------------------
    // Email

    /**
     * Retrieve the list of customer email address interested to a billing like document,
     * containing warnings like high call costs and so on.
     * Usually they are all the users associated to the $unitId, and parent $unitId, until the first billable organization.
     * Only users with the proper permissions are used.
     * Common emails are not repeated.
     *
     * @param int $unitId the unitId interested to the info
     * @return array of list(email, name) the first element is the receiver, other elements are CC elements
     */
    protected function getEmailsForWarningsAboutAccount($unitId)
    {
        $now = time();
        $info = OrganizationUnitInfo::getInstance();

        /**
         * dictionary of found emails
         *
         * @var bool[string] $emailDict
         */
        $emailDict = array();

        /**
         * list(email, name)
         * @var array $emails
         */
        $emails = array();

        while (!is_null($unitId)) {
            $data = $info->getDataInfo($unitId, $now);

            $usersAndPermissions = $info->getDirectUsersWithAllPermissions($unitId);
            foreach ($usersAndPermissions as $userId => $permissions) {
                if (in_array(ArPermission::CAN_RECEIVE_EMAILS, $permissions)) {
                    $user = ArUserPeer::retrieveByPK($userId);
                    $this->assertCondition(!is_null($user));
                    $party = $user->getArParty();
                    if (!is_null($party)) {
                        $email = $party->getEmail();
                        if (!isEmptyOrNull($email)) {
                            if (!array_key_exists($email, $emailDict)) {
                                $emailDict[$email] = true;
                                $emails[] = array($email, $party->getName());
                            }
                        }
                    }
                }
            }

            if ($data[OrganizationUnitInfo::DATA_UNIT_IS_BILLABLE]) {
                // stop at the first billable organization
                $unitId = null;
            } else {
                $unitId = $info->getParentId($unitId, $now);
            }
        }

        return $emails;
    }

    /**
     * Retrieve the legal email of the customer/organization, for receiving invoices.
     * This is the email directly associated to the billing party, and not to a user.
     * An invoice is a legal document, and it must be sent to a legal entity/email.
     *
     * @param int $unitId the unitId interested to the info
     * @return array|null list(email, name), null if the email does not exists
     */
    protected function getLegalEmailForInvoices($unitId)
    {
        $now = time();
        $info = OrganizationUnitInfo::getInstance();

        $billableUnitId = null;
        $billableDataInfo = null;
        while (!is_null($unitId)) {
            $data = $info->getDataInfo($unitId, $now);
            if ($data[OrganizationUnitInfo::DATA_UNIT_IS_BILLABLE]) {
                $billableUnitId = $unitId;
                $billableDataInfo = $data;

                // stop at the first billable organization
                $unitId = null;
            } else {
                $unitId = $info->getParentId($unitId, $now);
            }
        }

        if (is_null($billableUnitId)) {
            return null;
        }

        $partyId = $billableDataInfo[OrganizationUnitInfo::DATA_PARTY_ID];
        if (is_null($partyId)) {
            return null;
        }

        $party = ArPartyPeer::retrieveByPK($partyId);
        $this->assertCondition(!is_null($party));

        if (isEmptyOrNull($party->getEmail())) {
            return null;
        }

        return array($party->getEmail(), $party->getName());
    }

    /**
     * Send an email using the configured methods, in main params.
     *
     * Signal problems also in the error table.
     *
     * @param string $receiverName
     * @param string $receiverAddress
     * @param array $ccAddressesAndNames array of list(email, name)
     * @param string $subject
     * @param string $body
     * @param array|null $attachments array of list(fileName, mimeType, content)
     * @return bool true if the email was sent sucessfully.
     * NOTE: if there is a mailer daemon, we can not be sure that the email is really sent
     *
     * @require $receiverAddress is a valid email address
     * @require $ccAddressesAndNames are valid email address
     *
     * @throws ArProblemException
     */
    protected function sendEmail($receiverAddress, $receiverName, $ccAddressesAndNames, $subject, $body, $attachments = null)
    {
        $ccAddressesAndNamesDescr = '';
        foreach ($ccAddressesAndNames as $e => $n) {
            $ccAddressesAndNamesDescr .= "$e <$n>, ";
        }
        if (!isEmptyOrNull($ccAddressesAndNamesDescr)) {
            $ccAddressesAndNamesDescr = ', (with CC ' . $ccAddressesAndNamesDescr . ') ';
        }

        try {
            static $mailer = null;

            $garbageKey = 'send-email-' . get_class($this);

            ArProblemException::garbageCollect($garbageKey, null, null);

            if (!$this->areEmailSettingsConfigured()) {

                $problemDuplicationKey = get_class($this) . ' - SMTP not configured';
                $problemDescription = 'It is not possible sending emails to Web Accounts and Users.';
                $problemProposedSolution = "Configure SMTP params.";
                $problemEffect = "Emails are not sent.";
                ArProblemException::createWithGarbageCollection(
                    ArProblemType::TYPE_ERROR,
                    ArProblemDomain::CONFIGURATIONS,
                    null,
                    $problemDuplicationKey,
                    $garbageKey,
                    null,
                    null,
                    $problemDescription,
                    $problemEffect,
                    $problemProposedSolution,
                    null);

                return false;
            }

            $params = ArParamsPeer::getDefaultParams();

            $fromAddress = $params->getInvoicingEmailAddress();
            $fromName = $params->getSenderNameOnInvoicingEmails();

            $message = Swift_Message::newInstance($subject, $body, null, 'UTF-8');
            $message->setFrom(array($fromAddress => $fromName));
            $message->setTo(array($receiverAddress => $receiverName));
            $message->setCc($ccAddressesAndNames);

            if (!is_null($attachments)) {

                foreach ($attachments as $attachment) {
                    list($fileName, $mimeType, $data) = $attachment;
                    $messageAttachment = Swift_Attachment::newInstance()
                        ->setFileName($fileName)
                        ->setContentType($mimeType)
                        ->setBody($data);

                    $message->attach($messageAttachment);
                }
            }

            $message = $this->normalizeMessage($message);

            if (is_null($mailer)) {
                $mailer = $this->getMailerNewInstance();
            }

            $numSent = $mailer->send($message);

            if ($numSent == 0) {
                $problemDuplicationKey = "sending email " . $receiverAddress;
                $problemDescription = "Problems sending email to <" . $receiverAddress . ">, for user $receiverName $ccAddressesAndNamesDescr, inside job " . get_class($this);
                $problemProposedSolution = "Check if the email is correct.";
                $problemEffect = "This email is not sent. The email will be sent after this problem is solved.";

                ArProblemException::createWithGarbageCollection(
                    ArProblemType::TYPE_ERROR,
                    ArProblemDomain::CONFIGURATIONS,
                    null,
                    $problemDuplicationKey,
                    $garbageKey,
                    null,
                    null,
                    $problemDescription,
                    $problemEffect,
                    $problemProposedSolution,
                    null);

                return false;
            } else {
                $problemDuplicationKey = "sending email " . $receiverAddress . ' - ' . microtime_float();

                $problemDescription = "Sent email to <" . $receiverAddress . ">, for user $receiverName $ccAddressesAndNamesDescr, inside job " . get_class($this);
                $problemProposedSolution = "This is only an informative message.";
                $problemEffect = "An email was sent.";

                ArProblemException::createWithoutGarbageCollection(
                    ArProblemType::TYPE_INFO,
                    ArProblemDomain::REPORTS,
                    null,
                    $problemDuplicationKey,
                    $problemDescription,
                    $problemEffect,
                    $problemProposedSolution,
                    null);

                return true;
            }

        } catch (Exception $e) {
            $problemDuplicationKey = "sending email " . $receiverAddress;
            $problemDescription = "Problems sending email to <" . $receiverAddress . ">, for user $receiverName $ccAddressesAndNamesDescr, inside job " . get_class($this);
            $problemProposedSolution = "Check if the email is correct.";
            $problemEffect = "This email is not sent. The email will be sent after this problem is solved.";

            ArProblemException::createFromGenericExceptionWithGarbageCollection(
                $e,
                ArProblemType::TYPE_ERROR,
                ArProblemDomain::CONFIGURATIONS,
                null,
                $problemDuplicationKey,
                $garbageKey,
                null,
                null,
                $problemDescription,
                $problemEffect,
                $problemProposedSolution,
                null);

            return false;
        }

        return true;
    }

    /**
     * @return Swift_Mailer a configured / initialized Swift_Mailer.
     * @throws ArProblemException
     */
    protected function getMailerNewInstance()
    {

        $smtpCommand = getInstanceConfigValueFromKeys(array('smtp', 'smtp_command'));

        if (isEmptyOrNull($smtpCommand)) {

            $transport = Swift_SmtpTransport::newInstance();
            $transport->setHost(trim(getInstanceConfigValueFromKeys(array('smtp', 'host'))));
            $transport->setPort(trim(getInstanceConfigValueFromKeys(array('smtp', 'port'))));

            $username = trim(getInstanceConfigValueFromKeys(array('smtp', 'username')));
            $password = trim(getInstanceConfigValueFromKeys(array('smtp', 'password')));

            if (isEmptyOrNull($username) && isEmptyOrNull($password)) {
                // no authentication is needed
            } else {
                $transport->setUsername($username);
                $transport->setPassword($password);
            }

            $encryption = getInstanceConfigValueFromKeys(array('smtp', 'encryption'));

            if (!isEmptyOrNull($encryption)) {
                $encryption = trim(strtolower($encryption));
                if ($encryption != 'plain') {
                    $transport->setEncryption($encryption);
                }
            }

            $mailer = Swift_Mailer::newInstance($transport);

            $emailsLimit = getInstanceConfigValueFromKeys(array('smtp', 'reconnect_after_nr_of_messages'));
            $pause = getInstanceConfigValueFromKeys(array('smtp', 'seconds_of_pause_after_reconnection'));

            if (!is_null($emailsLimit) && $emailsLimit != 0) {
                $plugin = new Swift_Plugins_AntiFloodPlugin();
                $plugin->setThreshold($emailsLimit);
                if (!is_null($pause) && $pause != 0) {
                    $plugin->setSleepTime($pause);
                }
                $mailer->registerPlugin($plugin);
            }

        } else {
            $transport = Swift_SendmailTransport::newInstance($smtpCommand);
            $mailer = Swift_Mailer::newInstance($transport);
        }

        return $mailer;
    }

    /**
     * @return bool
     */
    protected function areEmailSettingsConfigured()
    {
        $smtpCommand = getInstanceConfigValueFromKeys(array('smtp', 'smtp_command'));
        $smtpHost = getInstanceConfigValueFromKeys(array('smtp', 'host'));

        if (isEmptyOrNull($smtpCommand) && isEmptyOrNull($smtpHost)) {
            return false;
        } else {
            return true;
        }
    }

    /**
     * Supporting application wide disabling of real email sending.
     *
     * @param Swift_Mime_Message $message
     * @return Swift_Mime_Message
     */
    protected function normalizeMessage(Swift_Mime_Message $message)
    {
        $thereIsNewReceiver = false;
        $count = 0;
        $continue = true;
        $isFirst = true;
        $newReceiver = null;
        $newCC = array();

        while ($continue) {

            $value = sfConfig::get('app_send_emails_to_these_users_instead_of_original_receiver_' . $count);
            $count++;

            if (is_null($value)) {
                $continue = false;
            } else {
                $thereIsNewReceiver = true;
                $receiver = $value;
                if ($isFirst) {
                    $newReceiver = $receiver;
                    $isFirst = false;
                } else {
                    $newCC[] = $receiver;
                }
            }
        }

        if ($thereIsNewReceiver) {

            $eol = "\n\r";

            $originalReceiver = print_r($message->getTo(), true);

            $newMessage = clone $message;
            $newMessage->setTo($newReceiver);
            $newMessage->setCc($newCC);

            $descr = "This message was originally meant to be sent to \"$originalReceiver\", but it is enabled the debug setting \"send_emails_to_these_users_instead_of_original_receiver\". The original receiver, will not receive this message. $eol"
                . $eol . $message->getBody();
            $newMessage->setBody($descr);

            return $newMessage;
        } else {
            return $message;
        }
    }

    // --------------------------
    // Files

    /**
     * @param string $directory
     * @throws ArProblemException
     */
    protected function maybeCreateDirectory($directory)
    {
        if (!file_exists($directory)) {
            $r = @mkdir($directory, 0755, true);
            if ($r === FALSE) {
                $problemDuplicationKey = get_class($this) . " create directory " . $directory;
                $problemDescription = "Error creating directory \"$directory\", in job " . get_class($this);
                $problemEffect = "The files inside the directory are not processed.";
                $problemProposedSolution = "Fix the problem in the directory. Probably there are file access rights problems. These files will be processed automatically at the next iteration of job processor.";
                $p = ArProblemException::createWithoutGarbageCollection(
                    ArProblemType::TYPE_ERROR,
                    ArProblemDomain::APPLICATION,
                    null,
                    $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
                throw ($p);
            }
        }
    }


    /**
     * @param string $directory
     * @param string $errorName
     * @throws ArProblemException
     */
    static public function maybeCreateDirectoryStatic($directory, $errorName)
    {
        if (!file_exists($directory)) {
            $r = @mkdir($directory, 0755, true);
            if ($r === FALSE) {
                $problemDuplicationKey = $errorName . " - create directory " . $directory;
                $problemDescription = "Error creating directory \"$directory\", in job " . $errorName;
                $problemEffect = "The files inside the directory are not processed.";
                $problemProposedSolution = "Fix the problem in the directory. Probably there are file access rights problems. These files will be processed automatically at the next iteration of job processor.";
                $p = ArProblemException::createWithoutGarbageCollection(
                    ArProblemType::TYPE_ERROR,
                    ArProblemDomain::APPLICATION,
                    null,
                    $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
                throw ($p);
            }
        }
    }

    // ---------------------------------
    // Reports

    const RATE_ENGINE_CHANGED_DAYS_JOB_NAME = 'external_haskell_rating_engine';

    /**
     * @return int|null
     */
    static public function getRateEngineChangedDaysJobId()
    {
        static $id = null;

        if (is_null($id)) {
            $conn = Propel::getConnection();
            $stm = $conn->prepare('SELECT id FROM ar_daily_status_job WHERE NAME = ?');
            $stm->execute(array(self::RATE_ENGINE_CHANGED_DAYS_JOB_NAME));
            while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
                $id = $rs[0];
            }
            $stm->closeCursor();
        }

        return $id;
    }

    /**
     * @param PDO $conn
     * @param int $fromDate the timeframe with new/updated CDRs
     */
    static public function invalidatePendingReports($conn, $fromDate)
    {
        $conn->beginTransaction();

        $query = 'UPDATE ar_report
                  SET    produced_report_must_be_regenerated = 1
                  WHERE  produced_report_already_reviewed = 0
                  AND    (? > from_date AND ? < to_date)
                  OR     (? <= from_date)';

        $fromDateS = fromUnixTimestampToMySQLTimestamp($fromDate);
        $stm = $conn->prepare($query);
        $stm->execute(array($fromDateS, $fromDateS, $fromDateS));

        self::commitTransactionOrSignalProblem_static($conn, 'invalidate pending reports');
    }

}
