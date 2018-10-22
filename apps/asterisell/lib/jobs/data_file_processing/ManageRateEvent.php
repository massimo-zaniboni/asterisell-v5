<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Rate the calls.
 * First CDRS are imported from `ImportDataFiles` into `ar_source_cdr` table, and the rerating fields are completed.
 * Then this job rate them, according the specified rerating time frame.
 *
 * Export rate configurations to the external rate engine, using textual files.
 *
 * In case of a CSV line with import problems, generate a CDR with error, and link to an error message.
 * In case of a CDR with rate problems, generate a CDR with error, and link to an error message.
 *
 * CDR with errors update always the stats of CDR with errors.
 *
 * If the table with error is too big, it can be deleted. In case of rerate event, the errors are generated again.
 *
 * The requirements are:
 * - CDR stats with good/error CDRs are updated
 * - errors are signaled
 * - in case of an unknown number of CDRs with rating errors, a critical error is always reported
 * - CDR with errors are linked to the error in the table
 * - no CDRs are lost
 * - rerating of the current unbilled (recent) rating time-frame is not lost, also in case of big problems in the rating engine
 * - only explicit rerating event scheduled from the user can be lost or overwritten
 * - every lost of reratingn event, is signaled with an info/warning/error/critical
 */
class ManageRateEvent extends FixedJobProcessor
{
    const EXTENSIONS_FILE_NAME = 'extensions.csv';

    const RATE_DEBUG_INFO = 'rating_debug.txt';

    const RATING_ERRORS_FILE_NAME = 'rating_errors.info';

    const RATE_DEBUG_FILE_NAME = 'rate_debug.info';

    const IMPORT_DEBUG_FILE_NAME = 'import_debug.info';

    const SIGNAL_FILE_NAME = 'signal.lock';

    const GARBAGE_KEY = 'rating-calls-critical';

    const DEBUG_INFO_REPORT_INTERNAL_NAME = 'rating_debug_info';



    // IMPORTANT: do not change nothing, because this value
    // will be updated by the management procedure in case
    // of debug mode.
    const DONT_TOUCH_DEBUG_MODE_GHC_PARAMS = " ";

    /**
     * @var int|null a date to use for limiting (during testing) the ar_source_cdr to load
     */
    public $testToDate = null;

    /**
     * @var int 0 for using all the cores
     */
    public $useCores = 0;

    /**
     * @var int 1 for (experimental/buggy) fast grouping of CDRS
     */
    public $useFastGrouping = 1;

    public function process()
    {

        $prof = new JobProfiler('rated CDRs');
        $conn = Propel::getConnection();

        // NOTE: Now I rate always immediately, because if there are jobs
        // changing continuosly the data, rating process can be postponed indefinitely
        //
        // if (self::getWaitForScheduledRerate($conn)) {
        //    // Signal that at the next execution, it can start rating.
        //    self::setWaitForScheduledRerate(false, $conn);
        //    return "Rating event will be started at next cron job processor run, if there aren't meantime new changes in rating settings.";
        // }

        self::setWaitForScheduledRerate(false, $conn);

        //
        // Calculate the rating time frame
        //
        // Usually we have
        // case A) rate only new imported CDRS
        // case B) rate only/also unbilled calls
        // case C) rate only/also user specified time frame
        //
        // Try to create the bigger time-frame containing all these events.
        //

        $globalStartingDate = $this->getGlobalStartingDate();

        $rateFromDate = null;

        $rateImportedCDRs = false;
        $rateUnbilledCalls = self::getRerateFromOfficialCallDate($conn);
        $rateUserSpecifiedTimeFrame = false;

        $d = self::getNewImportedCdrsCallDate($conn);
        if (!is_null($d)) {
            $rateFromDate = self::getMinFromDate($rateFromDate, $d);
            $rateImportedCDRs = true;
        }

        $d = self::getRerateTimeFrame($conn);
        if (!is_null($d)) {
            $rateUserSpecifiedTimeFrame = true;
            $rateFromDate = self::getMinFromDate($rateFromDate, $d);
        }

        if ($rateUnbilledCalls) {
            $d = self::getOfficialCallDate($conn);
            $rateFromDate = self::getMinFromDate($rateFromDate, $d);
        }

        // Normalize dates

        if (!is_null($rateFromDate)) {
            $rateFromDate = self::getMaxFromDate($rateFromDate, $globalStartingDate);
        }

        // Prepare the string

        $executeRating = false;
        if (!is_null($rateFromDate)) {
            $rateFromDateS = fromUnixTimestampToMySQLTimestamp($rateFromDate);
            $executeRating = true;
        } else {
            $rateFromDateS = 'null';
        }

        // Execute Rating

        if ($executeRating) {

            self::startRatingProcess($conn);
            ArProblemException::garbageCollect(self::GARBAGE_KEY, null, null);

            $exportFromDate = $rateFromDate;
            $this->exportConfigurationFiles($exportFromDate);

            // Start with an empty state about this
            $conn->exec('TRUNCATE ar_expanded_extensions');

            if ($this->getDebugMode()) {
                $debugModeS = '1';
            } else {
                $debugModeS = '0';
            }

            $isBilling = sfConfig::get('app_is_billing');
            if ($isBilling) {
                $isBillingS = '1';
            } else {
                $isBillingS = '0';
            }
            $debugFileName = normalizeFileNamePath(ImportDataFiles::getMySQLAccessibleTmpDirectory(self::GARBAGE_KEY) . '/' . self::RATE_DEBUG_FILE_NAME);
            @unlink($debugFileName);

            $ratingParams = array();
            $cmd = RateEngineService::getToolExecutable()
                . ' --rate '
                . RateEngineService::writeWithDBAccessParams($ratingParams)
                . ' --debug-mode ' . $debugModeS
                . ' --run-level 0'
                . ' --use-cores ' . $this->useCores
                . ' --use-fast-grouping ' . $this->useFastGrouping
                . ' --is-voip-reseller ' . $isBillingS;

            $mask = sfConfig::get('app_mask_for_external_telephone_number');

            if (isEmptyOrNull($mask)) {
                $mask = 0;
            }

            $defaultTelephonePrefix = sfConfig::get('app_not_displayed_telephone_prefix');
            if (isEmptyOrNull($defaultTelephonePrefix) || $defaultTelephonePrefix == '-') {
                $defaultTelephonePrefix = '""';
            }

            $currencyPrecision = sfConfig::get('app_currency_decimal_places');
            if ($rateUnbilledCalls) {
                $unbilledCallsS = 'true';
            } else {
                $unbilledCallsS = 'false';
            }

            $cmd .= ' --digits-to-mask ' . $mask
                . ' --organization-to-ignore "' . sfConfig::get('app_organization_to_ignore') . '" '
                . ' --default-telephone-prefix ' . $defaultTelephonePrefix
                . ' --currency-precision ' . $currencyPrecision
                . ' --debug-file ' . $debugFileName
                . ' --from-date "' . $rateFromDateS . '" '
                . ' --rate-unbilled-calls ' . $unbilledCallsS;

            if (is_null($this->testToDate)) {
                $cmd .= ' --test-to-date null';
            } else {
                $cmd .= ' --test-to-date "' . fromUnixTimestampToMySQLTimestamp($this->testToDate) . '"';
            }

            $cmd .= self::DONT_TOUCH_DEBUG_MODE_GHC_PARAMS;

            // Execute the command
            if (JobQueueProcessor::$IS_INTERACTIVE) {
                echo "\nExecuted:\n" . $cmd;
            }

            $output = array();
            $exitStatus = 0;
            $resultLine = exec($cmd, $output, $exitStatus);

            if (JobQueueProcessor::$IS_INTERACTIVE) {
                echo implode("\n", $output);
            }

            //
            // Manage Result
            //

            if ($exitStatus != 0) {

                if (self::isRatingProcessProblemPermanent($conn, true)) {
                    $errorType = ArProblemType::TYPE_CRITICAL;
                    $defaultSolution = "Fix the problem, and rerate again the CDRs. If the problem does not make sense, it can be also a bug of the application. In case contact the assistance, specifying clearly the details of this problem as reported from the application.";
                    $defaultEffect = "These CDRs and all new CDRs will be not rated. This is a critical error not allowing the running of rating engine, so also stats about unrated CDRs are not any more reliable, until the problem is fixed.";
                } else {
                    // there can be temporary glitches with database transactions and so on.
                    $errorType = ArProblemType::TYPE_WARNING;
                    $defaultSolution = "Fix the problem, or wait another passage of the rating processor, because the problem can be also a temporary glitch. If the problem persist, it will be automatically signaled as critical at next execution of the rating engine.";
                    $defaultEffect = "CDRs are not rated. A new attempt will be made, and if the rating fails again, a critical error will be signaled. ";
                }

                if ($exitStatus == 2) {

                    throw(ArProblemException::createWithGarbageCollection(
                        $errorType,
                        ArProblemDomain::VOIP_ACCOUNTS,
                        ArProblemResponsible::ADMIN,
                        'rate calls accounts - ' . md5($cmd),
                        self::GARBAGE_KEY,
                        $exportFromDate,
                        null,
                        "There is an error in configurations,  not allowing the rating of CDRs. " . implode("\n", $output),
                        $defaultEffect,
                        $defaultSolution
                    ));
                } else if ($exitStatus == 3) {

                    throw(ArProblemException::createWithGarbageCollection(
                        $errorType,
                        ArProblemDomain::CONFIGURATIONS,
                        ArProblemResponsible::ADMIN,
                        'rate calls config - ' . md5($cmd),
                        self::GARBAGE_KEY,
                        $exportFromDate,
                        null,
                        "There is an error in configurations of Telephone Prefixes,  not allowing the rating of CDRs. " . implode("\n", $output),
                        $defaultEffect,
                        $defaultSolution
                    ));
                } else if ($exitStatus == 4) {

                    throw(ArProblemException::createWithGarbageCollection(
                        $errorType,
                        ArProblemDomain::CONFIGURATIONS,
                        ArProblemResponsible::ADMIN,
                        'rate calls critical - ' . md5($cmd),
                        self::GARBAGE_KEY,
                        $exportFromDate,
                        null,
                        "There is an error in configurations of Rates,  not allowing the rating of CDRs. " . implode("\n", $output),
                        $defaultEffect,
                        $defaultSolution
                    ));
                } else {
                    throw(ArProblemException::createWithGarbageCollection(
                        $errorType,
                        ArProblemDomain::APPLICATION,
                        null,
                        'rate calls application  - ' . md5($cmd),
                        self::GARBAGE_KEY,
                        $exportFromDate,
                        null,
                        "Error executing command:\n\n$cmd\n\n" . implode("\n", $output),
                        $defaultEffect,
                        $defaultSolution
                    ));
                }

            } else {
                $totCDRS = intval(trim($resultLine));
                $prof->addToProcessedUnits($totCDRS);

                // NOTE: it can execute this outside a transaction, because in worst case scenario, the rerating command will be executed again, without any loss of CDRs, but only a loss of time.
                // NOTE: it can write many of these fields because they are managed only from cron-processor, so they are logically locked

                self::invalidatePendingReports($conn, $exportFromDate, null);

                if ($rateImportedCDRs) {
                    self::resetImportedCDRSTimeFrame($conn);
                }

                if ($rateUnbilledCalls) {
                    self::signalAsDoneRerateCallsFromOfficialCalldate($conn, false);
                }

                if ($rateUserSpecifiedTimeFrame) {
                    self::rerateCalls(null, $conn);
                }

                // Update stats about CDRS and errors
                // NOTE: execute the task here, so it is executed only one time,
                // when we are sure we have rerated the CDRS
                $job = new GroupCDRSWithErrors();
                $job->process();

                // this is the last pass, mandatory, so it is signaled all ok only when it is all for sure ok.
                self::correctEndOfRatingProcess($conn);
                return $prof->stop();
            }
        } else {
            return "Nothing to do.";
        }
    }

    //////////////////////////////////
    // EXPORT RATING CONFIGURATIONS //
    //////////////////////////////////

    /**
     * @param string $fileName
     * @return string the complete path and name of the file.
     */
    public static function getParamsFileCompleteName($fileName)
    {
        return normalizeFileNamePath(ImportDataFiles::getAbsoluteParamsDirectory() . '/' . $fileName);
    }

    /**
     * @param int $fromDate the initial planned rating date
     * @return void
     * @require CDRS before $fromDate are not changed, and so the bundle rate status
     * of a date prior to $fromDate can be considerd safe/stable/updated, until $fromDate excluded.
     */
    public function exportConfigurationFiles($fromDate)
    {
        $this->maybeCreateDirectory(ImportDataFiles::getAbsoluteParamsDirectory());
        // nothing to do, because up to date all settings are read directly
        // from the DB from the rating engine
    }

    /**
     * @return int|null the max call date in CSV files.
     */
    protected
    function getMaxCallDateInSourceCDRS()
    {
        $stm = Propel::getConnection()->prepare('SELECT max(calldate) FROM ar_source_cdr');
        $stm->execute();

        $r = null;
        while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
            $r = fromMySQLTimestampToUnixTimestamp($rs[0]);
        }
        $stm->closeCursor();

        return $r;
    }

    /**
     * @return int the min call date in CSV files.
     */
    protected
    function getMinCallDateInSourceCDRS()
    {
        $stm = Propel::getConnection()->prepare('SELECT max(calldate) FROM ar_source_cdr');
        $stm->execute();

        $r = null;
        while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
            $r = fromMySQLTimestampToUnixTimestamp($rs[0]);
        }
        $stm->closeCursor();
        return $r;
    }

    /**
     * Execute different rerating events and checks if the totals are the same.
     *
     * @param int $maxDaysInThePast
     * @param int $hoursToAdvance
     * @param bool $interactive
     * @return bool true if all tests are ok
     * @ensure if all tests are ok, all calls are rated, so there is no change in the DB
     */
    public function stressRerating($maxDaysInThePast, $hoursToAdvance, $interactive)
    {
        $conn = Propel::getConnection();

        $lock = new JobQueueProcessor();
        if (!$lock->lock()) {
            echo "\n!!! Can not acquire exclusive lock on the rating process. Tests are not performed !!!\n";
            return false;
        }

        try {

            JobQueueProcessor::setIsInteractive($interactive);

            $maxCallDate = $this->getMaxCallDateInSourceCDRS();
            $minCallDate = $maxCallDate;
            $minCallDate = strtotime('-' . $maxDaysInThePast . ' days', $minCallDate);

            $this->deleteFromTable('ar_cdr', $minCallDate, false);
            $this->deleteFromTable('ar_cached_grouped_cdr', $minCallDate, true);
            $this->deleteFromTable('ar_cached_errors', $minCallDate, true);
            $this->deleteFromTable('ar_test_cached_grouped_cdr', $minCallDate, true);
            $this->deleteFromTable('ar_test_cached_errors', $minCallDate, true);

            // Simulate incremental arrival of new CDRS
            $d2 = $minCallDate;
            while ($d2 <= $maxCallDate) {
                // simulate previous pending calls
                $d1 = $d2;
                $d1 = strtotime('-' . rand(0, 4) . ' minutes', $d1);
                if ($d1 < $minCallDate) {
                    $d1 = $minCallDate;
                }

                // rate these new calls
                $d2 = strtotime('+' . $hoursToAdvance . ' hours', $d2);
                $d2 = strtotime('+' . rand(0, 59) . ' minutes', $d2);
                $d2 = strtotime('+' . rand(0, 59) . ' seconds', $d2);

                self::signalAsDoneRerateCallsFromOfficialCalldate($conn, true);
                $this->testToDate = $d2;
                self::rerateCalls($d1);
                JobQueueProcessor::setIsInteractive($interactive);
                $msg = $this->process();
                if ($interactive) {
                    echo "\n$msg";
                }

                $this->calcTestCachedCDRS($minCallDate);
                $r = $this->compareTestCachedCDRS($minCallDate);
                if ($r !== true) {
                    echo "\nError: " . $r;
                    return false;
                }
            }

            echo "\nAll incremental rerating were successfull";

            // Check if incremental rerating is equivalent to rerating from scratch
            echo "\nCheck if incremental rerating is equivalent to rerating from scratch...\n";
            $this->calcTestCachedCDRS($minCallDate);
            self::signalAsDoneRerateCallsFromOfficialCalldate($conn, true);
            $this->testToDate = null;
            self::rerateCalls($minCallDate);
            JobQueueProcessor::setIsInteractive($interactive);
            $msg = $this->process();
            if ($interactive) {
                echo "\n$msg";
            }

            $r = $this->compareTestCachedCDRS($minCallDate);
            if ($r !== true) {
                echo "\nError: " . $r;
                return false;
            }

            // Check if incremental rerating is equivalent to safe rerating from scratch using 1 thread
            // IMPORTANT: leave the complete rerating at the end, so all calls are correctly rated
            echo "\nCheck if incremental rerating is equivalent to rerating from scratch using only 1 thread...\n";
            self::signalAsDoneRerateCallsFromOfficialCalldate($conn, true);
            $this->testToDate = null;
            $this->useCores = 1;
            self::rerateCalls($minCallDate);
            JobQueueProcessor::setIsInteractive($interactive);
            $msg = $this->process();
            $this->useCores = 0;
            if ($interactive) {
                echo "\n$msg";
            }
            $r = $this->compareTestCachedCDRS($minCallDate);
            if ($r !== true) {
                echo "\nError: " . $r;
                return false;
            }

            // Try to execute random rating events inside the time-frame, for simulating different
            // operations respect incremental rating.
            // Execute the same number to test, as the incremental rerating test.

            $secDiff = $maxCallDate - $minCallDate;
            $maxSeconds = $hoursToAdvance * 60 * 60;
            $testsToDo = (int) ($secDiff / $maxSeconds);
            while ($testsToDo > 0) {
                $testsToDo--;
                echo "\n\n## Left tests: $testsToDo\n";

                $secs = rand(10, $secDiff);
                $rateFrom = strtotime('+' . $secs . ' seconds', $minCallDate);
                self::signalAsDoneRerateCallsFromOfficialCalldate($conn, true);
                $this->testToDate = null;
                $this->useCores = 0;
                self::rerateCalls($rateFrom);
                JobQueueProcessor::setIsInteractive($interactive);
                $msg = $this->process();
                if ($interactive) {
                    echo "\n$msg";
                }

                $this->calcTestCachedCDRS($minCallDate);
                $r = $this->compareTestCachedCDRS($minCallDate);
                if ($r !== true) {
                    echo "\nError: " . $r;
                    return false;
                }
            }

            $lock->unlock();
            return true;

        } catch (Exception $e) {
            $lock->unlock();
            echo "\n" . $e->getTraceAsString();
            echo "\n!!! Exception during execution of the code: " . $e->getMessage();
            return false;
        }
    }

    /**
     * @param string $tableName
     * @param int $fromDate
     * @param bool $useWholeDay
     */
    protected function deleteFromTable($tableName, $fromDate, $useWholeDay)
    {
        $conn = Propel::getConnection();
        $query = "DELETE FROM $tableName WHERE calldate >= ?";

        if ($useWholeDay) {
            $fromDateS = fromUnixTimestampToMySQLDate($fromDate);
        } else {
            $fromDateS = fromUnixTimestampToMySQLTimestamp($fromDate);
        }

        $stm = $conn->prepare($query);
        $stm->execute(array($fromDateS));
    }

    /**
     * NOTE: it is important testing these, because in the rating-engine they are calculated
     * using code, and not a query.
     * @param int $fromDate
     */
    protected function calcTestCachedCDRS($fromDate)
    {
        $conn = Propel::getConnection();
        $fromDateS = fromUnixTimestampToMySQLDate($fromDate);

        $this->deleteFromTable('ar_test_cached_grouped_cdr', $fromDate, true);

        // calculate totals grouped by customers
        $query = '
                 INSERT INTO ar_test_cached_grouped_cdr(
                   cached_parent_id_hierarchy
                 , billable_ar_organization_unit_id
                 , `calldate`
                 , `destination_type`
                 , ar_communication_channel_type_id
                 , operator_type
                 , ar_vendor_id
                 , geographic_location
                 ,`count_of_calls`
                 ,`billsec`
                 ,`income`
                 ,`cost_saving`
                 ,`cost`)
                 SELECT
                   cached_parent_id_hierarchy
                 , billable_ar_organization_unit_id
                 , ANY_VALUE(MAKEDATE(YEAR(calldate), DAYOFYEAR(calldate)))
                 , destination_type
                 , ar_communication_channel_type_id
                 , p.operator_type
                 , ar_vendor_id
                 , p.geographic_location
                 , SUM(count_of_calls)
                 , SUM(billsec)
                 , SUM(income)
                 , SUM(cost_saving)
                 , SUM(cost)
                 FROM ar_cdr
                 INNER JOIN ar_telephone_prefix AS p
                 ON ar_cdr.ar_telephone_prefix_id = p.id
                 WHERE ar_cdr.calldate >= ?
                 AND destination_type <> ?
                 AND destination_type <> ?
                 GROUP BY
                   YEAR(calldate)
                 , DAYOFYEAR(calldate)
                 , cached_parent_id_hierarchy
                 , billable_ar_organization_unit_id
                 , destination_type
                 , ar_communication_channel_type_id
                 , p.operator_type
                 , ar_vendor_id
                 , p.geographic_location
                 ;';
        // DEV-NOTE: do not change the order of the `group by` because
        // it uses the ar_cdr index on call-date.

        $stmt = $conn->prepare($query);
        $stmt->execute(array($fromDateS, DestinationType::error, DestinationType::ignored));

        // calculate grand-totals of all customers
        $query = '
                 INSERT INTO ar_test_cached_grouped_cdr(
                   cached_parent_id_hierarchy
                 , billable_ar_organization_unit_id
                 , `calldate`
                 , `destination_type`
                 , ar_communication_channel_type_id
                 , operator_type
                 , ar_vendor_id
                 , geographic_location
                 ,`count_of_calls`
                 ,`billsec`
                 ,`income`
                 ,`cost_saving`
                 ,`cost`)
                 SELECT
                   \'\'
                 , 0
                 , ANY_VALUE(MAKEDATE(YEAR(calldate), DAYOFYEAR(calldate)))
                 , destination_type
                 , ar_communication_channel_type_id
                 , p.operator_type
                 , ar_vendor_id
                 , p.geographic_location
                 , SUM(count_of_calls)
                 , SUM(billsec)
                 , SUM(income)
                 , SUM(cost_saving)
                 , SUM(cost)
                 FROM ar_cdr
                 INNER JOIN ar_telephone_prefix AS p
                 ON ar_cdr.ar_telephone_prefix_id = p.id
                 WHERE ar_cdr.calldate >= ?
                 AND destination_type <> ?
                 AND destination_type <> ?
                 GROUP BY
                   YEAR(calldate)
                 , DAYOFYEAR(calldate)
                 , destination_type
                 , ar_communication_channel_type_id
                 , p.operator_type
                 , ar_vendor_id
                 , p.geographic_location
                 ;';

        $stmt = $conn->prepare($query);
        $stmt->execute(array($fromDateS, DestinationType::error, DestinationType::ignored));

        $this->deleteFromTable('ar_test_cached_errors', $fromDate, true);
        $query = 'INSERT INTO ar_test_cached_errors(
                    `calldate`
                    , `destination_type`
                    , `error_destination_type`
                    ,`count_of_calls`
                    )
                    SELECT
                    ANY_VALUE(MAKEDATE(YEAR(calldate), DAYOFYEAR(calldate)))
                    , destination_type
                    , error_destination_type
                    , SUM(count_of_calls)
                    FROM ar_cdr
                    WHERE ar_cdr.calldate >= ?
                    AND destination_type <> ?
                    GROUP BY
                      YEAR(calldate)
                    , DAYOFYEAR(calldate)
                    , destination_type
                    , error_destination_type
                    ;';

        $stmt = $conn->prepare($query);
        $stmt->execute(array($fromDateS, DestinationType::ignored));
    }

    /**
     * @param string $tableName1
     * @param string $tableName2
     * @param bool $areCDRS false for comparing the grand-totals
     * @param array $fields
     * @param string $fromDateS
     * @return int
     */
    protected function getCountOfComparedTables($tableName1, $tableName2, $areCDRS, $fields, $fromDateS)
    {
        $conn = Propel::getConnection();

        $query = "SELECT COUNT(*) FROM $tableName1 , $tableName2
                  WHERE $tableName1.calldate >= ? AND $tableName2.calldate >= ? ";
        foreach ($fields as $f) {
            $query .= " AND $tableName1.$f = $tableName2.$f";
        }
        $stm = $conn->prepare($query);
        $stm->execute(array($fromDateS, $fromDateS));
        $count1 = 0;
        while ((($rs = $stm->fetch(PDO::FETCH_NUM)) !== false)) {
            $count1 = (int)$rs[0];
        }
        $stm->closeCursor();

        return $count1;
    }

    /**
     * @param string $tableName
     * @param array $fields
     * @param string $fromDateS
     * @param string $fileName
     */
    protected function writeCachedTablesToCSVFile($tableName, $fields, $fromDateS, $fileName)
    {
        unlink($fileName);

        $conn = Propel::getConnection();

        $query = "SELECT ";
        $isFirst = '';
        foreach ($fields as $f) {
            $query .= $isFirst . $f;
            $isFirst = ',';
        }

        $query .= " INTO OUTFILE '$fileName' FROM $tableName WHERE calldate >= ? ORDER BY ";
        $isFirst = '';
        foreach ($fields as $f) {
            $query .= $isFirst . $f;
            $isFirst = ',';
        }

        $stm = $conn->prepare($query);
        $stm->execute(array($fromDateS));
        $stm->closeCursor();
    }

    /**
     * @param string $tableName1
     * @param bool $areCDRS
     * @param string $fromDateS
     * @return int
     */
    protected function getCountOfTable($tableName1, $areCDRS, $fromDateS)
    {
        $conn = Propel::getConnection();

        $query = 'SELECT COUNT(*) FROM ' . $tableName1
            . ' WHERE calldate >= ? ';

        $stm = $conn->prepare($query);
        $stm->execute(array($fromDateS));
        $count1 = 0;
        while ((($rs = $stm->fetch(PDO::FETCH_NUM)) !== false)) {
            $count1 = (int)$rs[0];
        }
        $stm->closeCursor();

        return $count1;
    }

    /**
     * Compare `ar_test_cached_grouped_cdr` with `ar_cached_grouped_cdr`,
     * and `ar_cached_errors` with `ar_test_cached_errors`.
     *
     * NOTE: it is important testing these, because in the rating-engine they are calculated
     * using code, and not a query.
     * @param int $fromDate
     * @return true|string if the tables are equals, the error otherwise
     */
    protected function compareTestCachedCDRS($fromDate)
    {
        $conn = Propel::getConnection();
        $fromDateS = fromUnixTimestampToMySQLDate($fromDate);

        $fields = array(
          'cached_parent_id_hierarchy'
        , 'billable_ar_organization_unit_id'
        , 'calldate'
        , 'destination_type'
        , 'ar_communication_channel_type_id'
        , 'operator_type'
        , 'ar_vendor_id'
        , 'geographic_location'
        , 'count_of_calls'
        , 'billsec'
        , 'income'
        , 'cost_saving'
        , 'cost');

        $count1 = $this->getCountOfComparedTables('ar_test_cached_grouped_cdr', 'ar_cached_grouped_cdr', true, $fields, $fromDateS);
        $count2 = $this->getCountOfTable('ar_test_cached_grouped_cdr', true, $fromDateS);
        if ($count1 !== $count2) {
            $fastCalcs = '/var/tmp/' . getInstanceCodeName() . '_fast_calcs.csv';
            $safeCalcs = '/var/tmp/' . getInstanceCodeName() . '_safe_calcs.csv';
            $this->writeCachedTablesToCSVFile('ar_cached_grouped_cdr', $fields, $fromDateS, $fastCalcs);
            $this->writeCachedTablesToCSVFile('ar_test_cached_grouped_cdr', $fields, $fromDateS, $safeCalcs);
            return ("There are " . ($count2 - $count1) . " differences in cached CDRS.\nThey are written in $fastCalcs and $safeCalcs\n");
        }

        $fields = array(
          'calldate'
        , 'destination_type'
        , 'error_destination_type'
        , 'count_of_calls'
        );

        $count1 = $this->getCountOfComparedTables('ar_test_cached_errors', 'ar_cached_errors', false, $fields, $fromDateS);
        $count2 = $this->getCountOfTable('ar_test_cached_errors', false, $fromDateS);
        if ($count1 !== $count2) {
            $fastCalcs = '/var/tmp/' . getInstanceCodeName() . '_fast_errors.csv';
            $safeCalcs = '/var/tmp/' . getInstanceCodeName() . '_safe_errors.csv';
            $this->writeCachedTablesToCSVFile('ar_cached_errors', $fields, $fromDateS, $fastCalcs);
            $this->writeCachedTablesToCSVFile('ar_test_cached_errors', $fields, $fromDateS, $safeCalcs);
            return ("There are " . ($count2 - $count1) . " differences in cached errors.\nThey are written in $fastCalcs and $safeCalcs\n");
        }

        return true;
    }

///////////////////////
// SUPPORT FUNCTIONS //
///////////////////////

    /**
     * @param string|null $v
     * @param bool $useNullIfEmpty true for using 'NULL' in case of empty string
     * @return mixed
     */
    protected
    function stringSQL($v, $useNullIfEmpty)
    {
        if ($useNullIfEmpty) {
            if (isEmptyOrNull($v)) {
                return null;
            } else {
                return $v;
            }
        } else {
            return $v;
        }

    }

    /**
     * @param bool|null $v
     * @param bool $convertNullToFalse
     * @return string|null 0 for false, 1 for true, NULL for NULL
     */
    protected
    function boolSQL($v, $convertNullToFalse)
    {
        if (is_null($v)) {
            if ($convertNullToFalse) {
                return '0';
            } else {
                return null;
            }
        } else if ($v) {
            return '1';
        } else {
            return '0';
        }
    }

    /**
     * @param int|null $v
     * @param bool $isRequired
     * @return int|null
     */
    protected
    function intSQL($v, $isRequired)
    {
        if (is_null($v)) {
            if ($isRequired) {
                return 0;
            } else {
                return null;
            }
        } else {
            return $v;
        }
    }
}
