<?php

/* $LICENSE 2013, 2014, 2015, 2017:
 *
 * Copyright (C) 2013, 2014, 2015, 2017 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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
 * Rate the calls.
 * First CDRS are imported from `ImportDataFiles` into `ar_source_cdr` table, and the rerating fields are completed.
 * Then this job rate them, according the specified rerating time frame.
 * The rate engine delete all CDRs between MIN and MAX calldate, and import all CDRs within this date.
 *
 * Export rate configurations to the external rate engine, using textual files.
 * NOTE: MySQL driver license does not allow commercial licensed code, but only GPL code,
 * so PHP export the data using CSV files. PHP applications can instead connect to MySQL
 * without being forced to be GPL.
 * NOTE: MariaDB uses now a LGPL or BSD driver, so it can be used in the future.
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

    const GARBAGE_KEY = 'rate calls event';

    const DEBUG_INFO_REPORT_INTERNAL_NAME = 'rating_debug_info';

    const RATE_ENGINE_CHANGED_DAYS_JOB_NAME = 'external_haskell_rating_engine';


    // IMPORTANT: do not change nothing, because this value
    // will be updated by the management procedure in case
    // of debug mode.
    const DONT_TOUCH_DEBUG_MODE_GHC_PARAMS = " ";

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
     * @param int $toDate
     */
    static public function invalidatePendingReports($conn, $fromDate, $toDate) {

        $rateFromDate = fromUnixTimestampToMySQLTimestamp($fromDate);
        $rateToDate = fromUnixTimestampToMySQLTimestamp($toDate);

        $conn->beginTransaction();

        // A report is affected if its start or its end is inside the rerating time frame.

        $query = 'UPDATE ar_report
                  SET    produced_report_must_be_regenerated = 1
                  WHERE  produced_report_already_reviewed = 0
                  AND    (   (? <= from_date AND from_date < ?)
                          OR (? < to_date AND to_date <= ?))';

        $stm = $conn->prepare($query);
        $stm->execute(array($rateFromDate, $rateToDate, $rateFromDate, $rateToDate));

        self::commitTransactionOrSignalProblem_static($conn, 'invalidate pending reports');
    }

    public function process()
    {

        $prof = new JobProfiler('rated CDRs');
        $conn = Propel::getConnection();

        // Exit if rating event must be post-poned.

        if (self::getWaitForScheduledRerate($conn)) {
            // Signal that at the next execution, it can start rating.
            self::setWaitForScheduledRerate(false, $conn);
            return "Rating event will be started at next cron job processor run, if there aren't meantime new changes in rating settings.";
        }

        //
        // Calculate the rating time frame
        //
        // Usually we have
        // case A) rate only new imported CDRS
        // case B) rate only/also unbilled calls
        // case C) rate only/also user specified time frame
        //
        // Try to create the bigger time-frame containing all these events.
        // If it is not possible, rate every different time-frame, at different execution runs, favoring:
        // 1) user specified time time-frame
        // 2) unbilled calls
        // 3) new imported CDRS
        //
        // This order is important because new imported CDRs can postpone forever all rating events,
        // if they have greather priority.
        //

        $globalStartingDate = $this->getGlobalStartingDate();

        $somethingToDo = false;
        $rateImportedCDRs = false;
        $rateUnbilledCalls = false;
        $rateUserSpecifiedTimeFrame = false;
        $eventFromDate = null;
        $eventToDate = null;
        $unbilledCallsFrom = null;

        list($newCDRSFromDate, $newCDRSToDate) = self::getNewImportedCdrsCallDate($conn);
        list($serviceCDRSFromDate, $serviceCDRSToDate) = self::getScheduledImportedServicesRatingEvent($conn);
        list($userRateFromDate, $userRateToDate) = self::getRerateTimeFrame($conn);
        if (self::getRerateFromOfficialCallDate($conn)) {
            $unbilledCallsFrom = self::getOfficialCallDate($conn);
        }

        // Decide which rate events activate

        if (!is_null($newCDRSFromDate)) {
            $rateImportedCDRs = true;
            $somethingToDo = true;
            $eventFromDate = $newCDRSFromDate;
            $eventToDate = $newCDRSToDate;
        }

        if (!is_null($serviceCDRSFromDate)) {
            $somethingToDo = true;
            $rateImportedCDRs = true;
            // NOTE: service CDRs do not involve a complete rerating of all other CDRs,
            // so their time-frame is always accepted.
        }

        if (!is_null($unbilledCallsFrom)) {
            $somethingToDo = true;
            if (is_null($eventFromDate)) {
                $rateUnbilledCalls = true;
                $eventFromDate = $unbilledCallsFrom;
                $eventToDate = null;
            } else {
                if (self::isTimeFrameJoinableWith($unbilledCallsFrom, null, $eventFromDate, $eventToDate)) {
                    $rateUnbilledCalls = true;
                    $eventFromDate = self::getMinFromDate($unbilledCallsFrom, $eventFromDate);
                    $eventToDate = null;
                } else {
                    // Higher priority
                    $rateUnbilledCalls = true;
                    $rateImportedCDRs = false;
                    $eventFromDate = $unbilledCallsFrom;
                    $eventToDate = null;
                }
            }
        }

        if (!is_null($userRateFromDate)) {
            $somethingToDo = true;
            if (is_null($eventFromDate)) {
                $rateUserSpecifiedTimeFrame = true;
                $eventFromDate = $userRateFromDate;
                $eventToDate = $userRateToDate;
            } else {
                if (self::isTimeFrameJoinableWith($userRateFromDate, $userRateToDate, $eventFromDate, $eventToDate)) {
                    $rateUserSpecifiedTimeFrame = true;
                    $eventFromDate = self::getMinFromDate($eventFromDate, $userRateFromDate);
                    $eventToDate = self::getMaxToDate($eventToDate, $userRateToDate);
                } else {
                  // highest priority
                  $rateUnbilledCalls = false;
                  $rateImportedCDRs = false;
                  $rateUserSpecifiedTimeFrame = true;
                  $eventFromDate = $userRateFromDate;
                  $eventToDate = $userRateToDate;
                }
            }
        }

        if ($somethingToDo) {
            // Sanitize call dates

            if (is_null($eventFromDate)) {
                $eventFromDateS = 'null';
                $eventToDateS = 'null';
            } else {
                if (is_null($eventToDate)) {
                    // If there is no specified end date, consider the maximum between now and the last CDR in the database.
                    // In this way all the CDRs are rated, and all the services are generated, also if:
                    // * case A) there are CDRs in the future
                    // * case B) there are no many CDRs until current date

                    $caseA = $this->getMaxCallDateInSourceCDRS();
                    $caseB = time();

                    if (!is_null($caseA)) {
                        if ($caseA > $caseB) {
                            // the call date must be exclusive, so consider a little bigger interval, for including
                            // all the needed values. It is not a problem if some more CDR in the future is rated.
                            $eventToDate = strtotime('+1 second', $caseA);
                        } else {
                            $eventToDate = $caseB;
                        }
                    }
                }
                if ($eventFromDate < $globalStartingDate) {
                    $eventFromDate = $globalStartingDate;
                }

                assert(!is_null($eventToDate));
                if ($eventToDate < $globalStartingDate) {
                    $eventToDate = $globalStartingDate;
                }

                $eventFromDateS = fromUnixTimestampToMySQLTimestamp($eventFromDate);
                $eventToDateS = fromUnixTimestampToMySQLTimestamp($eventToDate);
            }

            if (is_null($serviceCDRSFromDate)) {
                $eventServiceFromDateS = 'null';
                $eventServiceToDateS = 'null';
            } else {
                if ($serviceCDRSFromDate < $globalStartingDate) {
                    $serviceCDRSFromDate = $globalStartingDate;
                }

                if ($serviceCDRSToDate < $globalStartingDate) {
                    $serviceCDRSToDate = $globalStartingDate;
                }

                $eventServiceFromDateS = fromUnixTimestampToMySQLTimestamp($serviceCDRSFromDate);
                $eventServiceToDateS = fromUnixTimestampToMySQLTimestamp($serviceCDRSToDate);
            }

            //
            // Execute Rating Event
            //

            // Export the rating info

            self::startRatingProcess($conn);
            ArProblemException::garbageCollect(self::GARBAGE_KEY, $eventFromDate, $eventToDate);

            $exportFromDate = $eventFromDate;
            if (is_null($exportFromDate) || (!is_null($serviceCDRSFromDate) && $serviceCDRSFromDate < $exportFromDate)) {
                $exportFromDate = $serviceCDRSFromDate;
            }

            self::assertCondition(!is_null($exportFromDate), "Rate without an associated time-frame from which load rating params.");
            $this->exportConfigurationFiles($exportFromDate);

            // Start with an empty state about this
            $conn->exec('TRUNCATE ar_expanded_extensions');

            // Call the external rating engine.

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

            $cmd = RateEngineService::getToolExecutable()
                . ' --rate '
                . ' --debug-mode ' . $debugModeS
                . ' --run-level 0'
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

            $cmd .= ' --digits-to-mask ' . $mask
                . ' --default-telephone-prefix ' . $defaultTelephonePrefix
                . ' --currency-precision ' . $currencyPrecision
                . ' --load-extensions ' . self::getParamsFileCompleteName(ManageRateEvent::EXTENSIONS_FILE_NAME)
                . ' --debug-file ' . $debugFileName
                . ' --from-date "' . $eventFromDateS . '" '
                . ' --to-date "' . $eventToDateS . '" '
                . ' --from-date "' . $eventServiceFromDateS . '" '
                . ' --to-date "' . $eventServiceToDateS . '" ';

            if ($rateUnbilledCalls) {
                $unbilledCallsS = 'true';
            } else {
                $unbilledCallsS = 'false';
            }
            $cmd .= ' --rate-unbilled-calls ' . $unbilledCallsS;

            list($dbName, $dbUser, $dbPassword) = getDatabaseNameUserAndPassword(true);
            $cmd .= " --db-name $dbName --db-user $dbUser --db-password $dbPassword";

            $cmd .= self::DONT_TOUCH_DEBUG_MODE_GHC_PARAMS;

            // Execute the command
            if (JobQueueProcessor::$IS_INTERACTIVE) {
                echo "\nExecuted:\n" . $cmd;
            }

            $output = array();
            $exitStatus = 0;
            $resultLine = exec($cmd, $output, $exitStatus);

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
                        $eventFromDate,
                        $eventToDate,
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
                        $eventFromDate,
                        $eventToDate,
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
                        $eventFromDate,
                        $eventToDate,
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
                        $eventFromDate,
                        $eventToDate,
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

                self::invalidatePendingReports($conn, $eventFromDate, $eventToDate);

                if ($rateImportedCDRs) {
                    self::resetImportedCDRSTimeFrame($conn);
                }

                if ($rateUnbilledCalls) {
                    self::signalAsDoneRerateCallsFromOfficialCalldate($conn, false);
                }

                if ($rateUserSpecifiedTimeFrame) {
                    self::rerateCalls(null, null, $conn);
                }

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
        $this->exportOrganizationAndExtensions();
    }

    public function exportOrganizationAndExtensions()
    {
        $garbageKey = self::GARBAGE_KEY;


        $resultFileName = self::getParamsFileCompleteName(self::EXTENSIONS_FILE_NAME);
        $outFileHandle = fopen($resultFileName, 'w');

        if ($outFileHandle === FALSE) {
            $problemDuplicationKey = "Can not open - " . $resultFileName;
            $problemDescription = "The file \"" . $resultFileName . "\" can not be open for writing data on it.";
            $problemEffect = "CDRs of the current rating event can not be rated.";
            $problemProposedSolution = "Try to force a rerate again. If the problem persist contact the assistance.";
            $p = ArProblemException::createWithGarbageCollection(
                ArProblemType::TYPE_ERROR,
                ArProblemDomain::APPLICATION,
                null,
                $problemDuplicationKey,
                $garbageKey,
                null,
                null,
                $problemDescription,
                $problemEffect,
                $problemProposedSolution);
            throw ($p);
        }

        /**
         * @var PDOStatement $stm
         */
        $stm = Propel::getConnection()->prepare('
        SELECT ar_organization_unit.id
        , ar_organization_unit_type.id
        , ar_organization_unit_type.name
        , ar_organization_unit_type.short_code
        , ar_organization_unit_has_structure.id
        , ar_organization_unit_has_structure.ar_parent_organization_unit_id
        , ar_organization_unit_has_structure.from
        , ar_organization_unit_has_structure.exists
        , ar_organization_unit_has_structure.ar_rate_category_id
        , ar_organization_unit_has_structure.ar_party_id
        , ar_organization_unit_has_structure.extension_codes
        , ar_organization_unit_has_structure.extension_name
        , ar_organization_unit_has_structure.extension_user_code
        , ar_rate_category.short_description
        , ar_rate_category.internal_name
        , ar_party.name
        , ar_party.compact_name
        , ar_party.is_billable
        , ar_party.is_active
        , ar_party.ar_reseller_id
        FROM ar_organization_unit_has_structure
        LEFT JOIN ar_organization_unit ON ar_organization_unit_has_structure.ar_organization_unit_id = ar_organization_unit.id
        LEFT JOIN ar_organization_unit_type ON ar_organization_unit_has_structure.ar_organization_unit_type_id = ar_organization_unit_type.id
        LEFT JOIN ar_rate_category ON ar_organization_unit_has_structure.ar_rate_category_id = ar_rate_category.id
        LEFT JOIN ar_party ON ar_organization_unit_has_structure.ar_party_id = ar_party.id
        ORDER BY ar_organization_unit_has_structure.from
        ');
        $stm->execute();

        while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
            // write data to file

            $i = 0;
            $data = array();
            $data[] = $this->intSQL($rs[$i++], false);
            $data[] = $this->intSQL($rs[$i++], false);
            $data[] = $this->stringSQL($rs[$i++], false);
            $data[] = $this->stringSQL($rs[$i++], false);
            $data[] = $this->intSQL($rs[$i++], false);
            $data[] = $this->intSQL($rs[$i++], false);
            $data[] = $this->stringSQL($rs[$i++], true);
            $data[] = $this->boolSQL($rs[$i++], true);
            $data[] = $this->intSQL($rs[$i++], false);
            $data[] = $this->intSQL($rs[$i++], false);
            $data[] = $this->stringSQL($rs[$i++], false);
            $data[] = $this->stringSQL($rs[$i++], false);
            $data[] = $this->stringSQL($rs[$i++], false);
            $data[] = $this->stringSQL($rs[$i++], false);
            $data[] = $this->stringSQL($rs[$i++], false);
            $data[] = $this->stringSQL($rs[$i++], false);
            $data[] = $this->stringSQL($rs[$i++], false);
            $data[] = $this->boolSQL($rs[$i++], true);
            $data[] = $this->boolSQL($rs[$i++], true);
            $data[] = $this->intSQL($rs[$i++], false);

            $this->assertCondition($i == 20, "Unexpected number of records: $i");

            $isOk = safe_fputcsv($outFileHandle, $data);
            if ($isOk === FALSE) {
                $problemDuplicationKey = "Can not write file - $resultFileName";
                $problemDescription = "The file \"" . $resultFileName . "\" can not be written correctly.";
                $problemEffect = "CDRs of the current rating event can not be rated.";
                $problemProposedSolution = "Try to force a rerate again. If the problem persist contact the assistance.";
                $p = ArProblemException::createWithGarbageCollection(
                    ArProblemType::TYPE_ERROR,
                    ArProblemDomain::APPLICATION,
                    null,
                    $problemDuplicationKey,
                    $garbageKey,
                    null,
                    null,
                    $problemDescription,
                    $problemEffect,
                    $problemProposedSolution);
                throw ($p);
            }
        }

        $isOk = fclose($outFileHandle);
        if ($isOk === FALSE) {
            $problemDuplicationKey = "Can not close file - $resultFileName";
            $problemDescription = "The file \"" . $resultFileName . "\" can not be closed correctly.";
            $problemEffect = "CDRs of the current rating event can not be rated.";
            $problemProposedSolution = "Try to force a rerate again. If the problem persist contact the assistance.";
            $p = ArProblemException::createWithGarbageCollection(
                ArProblemType::TYPE_ERROR,
                ArProblemDomain::APPLICATION,
                null,
                $problemDuplicationKey,
                $garbageKey,
                null,
                null,
                $problemDescription,
                $problemEffect,
                $problemProposedSolution);
            throw ($p);
        }

        $stm->closeCursor();

        return '';
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
     * @param int $times
     * @param bool $interactive
     * @return bool true if all tests are ok
     */
    public function stressRerating($maxDaysInThePast, $times, $interactive)
    {
        $minCallDate = strtotime('-' . $maxDaysInThePast . ' days');

        // First rerate all the calls for starting with a sane configuration
        self::rerateCalls($minCallDate, null);
        JobQueueProcessor::setIsInteractive($interactive);
        $msg = $this->process();
        if ($interactive) {
            echo "\n\nInitial complete rerating: $msg";
        }

        $cdrStats1 = $this->getCDRStats($minCallDate);

        // Now try many rerates
        while ($times > 0) {
            $isOpenInterval = rand(0, 1);
            $d1 = strtotime('-' . rand(0, $maxDaysInThePast) . ' days');
            if ($isOpenInterval == 1) {
                $d2 = null;
            } else {
                $d2 = strtotime('-' . rand(0, $maxDaysInThePast) . ' days');
                if ($d1 > $d2) {
                    $t = $d2;
                    $d2 = $d1;
                    $d1 = $t;
                }
            }
            self::rerateCalls($d1, $d2);

            $rateAlsoServiceCDRS = rand(0, 1);
            if ($rateAlsoServiceCDRS == 1) {
                $sd1 = strtotime('-' . rand(0, $maxDaysInThePast) . ' days');
                $sd2 = strtotime('-' . rand(0, $maxDaysInThePast) . ' days');

                if ($sd1 > $sd2) {
                    $t = $sd2;
                    $sd2 = $sd1;
                    $sd1 = $t;
                }
                self::rerateImportedServicesCalls($sd1, $sd2);
            } else {
                $sd1 = null;
                $sd2 = null;
            }

            JobQueueProcessor::setIsInteractive($interactive);
            $msg = $this->process();
            if ($interactive) {
              echo "\nRerating $times: $msg";
            }

            $cdrStats2 = $this->getCDRStats($minCallDate);
            // NOTE: calc all CDRS, not only the CDRS in the specified rating time frame

            if (! $this->compareCDRStatsAndShowDifferences($cdrStats1, $cdrStats2, false, $interactive)) {
                return false;
            }
            $times--;
        }

        return true;
    }

    /**
     * @param int $minCallDate
     * @return array with date, parent-id-hierachy and destination type as key string,
     * and a string with the other fields as value.
     */
    public function getCDRStats($minCallDate)
    {
        $conn = Propel::getConnection();

        // NOTE: I'm returning a date, without the time part.
        $minCallDateS = fromUnixTimestampToMySQLDate($minCallDate);

        $query = 'SELECT MAKEDATE(YEAR(calldate), DAYOFYEAR(calldate)),
                         cached_parent_id_hierarchy,
                         destination_type,
                         SUM(count_of_calls),
                         SUM(billsec),
                         SUM(income),
                         SUM(cost_saving),
                         SUM(cost)
                  FROM ar_cdr
                  WHERE ar_cdr.calldate >= ?
                  AND destination_type <> ?
                  GROUP BY YEAR(calldate), DAYOFYEAR(calldate), cached_parent_id_hierarchy, destination_type
                  ORDER BY YEAR(calldate), DAYOFYEAR(calldate), cached_parent_id_hierarchy, destination_type';

        $stm = $conn->prepare($query);
        $stm->execute(array($minCallDateS, DestinationType::ignored));
        // NOTE: ignored calls can be produced by the system for different reasons, so we don't compare them.
        // NOTE: errors, internal and system calls are important and they should be remain constant after each rating pass.

        $r = array();
        while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
            $key = $rs[0] . ',' . $rs[1] . ',' . $rs[2];
            $value = $rs[3] . ',' . $rs[4] . ',' . $rs[5] . ',' .$rs[6] . ',' .$rs[7];
            $r[$key] = $value;
        }
        $stm->closeCursor();
        return $r;
    }

    /**
     * Show the differences in case
     * @param array $stats1 the initial stats returned from `getCDRStats()`
     * @param array $stats2 the last stats after the rerating
     * @param bool $areReversed true if the first array is instead the array with the final stats
     * @param bool $isInteractive true if it must show the differences
     * @return bool true if the content is the same, false otherwise
     *
     */
    public function compareCDRStatsAndShowDifferences(&$stats1, &$stats2, $areReversed, $isInteractive) {
        $count1 = count($stats1);
        $count2 = count($stats2);

        if ($count1 < $count2) {
            return $this->compareCDRStatsAndShowDifferences($stats2, $stats1, ! $areReversed, $isInteractive);
        }

        $header1 = 'original stats';
        $header2 = 'stats after rerating';
        if ($areReversed) {
            $t = $header2;
            $header2 = $header1;
            $header1 = $t;
        }

        $r = true;
        foreach($stats1 as $key => $value1) {
             $errorMsg = null;
             if (array_key_exists($key, $stats2)) {
                $value2 = $stats2[$key];
                if ($value2 !== $value1) {
                    $r = false;
                    if ($isInteractive) {
                        $errorMsg = $value2;
                    }
                }
            } else {
                $r = false;

                if ($isInteractive) {
                    $errorMsg = '<nothing>';
                }
            }

            if (!is_null($errorMsg)) {
                echo "\ndate, parent-hierachy, destination-type\n$key\nthe $header1 has\ncount_of_calls, billsec, sum_of_income, sum_of_cost_saving, sum_of_cost\n$value1\nwhile the $header2\n$errorMsg";
            }
        }

        return $r;
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
