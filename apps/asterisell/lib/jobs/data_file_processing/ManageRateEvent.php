<?php

/* $LICENSE 2013, 2014, 2015:
 *
 * Copyright (C) 2013, 2014, 2015 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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
    const TELEPHONE_PREFIXES_FILE_NAME = 'telephone_prefixes.csv';
    const VENDORS_FILE_NAME = 'vendors.csv';
    const EXTENSIONS_FILE_NAME = 'extensions.csv';
    const CHANNEL_TYPES_FILE_NAME = 'channel_types.csv';
    const CHANNEL_DOMAINS = 'channel_domains.csv';
    const SERVICE_FILE_NAME = 'services.csv';
    const SERVICE_PRICE_LIST_FILE_NAME = 'services_price_list.csv';
    const ASSIGNED_SERVICE_FILE_NAME = 'assigned_services.csv';

    const RATE_PLAN_FILE_NAME = 'rate_plan.csv';

    const RATE_CATEGORY_FILE_NAME = 'rate_category.csv';

    // NOTE: this file is generated from ManageRateEvent job
    const FILE_TO_RATE_LIST = 'files_to_rate.csv';

    /**
     * The base name, then for each id_XX is generated a specific file.
     * The default suffix is '.rate'
     */
    const RATE_PLAN_ID_FILE_NAME = 'rate_plan_id_';

    const RATE_DEBUG_INFO = 'rating_debug.txt';

    const RATED_CDRS_FILE_NAME = 'rated_cdrs.csv';

    const COMPILED_TELEPHONE_PREFIXES = 'compiled_telephone_prefixes.csv';

    const RATE_FILE_NAME = 'rate.txt';

    const RATING_ERRORS_FILE_NAME = 'rating_errors.info';

    const RATE_DEBUG_FILE_NAME = 'rate_debug.info';

    const IMPORT_DEBUG_FILE_NAME = 'import_debug.info';

    const SIGNAL_FILE_NAME = 'signal.lock';

    const GARBAGE_KEY = 'rate calls event';

    const DEBUG_INFO_REPORT_INTERNAL_NAME = 'rating_debug_info';

    const RATE_ENGINE_CHANGED_DAYS_JOB_NAME = 'external_haskell_rating_engine';

    /**
//     * @return int|null
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

        $tf = fromUnixTimestampToMySQLTimestamp($fromDate);
        $tt = fromUnixTimestampToMySQLTimestamp($toDate);

        $conn->beginTransaction();

        // a report is affected if it is not one of this condition
        // > <rf----rt> <tf-----tt> <rf----rt>

        $query = 'UPDATE ar_report
                  SET    produced_report_must_be_regenerated = 1
                  WHERE  produced_report_already_reviewed = 0
                  AND    (to_date < ? OR ? < from_date)';

        $stm = $conn->prepare($query);
        $stm->execute(array($tf, $tt));

        self::commitTransactionOrSignalProblem($conn, 'invalidate pending reports');
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

            // Call the external rating engine.

            if ($this->getDebugMode()) {
                $debugModeS = '1';
            } else {
                $debugModeS = '0';
            }

            $isVoIpReseller = sfConfig::get('app_is_voip_reseller');
            if ($isVoIpReseller) {
                $isVoIpResellerS = '1';
            } else {
                $isVoIpResellerS = '0';
            }
            $debugFileName = normalizeFileNamePath(ImportDataFiles::getMySQLAccessibleTmpDirectory(self::GARBAGE_KEY) . '/' . self::RATE_DEBUG_FILE_NAME);
            @unlink($debugFileName);

            $cmd = RateEngineService::getToolExecutable()
                . ' --rate '
                . ' --debug-mode ' . $debugModeS
                . ' --is-voip-reseller ' . $isVoIpResellerS
                . ' --load-rate-categories ' . self::getParamsFileCompleteName(ManageRateEvent::RATE_CATEGORY_FILE_NAME)
                . ' --load-vendors ' . self::getParamsFileCompleteName(ManageRateEvent::VENDORS_FILE_NAME)
                . ' --load-channels-types ' . self::getParamsFileCompleteName(ManageRateEvent::CHANNEL_TYPES_FILE_NAME)
                . ' --load-channel-domains ' . self::getParamsFileCompleteName(ManageRateEvent::CHANNEL_DOMAINS)
                . ' --load-telephone-prefixes ' . self::getParamsFileCompleteName(ManageRateEvent::TELEPHONE_PREFIXES_FILE_NAME);

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
                . ' --load-rate-plan-changes ' . self::getParamsFileCompleteName(ManageRateEvent::RATE_PLAN_FILE_NAME)
                . ' --load-rate-plan ' . self::getParamsFileCompleteName(ManageRateEvent::RATE_PLAN_ID_FILE_NAME)
                . ' --load-services ' . self::getParamsFileCompleteName(ManageRateEvent::SERVICE_FILE_NAME)
                . ' --load-service-price-list ' . self::getParamsFileCompleteName(ManageRateEvent::SERVICE_PRICE_LIST_FILE_NAME)
                . ' --load-assigned-services ' . self::getParamsFileCompleteName(ManageRateEvent::ASSIGNED_SERVICE_FILE_NAME)
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

    /**
     * @return bool true if the operation were executed with success
     */
    static public function updateAllCachedGroupedCDRS()
    {

        // Call the external rating engine.
        list($dbName, $dbUser, $dbPassword) = getDatabaseNameUserAndPassword(true);

        $cmd = RateEngineService::getToolExecutable()
            . ' --update-cached-grouped-cdrs '
            . " --db-name $dbName --db-user $dbUser --db-password $dbPassword";

        // Execute the command
        if (JobQueueProcessor::$IS_INTERACTIVE) {
            echo "\nExecuted:\n" . $cmd;
        }

        $output = array();
        $exitStatus = 0;
        exec($cmd, $output, $exitStatus);

        if ($exitStatus != 0) {
            return false;
        } else {
            return true;
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

        $this->exportRateCategories();
        $this->exportTelephonePrefixes();
        $this->exportOrganizationAndExtensions();
        $this->exportVendorDomains();
        $this->exportServices($fromDate);
        $this->exportRates();
        $exportTelephonePrefixes = $this->compileRates();
        $exportTelephonePrefixes = $this->compileServices() || $exportTelephonePrefixes;
        if ($exportTelephonePrefixes) {
            $this->exportTelephonePrefixes();
        }

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

    public function exportTelephonePrefixes()
    {
        //
        // Export telephone prefixes
        //

        $garbageKey = self::GARBAGE_KEY;
        $resultFileName = self::getParamsFileCompleteName(self::TELEPHONE_PREFIXES_FILE_NAME);
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
        $stm = Propel::getConnection()->prepare('SELECT * FROM ar_telephone_prefix');
        $stm->execute();

        while (($rs = $stm->fetch(PDO::FETCH_ASSOC)) !== false) {

            // convert to prefixes containing ? and *

            $prefix = $rs['prefix'];
            $matchExactlyNDigits = $rs['match_only_numbers_with_n_digits'];

            if (is_null($matchExactlyNDigits) || $matchExactlyNDigits == 0) {
                $convertedPrefix = $prefix . 'X*';
            } else {
                $convertedPrefix = $prefix;
                for ($i = strlen($prefix); $i < $matchExactlyNDigits; $i++) {
                    $convertedPrefix .= 'X';
                }
            }

            // write data to file

            $data = array();
            $data[] = $rs['id'];
            $data[] = $convertedPrefix;
            $data[] = $this->stringSQL($rs['geographic_location'], false);
            $data[] = $this->stringSQL($rs['operator_type'], false);

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


    public function exportRateCategories()
    {
        //
        // Export rate categories
        //

        $garbageKey = self::GARBAGE_KEY;

        $resultFileName = self::getParamsFileCompleteName(self::RATE_CATEGORY_FILE_NAME);
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
        $stm = Propel::getConnection()->prepare('SELECT * FROM ar_rate_category');
        $stm->execute();

        while (($rs = $stm->fetch(PDO::FETCH_ASSOC)) !== false) {
            // write data to file

            $data = array();
            $data[] = $rs['id'];
            $data[] = $rs['short_description'];
            $data[] = $rs['internal_name'];

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

    public function exportVendorDomains()
    {
        $garbageKey = self::GARBAGE_KEY;

        //
        // Export Channel Types
        //

        $resultFileName = self::getParamsFileCompleteName(self::CHANNEL_TYPES_FILE_NAME);
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
        $stm = Propel::getConnection()->prepare('SELECT * FROM ar_communication_channel_type');
        $stm->execute();

        while (($rs = $stm->fetch(PDO::FETCH_ASSOC)) !== false) {

            $data = array();
            $data[] = $rs['id'];
            $data[] = $this->stringSQL($rs['internal_name'], true);

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


        //
        // Export Vendors
        //

        $resultFileName = self::getParamsFileCompleteName(self::VENDORS_FILE_NAME);
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
        $stm = Propel::getConnection()->prepare('SELECT * FROM ar_vendor');
        $stm->execute();

        while (($rs = $stm->fetch(PDO::FETCH_ASSOC)) !== false) {

            $data = array();
            $data[] = $rs['id'];
            $data[] = $this->stringSQL($rs['internal_name'], true);

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

        //
        // Export Vendor Domains
        //

        $resultFileName = self::getParamsFileCompleteName(self::CHANNEL_DOMAINS);
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
        $stm = Propel::getConnection()->prepare('SELECT * FROM ar_vendor_domain');
        $stm->execute();

        while (($rs = $stm->fetch(PDO::FETCH_ASSOC)) !== false) {

            $data = array();
            $data[] = $rs['id'];
            $data[] = $this->stringSQL($rs['internal_name'], true);
            $data[] = $this->intSQL($rs['ar_vendor_id'], false);
            $data[] = $this->intSQL($rs['ar_communication_channel_type_id'], false);
            $data[] = $this->stringSQL($rs['domain'], true);
            $data[] = $this->boolSQL($rs['is_prefix'], true);
            $data[] = $this->boolSQL($rs['is_suffix'], true);
            $data[] = $this->stringSQL($rs['from'], true);
            $data[] = $this->stringSQL($rs['to'], true);

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
     * @param int $fromDate
     * @return string
     * @throws ArProblemException
     */
    public function exportServices($fromDate)
    {
        $garbageKey = self::GARBAGE_KEY;

        //
        // Export Services
        //

        $resultFileName = self::getParamsFileCompleteName(self::SERVICE_FILE_NAME);
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
        $stm = Propel::getConnection()->prepare('SELECT * FROM ar_service');
        $stm->execute();

        while (($rs = $stm->fetch(PDO::FETCH_ASSOC)) !== false) {

            $data = array();
            $data[] = $rs['id'];
            $data[] = $this->stringSQL($rs['customer_name'], false);
            $data[] = $this->stringSQL($rs['customer_description'], false);
            $data[] = $this->boolSQL($rs['customer_price_depend_from_activation_date'], true);
            $data[] = $this->boolSQL($rs['customer_price_change_with_price_list'], true);
            $data[] = $this->boolSQL($rs['is_applied_only_one_time'], true);
            $data[] = $this->stringSQL($rs['schedule_timeframe'], false);
            $data[] = $this->stringSQL($rs['schedule_from'], false);

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

        //
        // Export Price Lists
        //

        $resultFileName = self::getParamsFileCompleteName(self::SERVICE_PRICE_LIST_FILE_NAME);
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
        $stm = Propel::getConnection()->prepare('SELECT * FROM ar_service_price ORDER BY from_date DESC');
        $stm->execute();

        while (($rs = $stm->fetch(PDO::FETCH_ASSOC)) !== false) {

            $data = array();
            $data[] = $rs['id'];
            $data[] = $this->intSQL($rs['ar_service_id'], false);
            $data[] = $this->stringSQL($rs['from_date'], true);
            $data[] = $this->intSQL($rs['price'], true);

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

        //
        // Export Assignment
        //

        $resultFileName = self::getParamsFileCompleteName(self::ASSIGNED_SERVICE_FILE_NAME);
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
        $stm = Propel::getConnection()->prepare('SELECT * FROM ar_assigned_service ORDER BY from_date DESC, nr_of_items DESC');
        $stm->execute();
        // DEV-NOTE: order also on nr_of_items for using in the new time frame the new configured items, instead of 0 items, in case the items are set to 0 and then to a new number.

        while (($rs = $stm->fetch(PDO::FETCH_ASSOC)) !== false) {

            $data = array();
            $data[] = $rs['id'];
            $data[] = $this->intSQL($rs['ar_service_id'], false);
            $data[] = $this->intSQL($rs['ar_organization_unit_id'], false);
            $data[] = $this->intSQL($rs['nr_of_items'], true);
            $data[] = $this->stringSQL($rs['from_date'], true);
            $data[] = $this->intSQL($rs['discount'], true);

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
     * @return bool true if at least one new rate was imported
     * @throws ArProblemException
     */
    protected function compileRates()
    {
        $oneExportedRate = false;

        $c = new Criteria();
        $c->add(ArRatePeer::WAS_COMPILED, false);

        // NOTE: usually there are few rates of this type, so it is safe caching them
        $rates = ArRatePeer::doSelect($c);
        foreach ($rates as $rate) {
            /**
             * @var ArRate $rate
             */

            if ($rate->getInternalName() == ArRatePeer::MAIN_COST_RATE
                || $rate->getInternalName() == ArRatePeer::MAIN_INCOME_RATE
            ) {

                try {
                    // MySQL requires that the file is in tmp directory.
                    $resultDirectory = ImportDataFiles::getMySQLAccessibleTmpDirectory(self::GARBAGE_KEY);

                    $resultFileName = normalizeFileNamePath($resultDirectory . '/' . self::COMPILED_TELEPHONE_PREFIXES);
                    @unlink($resultFileName);

                    $rateFileName = normalizeFileNamePath($resultDirectory . '/' . self::RATE_FILE_NAME);
                    $isOk = file_put_contents($rateFileName, $rate->getSourceDataFileContentInPlainText());
                    if ($isOk === FALSE) {
                        $problemDuplicationKey = "Can not create - $rateFileName ";
                        $problemDescription = "The rating procedure was not able to create filse \"$rateFileName \", for writing the content of rate " . $rate->getId();
                        $problemEffect = "CDRs of the current rating event can not be rated.";
                        $problemProposedSolution = "Try to force a re-rate again. If the problem persist contact the assistance.";
                        $p = ArProblemException::createWithGarbageCollection(ArProblemType::TYPE_ERROR, ArProblemDomain::APPLICATION, null, $problemDuplicationKey, self::GARBAGE_KEY, $rate->getFromTime(), null, $problemDescription, $problemEffect, $problemProposedSolution);
                        throw ($p);
                    }

                    RateEngineService::compileRate($rateFileName, $resultFileName, self::GARBAGE_KEY, $rate->getFromTime(), null);

                    $conn = Propel::getConnection();
                    $conn->beginTransaction();
                    try {

                        // Insert into MySQL the rated CDRs
                        // MySQL requires that the file can be readable from all
                        $isOk = chmod($resultFileName, 0644);
                        if ($isOk === FALSE) {
                            $problemDuplicationKey = "Can not assign rights  commit file - " . $resultFileName;
                            $problemDescription = "The file \"" . $resultFileName . "\" can not be set with the correct read access rights.";
                            $problemEffect = "CDRs of the current rating event can not be rated.";
                            $problemProposedSolution = "Try to force a rerate again. If the problem persist contact the assistance.";
                            $p = ArProblemException::createWithGarbageCollection(ArProblemType::TYPE_ERROR, ArProblemDomain::APPLICATION, null, $problemDuplicationKey, self::GARBAGE_KEY, $rate->getFromTime(), null, $problemDescription, $problemEffect, $problemProposedSolution);
                            throw ($p);
                        }

                        if (filesize($resultFileName) > 0) {

                            // This is the fastest possible commands for inserting CDRs in the database
                            $cmd = "LOAD DATA INFILE '$resultFileName' REPLACE INTO TABLE ar_telephone_prefix ";
                            $cmd .= <<<'NOWDOC'
        CHARACTER SET 'utf8'
        FIELDS TERMINATED BY ','
        OPTIONALLY ENCLOSED BY  '"'
        ESCAPED BY '\\'
        LINES TERMINATED BY '\r\n' STARTING BY ''
        (prefix,
         match_only_numbers_with_n_digits,
         name,
         geographic_location,
         operator_type,
         display_priority_level);
NOWDOC;

                            $isOk = $conn->exec($cmd);
                            if ($isOk === FALSE) {
                                $problemDuplicationKey = "Can not import telephone prefixes - " . $resultFileName;
                                $problemDescription = "The telephone prefixes inside file \"" . $resultFileName . "\" are not inserted and committed to the database. There is an error in query \n$cmd \nThe error message is " . $conn->errorInfo();
                                $problemEffect = "CDRs can not be rated, because telephone prefix table is not correctly initializated according the rate params.";
                                $problemProposedSolution = "Try to force a rerate again. If the problem persist contact the assistance.";
                                $p = ArProblemException::createWithGarbageCollection(
                                    ArProblemType::TYPE_ERROR,
                                    ArProblemDomain::APPLICATION,
                                    null,
                                    $problemDuplicationKey,
                                    self::GARBAGE_KEY,
                                    $rate->getFromTime(),
                                    null,
                                    $problemDescription,
                                    $problemEffect,
                                    $problemProposedSolution);
                                throw ($p);
                            }
                            $oneExportedRate = true;
                        }

                        $this->commitTransactionOrSignalProblem($conn);

                    } catch (ArProblemException $e) {
                        $this->maybeRollbackTransaction($conn);
                        throw($e);
                    }
                } catch (ArProblemException $e) {
                    // skip the import of this rate
                }
            }

            // also in case of errors signal the rate as imported
            $rate->setWasCompiled(true);
            $rate->save();
        }

        ArRatePeer::clearInstancePool();
        ArRatePeer::clearRelatedInstancePool();

        return $oneExportedRate;
    }


    public function exportRates()
    {
        $garbageKey = self::GARBAGE_KEY;

        $resultFileName = self::getParamsFileCompleteName(self::RATE_PLAN_FILE_NAME);
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

        // This retrieve all the changes, that is a compact definition
        $stm = Propel::getConnection()->prepare('
        SELECT ar_rate.id,
        ar_rate_format.internal_name,
        ar_rate.from_time,
        ar_rate.internal_name,
        ar_rate.ar_rate_id,
        ar_rate.short_description
        FROM ar_rate
        INNER JOIN ar_rate_format ON ar_rate.ar_rate_format_id = ar_rate_format.id
        ORDER BY ar_rate.from_time DESC
        ');
        $stm->execute();
        // NOTE: the descending order on from_time is very important, because the Haskell side,
        // load only the relevant part, discarding the rest.

        while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
            // write data to file

            $i = 0;
            $data = array();
            $id = $rs[$i++];
            $data[] = $this->intSQL($id, false);
            $data[] = $this->stringSQL($rs[$i++], true);
            $data[] = $this->stringSQL($rs[$i++], true);
            $data[] = $this->stringSQL($rs[$i++], true);
            $data[] = $this->intSQL($rs[$i++], false);
            $data[] = $this->stringSQL($rs[$i++], true);
            $this->assertCondition($i == 6, "Unexpected number of records: $i");

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

            // Write a file with the content of the sourcedata file,
            // using a stream (efficient) approach

            $rateFileName = self::getParamsFileCompleteName(self::RATE_PLAN_ID_FILE_NAME . $id . '.rate');
            $rateFileHandle = fopen($rateFileName, 'w');
            if ($rateFileHandle === FALSE) {
                $problemDuplicationKey = "Can not open - " . $rateFileName;
                $problemDescription = "The file \"" . $rateFileName . "\" can not be open for writing data on it.";
                $problemEffect = "CDRs of the current rating event can not be rated.";
                $problemProposedSolution = "Try to force a rerate again. If the problem persist contact the assistance.";
                $p = ArProblemException::createWithGarbageCollection(
                    ArProblemType::TYPE_ERROR,
                    ArProblemDomain::APPLICATION,
                    null,
                    $problemDuplicationKey, $garbageKey, null, null, $problemDescription, $problemEffect, $problemProposedSolution);
                throw ($p);
            }

            $rate = ArRatePeer::retrieveByPK($id);
            fwrite($rateFileHandle, $rate->getSourceDataFileContentInPlainText());
            fclose($rateFileHandle);

            ArRatePeer::clearInstancePool();
        }

        $isOk = fclose($outFileHandle);
        if ($isOk === FALSE) {
            $problemDuplicationKey = "Can not close file - $resultFileName";
            $problemDescription = "The file \"" . $resultFileName . "\" can not be closed correctly.";
            $problemEffect = "CDRs of the current rating event can not be rated.";
            $problemProposedSolution = "Try to force a rerate again. If the problem persist contact the assistance.";
            $p = ArProblemException::createWithGarbageCollection(ArProblemType::TYPE_ERROR, ArProblemDomain::APPLICATION, null, $problemDuplicationKey, $garbageKey, null, null, $problemDescription, $problemEffect, $problemProposedSolution);
            throw ($p);
        }

        $stm->closeCursor();

        return '';
    }

    /**
     * @return bool true if at least one new rate was imported
     * @throws ArProblemException
     */
    protected function compileServices()
    {

        $c = new Criteria();
        $c->add(ArServicePeer::WAS_COMPILED, false);
        $service = ArServicePeer::doSelectOne($c);
        if (is_null($service)) {
            $oneExportedService = false;
        } else {
            $oneExportedService = true;
        }

        if ($oneExportedService) {

            try {
                // MySQL requires that the file is in tmp directory.
                $resultDirectory = ImportDataFiles::getMySQLAccessibleTmpDirectory(self::GARBAGE_KEY);

                $resultFileName = normalizeFileNamePath($resultDirectory . '/' . self::COMPILED_TELEPHONE_PREFIXES);
                @unlink($resultFileName);

                RateEngineService::compileServices($resultFileName, self::GARBAGE_KEY, $this->getGlobalStartingDate(), null);

                $conn = Propel::getConnection();
                $conn->beginTransaction();
                try {

                    // Insert into MySQL the rated CDRs
                    // MySQL requires that the file can be readable from all
                    $isOk = chmod($resultFileName, 0644);
                    if ($isOk === FALSE) {
                        $problemDuplicationKey = "Can not assign rights  commit file - " . $resultFileName;
                        $problemDescription = "The file \"" . $resultFileName . "\" can not be set with the correct read access rights.";
                        $problemEffect = "CDRs of the current rating event can not be rated.";
                        $problemProposedSolution = "Try to force a rerate again. If the problem persist contact the assistance.";
                        $p = ArProblemException::createWithGarbageCollection(ArProblemType::TYPE_ERROR, ArProblemDomain::APPLICATION, null, $problemDuplicationKey, self::GARBAGE_KEY, $this->getGlobalStartingDate(), null, $problemDescription, $problemEffect, $problemProposedSolution);
                        throw ($p);
                    }

                    if (filesize($resultFileName) > 0) {

                        // This is the fastest possible commands for inserting CDRs in the database
                        $cmd = "LOAD DATA INFILE '$resultFileName' REPLACE INTO TABLE ar_telephone_prefix ";
                        $cmd .= <<<'NOWDOC'
        CHARACTER SET 'utf8'
        FIELDS TERMINATED BY ','
        OPTIONALLY ENCLOSED BY  '"'
        ESCAPED BY '\\'
        LINES TERMINATED BY '\r\n' STARTING BY ''
        (prefix,
         match_only_numbers_with_n_digits,
         name,
         geographic_location,
         operator_type,
         display_priority_level);
NOWDOC;

                        $isOk = $conn->exec($cmd);
                        if ($isOk === FALSE) {
                            $problemDuplicationKey = "Can not import telephone prefixes - " . $resultFileName;
                            $problemDescription = "The telephone prefixes inside file \"" . $resultFileName . "\" are not inserted and committed to the database. There is an error in query \n$cmd \nThe error message is " . $conn->errorInfo();
                            $problemEffect = "CDRs can not be rated, because telephone prefix table is not correctly initializated according the rate params.";
                            $problemProposedSolution = "Try to force a rerate again. If the problem persist contact the assistance.";
                            $p = ArProblemException::createWithGarbageCollection(
                                ArProblemType::TYPE_ERROR,
                                ArProblemDomain::APPLICATION,
                                null,
                                $problemDuplicationKey,
                                self::GARBAGE_KEY,
                                $this->getGlobalStartingDate(),
                                null,
                                $problemDescription,
                                $problemEffect,
                                $problemProposedSolution);
                            throw ($p);
                        }
                    }

                    $cmd = "UPDATE ar_service SET was_compiled = 1 WHERE was_compiled <> 1;";
                    $isOk = $conn->exec($cmd);
                    if ($isOk === FALSE) {
                        $problemDuplicationKey = "Can not import telephone prefixes - " . $resultFileName;
                        $problemDescription = "The telephone prefixes inside file \"" . $resultFileName . "\" are not inserted and committed to the database. There is an error in query \n$cmd \nThe error message is " . $conn->errorInfo();
                        $problemEffect = "CDRs can not be rated, because telephone prefix table is not correctly initializated according the rate params.";
                        $problemProposedSolution = "Try to force a rerate again. If the problem persist contact the assistance.";
                        $p = ArProblemException::createWithGarbageCollection(
                            ArProblemType::TYPE_ERROR,
                            ArProblemDomain::APPLICATION,
                            null,
                            $problemDuplicationKey,
                            self::GARBAGE_KEY,
                            $this->getGlobalStartingDate(),
                            null,
                            $problemDescription,
                            $problemEffect,
                            $problemProposedSolution);
                        throw ($p);
                    }


                    $this->commitTransactionOrSignalProblem($conn);

                } catch (ArProblemException $e) {
                    $this->maybeRollbackTransaction($conn);
                    throw($e);
                }
            } catch (ArProblemException $e) {
                // skip the import of this rate
            }
        }


        return $oneExportedService;
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
     * @param int $maxDaysInThePast
     * @param int $times
     * @param bool $interactive
     * @return bool true if all tests are ok
     */
    public function stressRerating($maxDaysInThePast, $times, $interactive)
    {

        $minCallDate = strtotime('-' . $maxDaysInThePast . ' days');

        list($initialChecksum1, $initialChecksum2) = $this->stressRerating_getCallsChecksum($minCallDate);
        if (strcmp($initialChecksum1, $initialChecksum2) !== 0) {
            echo "\n   XXX cached initial checksum differs from computed initial checksum on ar_cdr table.";
            return false;
        }

        while ($times > 0) {
            $i1 = rand(0, $maxDaysInThePast);
            $i2 = rand(0, $maxDaysInThePast);

            $d1 = strtotime('-' . $i1 . ' days');
            $d2 = strtotime('-' . $i2 . ' days');

            if ($d1 > $d2) {
                $t = $d2;
                $d2 = $d1;
                $d1 = $t;
            }

            self::rerateCalls($d1, $d2);

            JobQueueProcessor::setIsInteractive($interactive);
            $this->process();

            list($checksum1, $checksum2) = $this->stressRerating_getCallsChecksum($minCallDate);

            if (strcmp($checksum1, $checksum2) !== 0) {
                echo "\n   XXX cached checksum differs from computed checksum on ar_cdr table.";
                return false;
            }

            if (strcmp($checksum1, $initialChecksum1) !== 0) {
                echo "\n   XXX $checksum1 !== $initialChecksum1";
                return false;
            }

            $times--;
        }

        return true;

    }

    /**
     * @param int $minCallDate
     * @return array list(string, string) two unique checksum to use for comparing results.
     * The first checksum is calculate using the pre-cached table, and the second performing the same calculations
     * scanning the CDR table. They must be equals.
     */
    protected function stressRerating_getCallsChecksum($minCallDate)
    {
        $conn = Propel::getConnection();

        // NOTE: I'm returning a date, without the time part.
        $minCallDateS = fromUnixTimestampToMySQLDate($minCallDate);

        $query = 'SELECT calldate,
                         cached_parent_id_hierarchy,
                         destination_type,
                         count_of_calls,
                         billsec,
                         income,
                         cost_saving,
                         cost
                  FROM  ar_cached_grouped_cdr
                  WHERE calldate >= ?
                  ORDER BY calldate, cached_parent_id_hierarchy, destination_type';

        $r = '';
        $stm = $conn->prepare($query);
        $stm->execute(array($minCallDateS));
        while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
            $r .= $rs[0] . $rs[1] . $rs[2] . $rs[3] . $rs[4] . $rs[5] . $rs[6] . $rs[7];
            $r = md5($r);
        }
        $stm->closeCursor();

        $r1 = $r;

        // There were alignement problems, so I compare also with manual calculations (and not cached)

        $query = 'SELECT MAKEDATE(YEAR(calldate), DAYOFYEAR(calldate)),
                         cached_parent_id_hierarchy,
                         destination_type,
                            SUM(count_of_calls),
                            SUM(billsec),
                            SUM(income),
                            SUM(cost_saving),
                            SUM(cost)
                        FROM ar_cdr WHERE ar_cdr.calldate >= ?
                        AND destination_type <> 4
                        AND destination_type <> 5
                        GROUP BY YEAR(calldate), DAYOFYEAR(calldate), cached_parent_id_hierarchy, destination_type
                        ORDER BY YEAR(calldate), DAYOFYEAR(calldate), cached_parent_id_hierarchy, destination_type';

        $r = '';
        $stm = $conn->prepare($query);
        $stm->execute(array($minCallDateS));
        while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
            $r .= $rs[0] . $rs[1] . $rs[2] . $rs[3] . $rs[4] . $rs[5] . $rs[6] . $rs[7];
            $r = md5($r);
        }
        $stm->closeCursor();

        $r2 = $r;

        return array($r1, $r2);
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
