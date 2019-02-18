<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Export instance status to aggregate server.
 * The last status override always previous status, so there is no history about the status
 * of the application.
 */
class ExportStatusForAggregateServer extends FixedJobProcessor
{

    ////////////
    // PARAMS //
    ////////////

    /**
     * Where put files exported to aggregate server.
     */
    const EXPORT_DIRECTORY = 'data_files/export_status_to_aggregate_server/';

    const TEMP_FILE_NAME = 'temp_file.tmp';

    const FILE_NAME = 'status.csv';

    const CONTENT_TYPE_VERSION = 'status-001';

    /////////////////////
    // CSV FILE FIELDS //
    /////////////////////

    const FIELD_CONTENT_TYPE = 0;

    const FIELD_PROPERTY_NAME = 1;

    const FIELD_PROPERTY_PARAM = 2;

    const FIELD_PROPERTY_VALUE = 3;

    ////////////////////
    // PROPERTY NAMES //
    ////////////////////

    const PROPERTY_SERVER_NAME = 'SERVER_NAME';

    const PROPERTY_APPLICATION_VERSION = 'APPLICATION_VERSION';

    const PROPERTY_NR_OF_CRITICAL_ERRORS = 'NR_OF_CRITICAL_ERRORS';

    const PROPERTY_NR_OF_IMPORTANT_ERRORS = 'NR_OF_IMPORTANT_ERRORS';

    const PROPERTY_NR_OF_WARNING_ERRORS = 'NR_OF_WARNING_ERRORS';

    const PROPERTY_NR_OF_UNSPECIFIED_EXTENSIONS = 'NR_OF_UNSPECIFIED_EXTENSIONS';

    const PROPERTY_NR_OF_EXTENSIONS = 'NR_OF_EXTENSIONS';

    const PROPERTY_NR_WITH_ERRORS_OUTGOING_PREVIOUS_MONTH = 'NR_WITH_ERRORS_OUTGOING_PREVIOUS_MONTH';

    const PROPERTY_NR_WITH_ERRORS_INCOMING_PREVIOUS_MONTH = 'NR_WITH_ERRORS_INCOMING_PREVIOUS_MONTH';

    const PROPERTY_NR_WITH_ERRORS_INTERNAL_PREVIOUS_MONTH = 'NR_WITH_ERRORS_INTERNAL_PREVIOUS_MONTH';

    const PROPERTY_NR_WITH_ERRORS_OUTGOING_LAST_30_DAYS = 'NR_WITH_ERRORS_OUTGOING_LAST_30_DAYS';

    const PROPERTY_NR_WITH_ERRORS_INCOMING_LAST_30_DAYS = 'NR_WITH_ERRORS_INCOMING_LAST_30_DAYS';

    const PROPERTY_NR_WITH_ERRORS_INTERNAL_LAST_30_DAYS = 'NR_WITH_ERRORS_INTERNAL_LAST_30_DAYS';

    const PROPERTY_NR_OUTGOING_PREVIOUS_MONTH = 'NR_OUTGOING_PREVIOUS_MONTH';

    const PROPERTY_NR_INCOMING_PREVIOUS_MONTH = 'NR_INCOMING_PREVIOUS_MONTH';

    const PROPERTY_NR_INTERNAL_PREVIOUS_MONTH = 'NR_INTERNAL_PREVIOUS_MONTH';

    const PROPERTY_NR_OUTGOING_LAST_30_DAYS = 'NR_OUTGOING_LAST_30_DAYS';

    const PROPERTY_NR_INCOMING_LAST_30_DAYS = 'NR_INCOMING_LAST_30_DAYS';

    const PROPERTY_NR_INTERNAL_LAST_30_DAYS = 'NR_INTERNAL_LAST_30_DAYS';

    const PROPERTY_TIMESTAMP = 'TIMESTAMP';

    const PROPERTY_LAST_PROCESSED_CDR_TIMESTAMP = 'LAST_PROCESSED_CDR_TIMESTAMP';

    ///////////////////////
    // PROCESS EXECUTION //
    ///////////////////////

    public function process()
    {
        $timeFrameInMinutes = sfConfig::get('app_check_new_external_files_to_import_after_minutes');
        $checkFile = get_class($this);
        $checkLimit = strtotime("-$timeFrameInMinutes minutes");
        $mutex = new Mutex($checkFile);

        if ($mutex->maybeTouch($checkLimit)) {

            $profiler = new JobProfiler("status info");

            if (JobQueueProcessor::$IS_INTERACTIVE) {
                echo "\n      export to " . $this->getCompleteFileName();
            }

            $problemEffect = 'The procedure can not export status information. Thy will be exported at next try.';
            $problemProposedSolution = 'If the problem persist, contact the assistance.';

            try {

                /**
                 * @var PropelPDO $conn
                 */
                $conn = Propel::getConnection();

                // Open file

                $ok = $this->maybeMakeDir($this->getCompleteDir());
                if ($ok === FALSE) {
                    $problemDuplicationKey = get_class($this) . " - create directory ";
                    $problemDescription = get_class($this) . ' can not create directory "' . $this->getCompleteDir() . '".';
                    $p = ArProblemException::createWithoutGarbageCollection(
                        ArProblemType::TYPE_ERROR,
                        ArProblemDomain::APPLICATION,
                        null,
                        $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
                    throw($p);
                }

                $fh = fopen($this->getCompleteTempFileName(), "w");

                if ($fh === FALSE) {
                    $problemDuplicationKey = get_class($this) . " - file open ";
                    $problemDescription = get_class($this) . ' can not create file "' . $this->getCompleteTempFileName() . '".';
                    $p = ArProblemException::createWithoutGarbageCollection(
                        ArProblemType::TYPE_ERROR,
                        ArProblemDomain::APPLICATION,
                        null,
                        $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
                    throw($p);
                }

                // Write properties

                $profiler->incrementProcessedUnits();
                $serverCode = getInstanceConfigValue('instance_code_name');
                $this->writeProperty($fh, self::PROPERTY_SERVER_NAME, '', $serverCode);

                $profiler->incrementProcessedUnits();
                $version = getApplicationVersion();
                $this->writeProperty($fh, self::PROPERTY_APPLICATION_VERSION, '', $version);

                $profiler->incrementProcessedUnits();
                $this->writeProperty(
                    $fh,
                    self::PROPERTY_NR_OF_CRITICAL_ERRORS,
                    '',
                    getNrOfRecordsInTable('ar_current_problem', $conn, '   ar_problem_type_id = ?', array(ArProblemType::TYPE_CRITICAL))
                );

                $profiler->incrementProcessedUnits();
                $this->writeProperty(
                    $fh,
                    self::PROPERTY_NR_OF_IMPORTANT_ERRORS,
                    '',
                    getNrOfRecordsInTable('ar_current_problem', $conn, '   ar_problem_type_id = ?', array(ArProblemType::TYPE_ERROR))
                );

                $profiler->incrementProcessedUnits();
                $this->writeProperty(
                    $fh,
                    self::PROPERTY_NR_OF_WARNING_ERRORS,
                    '',
                    getNrOfRecordsInTable('ar_current_problem', $conn, '   ar_problem_type_id = ?', array(ArProblemType::TYPE_WARNING))
                );

                $profiler->incrementProcessedUnits();
                $this->writeProperty(
                    $fh,
                    self::PROPERTY_NR_OF_EXTENSIONS,
                    '',
                    getNrOfRecordsInTable('ar_organization_unit_has_structure', $conn, 'extension_name IS NOT NULL', array())
                );

                $system = ArOrganizationUnitPeer::retrieveByInternalName(ConfigureOrganizationHierarchy::UNSPECIFIED_EXTENSION_INTERNAL_NAME);
                if (!is_null($system)) {
                    $systemId = $system->getId();
                    $systemCount = getNrOfRecordsInTable('ar_organization_unit_has_structure', $conn, 'ar_parent_organization_unit_id = ?', array($systemId));

                } else {
                    $systemCount = 0;
                }

                $profiler->incrementProcessedUnits();
                $this->writeProperty(
                    $fh,
                    self::PROPERTY_NR_OF_UNSPECIFIED_EXTENSIONS,
                    '',
                    $systemCount
                );

                $startDate = getPreviousMonth(time());
                $endDate = strtotime('+1 month', $startDate);
                list($cdrsCorrect, $cdrsWithError) = CustomCDRServices::getInstance()->getRatedCDRStats($startDate, $endDate);

                $profiler->incrementProcessedUnits();
                $this->writeProperty(
                    $fh,
                    self::PROPERTY_NR_WITH_ERRORS_OUTGOING_PREVIOUS_MONTH,
                    '',
                    $cdrsWithError[DestinationType::outgoing]
                );
                $profiler->incrementProcessedUnits();
                $this->writeProperty(
                    $fh,
                    self::PROPERTY_NR_OUTGOING_PREVIOUS_MONTH,
                    '',
                    $cdrsCorrect[DestinationType::outgoing]
                );


                $profiler->incrementProcessedUnits();
                $this->writeProperty(
                    $fh,
                    self::PROPERTY_NR_WITH_ERRORS_INCOMING_PREVIOUS_MONTH,
                    '',
                    $cdrsWithError[DestinationType::incoming]
                );
                $profiler->incrementProcessedUnits();
                $this->writeProperty(
                    $fh,
                    self::PROPERTY_NR_INCOMING_PREVIOUS_MONTH,
                    '',
                    $cdrsCorrect[DestinationType::incoming]
                );


                $profiler->incrementProcessedUnits();
                $this->writeProperty(
                    $fh,
                    self::PROPERTY_NR_WITH_ERRORS_INTERNAL_PREVIOUS_MONTH,
                    '',
                    $cdrsWithError[DestinationType::internal]
                );
                $profiler->incrementProcessedUnits();
                $this->writeProperty(
                    $fh,
                    self::PROPERTY_NR_INTERNAL_PREVIOUS_MONTH,
                    '',
                    $cdrsCorrect[DestinationType::internal]
                );

                $startDate = strtotime('-30 day', time());
                $endDate = time();

                list($cdrsCorrect, $cdrsWithError) = CustomCDRServices::getInstance()->getRatedCDRStats($startDate, $endDate);

                $profiler->incrementProcessedUnits();
                $this->writeProperty(
                    $fh,
                    self::PROPERTY_NR_WITH_ERRORS_OUTGOING_LAST_30_DAYS,
                    '',
                    $cdrsWithError[DestinationType::outgoing]
                );
                $profiler->incrementProcessedUnits();
                $this->writeProperty(
                    $fh,
                    self::PROPERTY_NR_OUTGOING_LAST_30_DAYS,
                    '',
                    $cdrsCorrect[DestinationType::outgoing]
                );


                $profiler->incrementProcessedUnits();
                $this->writeProperty(
                    $fh,
                    self::PROPERTY_NR_WITH_ERRORS_INCOMING_LAST_30_DAYS,
                    '',
                    $cdrsWithError[DestinationType::incoming]
                );
                $profiler->incrementProcessedUnits();
                $this->writeProperty(
                    $fh,
                    self::PROPERTY_NR_INCOMING_LAST_30_DAYS,
                    '',
                    $cdrsCorrect[DestinationType::incoming]
                );


                $profiler->incrementProcessedUnits();
                $this->writeProperty(
                    $fh,
                    self::PROPERTY_NR_WITH_ERRORS_INTERNAL_LAST_30_DAYS,
                    '',
                    $cdrsWithError[DestinationType::internal]
                );
                $profiler->incrementProcessedUnits();
                $this->writeProperty(
                    $fh,
                    self::PROPERTY_NR_INTERNAL_LAST_30_DAYS,
                    '',
                    $cdrsCorrect[DestinationType::internal]
                );

                $profiler->incrementProcessedUnits();
                $this->writeProperty(
                    $fh,
                    self::PROPERTY_TIMESTAMP,
                    '',
                    fromUnixTimestampToMySQLTimestamp(time())
                );

                $profiler->incrementProcessedUnits();
                $this->writeProperty(
                    $fh,
                    self::PROPERTY_LAST_PROCESSED_CDR_TIMESTAMP,
                    '',
                    fromUnixTimestampToMySQLTimestamp($this->getLastProcessedCdrTimestamp())
                );

                // Generate final file

                $ok = fclose($fh);
                if (!$ok) {
                    $problemDuplicationKey = get_class($this) . " - file save ";
                    $problemDescription = get_class($this) . ' can not write to file "' . $this->getCompleteTempFileName() . '".';
                    $p = ArProblemException::createWithoutGarbageCollection(
                        ArProblemType::TYPE_ERROR,
                        ArProblemDomain::APPLICATION,
                        null,
                        $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
                    throw($p);
                }

                $ok = $this->myMove($this->getCompleteTempFileName(), $this->getCompleteFileName());
                if (!$ok) {
                    $problemDuplicationKey = get_class($this) . " - file move ";
                    $problemDescription = get_class($this) . ' can not create file "' . $this->getCompleteFileName() . '".';
                    $p = ArProblemException::createWithoutGarbageCollection(
                        ArProblemType::TYPE_ERROR,
                        ArProblemDomain::APPLICATION,
                        null,
                        $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
                    throw($p);
                }

            } catch (ArProblemException $e) {
                // problem already inserted
                throw($e);
            } catch (Exception $e) {
                $p = ArProblemException::createFromGenericExceptionWithoutGarbageCollection($e, get_class($this), 'Unexpected error during execution of ' . get_class($this));
                throw($p);
            }


            return get_class($this) . ": " . $profiler->stop();
        } else {
            return get_class($this) . " will be executed later, every $timeFrameInMinutes minutes.";
        }
    }

    //////////////////////
    // FILES MANAGEMENT //
    //////////////////////

    /**
     * @param resource $fh
     * @param string $propertyName
     * @param string $propertyType
     * @param string $propertyValue
     */
    protected function writeProperty($fh, $propertyName, $propertyType, $propertyValue)
    {
        $l = csv_field(self::CONTENT_TYPE_VERSION, true)
                . csv_field($propertyName, false)
                . csv_field($propertyType, false)
                . csv_field($propertyValue, false)
                . "\n";

        fwrite($fh, $l);

    }

    /**
     * @return string
     */
    static public function getExportDirectory()
    {
        return getAsterisellRootDirectory() . DIRECTORY_SEPARATOR . self::EXPORT_DIRECTORY;
    }

    protected function getCompleteDir()
    {
        return self::getExportDirectory();
    }

    /**
     * @return string
     */
    protected function getCompleteTempFileName()
    {
        return $this->getCompleteDir() . self::TEMP_FILE_NAME;
    }

    /**
     * @return string
     */
    protected function getCompleteFileName()
    {
        return $this->getCompleteDir() . self::FILE_NAME;
    }


    /**
     * @param string $file1
     * @param string $file2
     * @return bool
     */
    protected function myMove($file1, $file2)
    {
        if (@file_exists($file2)) {
            @unlink($file2);
        }
        $ok1 = @rename($file1, $file2);
        $ok2 = @chmod($file2, 0744);

        return $ok1 && $ok2;
    }

    /**
     * @static
     * @param string $dir
     * @return bool
     */
    protected static function maybeMakeDir($dir)
    {
        if (!file_exists($dir)) {
            $ok1 = @mkdir($dir);
            $ok2 = @chmod($dir, 0755);

            return $ok1 && $ok2;
        }
        return true;
    }

    /**
     * @return int unix timestamp
     */
    protected function getLastProcessedCdrTimestamp()
    {
        $stmt = Propel::getConnection()->prepare('
            SELECT ar_cdr.calldate
            FROM ar_cdr 
            ORDER BY ar_cdr.calldate DESC
            LIMIT 1
            ');

        $r = strtotime('-1 year');

        $stmt->execute();
        while ((($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false)) {
            $r = fromMySQLTimestampToUnixTimestamp($rs[0]);
        }
        $stmt->closeCursor();

        return $r;
    }
}
