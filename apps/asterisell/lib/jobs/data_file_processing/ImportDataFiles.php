<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Import inside `ar_source_cdr` table the content of source input files.
 *
 * Only info about the calldate will be completed. All the other info will be processed
 * in next rating phases.
 *
 * This process in two steps is done, because often it is not clear how much info extract from
 * CDRs in source format, so they are mantained in their original format from the application.
 *
 * TokuDB can compress this info, storing space is cheap, backup can be incremental,
 * so it is better maintaning this info, than loosing it.
 *
 * The name of the file identifies its format. The name can be something like:
 *
 * > base-name.cdr-provider__logical-type__version
 * > base-name.YYYY-MM-DD.cdr-provider__logical-type__version
 *
 * where:
 * - base-name is a unique name, with a free format. It is better that it is ordered according CDRs calldate.
 * - cdr-provider identifies the provider that generated the CDRS
 * - logical-type identifies the logical content of the file, but in reality it identifies the physical format while the logical type is the cdr-provider
 * - version identifies wich version/physical format using for the logical-type
 * - YYYY-MM-DD must be specified only for status files representing all the values in a time-frame
 * - if YYYY-MM-DD part is not specified, then the file is incremental, and it adds info to the system
 * - 2013-01-01 represent all the data at date 2013-01-01
 * - 2013-01-00 represent all the data at month 2013-01
 * - 2013-00-00 represent all the data of the year 2013
 * - 0000-00-00 represent a status info without a specific time (for example a ping message)
 *
 * The operation is transaction safe, because processed file are saved (temporarly)
 * in `ar_local_file_to_delete` table.
 */
class ImportDataFiles extends FixedJobProcessor
{

    const GARBAGE_KEY = 'ImportDataFiles';

    const INPUT_DIRECTORY = 'data_files/messages/input/';

    /**
     * The directory where imported files are put.
     */
    const ARCHIVE_DIRECTORY = 'data_files/messages/archive/';

    const WORK_DIRECTORY = 'data_files/messages/tmp/';

    const PARAMS_DIRECTORY = 'data_files/messages/params/';

    const MYSQL_ACCESSBILE_TMP_DIRECTORY = '/var/tmp/';

    /**
     * The list of files to export.
     */
    const FILES_TO_EXPORT_LIST = 'files_to_export.lst';

    /**
     * A file indicating this is an Haskell directory.
     */
    const IS_ASTERISELL_PROVIDER_DIRECTORY_CHECK = 'is_asterisell_directory.chk';

    /**
     * @param string $garbageKey
     * @return string the preferred tmp directory to use, because it is saved on a file system, without
     * using the RAM, and if Asterisell is mounted on the same partition, move operations are atomic.
     * Use a distinct directory for every instance, so multiple instances can work on the same server.
     * @throws ArProblemException
     */
    static public function getMySQLAccessibleTmpDirectory($garbageKey)
    {
        // MySQL requires that the file is in tmp directory.
        // I assign a unique name based on the installation directory, because in this way I'm sure it is unique.
        $resultDirectory = normalizeFileNamePath(self::MYSQL_ACCESSBILE_TMP_DIRECTORY . '/' . getAsterisellCompleteRootDirectory());

        if (!file_exists($resultDirectory)) {
            // NOTE: this code is called only inside a job on a separate session, so it is "safe"
            umask(0);
            $isOk = @mkdir($resultDirectory, 0777, true);
            if ($isOk === FALSE) {
                $problemDuplicationKey = "Can not create - $resultDirectory";
                $problemDescription = "The rating procedure was not able to create directory \"$resultDirectory\".";
                $problemEffect = "CDRs of the current rating event can not be rated.";
                $problemProposedSolution = "Try to force a re-rate again. If the problem persist contact the assistance.";
                $p = ArProblemException::createWithGarbageCollection(ArProblemType::TYPE_ERROR, ArProblemDomain::APPLICATION, null, $problemDuplicationKey, $garbageKey, null, null, $problemDescription, $problemEffect, $problemProposedSolution);
                throw ($p);
            }
        }

        return $resultDirectory;
    }

    /**
     * @return string
     */
    static public function getAbsoluteInputDirectory()
    {
        return normalizeFileNamePath(getAsterisellCompleteRootDirectory() . '/' . self::getRelativeInputDirectory());
    }

    /**
     * @return string
     */
    static public function getAbsoluteParamsDirectory()
    {
        return normalizeFileNamePath(getAsterisellCompleteRootDirectory() . '/' . self::getRelativeParamsDirectory());
    }

    /**
     * @return string the base archive directory, where are stored all the files.
     */
    static public function getBaseAbsoluteArchiveDirectory()
    {
        return normalizeFileNamePath(getAsterisellCompleteRootDirectory() . '/' . self::ARCHIVE_DIRECTORY);
    }

    /**
     * @param string $cdrProvider
     * @param string $logicalType
     * @param string $versionType
     * @param int $time
     * @return string
     */
    static public function getAbsoluteArchiveDirectory($cdrProvider, $logicalType, $versionType, $time)
    {
        return normalizeFileNamePath(getAsterisellCompleteRootDirectory() . '/' . self::getRelativeArchiveDirectory($cdrProvider, $logicalType, $versionType, $time));
    }

    /**
     * @return string
     */
    static public function getAbsoluteTmpDirectory()
    {
        return normalizeFileNamePath(getAsterisellCompleteRootDirectory() . '/' . self::WORK_DIRECTORY);
    }

    /**
     * Move the file using directories on the same file system used for archiving and processing CSV files.
     * First move files from a potential remote file system to a temporari directory on the same FS,
     * and then execute the atomic move.
     * NOTE: in reality PHP has no easy fsync access, so the move is not really atomic in case of loss of power on the machine.
     * @param string $sourceFile source file with complete path
     * @param string $destDirectory the destination directory. It must be on the same file system of the `getAbsoluteTmpDirectory`
     * @param string $destFileName only the name of the file
     * @return bool true if the file was moved, false if there were errors during moving
     * @requires $sourceFile is not already in ImportDataFiles::getAbsoluteTmpDirectory()
     * @requires $destDirectory is not already in ImportDataFiles::getAbsoluteTmpDirectory()
     */
    static public function moveAtomicallyFileFromMySQLDirToOtherDirectory($sourceFile, $destDirectory, $destFileName)
    {
        $destFile1 = normalizeFileNamePath(ImportDataFiles::getAbsoluteTmpDirectory() . '/' . $destFileName);
        $destFile2 = normalizeFileNamePath($destDirectory . '/' . $destFileName);

        @unlink($destFile1);

        $isOk = rename($sourceFile, $destFile1);
        if ($isOk === FALSE) {
            return false;
        }

        @unlink($destFile2);
        $isOk = rename($destFile1, $destFile2);
        if ($isOk === FALSE) {
            return false;
        }

        @unlink($destFile1);

        return true;
    }

    /**
     * @param string $cdrProvider
     * @param string $logicalType
     * @param string $versionType
     * @param int $time
     * @return string
     */
    static public function getRelativeArchiveDirectory($cdrProvider, $logicalType, $versionType, $time)
    {
        $r = self::ARCHIVE_DIRECTORY . 'y_' . date('Y', $time) . '/' . 'm_' . date('m', $time) . '/' . $cdrProvider . '/' . $logicalType . '/' . $versionType . '/';
        return normalizeFileNamePath($r);
    }

    static public function getRelativeInputDirectory()
    {
        return self::INPUT_DIRECTORY;
    }

    static public function getRelativeParamsDirectory()
    {
        return self::PARAMS_DIRECTORY;
    }

    static public function createAbsoluteInputStatusDataFileName($baseUniqueName, $cdrProvider, $logicalType, $physicalFormatName, $yyyy, $mm, $dd, $useTmpDir = false)
    {
        if ($useTmpDir) {
            $dir = self::getAbsoluteTmpDirectory();
        } else {
            $dir = normalizeFileNamePath(getAsterisellCompleteAdminDirectory() . '/' . self::INPUT_DIRECTORY . '/');
        }
        self::maybeCreateDirectoryStatic($dir, 'ImportDataFile');

        $completeName = $dir . '/' . self:: createInputStatusDataFileName($baseUniqueName, $cdrProvider, $logicalType, $physicalFormatName, $yyyy, $mm, $dd);

        return normalizeFileNamePath($completeName);
    }

    static public function createAbsoluteInputDataFileName($baseUniqueName, $cdrProvider, $logicalType, $physicalFormatName, $useTmpDir = false)
    {
        if ($useTmpDir) {
            $dir = self::getAbsoluteTmpDirectory();
        } else {
            $dir = normalizeFileNamePath(getAsterisellCompleteAdminDirectory() . '/' . self::INPUT_DIRECTORY . '/');
        }
        self::maybeCreateDirectoryStatic($dir, 'ImportDataFile');

        $completeName = $dir . '/' . self::createInputDataFileName($baseUniqueName, $cdrProvider, $logicalType, $physicalFormatName);
        return normalizeFileNamePath($completeName);
    }

    /**
     * @param string|null $baseUniqueName the initial part of the name, that must be unique,
     * for not overwriting pending files.
     * A good name should be sortable according generation date.
     * null for generating automatically the name.
     * In any case the name of the file will be lost, because only final file content is imported, with logical informations
     * about it, and the name is not important.
     * @param string $cdrProvider
     * @param string $logicalType
     * @param string $physicalFormatName
     * @param int|null $yyyy the year, or null for global status file
     * @param int|null $mm the month, or null for yearly status file
     * @param int|null $dd the day for daily status file, or null for monthly status file
     * @return string the status file name
     */
    static public function createInputStatusDataFileName($baseUniqueName, $cdrProvider, $logicalType, $physicalFormatName, $yyyy, $mm, $dd)
    {
        if (isEmptyOrNull($baseUniqueName)) {
            $baseUniqueName = get_ordered_timeprefix_with_unique_id();
        }

        $yyyyS = '0000';
        $mmS = '00';
        $ddS = '00';
        if (!is_null($yyyy)) {
            $yyyyS = str_pad($yyyy, 4, '0', STR_PAD_LEFT);
            if (!is_null($mm)) {
                $mmS = str_pad($mm, 2, '0', STR_PAD_LEFT);
                if (!is_null($dd)) {
                    $ddS = str_pad($dd, 2, '0', STR_PAD_LEFT);
                }
            }
        }

        $completeName = $baseUniqueName . '.' . "$yyyyS-$mmS-$ddS." . $cdrProvider . '__' . $logicalType . '__' . $physicalFormatName;

        return $completeName;
    }

    /**
     * @param string|null $baseUniqueName the initial part of the name, that must be unique,
     * for not overwriting pending files.
     * A good name should be sortable according generation date.
     * null for generating automatically the name.
     * In any case the name of the file will be lost, because only final file content is imported, with logical informations
     * about it, and the name is not important.
     * @param string $cdrProvider
     * @param string $logicalType
     * @param string $physicalFormatName
     * @return string the non status file name
     */
    static public function createInputDataFileName($baseUniqueName, $cdrProvider, $logicalType, $physicalFormatName)
    {
        if (isEmptyOrNull($baseUniqueName)) {
            $baseUniqueName = get_ordered_timeprefix_with_unique_id();
        }

        $completeName = $baseUniqueName . '.' . $cdrProvider . '__' . $logicalType . '__' . $physicalFormatName;

        return $completeName;
    }


    /**
     * Archive the files, signaling them as files to process.
     *
     * @return mixed
     * @throws ArProblemException
     */
    public function process()
    {
        $prof = new JobProfiler('imported CDRS');

        ArProblemException::garbageCollect(self::GARBAGE_KEY, null, null);

        $this->deleteAlreadyProcessedFiles();
        // NOTE: if there are files that can not be deleted, this job will be aborted

        if (self::thereAreNewImportedCDRs()) {
            // in case the rating process is blocked, we can not load new CDRs otherwise flags with new CDRs to rate will be overwritten.
            // And in any case it makes no sense to import new CDRs if there are are rating problems.

            if (self::isRatingProcessProblemPermanent()) {
                $garbageKey = self::GARBAGE_KEY;
                $problemDuplicationKey = 'ImportDataFiles is blocked';
                $problemDescription = "New CDRs can not be imported because the rating engine is blocked from a critical error.";
                $problemProposedSolution = "Inspect other error messages describing the type of error signaled from the rating engine.";
                $problemEffect = "New CDRs are not imported.";
                $p = ArProblemException::createWithGarbageCollection(
                    ArProblemType::TYPE_CRITICAL,
                    ArProblemDomain::CONFIGURATIONS,
                    null,
                    $problemDuplicationKey,
                    $garbageKey,
                    null,
                    null,
                    $problemDescription,
                    $problemEffect,
                    $problemProposedSolution);
                throw($p);
            } else {
                // this can happen if there were more rating events, and the previous passage run them, instead of rating new imported CDRs
                return 'rating process temporary blocked.';
            }
        }

        $this->maybeCreateDirectory($this->getAbsoluteInputDirectory());
        $sourceFiles = scandir($this->getAbsoluteInputDirectory());
        if ($sourceFiles === FALSE) {
            self::signalError(
                '1 - ' . $this->getAbsoluteInputDirectory(),
                "Unable to access the data files directory \"" . $this->getAbsoluteInputDirectory() . "\".",
                "Data files will be not imported.",
                "Contact the assistance."
            );
        }

        if ($this->getDebugMode()) {
            $debugFileName = normalizeFileNamePath(self::getMySQLAccessibleTmpDirectory(self::GARBAGE_KEY) . '/' . ManageRateEvent::IMPORT_DEBUG_FILE_NAME);
            if (file_exists($debugFileName)) {
                @unlink($debugFileName);
            }
        } else {
            $debugFileName = null;
            // mandatory for signaling that there is no debug, and it is normal processing
        }

        // the files sent from the client, can be ordered according the generation date,
        // and so insert first the files that are older. In this case if there are queued
        // status files, are processed first the older status files, and then the new
        // status files overwriting the older versions.
        sort($sourceFiles);
        $countFiles = 0;

        $fromDateResult = null;
        $toDateResult = null;

        foreach ($sourceFiles as $sourceFile) {
            if ($sourceFile === '.'
                || $sourceFile === '..'
                || $sourceFile === self::FILES_TO_EXPORT_LIST
                || $sourceFile === self::IS_ASTERISELL_PROVIDER_DIRECTORY_CHECK
            ) {
                continue;
            }

            $totLines = $this->processFile($sourceFile, $debugFileName);
            $prof->addToProcessedUnits($totLines);

            $countFiles++;
        }

        try {
            $this->deleteAlreadyProcessedFiles();
        } catch (ArProblemException $e) {
            // problem already signaled
        }

        if ($countFiles > 0) {
            return $prof->stop() . ". Imported  $countFiles data files.  ";
        } else {
            return 'no files to import';
        }
    }

    /**
     * @param string $sourceFile file name inside input directory, to import.
     * @param string|null $debugFileName null for normal processing
     * @return int processed CDRs
     */
    public function processFile($sourceFile, $debugFileName)
    {

        $completeSourceFile = normalizeFileNamePath($this->getAbsoluteInputDirectory() . '/' . $sourceFile);

        if (!is_file($completeSourceFile)) {
            // skip
            return 0;
        }

        if (filesize($completeSourceFile) === 0) {
            @unlink($completeSourceFile);
            return 0;
        }

        try {
            // Retrieve the elements of the file name

            $m = array();

            $statusYYYY = null;
            $statusMM = null;
            $statusDD = null;

            if (preg_match('/^([^.]+)\.(\d\d\d\d)-(\d\d)-(\d\d)\.([^.]+)__([^.]+)__([^.]+)$/i', $sourceFile, $m)) {
                // > base-name.YYYY-MM-DD.cdr-provider__logical-type__version

                $isStatus = true;
                $statusYYYY = $m[2];
                $statusMM = $m[3];
                $statusDD = $m[4];

                $cdrProvider = $m[5];
                $logicalType = $m[6];
                $versionType = $m[7];

            } else if (preg_match('/^([^.]+)\.([^.]+)__([^.]+)__([^.]+)$/i', $sourceFile, $m)) {

                $isStatus = false;
                $cdrProvider = $m[2];
                $logicalType = $m[3];
                $versionType = $m[4];

            } else {
                self::signalError(
                    "42 - $sourceFile",
                    "The source data file \"$completeSourceFile\", has a name with an unknown format.",
                    "This source data file will be not processed.",
                    "Contact the assistance."
                );
                return null;
            }

            // Calculate the min and max date of the current new status file.

            if ($isStatus) {
                $year = intval($statusYYYY);
                $month = intval($statusMM);
                $day = intval($statusDD);

                if ($year == 0) {
                    $year = null;
                }
                if ($month == 0) {
                    $month = null;
                }
                if ($day == 0) {
                    $day = null;
                }

                list($ignored, $minDate, $maxDate) = self::getTimeFrame($year, $month, $day);

            } else {
                $minDate = null;
                $maxDate = null;
            }

            // Calculate the type of file

            $cdrProviderId = CustomCDRServices::getInstance()->getCdrProviderId($cdrProvider);
            $logicalTypeId = CustomCDRServices::getInstance()->getLogicalTypeId($logicalType);
            $versionTypeId = CustomCDRServices::getInstance()->getLogicalTypeAndVersionId($logicalType, $versionType);

            if (is_null($cdrProviderId)) {
                self::signalError(
                    "1275 - $sourceFile",
                    "The source data file \"$completeSourceFile\", has an unknown CDR provider name \"$cdrProvider\".",
                    "This source data file will be not processed.",
                    "Define the CDR provider name in the PARAMS->CDR Provider section of Asterisell.",
                    false,
                    ArProblemDomain::CONFIGURATIONS
                );
                return 0;
            }

            if (is_null($logicalTypeId) || is_null($versionTypeId)) {
                self::signalError(
                    "logical type - $sourceFile",
                    "Missing type $logicalType, with version $versionType, for file $completeSourceFile",
                    "CDRS will be not imported.",
                    "Improve configuration of jobs, inside \"asterisell_instances.py\"",
                    false,
                    ArProblemDomain::CONFIGURATIONS);
            }

            if ($logicalTypeId == ExportCDRSToReseller::SERVICE_LOGICAL_TYPE) {
                $isImportedService = 1;
            } else {
                $isImportedService = 0;
            }

            if ($isStatus) {
                $isStatusF = 'true';
            } else {
                $isStatusF = 'false';
            }

            if (is_null($minDate)) {
                $minDateS = 'null';
            } else {
                $minDateS = '"' . fromUnixTimestampToMySQLTimestamp($minDate) . '"';
            }

            if (is_null($maxDate)) {
                $maxDateS = 'null';
            } else {
                $maxDateS = '"' . fromUnixTimestampToMySQLTimestamp($maxDate) . '"';
            }

            if (JobQueueProcessor::$IS_INTERACTIVE) {
                echo "\n      import $completeSourceFile";
            }

            if (is_null($debugFileName)) {
                $debugFileName = 'null';
            }

            $importFromDateS = '"' . fromUnixTimestampToMySQLTimestamp($this->getGlobalStartingDate()) . '"';

            $ratingParams = array();
            $cmd = RateEngineService::getToolExecutable()
                . ' --import-data-file ' . $completeSourceFile
                . ' --params ' . RateEngineService::writeParams($ratingParams)
                . ' --debug-file ' . $debugFileName
                . ' --provider ' . $cdrProvider
                . ' --provider-id ' . $cdrProviderId
                . ' --file-logical-type ' . $logicalType
                . ' --file-logical-type-id ' . $logicalTypeId
                . ' --file-version-type ' . $versionType
                . ' --file-version-type-id ' . $versionTypeId
                . ' --from-date ' . $importFromDateS
                . ' --is-imported-service ' . $isImportedService
                . ' --is-status-file ' . $isStatusF
                . ' --from-date ' . $minDateS
                . ' --to-date ' . $maxDateS
                . ' '
                . ManageRateEvent::DONT_TOUCH_DEBUG_MODE_GHC_PARAMS;

            if ($this->getDebugMode()) {
                echo "\nExecute command: \n$cmd\n";
            }

            $output = array();
            $exitStatus = 0;
            $resultLine = exec($cmd, $output, $exitStatus);

            RateEngineService::signalIfThereAreCDRSAlreadyBilled("file $completeSourceFile");

            // NOTE: the file will be signaled as imported, from the Haskell rating engine

            $totLines = 0;
            if ($exitStatus == 0) {

                $results = explode(',', $resultLine);
                $i = 0;

                // The result is composed of values separated from a ",":
                // - minimum date
                // - maximum date
                // - total lines
                // - tot lines with errors

                if (count($results) < 4) {
                    self::signalError(
                        "error in result executing command $cmd",
                        "Error executing command \"$cmd\". Unexpected result \"$resultLine\"",
                        "This Source Data CSV file will be not imported. Stats about not rated CDRs are not updated in this case, and all the CDRs on the CSV file are not rated.",
                        "",
                        true,
                        ArProblemDomain::APPLICATION
                    );
                }

                $minDateStr = $results[$i++];
                $maxDateStr = $results[$i++];
                $totLines = intval($results[$i++]);
                $totLinesWithErrors = intval($results[$i++]);

                // Preserve profiling files
                $profilingFiles = array('RateEngine.prof' => 'RateEngineImport.prof', 'RateEngine.hp' => 'RateEngineImport.hp');
                foreach ($profilingFiles as $pf => $pfi) {
                    $pff = normalizeFileNamePath(getAsterisellCompleteAdminDirectory() . '/' . $pf);
                    $pfii = normalizeFileNamePath(getAsterisellCompleteAdminDirectory() . '/' . $pfi);
                    if (file_exists($pff)) {
                        @unlink($pfii);
                        @rename($pff, $pfii);
                    }
                }

            } else {
                self::signalError(
                    "error executing command $cmd",
                    "Error executing command \"$cmd\": " . implode("\n", $output),
                    "This CSV file will be not imported. Stats about not rated CDRs are not updated in this case, and all the CDRs on the CSV file are not rated.",
                    "Contact the assistance.",
                    true);
            }

            return $totLines;

        } catch (ArProblemException $e) {
            // nothing to do, already signaled
            return 0;

        } catch (Exception $e) {

            self::signalError(
                "675 - $completeSourceFile",
                "Unable to process source data file \"$completeSourceFile\". Error: " . $e->getMessage(),
                "This source data file will be processed next time.",
                "If the problem persist, contact the assistance.",
                false // do not throw the error, but only signal
            );
            return 0;
        }
    }

    /**
     * @param int|null $year
     * @param int|null $month
     * @param int|null $day
     * @return array|null list(statusName, minDate, maxDate)
     */
    static public function getTimeFrame($year, $month, $day)
    {
        if (is_null($year)) {
            return array('0000-00-00', null, null);
        } else if (is_null($month)) {
            $ds = str_pad($year, 4, '0', STR_PAD_LEFT) . '-';
            $d1s = $ds . '01-01';
            $ds .= '00-00';

            $d1 = strtotime($d1s);
            $d2 = strtotime('+1 year', $d1);

        } else if (is_null($day)) {
            $ds = str_pad($year, 4, '0', STR_PAD_LEFT) . '-' . str_pad($month, 2, '0', STR_PAD_LEFT) . '-';
            $d1s = $ds . '01';
            $ds .= '00';

            $d1 = strtotime($d1s);
            $d2 = strtotime('+1 month', $d1);
        } else {
            $ds = str_pad($year, 4, '0', STR_PAD_LEFT) . '-' . str_pad($month, 2, '0', STR_PAD_LEFT) . '-' . str_pad($day, 2, '0', STR_PAD_LEFT);
            $d1s = $ds;
            $d1 = strtotime($d1s);
            $d2 = strtotime('+1 day', $d1);
        }

        return array($ds, $d1, $d2);
    }

    /**
     * @param string $key
     * @param string $description
     * @param string $effect
     * @param string $solution
     * @param bool $throw true for throwing the exception
     * @param int $problemDomain
     * @throws ArProblemException
     */
    static public function signalError($key, $description, $effect, $solution, $throw = true, $problemDomain = ArProblemDomain::APPLICATION)
    {
        $garbageKey = self::GARBAGE_KEY;
        $problemDuplicationKey = 'ImportDataFiles' . ' -  ' . $key;
        $problemDescription = $description;
        $problemProposedSolution = $solution;
        $problemEffect = $effect;
        $p = ArProblemException::createWithGarbageCollection(ArProblemType::TYPE_CRITICAL, $problemDomain, null, $problemDuplicationKey, $garbageKey, null, null, $problemDescription, $problemEffect, $problemProposedSolution);
        if ($throw) {
            throw($p);
        }
    }

    /**
     * @param $sourceFile
     * @param $destFile
     * @return bool false in case of errors
     */
    static public function gunzip($sourceFile, $destFile)
    {
        $buffer_size = 4096 * 10;

        $file = gzopen($sourceFile, 'rb');
        if ($file === FALSE) {
            return false;
        }

        $out_file = fopen($destFile, 'wb');
        if ($out_file === FALSE) {
            gzclose($file);
            return false;
        }

        while (!gzeof($file)) {
            $isOk = fwrite($out_file, gzread($file, $buffer_size));
            if ($isOk === FALSE) {
                fclose($out_file);
                gzclose($file);
                return false;
            }
        }

        fclose($out_file);
        gzclose($file);

        return true;
    }

    /**
     * Export a file from ar_source_cdr table, using the corresponding Haskell Rating Engine Service.
     *
     * @param string $sourceFileBaseName base file name to use
     * @param bool $onlyCdrsToMove
     * @param string $destCdrProviderName
     * @param string $cdrProviderName
     * @param string $logicalTypeName
     * @param string $formatName
     * @param int $year
     * @param int|null $month
     * @param int|null $day
     * @return string|null the name of the created file, null in case of errors
     */
    public function exportStatusFile($sourceFileBaseName, $onlyCdrsToMove, $destCdrProviderName, $cdrProviderName, $logicalTypeName, $formatName, $year, $month, $day)
    {
        list($statusPrefix, $minDate, $maxDate) = self::getTimeFrame($year, $month, $day);
        $sourceFileBaseNameWithStatus = $sourceFileBaseName . '.' . $statusPrefix;
        return $this->exportFile($sourceFileBaseNameWithStatus, $onlyCdrsToMove, $destCdrProviderName, $cdrProviderName, $logicalTypeName, $formatName, $minDate, $maxDate);
    }

    /**
     * Export a file from ar_source_cdr table, using the corresponding Haskell Rating Engine Service.
     *
     * @param string $sourceFileBaseName base file name to use
     * @param bool $onlyCdrsToMove
     * @param string $destCdrProviderName
     * @param string $cdrProviderName
     * @param string $logicalTypeName
     * @param string $formatName
     * @param int $fromDate
     * @param int $toDate
     * @return string|null the name of the created file, null in case of errors
     */
    public function exportFile($sourceFileBaseName, $onlyCdrsToMove, $destCdrProviderName, $cdrProviderName, $logicalTypeName, $formatName, $fromDate, $toDate)
    {
        $completeSourceFile = self::createAbsoluteInputDataFileName($sourceFileBaseName, $destCdrProviderName, $logicalTypeName, $formatName, true);

        $cdrProviderId = CustomCDRServices::getInstance()->getCdrProviderId($cdrProviderName);
        $logicalTypeId = CustomCDRServices::getInstance()->getLogicalTypeId($logicalTypeName);
        $versionTypeId = CustomCDRServices::getInstance()->getLogicalTypeAndVersionId($logicalTypeName, $formatName);

        if (is_null($cdrProviderId)) {
            return null;
        }

        if ($onlyCdrsToMove) {
            $onlyCdrsToMoveS = "true";
        } else {
            $onlyCdrsToMoveS = "false";
        }

        $minDateS = '"' . fromUnixTimestampToMySQLTimestamp($fromDate) . '"';
        $maxDateS = '"' . fromUnixTimestampToMySQLTimestamp($toDate) . '"';

        $ratingParams = array();
        $cmd = RateEngineService::getToolExecutable()
            . ' --export-cdrs ' . $completeSourceFile
            . ' --params ' . RateEngineService::writeParams($ratingParams)
            . ' --provider ' . $cdrProviderName
            . ' --provider-id ' . $cdrProviderId
            . ' --file-logical-type ' . $logicalTypeName
            . ' --file-logical-type-id ' . $logicalTypeId
            . ' --file-version-type ' . $formatName
            . ' --file-version-type-id ' . $versionTypeId
            . ' --from-date ' . $minDateS
            . ' --to-date ' . $maxDateS
            . ' --use-only-cdrs-to-move ' . $onlyCdrsToMoveS;

        $output = array();
        $exitStatus = 0;
        exec($cmd, $output, $exitStatus);

        if ($exitStatus == 0) {
            return $completeSourceFile;
        } else {
            echo "Error executing:\n\n$cmd\n\n";
            echo implode("\n", $output);
            return null;
        }
    }

    public function deleteAlreadyProcessedFiles()
    {
        $conn = Propel::getConnection();

        $allOk = true;
        $notDeletedFiles = '';
        $query = 'SELECT name FROM ar_local_file_to_delete ORDER BY id';
        $stmt = $conn->prepare($query);
        $stmt->execute();
        while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
            $completeSourceFile = $rs[0];
            if (file_exists($completeSourceFile)) {
                $isOk = unlink($completeSourceFile);
                if (!$isOk) {
                    $allOk = false;
                    $notDeletedFiles .= $completeSourceFile . ', ';
                }
            }
        }
        $stmt->closeCursor();

        $query = 'TRUNCATE ar_local_file_to_delete';
        $conn->exec($query);

        if (!$allOk) {
            throw (ArProblemException::createWithoutGarbageCollection(
                ArProblemType::TYPE_CRITICAL
                , ArProblemDomain::CONFIGURATIONS
                , null
                , get_class($this) . ' - ' . md5($notDeletedFiles)
                , "The job " . get_class($this) . " was unable to delete these already processed files $notDeletedFiles"
                , "The content of these files will be processed more than one time, and so there can be duplicated data inside Asterisell database."
                , "Check the rights of directories and users. The Asterisell user is \"apache\""));
        }
    }
}
