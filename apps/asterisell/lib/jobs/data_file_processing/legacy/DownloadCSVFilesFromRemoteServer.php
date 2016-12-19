<?php

/* $LICENSE 2012:
 *
 * Copyright (C) 2012 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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
 * Import CSV files with CDRs, from an external VoIP servers, using RSYNC, according specific instance settings.
 *
 * The work flow is:
 * - there are CSV files on remote server
 * - we read them, using rsync and copying into a clone directory on the local machine, that must be put in synchro
 * - each new or changed file, is put in the file store, and signaled as to process
 *
 * Some notes:
 * - all the files must be mantained on the clone directory, otherwise rsync can not synchronize in an efficient way
 * - when a file is removed from the remote server, it is removed also from the local directory, but not from the
 *   file store/database
 * - new files and modified files, are recognized using the modification time
 *
 * DEV-NOTE: up to date it is used only for ITC classes, so it is not yet used in production
 * TODO extend for using the ar_cdr_provider_id field.
 */
abstract class DownloadCSVFilesFromRemoteServer extends FixedJobProcessor
{

    ////////////
    // PARAMS //
    ////////////

    /**
     * A stat on remote files is done for each file, and this can be slow.
     * Older files on remote servers should be archived.
     */
    const TOO_MUCH_LOCAL_FILES_TO_CHECK = 20000;

    /**
     * @const bool true for processing files retrieved from remote servers
     */
    const COPY_TO_PROCESSING_QUEUE = true;

    /**
     * @const true for executing rsync procedure, false for disabling it, for debugging porpouse
     */
    const EXECUTE_RSYNC = true;

    ////////////////////////////
    // INTERFACE TO CUSTOMIZE //
    ////////////////////////////

    /**
     * @abstract
     * @return string the directory where RSync will copy changed files
     */
    abstract public function getLocalRSyncDirectory();

    /**
     * @abstract
     * @return string the instance setting name, where is stored the list of servers from wich rsync
     */
    abstract public function getInstanceSettingsName();

    ////////////////////
    // IMPLEMENTATION //
    ////////////////////

    /**
     * This file as prefix + class name, contains the date of last check of call cost limits.
     */
    const FILE_WITH_LAST_CHECK_DATE = "import_csv_files_from_remote_server";

    public function process()
    {
        return $this->forceProcess(false);
    }

    /**
     * @param bool $forceProcess true for forcing processing
     * @return string
     * @throws ArProblemException
     */
    public function forceProcess($forceProcess)
    {

        // check if the job can be started
        $timeFrameInMinutes = sfConfig::get('app_check_new_external_files_to_import_after_minutes');
        $checkFile = self::FILE_WITH_LAST_CHECK_DATE . '-' . get_class($this);
        $checkLimit = strtotime("-$timeFrameInMinutes minutes");
        $mutex = new Mutex($checkFile);

        if ($forceProcess || $mutex->maybeTouch($checkLimit)) {
            //
            // Define standard messages.
            //

            $effectOnFile = "This file is not retrieved. Another attempt will be made later.";
            $effectOnServer = "The files on the server are not retrieved. Another attempt wil be made later.";
            $suggestedSolution = "If the error persist, contact the assistance.";

            $profiler = new JobProfiler("CSV Files");

            $localCloneBaseDirectory = getAsterisellCompleteRootDirectory() . DIRECTORY_SEPARATOR . $this->getLocalRSyncDirectory();

            $dirMode = 0770; // ug+rwx
            $fileMode = 0660; // ug+rw

            $log = '';

            $servers = getInstanceConfigValue($this->getInstanceSettingsName());
            foreach ($servers as $serverCode => $serverParams) {

                $imports = $serverParams['import'];
                foreach ($imports as $importCode => $importParams) {
                    $serverDescription = $serverCode . ' (' . $serverParams['remote_host'] . ':' . $importParams['remote_directory'] . ')';
                    $localCloneServerDirectory = $localCloneBaseDirectory . $serverCode . DIRECTORY_SEPARATOR . $importCode . DIRECTORY_SEPARATOR;
                    $destProcessingDirectory = getAsterisellCompleteRootDirectory() . DIRECTORY_SEPARATOR . getInstanceConfigValue('local_archive_of_csv_files');
                    $contentType = $importParams['content_type'];

                    // create destination directory
                    if (!file_exists($destProcessingDirectory)) {
                        $done = mkdir($destProcessingDirectory, $dirMode, true);
                        if ($done === FALSE) {

                            $problemDuplicationKey = "exception on " . get_class($this) . ' - create dir - ' . $serverDescription;
                            $problemDescription = 'In class ' . get_class($this) . ' it was not possible creating directory ' . $destProcessingDirectory;
                            $problemProposedSolution = "If the error persist contact the assistance.";
                            $problemEffect = $effectOnServer;
                            $p = ArProblemException::createWithoutGarbageCollection(
                                ArProblemType::TYPE_ERROR,
                                ArProblemDomain::APPLICATION,
                                null,
                                $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
                            throw($p);
                        }
                    }

                    $countServerFiles = 0;

                    $log .= "\n==================="
                            . "\nProcessing server  "
                            . "\n$serverCode  "
                            . "\n===================\n";

                    //
                    // Retrieve files from remote server
                    //

                    try {

                        // create destination directory
                        if (!file_exists($localCloneServerDirectory)) {
                            $done = mkdir($localCloneServerDirectory, $dirMode, true);
                            if ($done === FALSE) {

                                $problemDuplicationKey = "exception on " . get_class($this) . ' - create dir - ' . $serverDescription;
                                $problemDescription = 'In class ' . get_class($this) . ' it was not possible creating directory ' . $localCloneServerDirectory;
                                $problemProposedSolution = "If the error persist contact the assistance.";
                                $problemEffect = $effectOnServer;
                                $p = ArProblemException::createWithoutGarbageCollection(
                                    ArProblemType::TYPE_ERROR,
                                    ArProblemDomain::APPLICATION,
                                    null,
                                    $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
                                throw($p);
                            }
                        }

                        if (getContactExternalHosts() && $serverParams['is_active'] == true) {
                            if ($serverParams['remote_port'] != '22') {

                                $problemDuplicationKey = "exception on " . get_class($this) . ' - port 22 - ' . $serverDescription;
                                $problemDescription = 'The server ' . $serverDescription . ' requires the port ' . $serverParams['remote_port'] . ', but only the port 22 is supported. ';
                                $problemProposedSolution = "Contact the assistance.";
                                $problemEffect = $effectOnServer;
                                $p = ArProblemException::createWithoutGarbageCollection(
                                    ArProblemType::TYPE_ERROR,
                                    ArProblemDomain::CONFIGURATIONS,
                                    null,
                                    $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
                                throw($p);
                            }

                            if (isEmptyOrNull($serverParams['remote_host'])) {
                                // use file copy

                                $cmd = 'cp -p -u -v ' . $importParams['remote_directory'] . '/' . $importParams['remote_file_name_prefix'] . '* '
                                        . ' '
                                        . $localCloneServerDirectory;

                                echo "\n   command: $cmd\n";
                                exec($cmd, $out, $exit);

                            } else {

                                $out = array();
                                $exit = 0;
                                $cmd = 'rsync -t -h -z -d --stats --include=' . $importParams['remote_file_name_prefix'] . '* '
                                        . ' --exclude=* --chmod=ug+rw '
                                        . ' -e ssh ' . $serverParams['remote_login'] . '@' . $serverParams['remote_host'] . ':/' . $importParams['remote_directory']
                                        . ' '
                                        . $localCloneServerDirectory;

                                if (self::EXECUTE_RSYNC) {
                                    // rsync

                                    echo "\n   command: $cmd\n";
                                    exec($cmd, $out, $exit);
                                } else {
                                    echo "\n  WARNING: rsync command is temporary disabled. Remember to activate it.";

                                    $problemDuplicationKey = "disabled rsync " . get_class($this);
                                    $problemDescription = 'In class ' . get_class($this) . ' rsync command is temporary disabled. Remember to activate it.';
                                    $problemProposedSolution = "If this is not a DEVELOPMENT VESION, contact the assistance.";
                                    $problemEffect = $effectOnServer;
                                    ArProblemException::createWithoutGarbageCollection(
                                        ArProblemType::TYPE_WARNING,
                                        ArProblemDomain::CONFIGURATIONS,
                                        null,
                                        $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
                                }
                            }

                            $log .= implode("\n", $out) . "\n";

                            if ($exit != 0) {

                                $problemDuplicationKey = "exception on " . get_class($this) . ' - rsync  - ' . $serverDescription;
                                $problemDescription = 'In class ' . get_class($this) . ' for server ' . $serverDescription . ', it was not possible executing rsync command "' . $cmd . '". The result of the command is: ' . $log;
                                $problemProposedSolution = "If the error persist contact the assistance.";
                                $problemEffect = $effectOnServer;
                                $p = ArProblemException::createWithoutGarbageCollection(
                                    ArProblemType::TYPE_ERROR,
                                    ArProblemDomain::CONFIGURATIONS,
                                    null,
                                    $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
                                throw($p);
                            }
                        }
                    } catch (ArProblemException $e) {
                        // continue moving the files
                    } catch (Exception $e) {
                        $problemDuplicationKey = "exception on " . get_class($this);
                        $problemDescription = 'In class ' . get_class($this) . ' it was not possible perform rsync on ' . $serverDescription . '. The error message is ' . $e->getMessage() . "\n" . $e->getTraceAsString();
                        $problemProposedSolution = "If the error persist contact the assistance.";
                        $problemEffect = $effectOnServer;
                        ArProblemException::createWithoutGarbageCollection(
                            ArProblemType::TYPE_ERROR,
                            ArProblemDomain::CONFIGURATIONS,
                            null,
                            $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
                    }

                    //
                    // Store new and changed files
                    //

                    try {
                        if (self::COPY_TO_PROCESSING_QUEUE) {
                            $dir = opendir($localCloneServerDirectory);
                            if ($dir === FALSE) {
                                $problemDuplicationKey = "exception on " . get_class($this) . ' - rsync  - ' . $serverDescription;
                                $problemDescription = 'In class ' . get_class($this) . ' for server ' . $serverDescription . ', it was not possible opening directory "' . $localCloneServerDirectory . '"';
                                $problemProposedSolution = "If the error persist contact the assistance.";
                                $problemEffect = $effectOnServer;
                                $p = ArProblemException::createWithoutGarbageCollection(
                                    ArProblemType::TYPE_ERROR,
                                    ArProblemDomain::APPLICATION,
                                    null,
                                    $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
                                throw($p);
                            }

                            while (($clonedFileName = readdir($dir)) !== false) {
                                $clonedFile = $localCloneServerDirectory . $clonedFileName;
                                if (is_file($clonedFile)) {
                                    if (isPrefixOf($importParams['remote_file_name_prefix'], $clonedFileName)) {
                                        // The file can be processed
                                        try {
                                            $countServerFiles++;

                                            if ($countServerFiles > self::TOO_MUCH_LOCAL_FILES_TO_CHECK) {

                                                $problemDuplicationKey = get_class($this) . ' - limit ' . $serverCode;
                                                $problemDescription = "On the server $serverDescription there are more than " . self::TOO_MUCH_LOCAL_FILES_TO_CHECK . " files to check. Consider archiving older files.";

                                                $problemEffect = 'Job retrieving CSV file, can be slow.';
                                                $problemProposedSolution = "Archive on a different directory older files.";
                                                ArProblemException::createWithoutGarbageCollection(
                                                    ArProblemType::TYPE_ERROR,
                                                    ArProblemDomain::CONFIGURATIONS,
                                                    null,
                                                    $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
                                            }

                                            // Local file name

                                            $destSuffix = $importParams['local_suffix'];
                                            $simpleFileName = $clonedFileName;
                                            $fileKey = $serverCode . '_' . $importCode . '_' . $simpleFileName . $destSuffix;
                                            $destProcessingFile = $destProcessingDirectory . $fileKey;

                                            $clonedFileMTime = filemtime($clonedFile);

                                            if ($clonedFileMTime === FALSE) {

                                                $problemDuplicationKey = get_class($this) . ' - mtime ' . $serverCode . ' - ' . $destProcessingFile;
                                                $problemDescription = "It was not possible checking the modification time of the local file \"$destProcessingFile\"";

                                                $problemEffect = $effectOnFile;
                                                $problemProposedSolution = $suggestedSolution;
                                                $p = ArProblemException::createWithoutGarbageCollection(
                                                    ArProblemType::TYPE_ERROR,
                                                    ArProblemDomain::CONFIGURATIONS,
                                                    null,
                                                    $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
                                                throw($p);
                                            }

                                            // File stored on the database
                                            $databaseMTime = ArItcSourceCsvFilePeer::getChecksumValue_fast($fileKey);
                                            if (is_null($databaseMTime)) {
                                                $databaseMTime = 0;
                                            }

                                            if ($databaseMTime != $clonedFileMTime) {
                                                $done = copy($clonedFile, $destProcessingFile);
                                                if ($done === FALSE) {

                                                    $problemDuplicationKey = get_class($this) . ' - read ' . $serverCode . ' - ' . $destProcessingFile;
                                                    $problemDescription = "On the server $serverDescription, it was not possible copy the file \"$clonedFile\" to \"$destProcessingFile\".";

                                                    $problemEffect = $effectOnFile;
                                                    $problemProposedSolution = $suggestedSolution;
                                                    $p = ArProblemException::createWithoutGarbageCollection(
                                                        ArProblemType::TYPE_ERROR,
                                                        ArProblemDomain::APPLICATION,
                                                        null,
                                                        $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
                                                    throw($p);
                                                }

                                                $done = chmod($destProcessingFile, $fileMode);
                                                if ($done === FALSE) {

                                                    $problemDuplicationKey = get_class($this) . ' - chmod ' . $serverCode . ' - ' . $destProcessingFile;
                                                    $problemDescription = "On the server $serverDescription, it was not possible change permissions of the file \"$destProcessingFile\".";

                                                    $problemEffect = $effectOnFile;
                                                    $problemProposedSolution = $suggestedSolution;
                                                    $p = ArProblemException::createWithoutGarbageCollection(
                                                        ArProblemType::TYPE_ERROR,
                                                        ArProblemDomain::APPLICATION,
                                                        null,
                                                        $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
                                                    throw($p);
                                                }

                                                $profiler->incrementProcessedUnits();
                                                $log .= "from \"$clonedFile\" to \"$destProcessingFile\"";

                                                // update the list of downloaded files

                                                $this->putFileOnTheQueue($fileKey, $clonedFileMTime, $contentType, $serverCode);
                                            }
                                        } catch (ArProblemException $e) {
                                            // in case of problem, signal it, but continue with the next file
                                        }
                                    }
                                }
                            }
                            closedir($dir);
                        }
                    } catch (ArProblemException $e) {
                        // continue with the next server
                    } catch (Exception $e) {
                        $problemDuplicationKey = "exception on " . get_class($this);
                        $problemDescription = 'In class ' . get_class($this) . ' it was not possible processing files of ' . $serverDescription . '. The error message is ' . $e->getMessage() . "\n" . $e->getTraceAsString();
                        $problemProposedSolution = "If the error persist contact the assistance.";
                        $problemEffect = $effectOnServer;
                        ArProblemException::createWithoutGarbageCollection(
                            ArProblemType::TYPE_ERROR,
                            ArProblemDomain::APPLICATION,
                            null,
                            $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
                    }
                }
            }
            return 'Imported external CSV files: ' . $profiler->stop() . $log;
        } else {
            return "External CSV files will be imported later, according application settings.";
        }
    }

    /**
     * Signal that the file is saved in the store database, and it must be processed.
     *
     * @param string $fileKey
     * @param string $clonedFileMTime
     * @param string $contentType
     * @param string $retrievedFromServer
     * @return void
     * @throws ArProblemException
     */
    protected
    function putFileOnTheQueue($fileKey, $clonedFileMTime, $contentType, $retrievedFromServer)
    {
        if (strlen($fileKey) > 254) {
            throw(ArProblemException::createWithoutGarbageCollection(
                ArProblemType::TYPE_ERROR,
                ArProblemDomain::CONFIGURATIONS,
                null,
                    get_class($this) . ' - file size ' . $fileKey,
                    'File name can not be longer than 255 characters: "' . $fileKey . '" .',
                'The file will be not processed. ',
                'Contact the assistance.')
            );

        }

        $conn = Propel::getConnection();

        try {

            // NOTE: this work because name is a unique index for `ar_itc_source_csv_file`.
            $q = 'REPLACE INTO ar_itc_source_csv_file
                  SET name = ?, checksum = ?,
                      min_calldate = NULL, max_calldate = NULL,
                      content_type = ?, retrieved_from_server = ?,
                      processing_date = ?,
                      imported = 0;';

            $stmt = $conn->prepare($q);
            $stmt->execute(array($fileKey, $clonedFileMTime, $contentType, $retrievedFromServer, fromUnixTimestampToMySQLTimestamp(time())));

        } catch (Exception $e) {
            throw(ArProblemException::createFromGenericExceptionWithoutGarbageCollection(
                $e,
                    get_class($this) . ' - file ' . $fileKey,
                    'Error during storing of the file "' . $fileKey . '" on the database side.',
                'The file will be not processed, during this phase. Another attempt will be made, at next run of job processor.',
                'If the error persist, contact the assistance.')
            );
        }
    }
}