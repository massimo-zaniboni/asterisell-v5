<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

require_once('ext_libs/phpsec/Net/SFTP.php');

/**
 * Import CSV files from an external server.
 */
class ImportCSVFilesFromRemoteServerUsingSFTP extends FixedJobProcessor
{

    ////////////
    // PARAMS //
    ////////////

    /**
     * A stat on remote files is done for each file, and this can be slow.
     * Older files on remote servers should be archived.
     */
    const TOO_MUCH_REMOTE_FILES_TO_CHECK = 3000;

    ////////////////////
    // IMPLEMENTATION //
    ////////////////////

    /**
     * This file contains the date of last check of call cost limits.
     */
    const FILE_WITH_LAST_CHECK_DATE = "import_csv_files_from_remote_server";

    public function process()
    {

        // check if the job can be started
        $timeFrameInMinutes = sfConfig::get('app_check_new_external_files_to_import_after_minutes');
        $checkFile = self::FILE_WITH_LAST_CHECK_DATE;
        $checkLimit = strtotime("-$timeFrameInMinutes minutes");
        $mutex = new Mutex($checkFile);

        if ($mutex->maybeTouch($checkLimit)) {
            //
            // Define standard messages.
            //

            $effectOnFile = "This file is not retrieved. Another attempt will be made later.";
            $effectOnServer = "The files on the server are not retrieved. Another attempt wil be made later.";
            $suggestedSolution = "If the error persist, contact the assistance.";

            $profiler = new JobProfiler("CSV Files");

            $serverConfigPrefix = 'app_external_csv_servers_';
            $servers = sfConfig::getAll();

            $log = '';

            foreach ($servers as $serverCode => $serverParams) {
                try {
                    if (isPrefixOf($serverConfigPrefix, $serverCode)) {
                        $serverCode = substr($serverCode, strlen($serverConfigPrefix));
                        $serverDescription = $serverCode . ' (' . $serverParams['remote_host'] . ':' . $serverParams['remote_directory'] . ')';

                        $countServerFiles = 0;

                        $log .= "\nProcessing server $serverCode: ";
                        $sftp = new Net_SFTP($serverParams['remote_host'], $serverParams['remote_port']);

                        if (!$sftp->login($serverParams['remote_login'], $serverParams['remote_password'])) {

                            $problemDuplicationKey = get_class($this) . ' - connection 2 ' . $serverCode;
                            $problemDescription = 'Could not connect to ' . $serverParams['remote_host'] . ' on port ' . $serverParams['remote_port'] . ', using the specicfied password.';

                            $problemEffect = $effectOnServer;
                            $problemProposedSolution = "If the error persist, check server access credentials, and update `app.yml` configuration files.";
                            $p = ArProblemException::createWithoutGarbageCollection(
                                ArProblemType::TYPE_ERROR,
                                ArProblemDomain::CONFIGURATIONS,
                                null,
                                $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
                            throw($p);
                        }

                        //
                        // Process all files inside remote directory
                        //

                        $remoteDir = $serverParams['remote_directory'];
                        if (!$sftp->chdir($remoteDir)) {

                            $problemDuplicationKey = get_class($this) . ' - chdir ' . $serverCode;
                            $problemDescription = 'Could not open directory ' . $remoteDir . ' on server ' . $serverDescription;

                            $problemEffect = $effectOnServer;
                            $problemProposedSolution = "If the error persist, check server access credentials, and update `app.yml` configuration files.";
                            $p = ArProblemException::createWithoutGarbageCollection(
                                ArProblemType::TYPE_ERROR,
                                ArProblemDomain::CONFIGURATIONS,
                                null,
                                $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
                            throw($p);
                        }

                        $remoteFiles = $sftp->rawlist();
                        foreach ($remoteFiles as $remoteFileName => $remoteStats) {
                            if (substr("$remoteFileName", 0, 1) != ".") {
                                if (isPrefixOf($serverParams['remote_file_name_prefix'], $remoteFileName)
                                        && isSuffixOf($serverParams['remote_file_name_suffix'], $remoteFileName)
                                ) {
                                    // The file can be processed
                                    try {
                                        $countServerFiles++;

                                        if ($countServerFiles > self::TOO_MUCH_REMOTE_FILES_TO_CHECK) {

                                            $problemDuplicationKey = get_class($this) . ' - limit ' . $serverCode;
                                            $problemDescription = "On the server $serverDescription there are more than " . self::TOO_MUCH_REMOTE_FILES_TO_CHECK . " files to check. Consider archiving older files.";

                                            $problemEffect = 'Job retrieving CSV file, can be slow.';
                                            $problemProposedSolution = "Archive on a different directory older files.";
                                            ArProblemException::createWithoutGarbageCollection(
                                                ArProblemType::TYPE_ERROR,
                                                ArProblemDomain::CONFIGURATIONS,
                                                ArProblemResponsible::ADMIN,
                                                $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
                                        }

                                        $remoteMTime = $remoteStats['mtime'];

                                        // Local file name

                                        $destDirectory = getAsterisellCompleteRootDirectory() . DIRECTORY_SEPARATOR . $serverParams['local_queue_directory'];
                                        $destSuffix = $serverParams['local_suffix'];

                                        $simpleFileName = $remoteFileName;
                                        $localFileName = $destDirectory . $serverCode . '_' . $simpleFileName . $destSuffix;

                                        // File stored on the database

                                        $fileKey = $serverParams['remote_host'] . '://' . $remoteFileName;
                                        $databaseFileEntry = ArProcessedFilePeer::retrieveByName($fileKey);
                                        if (is_null($databaseFileEntry)) {
                                            $databaseMTime = 0;
                                        } else {
                                            $databaseMTime = $databaseFileEntry->getChecksum();
                                        }

                                        if ($databaseMTime != $remoteMTime) {
                                            if (!$sftp->get($remoteFileName, $localFileName)) {

                                                $problemDuplicationKey = get_class($this) . ' - read ' . $serverCode . ' - ' . $remoteFileName;
                                                $problemDescription = "On the server $serverDescription, it was not possible copy the file \"$remoteFileName\" to \"$localFileName\".";

                                                $problemEffect = $effectOnFile;
                                                $problemProposedSolution = $suggestedSolution;
                                                $p = ArProblemException::createWithoutGarbageCollection(
                                                    ArProblemType::TYPE_ERROR,
                                                    ArProblemDomain::CONFIGURATIONS,
                                                    null,
                                                    $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
                                                throw($p);
                                            }

                                            $profiler->incrementProcessedUnits();
                                            $log .= "from \"$remoteFileName\" to \"$localFileName\"";

                                            // a paranoic check, because there were communication problems
                                            $remoteFileSize = $remoteStats['size'];
                                            $localFileSize = filesize($localFileName);
                                            if ($remoteFileSize !== $localFileSize) {

                                                $problemDuplicationKey = get_class($this) . ' - wrong filesize ' . $serverCode . ' - ' . $remoteFileName;
                                                $problemDescription = "Copied local \"$localFileName\" has size $localFileSize that is different than size $remoteFileSize of the corresponding remote file \"$remoteFileName\".";

                                                $problemEffect = $effectOnFile;
                                                $problemProposedSolution = $suggestedSolution;
                                                $p = ArProblemException::createWithoutGarbageCollection(
                                                    ArProblemType::TYPE_ERROR,
                                                    ArProblemDomain::APPLICATION,
                                                    null,
                                                    $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
                                                throw($p);
                                            }

                                            // update the list of downloaded files

                                            if (is_null($databaseFileEntry)) {
                                                $databaseFileEntry = new ArProcessedFile();
                                            }

                                            $databaseFileEntry->setName($fileKey);
                                            $databaseFileEntry->setChecksum($remoteMTime);
                                            $databaseFileEntry->setProcessingDate(time());
                                            $databaseFileEntry->save();

                                            $databaseFileEntry->clearAllReferences();
                                            ArProcessedFilePeer::clearInstancePool();
                                        }
                                    } catch (ArProblemException $e) {
                                        // continue with the next file
                                    }
                                }
                            }

                            $sftp->disconnect();
                        }
                    }
                } catch (ArProblemException $e) {
                    // continue with the next server
                } catch (Exception $e) {

                    $problemDuplicationKey = "exception on " . get_class($this);
                    $problemDescription = 'In class ' . get_class($this) . ' it was not possible load external CSV files. The error message is ' . $e->getMessage() . "\n" . $e->getTraceAsString();
                    $problemProposedSolution = "If the error persist contact the assistance.";
                    $problemEffect = $effectOnFile;
                    ArProblemException::createWithoutGarbageCollection(
                        ArProblemType::TYPE_ERROR,
                        ArProblemDomain::CONFIGURATIONS,
                        null,
                        $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
                }
            }
            return 'Imported external CSV files: ' . $profiler->stop() . $log;
        } else {
            return "External CSV files will be imported later, according application settings.";
        }
    }
}
