<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Import CSV files from an external FTP server.
 * This is an abstract class, that must be customized in inherited subclasses, for performing the real work.
 * Obviously the job must be added to the `import_cdrs_job` parameter.
 */
abstract class ImportCSVFilesFromFTPServer extends ImportCSVFilesFromRemoteServer
{

    /**
     * @return true if it is a FTPS (FTP over SSL connection).
     * False for normal FTP connection.
     */
    public function isFTPS() {
        return false;
    }

    public function process()
    {

        // check if the job can be started
        $timeFrameInMinutes = $this->checkNewFilesEveryMinutes();
        $checkFile = get_class($this);
        $checkLimit = strtotime("-$timeFrameInMinutes minutes");
        $mutex = new Mutex($checkFile);

        if ($mutex->maybeTouch($checkLimit)) {

            ArProblemException::garbageCollect(get_class($this), $this->getGarbageFromDate(), $this->getGarbageToDate());

            $connectionName = $this->getConnectionName();
            $r = getConnectionParams($connectionName);

            if (is_null($r)) {
                throw $this->createProblem(
                    "configurations"
                    , "Unable to find the settings for connection \"$connectionName\""
                    , "Complete configuration settings"
                );
            }

            list($conf_host, $conf_user, $conf_password, $conf_port) = $r;

            $profiler = new JobProfiler("imported remote CSV files");
            $log = '';

            if ($this->isFTPS()) {
                $conn_id = ftp_ssl_connect($conf_host, $conf_port);
            } else {
                $conn_id = ftp_connect($conf_host, $conf_port);
            }

            $login_result = ftp_login($conn_id, $conf_user, $conf_password);

            if ((!$conn_id) || (!$login_result)) {
                throw $this->createProblem(
                    "Can not connect "
                    , "Can not connect to the remote server, with params: " . print_r($r, true)
                    , "Complete configuration settings."
                );
            }

            ftp_pasv($conn_id, true);

            if (!isEmptyOrNull($this->getRemoteDirectory())) {
                $isOk = ftp_chdir($conn_id, $this->getRemoteDirectory());
                if ($isOk === FALSE) {
                    throw $this->createProblem(
                        "Can not change directory "
                        , "Can not open directory \"" . $this->getRemoteDirectory() . "\""
                        , "Check if the directory on the remote server exists and it is accessible."
                    );

                }
            }
            $remoteFiles = ftp_nlist($conn_id, ".");
            if ($remoteFiles === FALSE) {
                throw $this->createProblem(
                    "Can not list directory "
                    , "Can not list files inside directory \"" . $this->getRemoteDirectory() . "\""
                    , "Check if the directory on the remote server exists and it is accessible."
                );

            }

            foreach ($remoteFiles as $remoteFileName) {
                try {
                    // NOTE: call first this because the provider can change
                    if (isPrefixOf('./', $remoteFileName)) {
                       $remoteFileName = substr($remoteFileName, 2);
                    }
                    $fileName = $this->canAcceptFileName($remoteFileName);
                    if ($this->isNewfile($remoteFileName)) {

                        if ($fileName === false) {
                            throw $this->createProblem(
                                "not valid filename $remoteFileName"
                                , "The remote file \"$remoteFileName\" has an invalid name."
                                , "If the error persist, contact the assistance."
                                , $remoteFileName
                            );

                        } else if ($fileName === true) {
                            // can skip the file
                            $log .= "skip file $remoteFileName, ";
                        } else if (is_string($fileName)) {

                            $log .= "import $remoteFileName renaming into $fileName, ";

                            $tmpResultFileName = normalizeFileNamePath(ImportDataFiles::getAbsoluteTmpDirectory() . '/' . $fileName);
                            $tmpResultFileName2 = normalizeFileNamePath(ImportDataFiles::getAbsoluteTmpDirectory() . '/' . self::TEMP_FILE_NAME);

                            @unlink($tmpResultFileName);
                            @unlink($tmpResultFileName2);

                            $outHandle = fopen($tmpResultFileName, 'w');
                            if ($outHandle === FALSE) {
                                throw $this->createProblem(
                                    "not processable file $remoteFileName"
                                    , "Can not create local file \"$tmpResultFileName\"."
                                    , "If the error persist, contact the assistance."
                                    , $remoteFileName
                                );
                            }

                            $isOk = ftp_fget($conn_id, $outHandle, $remoteFileName, FTP_BINARY);
                            if ($isOk === FALSE) {
                                throw $this->createProblem(
                                    "not processable file $remoteFileName"
                                    , "Can not download remote file \"$remoteFileName\", into local file \"$tmpResultFileName\"."
                                    , "If the error persist, contact the assistance."
                                    , $remoteFileName
                                );
                            }
                            @fclose($outHandle);

                            if ($this->normalizeFileContent($remoteFileName, $tmpResultFileName, $tmpResultFileName2)) {
                                // the $tmpResultFileName was replaced with a normalized version
                                @unlink($tmpResultFileName);
                                $isOk = rename($tmpResultFileName2, $tmpResultFileName);

                                if (!$isOk) {
                                    throw $this->createProblem(
                                        "not processable file $remoteFileName"
                                        , "Can not process file \"$tmpResultFileName2\", and write normalized content into file \"$tmpResultFileName\"."
                                        , "If the error persist, contact the assistance."
                                        , $remoteFileName
                                    );
                                }
                            }

                            $this->maybeRecode($this->getSourceCharacterEncoding(), $tmpResultFileName, get_class($this));

                            $this->maybeArchiveFile($remoteFileName, $tmpResultFileName);
                            $archive = $this->processFile($tmpResultFileName);
                            if ($archive) {
                                $dstResultFileName = normalizeFileNamePath(ImportDataFiles::getAbsoluteInputDirectory() . '/' . $fileName);
                                @unlink($dstResultFileName);
                                $isOk = rename($tmpResultFileName, $dstResultFileName);
                                if ($isOk === FALSE) {
                                    throw $this->createProblem(
                                        "not processable file $remoteFileName"
                                        , "Can not move file \"$tmpResultFileName\", into \"$dstResultFileName\"."
                                        , "If the error persist, contact the assistance."
                                        , $remoteFileName
                                    );
                                }
                            } else {
                                $isOk = @unlink($tmpResultFileName);
                                if ($isOk === FALSE) {
                                    $this->createProblem(
                                        "not deletable file $remoteFileName"
                                        , "Can not delete file \"$tmpResultFileName\"."
                                        , "It can be a problem of directory permissions."
                                        , $remoteFileName
                                    );
                                }
                            }

                            $this->signalFileAsProcessed($remoteFileName);
                            $profiler->incrementProcessedUnits();
                        } else {
                            throw $this->createProblem(
                                "error in code on filenname $remoteFileName"
                                , "The remote file \"$remoteFileName\" can not be processed."
                                , "This is an error in application code. Contact the assistance."
                                , $remoteFileName
                            );
                        }
                    }
                } catch (ArProblemException $e) {
                    // nothing to do: problem already signaled, process next file

                } catch (Exception $e) {
                    $this->createProblem(
                        "generic exception " . $e->getCode()
                        , "Excetption: " . $e->getMessage()
                        , "If the error persist, contact the assistance."
                        , $remoteFileName
                    );
                }
            }

            ftp_close($conn_id);
            return $profiler->stop() . "\n" . $log;

        } else {
            return 'Job will be executed later.';
        }
    }
}
