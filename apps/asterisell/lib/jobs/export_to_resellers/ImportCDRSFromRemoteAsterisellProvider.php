<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Import CSV files from a WebDAV server.
 */
abstract class ImportCDRSFromRemoteAsterisellProvider extends ImportCDRSFromLocalAsterisellProvider
{

    const TEMP_FILE_NAME = 'tmp_file.tmp';

    /**
     * @return string the name of the connection with the user, password and other params.
     */
    abstract function getConnectionName();

    /**
     * @return bool true if the SSL certificate has a dedicated IP address,
     * it does not use SNI, and it can be recognized also from an old version of CURL.
     * true also if it is a self-signed certificated.
     * true for using the SSL certificate, but skipping its validation.
     * true in case the server is accessed using an IP (also from the router)
     * and the certificate can not be checked.
     *
     * As a rule of thumb: if the provider server resides externally respected the
     * reseller instance leave "false", so SSL certificates are checked.
     * If the provider server is on the same host of the reseller,
     * then usually its address is resolved
     * to an internal IP and then the SSL certificate can not be checked, so
     * use "true".
     */
    function skipSSLCertificateVerify()
    {
        return false;
    }

    /**
     * @return string the optional remote directory of the host, from which download.
     */
    public function getRemoteDirectory()
    {
        return '';
    }

    /**
     * @param array $conf list($conf_host, $conf_user, $conf_password, $conf_port)
     * @return string the complete remote URL ending with '/' character
     */
    protected function getBaseRemoteURL($conf)
    {
        list($conf_host, $conf_user, $conf_password, $conf_port) = $conf;

        $r = $conf_host;

        if (strlen($r) > 0) {
            if (substr($r, strlen($r) - 1, 1) === '/') {
                // nothing to do
            } else {
                $r .= '/';
            }
        }

        $r .= $this->getRemoteDirectory();

        if (strlen($r) > 0) {
            if (substr($r, strlen($r) - 1, 1) === '/') {
                // nothing to do
            } else {
                $r .= '/';
            }
        }

        return $r;
    }

    /**
     * @return string log message
     * @throws ArProblemException
     */
    public function initialProcessPhase()
    {
        $profiler = new JobProfiler("imported remote CSV files");

        $connectionName = $this->getConnectionName();
        $conf = getConnectionParams($connectionName);

        if (is_null($conf)) {
            throw $this->createProblem(
                "configurations"
                , "Unable to find the settings for connection \"$connectionName\""
                , "Complete configuration settings"
            );
        }

        // Check if there is connection with the server
        $this->importRemoteFile($conf, ImportDataFiles::IS_ASTERISELL_PROVIDER_DIRECTORY_CHECK, true, false);

        // Retrieve the list of files to import
        $thereAreFilesToImport = $this->importRemoteFile($conf, ImportDataFiles::FILES_TO_EXPORT_LIST, true, false, false);

        if ($thereAreFilesToImport) {
            $listOfFiles = normalizeFileNamePath($this->getInputDirectory() . '/' . ImportDataFiles::FILES_TO_EXPORT_LIST);
            $handle = fopen($listOfFiles, "r");
            if ($handle === FALSE) {
                return $profiler->stop();
            }

            // Import each file.

            while (($fileName = fgets($handle)) !== false) {
                $fileName = str_replace("\n", "", $fileName);
                try {
                    $profiler->incrementProcessedUnits();
                    $this->importRemoteFile($conf, $fileName, true, true);
                } catch (ArProblemException $e) {
                    // problem already signaled
                }
            }
            fclose($handle);

            // Delete the list of files to process, so the server generate new files.
            $this->importRemoteFile($conf, ImportDataFiles::FILES_TO_EXPORT_LIST, false, true);
        }

        return $profiler->stop();
    }

    /**
     * Import a remote file.
     *
     * @param array $conf
     * @param string $fileName
     * @param bool $executeImport
     * @param bool $executeDelete
     * @param bool $errorIfDoesNotExist
     * @return bool true if the file exists, and was imported
     * @throws ArProblemException
     */
    protected function importRemoteFile($conf, $fileName, $executeImport = true, $executeDelete = true, $errorIfDoesNotExist = true)
    {

        try {
            $baseUrl = $this->getBaseRemoteURL($conf);
            $remoteFileName = $baseUrl . $fileName;

            $destFile = null;
            if ($executeImport) {
                $tmpResultFileName = normalizeFileNamePath(ImportDataFiles::getAbsoluteTmpDirectory() . '/' . self::TEMP_FILE_NAME);
                @unlink($tmpResultFileName);
                $fp = fopen($tmpResultFileName, 'w');
                if ($fp === FALSE) {
                    throw $this->createProblem(
                        "not processable file $tmpResultFileName"
                        , "Can not create file \"$tmpResultFileName\"", null
                        , null
                    );
                }

                try {
                    $ch = $this->initCurl($conf);
                    curl_setopt($ch, CURLOPT_RETURNTRANSFER, false);
                    curl_setopt($ch, CURLOPT_URL, $remoteFileName);
                    curl_setopt($ch, CURLOPT_FILE, $fp);
                    curl_setopt($ch, CURLOPT_FOLLOWLOCATION, true);
                    $isOk = curl_exec($ch);
                    fclose($fp);

                    if ($isOk === FALSE) {
                        if ($errorIfDoesNotExist) {
                            $curlErr = curl_error($ch);
                            curl_close($ch);
                            throw $this->createProblem(
                                "not downloable file $remoteFileName"
                                , "Can not download remote file \"$remoteFileName\", into local file \"$tmpResultFileName\". Error: " . $curlErr
                                , null
                                , $remoteFileName
                            );
                        } else {
                            curl_close($ch);
                            return false;
                        }
                    } else {
                        curl_close($ch);
                    }
                } catch (Exception $e) {
                    if ($errorIfDoesNotExist) {
                        throw $this->createProblem(
                            "generic exception " . $e->getCode()
                            , "Exception: " . $e->getMessage()
                            , "If the error persist, contact the assistance."
                            , null
                        );
                    } else {
                        return false;
                    }
                }

                @mkdir($this->getInputDirectory(), 0660, true);
                $destFile = normalizeFileNamePath($this->getInputDirectory() . '/' . $fileName);
                $isOk = rename($tmpResultFileName, $destFile);
                if ($isOk === FALSE) {
                    throw $this->createProblem(
                        "not processable file $destFile"
                        , "Can not move file \"$tmpResultFileName\", to \"$destFile\"."
                        , "If the error persist, contact the assistance."
                        , null
                    );
                }
            }

            if ($executeDelete) {
                $ch = $this->initCurl($conf);
                curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
                curl_setopt($ch, CURLOPT_URL, $remoteFileName);
                curl_setopt($ch, CURLOPT_FOLLOWLOCATION, true);
                curl_setopt($ch, CURLOPT_CUSTOMREQUEST, 'DELETE');
                $isOk = curl_exec($ch);

                if ($isOk === FALSE) {

                    if (!is_null($destFile)) {
                        // delete the file, because in case it is not a status file, it must be not imported more than one time
                        @unlink($destFile);
                    }

                    $curlErr = curl_error($ch);
                    curl_close($ch);
                    throw $this->createProblem(
                        "not deletable file $remoteFileName"
                        , "Can not delete remote file \"$remoteFileName\". Error: " . $curlErr
                        , "If the error persist, contact the assistance."
                        , "The file will be downloaded, and processed again. If the error persist contact the assistance."
                        , $remoteFileName
                    );
                } else {
                    curl_close($ch);
                }
            }

        } catch (ArProblemException $e) {
            throw $e;
        } catch (Exception $e) {
            throw $this->createProblem(
                "generic exception " . $e->getCode()
                , "Exception: " . $e->getMessage()
                , "If the error persist, contact the assistance."
                , null
            );
        }

        return true;
    }

    /**
     * @param array $conf list($conf_host, $conf_user, $conf_password, $conf_port)
     * @return resource|bool curl connection
     */
    protected function initCurl($conf)
    {
        list($conf_host, $conf_user, $conf_password, $conf_port) = $conf;

        $remoteUrl = $this->getBaseRemoteURL($conf);

        $ch = curl_init();
        curl_setopt($ch, CURLOPT_URL, $remoteUrl);
        curl_setopt($ch, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
        curl_setopt($ch, CURLOPT_USERPWD, "$conf_user:$conf_password");
        curl_setopt($ch, CURLOPT_CONNECTTIMEOUT, 30);
        curl_setopt($ch, CURLOPT_TIMEOUT, 60 * 60);
        curl_setopt($ch, CURLOPT_SSL_VERIFYPEER, !$this->skipSSLCertificateVerify());
        curl_setopt($ch, CURLOPT_NOPROGRESS, true);
        curl_setopt($ch, CURLOPT_FAILONERROR, true);
        curl_setopt($ch, CURLOPT_ENCODING, "gzip,deflate");

        return $ch;
    }

    /**
     * @param string $key
     * @param string $problem
     * @param string|null $solution
     * @param string|null $remoteFileName null for an error for all CDRs, the file name otherwise
     * @return ArProblemException
     */
    protected function createProblem($key, $problem, $solution, $remoteFileName = null)
    {

        $problemDuplicationKey = $key . '-' . get_class($this);

        if (is_null($remoteFileName)) {
            $problemDescription = "The CDR importing procedure \"" . get_class($this) . "\", associated to the remote server connection params \"" . $this->getConnectionName() . "\", can not download CDRs. " . $problem;
            $problemEffect = "CDRs of the provider \"" . $this->getCdrProviderName() . "\", will not be rated.";
            $problemProposedSolution = $solution;
        } else {
            $problemDescription = "The CDR importing procedure \"" . get_class($this) . "\", associated to the remote server connection params \"" . $this->getConnectionName() . "\", can not process the remote file \"" . $remoteFileName . "\". " . $problem;
            $problemEffect = "This file will be not processed. ";
            $problemProposedSolution = $solution;
        }

        if (isEmptyOrNull($problemProposedSolution)) {
            $problemProposedSolution = "It can be a temporary problem on the remote host. If the problem persist check the connection parameters.";
        }

        $p = ArProblemException::createWithGarbageCollection(
            ArProblemType::TYPE_CRITICAL,
            ArProblemDomain::CONFIGURATIONS,
            null,
            $problemDuplicationKey,
            get_class($this),
            null,
            null,
            $problemDescription,
            $problemEffect,
            $problemProposedSolution);
        return $p;
    }
}
