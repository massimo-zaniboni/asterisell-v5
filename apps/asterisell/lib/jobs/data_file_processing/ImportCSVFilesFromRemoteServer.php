<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Import CSV files from an external (remote) server, or from a local directory that is the mirror
 * of a remote directory.
 * This is an abstract class, that must be customized in inherited subclasses, for performing the real work.
 * Obviously the job must be added to the `import_cdrs_job` parameter.
 */
abstract class ImportCSVFilesFromRemoteServer extends FixedJobProcessor
{

    //
    // Methods to customize in subclasses.
    // Make sure to respect the requirements in method headers.
    //

    /**
     * @return string the name of the CDR provider to use for importing the files.
     * Each Asterisell provider *must* have a distinct name, otherwise files will be overwritten,
     * CDRs will conflict togheter (without a signal from the application),
     * and calls are associated to the wrong Vendor.
     * In case two servers have the same logical Vendor, you can define two distinct providers
     * associated to the same Vendor in the Asterisell application configurations.
     */
    abstract public function getCDRProvider();

    /**
     * @return string the name of the connection params to use, and defined in app.yml under connection settings.
     */
    abstract public function getConnectionName();

    /**
     * @return string the remote directory where there are the files to download.
     */
    abstract public function getRemoteDirectory();

    /**
     * @param string $n file name on remote server
     * @return string|bool false if the name is unexpected, and an error must be signaled to the user,
     * true if the file can ignored, and not imported, and no error must be signaled to the user,
     * the name of the file to use during importing.
     * The file name to use, must be:
     * - unique
     * - contains info about the CDR provider
     * - contains info about logical and version type
     * - contains optional info about the status time-frame
     *
     * Use the functions like ImportDataFiles::createInputDataFileName or
     * ImportDataFiles::createInputStatusDataFileName
     * for generating proper file names.
     */
    abstract public function canAcceptFileName($n);

    /**
     * @param string $sourceFileName the name of the file, on the provider side
     * @return null|string null for no archiving the file.
     * A unique name, respect the provider, for the file to archive in ARCHIVE_DIRECTORY
     */
    public function fileArchiveName($sourceFileName)
    {
        return null;
    }

    /**
     * @return string the locale used from the provider.
     * It must be in Linux `recode` utility format, so something like:
     * - 'ISO-8859-1'
     * - 'UTF8'
     * If the format is different from UTF8, it will be converted to it using the `recode` utility.
     */
    public function getSourceCharacterEncoding()
    {
        return 'UTF8';
    }

    /**
     * @return int minutes of check frequency
     */
    public function checkNewFilesEveryMinutes()
    {
        return 60 * 3;
    }

    /**
     * Change (eventually) the content of the source file name. Usually it is a decompression operation.
     * @param string $remoteFileName
     * @param string $sourceFileName
     * @param string $destFileName
     * @return bool true if the content of the file was changed, false if $sourceFile must be used
     */
    public function normalizeFileContent($remoteFileName, $sourceFileName, $destFileName)
    {
        return false;
    }

    /**
     * Optionally process the file content also inside PHP code.
     * @param string $completeFileName complete file name with path
     * @return bool true for archiving the file and processing it also from the RatingEngine,
     * false for not archiving the file and not process it from the RatingEngine.
     * In any cases the name of the source file is stored in the table of downloaded files,
     * and it will be not downloaded/processed another time.
     * @throws ArProblemException
     */
    public function processFile($completeFileName)
    {
        return true;
    }

    //
    // Utils Functions
    //

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
            $problemEffect = "CDRs of the provider \"" . $this->getCdrProvider() . "\", will not be rated.";
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
            $this->getGarbageFromDate(),
            $this->getGarbageToDate(),
            $problemDescription,
            $problemEffect,
            $problemProposedSolution);
        return $p;
    }

    /**
     * @param string $fileName
     * @return bool true if the file is not already in the database of processed files
     */
    protected function isNewFile($fileName)
    {
        $result = true;

        $conn = Propel::getConnection();
        $stmt = $conn->prepare('
        SELECT id
        FROM ar_remote_file
        WHERE ar_cdr_provider_id = ?
        AND NAME = ?
        LIMIT 1'
        );
        $stmt->execute(array($this->getCdrProviderId(), $fileName));
        while ((($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false)) {
            $result = false;
        }
        $stmt->closeCursor();

        return $result;
    }

    /**
     * @param string $fileName
     */
    protected function signalFileAsProcessed($fileName)
    {
        $conn = Propel::getConnection();
        $stmt = $conn->prepare('
        INSERT INTO ar_remote_file(ar_cdr_provider_id, name, receiving_date)
        VALUES (?,?,?)
        ');
        $stmt->execute(array($this->getCdrProviderId(), $fileName, fromUnixTimestampToMySQLTimestamp(time())));
        $stmt->closeCursor();
    }

    protected function getCdrProviderId()
    {
        static $id = null;
        static $providerName = null;

        // NOTE: the provider can change for every file, so the cache is done only when possible
        if (is_null($id) || ($providerName !== $this->getCDRProvider())) {
            $providerName = $this->getCDRProvider();
            $v = ArCdrProviderPeer::retrieveByName($providerName);
            if (!is_null($v)) {
                $id = $v->getId();
            } else {
                $id = null;
            }
        }

        if (is_null($id)) {
            throw $this->createProblem(
                "not defined cdr provider - " . $this->getCdrProvider()
                , "There is no configured CDR provider \"" . $this->getCdrProvider() . "\"."
                , "Configure the CDR provider in `Params->CDR Providers` form of the Web User Interface."
            );
        }

        return $id;
    }

    protected function getGarbageFromDate()
    {
        return $this->getGlobalStartingDate();
    }

    /**
     * @return int return now date + 1 day in the future
     */
    protected function getGarbageToDate()
    {
        return strtotime('+1 hour', $this->getGlobalStartingDate());
    }

    const TEMP_FILE_NAME = 'temp_processing.tmp';

    //
    // Callable Functions in Subclasses, for common operations.
    //

    /**
     * @param string $remoteFileName
     * @param string $sourceFile ZIP file, with only one file in the archive
     * @param string $destFile
     * @throws ArProblemException
     */
    protected function unzipFileWithOnlyOneFile($remoteFileName, $sourceFile, $destFile)
    {
        $countFiles = 0;
        $zip = zip_open($sourceFile);
        $h = null;
        if (!is_resource($zip)) {
            throw $this->createProblem(
                "Invalid Zip file - " . $sourceFile
                , "Error opening Zip file \"$sourceFile\""
                , "There is a problem on the remote host, or in the application configuration."
                , $remoteFileName
            );
        }

        try {
            while ($zip_entry = zip_read($zip)) {
                $countFiles++;
                $h = null;

                if ($countFiles > 1) {
                    throw $this->createProblem(
                        "Invalid Zip file-  " . $sourceFile
                        , "The Zip file \"$sourceFile\" contains more than one file."
                        , "There is a problem on the remote host, or in the application configuration."
                        , $remoteFileName
                    );
                }

                $isOk = zip_entry_open($zip, $zip_entry, "r");
                if ($isOk === FALSE) {
                    throw $this->createProblem(
                        "Invalid Zip file - " . $sourceFile
                        , "Error opening Zip file \"$sourceFile\"."
                        , "There is a problem on the remote host, or in the application configuration."
                        , $remoteFileName
                    );
                }

                $h = fopen($destFile, 'wb');
                if ($h === FALSE) {
                    throw $this->createProblem(
                        "can not open file " . $destFile
                        , "Can not open the file \"$destFile\"."
                        , "There is a problem on directory settings, or in the application configuration."
                        , $remoteFileName
                    );
                }

                $again = true;
                while ($again) {
                    $str = zip_entry_read($zip_entry, 1024 * 64);
                    if ($str === FALSE) {
                        throw $this->createProblem(
                            "Can not decompress zip file " . $sourceFile
                            , "Error during decompression of Zip file \"$sourceFile\". "
                            , "There is a problem on the remote host, or in the application configuration."
                            , $remoteFileName
                        );
                    } else if (isEmptyOrNull($str)) {
                        $again = false;
                    } else {
                        $isOk = fwrite($h, $str);
                        if ($isOk === FALSE) {
                            throw $this->createProblem(
                                "error writing file " . $sourceFile
                                , "Error writing into file \"$destFile\", during decompression of \"$sourceFile\"."
                                , "There is a problem on the remote host, or in the application configuration."
                                , $remoteFileName
                            );
                        }
                    }
                }
                fclose($h);
                zip_entry_close($zip_entry);
            }

            zip_close($zip);

            if ($countFiles == 0) {
                throw $this->createProblem(
                    "Invalid Zip file " . $sourceFile
                    , "The Zip file \"$sourceFile\" contains no file."
                    , "There is a problem on the remote host, or in the application configuration."
                    , $remoteFileName
                );

            }
        } catch (ArProblemException $e) {
            zip_close($zip);
            if (!is_null($h)) {
                fclose($h);
            }
            throw $e;
        }
    }

    /**
     * Called from jobs for archiving the file, if the flag is activated.
     * @param string $sourceFile the name of the file on the provider side, without directories
     * @param string $completeSourceFile the file to copy/archive on the local file system
     * @param bool $useMove True for moving inside the same filesystem instead of copying
     */
    public function maybeArchiveFile($sourceFile, $completeSourceFile, $useMove = false)
    {
        $destFileName = $this->fileArchiveName($sourceFile);
        if (!isEmptyOrNull($destFileName)) {
            $archiveTime = time();
            $dirName = ImportDataFiles::getAbsoluteArchiveDirectory($this->getCDRProvider(), $this->getCDRProvider(), $this->getCDRProvider(), $archiveTime);
            $completeSourceFile2 = normalizeFileNamePath($dirName . '/' . $destFileName);
            if (file_exists($completeSourceFile2)) {
                @unlink($completeSourceFile2);
            }
            @mkdir($dirName, 0777, true);
            if ($useMove) {
                $isOk = @rename($completeSourceFile, $completeSourceFile2);
            } else {
                $isOk = @copy($completeSourceFile, $completeSourceFile2);
            }

            if ($isOk === FALSE) {
                $key = 'can not archive ' . $completeSourceFile2;
                $problemDuplicationKey = $key . '-' . get_class($this);

                $problemDescription =
                    "The CDR importing procedure \""
                    . get_class($this) . "\", can not archive into \"" . $completeSourceFile2
                    . "\" the file \"" . $completeSourceFile
                    . "\" of the provider \"" . $this->getCDRProvider() . "\"";
                $problemEffect = "This file will be not arhived. ";
                $problemProposedSolution = "Fix the directory permissions, and archive manually this file.";

                ArProblemException::createWithoutGarbageCollection(
                    ArProblemType::TYPE_ERROR,
                    ArProblemDomain::CONFIGURATIONS,
                    null,
                    $problemDuplicationKey,
                    $problemDescription,
                    $problemEffect,
                    $problemProposedSolution);
            }
        }
    }
}
