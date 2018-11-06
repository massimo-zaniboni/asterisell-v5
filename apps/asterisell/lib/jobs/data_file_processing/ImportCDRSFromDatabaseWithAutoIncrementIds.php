<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Read CDRs on a table of a (maybe external) MySQL database.
 * Convert to CSV files for rating them.
 * It uses auto_increment IDS for recognizing new inserted CDRS,
 * and the algo used is safe (no data loss, also in case of run-time exceptions)
 * also without using transactions on the remote database,
 * but only on the local (Asterisell owned) database.
 * So it is a non-intrusive job.
 *
 * This job tries also to work using small chunks of CDRS, for non imposing long locks
 * on the tables, in case the remote DBMS does not support transactions in a robust way.
 *
 * This is an abstract class, that must be customized in inherited subclasses, for performing the real work.
 *
 * NOTE: the remote server character set must be not specified, because it is converted automatically to UTF8.
 *
 * DEV-NOTE: an imported CDR can not be converted to a standard rating format in this phase, because
 * otherwise the backup of CSV files is not a exact copy of the source table content.
 */
abstract class ImportCDRSFromDatabaseWithAutoIncrementIds extends FixedJobProcessor
{

    const ARCHIVE_JOB_PREFIX = 'old_cdrs_';

    // ------------------------------------------------------------
    // Methods to customize in subclasses.
    // Make sure to respect the requirements in method headers.

    /**
     * @return string|null null if the connection is the same of the Asterisell instance (same host, same database).
     * Otherwise the name of the connection.
     * The connection params are specified in `asterisell_instances.py` file.
     */
    abstract public function getConnectionName();

    /**
     * @return string|null null if getConnectionName is null,
     * the name of the database to connect otherwise.
     */
    abstract public function getCollectorDatabaseName();

    /**
     * @return string the name of the table, containing CDRs.
     */
    abstract public function getCollectorTableName();

    /**
     * @return string the cdr-provider to use for imported CDRs.
     * This is a short descriptive name like "some-provider-name",
     * without spaces, and double "__" characters inside.
     * It will be used also as directory name, inside archive directory.
     */
    public function getCdrProvider()
    {
        return 'default';
    }

    /**
     * @return string the logical type name of the file to use for imported CDRs.
     * This is a short descriptive name like "asterisk-collector",
     * without spaces, and double "__" characters inside.
     * It will be used as part of filename, for recognizing the type of CSV file content.
     * You can use a list of known data formats, in case.
     * The rating engine will use this format name for rating the calls.
     */
    abstract public function getLogicalType();

    /**
     * @return string the physical type version name of the file to use for imported CDRs.
     * This is a short descriptive name like "v1",
     * without spaces, and double "__" characters inside.
     * It will be used as part of filename, for recognizing the version of the CSV file content.
     * You can use a list of known data formats, in case.
     * The rating engine will use this format name for rating the calls.
     */
    abstract public function getPhysicalType();

    /**
     * @return array list of field names to export, in the order of appeareance on the CSV file.
     * All the fields of the table should be in this list, in order to use the CSV file as a complete backup of the data.
     * Every time you change this format, you must also update the getPhysicalType method, and you must update the rating engine
     * processing these files.
     */
    abstract public function getListOfFields();

    /**
     * A field in the `getCollectorTableName()` that is auto-increment on the new generated CDRs.
     * The requirements are that a new generated CDR has a value of this field greater than previous CDRs.
     *
     * NOTE: it is not required that all calls dates are strictly ordered by ID, but only that
     * each new inserted CDR has an ID greater than previous CDRS.
     *
     * NOTE: we can not use the calldate, because there can be pending calls, started before already finished and added calls,
     * so they are no progressive, and there can be no efficient index on them.
     *
     * @return string SQL table field name
     */
    abstract public function getProgressiveField();

    /**
     * You can use the collector table like a queue, and removing very old CDRS.
     * A copy of the CDRS is mantained inside Asterinell `ar_source_cdr` table.
     *
     * @return int 0 for not removing the CDRs,
     * otherwise the number of the days after an already exported CDR can be removed/deleted from the collector table.
     */
    public function removeExportedCDRSOlderThanDays()
    {
        return 0;
    }

    /**
     * @return string the name of the field to use for `removeExportedCDRSOlderThanDays`.
     */
    abstract public function getCallDateField();

    /**
     * @return bool true if the getCalldateField is a TIMESTAMP (numeric value),
     * or false if the field is a DATETIME (string value).
     */
    abstract public function isCallDateFieldATimestamp();

    /**
     * @return int minutes to wait, before checking for old CDRs to remove.
     */
    public function getHowMuchCheckCDRSOlderThanDays()
    {
        return 60 * 6;
    }

    /**
     * @return string additional query conditions for CDRs to be exported. Important: start with an ' AND ' this condition.
     */
    public function getAdditionalQueryConditions()
    {
        return '';
    }

    /**
     * @return int split the CDRS to process  in chunks of this max size.
     * NOTE: use a low enough value, because queries results will be often cached by the MySQL driver.
     */
    public function chunkSize()
    {
        return 25000;
    }

    // -------------------------------
    // Job code

    /**
     * Customizable garbage-key.
     * @return string
     */
    protected function getGarbageKey()
    {
        return get_class($this);
    }

    /**
     * @return bool true for using the same connection of Asterisell instance.
     */
    protected function isLocalConnection()
    {
        if (is_null($this->getConnectionName())) {
            return true;
        } else {
            return false;
        }
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

    /**
     * @return string SQL condition on CDRS to export
     * @throws ArProblemException
     */
    protected function getConditionOnCDRToBeExported()
    {
        $r = '';

        $provider = ArCdrProviderPeer::retrieveByName($this->getCdrProvider());
        if (is_null($provider)) {
            $problemDuplicationKey = "Missing provider " . $this->getCdrProvider() . " - " . get_class($this);
            $problemDescription = "The provider " . $this->getCdrProvider() . " is not configured on the Asterisell side.";
            $problemEffect = "CDRs of the provider \"" . $this->getCdrProvider() . "\", will not be imported and rated.";
            $problemProposedSolution = "Add the provider.";
            $p = ArProblemException::createWithGarbageCollection(
                ArProblemType::TYPE_CRITICAL,
                ArProblemDomain::CONFIGURATIONS,
                null,
                $problemDuplicationKey,
                $this->getGarbageKey(),
                $this->getGarbageFromDate(),
                $this->getGarbageToDate(),
                $problemDescription,
                $problemEffect,
                $problemProposedSolution);
            throw ($p);
        }

        $fromId = $provider->getLastImportedId();
        if (is_null($fromId)) {
            $fromId = 0;
        }

        $r .= ' ' . $this->getProgressiveField() . ' > ' . $fromId
            . ' ' . $this->getAdditionalQueryConditions();

        return $r;
    }

    /**
     * @param string $fileName the file containing the result if it is a local connection
     * @return string the MySQL query for exporting the data, with a parameter on max progressive id
     */
    protected function getCSVExportingQuery($fileName)
    {
        $isFirst = true;
        $r = 'SELECT ';
        foreach ($this->getListOfFields() as $f) {
            if ($isFirst) {
                $isFirst = false;
            } else {
                $r .= ', ';
            }

            $r .= '`' . $f . '`';
        }

        if ($this->isLocalConnection()) {
            $r .= ' INTO OUTFILE \'' . $fileName . '\' ';
            $r .= <<<'NOWDOC'
        CHARACTER SET  'utf8mb4'
        FIELDS TERMINATED BY ','
        OPTIONALLY ENCLOSED BY '"'
        ESCAPED BY '"'
        LINES TERMINATED BY '\n'
NOWDOC;
        }

        $r .= ' FROM `' . $this->getCollectorTableName() . '` WHERE ';
        $r .= $this->getConditionOnCDRToBeExported(true);
        $r .= ' AND ' . $this->getProgressiveField() . ' <= ? ';

        return $r;
    }

    // TODO rewrite this or delete

    /**
     * @return string|null the query for signaling the CDRs as exported, with a parameter on progressive limit.
     * null if the method is not applicable.
     */
    protected function getSetExportedCDRSQuery()
    {
        if (is_null($this->getExportedStatusBooleanField())) {
            return null;
        } else {
            $r = 'UPDATE ' . $this->getCollectorTableName() . ' SET ' . $this->getConditionOnCDRToBeExported(false, true);
            $r .= ' WHERE ' . $this->getConditionOnCDRToBeExported(true);
            $r .= ' AND ' . $this->getProgressiveField() . ' <= ? ;';

            return $r;
        }
    }

    /**
     * @return int|null the current max ID to import, or null if there are no new CDRS to import
     */
    protected function getMaxProgressiveId()
    {
        $query = ' SELECT ' . $this->getProgressiveField()
               . ' FROM  ' . $this->getCollectorTableName()
               . ' WHERE ' . $this->getConditionOnCDRToBeExported()
               . ' ORDER BY ' . $this->getProgressiveField() . ' DESC '
               . ' LIMIT 1 ';
        // DEV-NOTE: the query written in this form it is for sure efficient

        $stmt = $this->getExternalConnection()->prepare($query);
        $stmt->execute();
        $maxId = NULL;
        while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
            $maxId = $rs[0];
        }
        $stmt->closeCursor();
        return $maxId;
    }

    protected function getUniqueFileName()
    {
        return 'cdrs_' . get_ordered_timeprefix_with_unique_id() . '.' . $this->getCdrProvider() . '__' . $this->getLogicalType() . '__' . $this->getPhysicalType();
    }



    /**
     * @return PDO a cached result
     * @throws ArProblemException
     */
    protected function getExternalConnection()
    {
        static $conn = null;

        if (is_null($conn)) {
            $conf_connectionString = null;

            if (is_null($this->getConnectionName())) {
                $conn = Propel::getConnection();
            } else {

                // Read configuration settings from app.yml

                $connectionName = $this->getConnectionName();
                $r = getConnectionParams($connectionName);

                if (is_null($r)) {
                    $problemDuplicationKey = "Can not import CDRS - " . get_class($this);
                    $problemDescription = "The CDR importing procedure \"" . get_class($this) . "\", was unable to find the settings for connection \"$connectionName\", inside configuration file \"apps/asterisell/config/app.yml\".";
                    $problemEffect = "CDRs of the provider \"" . $this->getCdrProvider() . "\", will not be rated.";
                    $problemProposedSolution = "Add the configuration settings.";
                    $p = ArProblemException::createWithGarbageCollection(
                        ArProblemType::TYPE_CRITICAL,
                        ArProblemDomain::APPLICATION,
                        null,
                        $problemDuplicationKey,
                        $this->getGarbageKey(),
                        $this->getGarbageFromDate(),
                        $this->getGarbageToDate(),
                        $problemDescription,
                        $problemEffect,
                        $problemProposedSolution);
                    throw ($p);
                }

                list($conf_host, $conf_user, $conf_password, $conf_port) = $r;

                try {
                    $conf_connectionString = 'mysql:host=' . $conf_host . ';port=' . $conf_port . ';dbname=' . $this->getCollectorDatabaseName();
                    $conn = new PDO($conf_connectionString, $conf_user, $conf_password);
                    $conn->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
                    $conn->setAttribute(PDO::MYSQL_ATTR_USE_BUFFERED_QUERY, true);
                    // NOTE: buffer results because they are split in chunks,
                    // and it is probably nicer with concurrent writes on the table
                    // of the data producers.

                    // The client (this app) will send utf8 strings to the remote server.
                    // The remote server must convert the results to utf8, before sending to the client.
                    // The character set used on the server, is automatically converted to utf8 by the server.
                    // NOTE: I'm not using the better 'utf8mb4' because on some (very) old MySQL instance it is not supported.
                    $conn->exec("SET NAMES 'utf8';");

                } catch (Exception $e) {
                    $problemDuplicationKey = get_class($this) . " - no connection - ";
                    $p = ArProblemException::createFromGenericExceptionWithGarbageCollection(
                        $e,
                        ArProblemType::TYPE_CRITICAL,
                        $problemDuplicationKey,
                        $this->getGarbageKey(),
                        $this->getGarbageFromDate(),
                        $this->getGarbageToDate(),
                        "Unable to retrieve and process CDRs from provider \"" . $this->getCdrProvider() . "\", using a database connection/table, according the code in class " . get_class($this) . ". There is an error opening the connection with name \"$connectionName\".",
                        "CDRs of the provider \"" . $this->getCdrProvider() . "\", will not be rated.",
                        "Configure correctly the params of the connection, inside file \"apps/asterisell/config/app.yml\"."
                    );

                    throw($p);
                }
            }
        }

        return $conn;
    }

    /**
     * @return String log
     * @throws ArProblemException
     */
    public
    function process()
    {
        $l = '';
        $l .= $this->importCDRS();
        $l .= ' ';
        $l .= $this->maybeDeleteOldCDRS();

        return $l;
    }

    /**
     * Import jobs from (remote) table, and write on a local file.
     *
     * The algo requirements are:
     * - do not use transactions on remote table
     * - can use transactions on local tables
     * - the behaviour is transaction safe (no data loss in case of exceptions)
     * - very light on locks created on the remote table, because the priority is non-blocking the insertion of new CDRS
     *
     * @return string log
     * @throws ArProblemException
     */
    public
    function importCDRS()
    {
        $prof = new JobProfiler('CDRS');

        // TODO append to the same file
        // TODO delete the file in case of exceptions
        // TODO split in chunks
        // TODO comment the reasons
        // TODO count the processed CDRS and stop when they are less that the chunk

        ArProblemException::garbageCollect($this->getGarbageKey(), $this->getGarbageFromDate(), $this->getGarbageToDate());

        // I'm using maxId because during query execution new CDRs can be inserted in the collector table,
        // and they will be processed in the next run of the job.
        $maxId = $this->getMaxProgressiveId();

        if (is_null($maxId)) {
            return $prof->stop();
        }

        $remoteConn = $this->getExternalConnection();
        $dstResultFileName = null;
        $tmpResultFileName = null;
        $debugFileName = normalizeFileNamePath(ImportDataFiles::getMySQLAccessibleTmpDirectory(get_class($this)) . '/' . ManageRateEvent::IMPORT_DEBUG_FILE_NAME);
        $fileImportJob = new ImportDataFiles();

        $again = true;
        while ($again) {

            try {
                if (!is_null($maxId)) {
                    if ($this->isLocalConnection()) {
                        // MySQL requires that the file is in tmp directory.
                        // I assign a unique name based on the installation directory, because in this way I'm sure it is unique.
                        $resultDirectory = ImportDataFiles::getMySQLAccessibleTmpDirectory(get_class($this));
                    } else {
                        // this is a directory on the same file-system of the archive message directory, so file move is an atomic operation.
                        $resultDirectory = normalizeFileNamePath(ImportDataFiles::getAbsoluteTmpDirectory());
                    }

                    if (!file_exists($resultDirectory)) {
                        $isOk = @mkdir($resultDirectory, 0777, true);
                        if ($isOk === FALSE) {
                            $problemDuplicationKey = "Can not create import directory - $resultDirectory";
                            $problemDescription = "The CDR importing procedure was unable to create directory \"$resultDirectory\".";
                            $problemEffect = "CDRs of the provider \"" . $this->getCdrProvider() . "\", will not be rated.";
                            $problemProposedSolution = "If the problem persist, contact the assistance.";
                            $p = ArProblemException::createWithGarbageCollection(
                                ArProblemType::TYPE_CRITICAL,
                                ArProblemDomain::APPLICATION,
                                null,
                                $problemDuplicationKey,
                                $this->getGarbageKey(),
                                $this->getGarbageFromDate(),
                                $this->getGarbageToDate(),
                                $problemDescription,
                                $problemEffect,
                                $problemProposedSolution);
                            throw ($p);
                        }
                    }

                    $fileName = $this->getUniqueFileName();
                    $tmpResultFileName = normalizeFileNamePath($resultDirectory . '/' . $fileName);
                    $dstResultFileName = normalizeFileNamePath(ImportDataFiles::getAbsoluteInputDirectory() . '/' . $fileName);
                    @unlink($tmpResultFileName);
                    @unlink($dstResultFileName);

                    $query = $this->getCSVExportingQuery($tmpResultFileName);
                    if ($this->isLocalConnection()) {
                        $stmt = $remoteConn->prepare($query);
                        if ($stmt === FALSE) {
                            $isOk = FALSE;
                        } else {
                            $isOk = $stmt->execute(array($maxId));
                        }

                    } else {

                        // Process the file using PHP

                        $h = fopen($tmpResultFileName, 'w');
                        if ($h === FALSE) {
                            $problemDuplicationKey = "Can not create tmp result file - $tmpResultFileName";
                            $problemDescription = "The CDR importing procedure was unable to create file \"$tmpResultFileName\".";
                            $problemEffect = "CDRs of the provider \"" . $this->getCdrProvider() . "\", will not be rated.";
                            $problemProposedSolution = "If the problem persist, contact the assistance.";
                            $p = ArProblemException::createWithGarbageCollection(
                                ArProblemType::TYPE_CRITICAL,
                                ArProblemDomain::APPLICATION,
                                null,
                                $problemDuplicationKey,
                                $this->getGarbageKey(),
                                $this->getGarbageFromDate(),
                                $this->getGarbageToDate(),
                                $problemDescription,
                                $problemEffect,
                                $problemProposedSolution);
                            throw ($p);
                        }

                        $isOk = TRUE;
                        $stmt = $remoteConn->prepare($query);
                        if ($stmt === FALSE) {
                            $isOk = FALSE;
                        } else {
                            $stmt->execute(array($maxId));
                            while ((($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) && (!($isOk === FALSE))) {
                                $isOk = safe_fputcsv($h, $rs, ",", '"');
                            }
                            $stmt->closeCursor();
                            $isOk = fclose($h) && $isOk;
                        }
                    }

                    if ($isOk === FALSE) {
                        $problemDuplicationKey = "Can not execute CDR import query - " . get_class($this);
                        $problemDescription = "Unable to retrieve and process CDRs from provider \"" . $this->getCdrProvider() . "\", using a database connection/table, according the code in class " . get_class($this) . '.' . "\nThe executed query is \n$query\n";
                        $problemEffect = "CDRs of the provider \"" . $this->getCdrProvider() . "\", will not be rated.";
                        $problemProposedSolution = "If the problem persist, contact the assistance.";
                        $p = ArProblemException::createWithGarbageCollection(
                            ArProblemType::TYPE_CRITICAL,
                            ArProblemDomain::APPLICATION,
                            null,
                            $problemDuplicationKey,
                            $this->getGarbageKey(),
                            $this->getGarbageFromDate(),
                            $this->getGarbageToDate(),
                            $problemDescription,
                            $problemEffect,
                            $problemProposedSolution);
                        throw ($p);
                    }

                    $query = $this->getSetExportedCDRSQuery();
                    if (!is_null($query)) {
                        // signal on the remote table the CDRS that are already imported
                        $stmt = $remoteConn->prepare($query);
                        if ($stmt === FALSE) {
                            $isOk = FALSE;
                        } else {
                            $isOk = $stmt->execute(array($maxId));
                        }
                        if ($isOk === FALSE) {
                            $problemDuplicationKey = "Can not execute CDR set as imported query - " . get_class($this);
                            $problemDescription = "Unable to set as already processed the CDRs of the provider \"" . $this->getCdrProvider() . "\", using a database connection/table, according the code in class " . get_class($this) . '.' . "\nThe executed query is \n$query\n";
                            $problemEffect = "CDRs of the provider \"" . $this->getCdrProvider() . "\", will not be rated.";
                            $problemProposedSolution = "If the problem persist, contact the assistance.";
                            $p = ArProblemException::createWithGarbageCollection(
                                ArProblemType::TYPE_CRITICAL,
                                ArProblemDomain::APPLICATION,
                                null,
                                $problemDuplicationKey,
                                $this->getGarbageKey(),
                                $this->getGarbageFromDate(),
                                $this->getGarbageToDate(),
                                $problemDescription,
                                $problemEffect,
                                $problemProposedSolution);
                            throw ($p);
                        }
                    } else {
                        // take note of the imported ID later
                    }

                    // Move the file to the input directory. It will be imported later.
                    $isOk = @rename($tmpResultFileName, $dstResultFileName);
                    if ($isOk === FALSE) {
                        $problemDuplicationKey = "Can not move file - " . $this->getGarbageKey();
                        $problemDescription = "Unable to move file \"$tmpResultFileName\" to \"$dstResultFileName\".";
                        $problemEffect = "CDRs of the provider \"" . $this->getCdrProvider() . "\", will not be rated.";
                        $problemProposedSolution = "If the problem persist, contact the assistance.";
                        $p = ArProblemException::createWithGarbageCollection(
                            ArProblemType::TYPE_CRITICAL,
                            ArProblemDomain::APPLICATION,
                            null,
                            $problemDuplicationKey,
                            $this->getGarbageKey(),
                            $this->getGarbageFromDate(),
                            $this->getGarbageToDate(),
                            $problemDescription,
                            $problemEffect,
                            $problemProposedSolution);
                        throw ($p);
                    }
                    // NOTE: PHP has no fsync support, so hope that the file is effectively written on the file system.
                    // NOTE: also in case of direct writing to the database, we can not close at the same time the remote
                    // and local connection, for being sure it is all ok.

                    if ($this->manageExportUsingIdField()) {
                        $fileImportJob->processFile($dstResultFileName, $debugFileName);
                        // NOTE: in case of ID method, we can safely import the file,
                        // and if it is all ok (committed on local DB) signal the imported ID at the end.
                        // Signaling the ID is a fast operation and likely to be committed.

                        $provider = ArCdrProviderPeer::retrieveByName($this->getCdrProvider());
                        $provider->setLastImportedId($maxId);
                        $provider->save();
                    }

                    $prof->addToProcessedUnits(filesize($dstResultFileName));

                }
                if ($this->manageExportUsingIdField()) {
                    // NOTE: in this case there is no transaction for signaling the already transferred CDRS
                    // because it is a simply annotation of last retrieved ID.
                } else {
                    $this->commitTransactionOrSignalProblem($remoteConn);
                }

            } catch (ArProblemException $e) {
                if (!is_null($dstResultFileName)) {
                    @unlink($dstResultFileName);
                }
                if (!is_null($tmpResultFileName)) {
                    @unlink($tmpResultFileName);
                }

                $this->maybeRollbackTransaction($remoteConn);

                throw($e);

            } catch (Exception $e) {
                if (!is_null($dstResultFileName)) {
                    @unlink($dstResultFileName);
                }
                if (!is_null($tmpResultFileName)) {
                    @unlink($tmpResultFileName);
                }

                $this->maybeRollbackTransaction($remoteConn);

                $problemDuplicationKey = $this->getGarbageKey() . " - unexpected error - " . $e->getCode();
                $p = ArProblemException::createFromGenericExceptionWithGarbageCollection(
                    $e,
                    ArProblemType::TYPE_CRITICAL,
                    $problemDuplicationKey,
                    $this->getGarbageKey(),
                    $this->getGarbageFromDate(),
                    $this->getGarbageToDate(),
                    "Unable to retrieve and process CDRs from provider \"" . $this->getCdrProvider() . "\", using a database connection/table, according the code in class " . get_class($this),
                    "CDRs of the provider \"" . $this->getCdrProvider() . "\", will not be rated.",
                    "If the problem persist contact the assistance."
                );

                throw($p);
            }
        }

        return $prof->stop();
    }

    // --------------------------------------------
    // Remove from the table/queue the old CDRS

    /**
     * Delete old CDRs from the collector/queue.
     *
     * @return string log
     * @throws ArProblemException
     */
    public
    function maybeDeleteOldCDRS()
    {
        if ($this->removeExportedCDRSOlderThanDays() > 0) {
            // use a separate garbage key because it is a separated logical job,
            // otherwise every time it is not executed, the error is removed.
            $garbageKey = "delete-cdrs-" . $this->getGarbageKey();
            ArProblemException::garbageCollect($garbageKey, null, null);

            $conn = $this->getExternalConnection();

            $checkFile = self::ARCHIVE_JOB_PREFIX . $this->getGarbageKey();
            $checkLimit = strtotime("-" . $this->getHowMuchCheckCDRSOlderThanDays() . " minutes");
            $mutex = new Mutex($checkFile);
            if ($mutex->maybeTouch($checkLimit)) {
                $prof = new JobProfiler(" job");

                $dateLimit = strtotime('-' . $this->removeExportedCDRSOlderThanDays() . ' days');
                if (!$this->isCallDateFieldATimestamp()) {
                    $dateLimit = fromUnixTimestampToMySQLTimestamp($dateLimit);
                }

                $query = 'DELETE FROM ' . $this->getCollectorTableName() . ' WHERE ' . $this->getCallDateField() . ' < ? ';
                $query .= ' AND ' . $this->getConditionOnCDRToBeExported(false);
                try {
                    // NOTE: do not use a transaction, because it does not play nice on some remote database,
                    // and there is no critical logic inside this simple query
                    $stmt = $conn->prepare($query);
                    $stmt->execute(array($dateLimit));
                } catch (Exception $e) {
                    $problemDuplicationKey = $this->getGarbageKey() . " - unexpected error during delete - " . $e->getCode();
                    $p = ArProblemException::createFromGenericExceptionWithGarbageCollection(
                        $e,
                        ArProblemType::TYPE_ERROR,
                        $problemDuplicationKey,
                        $garbageKey,
                        $this->getGarbageFromDate(),
                        $this->getGarbageToDate(),
                        "Unable to delete old imported CDRs from provider \"" . $this->getCdrProvider() . "\", using the database connection/table \"" . $this->getCollectorTableName() . "\", according the code in class " . get_class($this) . ", executing the query \"$query\".",
                        "Old CDRs of the provider \"" . $this->getCdrProvider() . "\", will be not removed from the table/collector. The table content can grow too much overtime (1-10 years time-frame).",
                        "If the problem persist contact the assistance."
                    );
                    throw($p);
                }
                return $prof->stop();
            }

            return '';
        } else {

            // nothing to do
            return '';
        }
    }

    // ---------------------------------
    // Create an Haskell code template
    // for processing data.

    public function getDevNotes()
    {
        $r = "### Haskell Rating Engine Code Template\n";
        $r .= $this->getHaskellCodeTemplate();
        $r .= "\n";
        return $r;
    }

    protected function strHaskellize($str)
    {
        $s1 = str_replace('-', '_', $str);
        return $s1;
    }

    /**
     * @return string some Haskell code template to use for processing the CSV file produced from this importer.
     */
    protected function getHaskellCodeTemplate()
    {
        $formatName = 'F__' . $this->strHaskellize($this->getLogicalType()) . '__' . $this->strHaskellize($this->getPhysicalType());
        $recPrefix = $this->strHaskellize($this->getLogicalType()) . '__' . $this->strHaskellize($this->getPhysicalType()) . '__';

        $nrOfFields = count($this->getListOfFields());

        $r = "

data $formatName
  = $formatName {
";

        foreach ($this->getListOfFields() as $f1) {
            $f = $this->strHaskellize($f1);
            $r .= "\n          , $recPrefix" . "__" . $f . ":: !(ExportMaybeNull Text.Text)";
        }

        $r .= "\n  } deriving(Show)\n";

        $r .= "

instance FromRecord $formatName where
     parseRecord v =
         let expectedCols = $nrOfFields
         in case V.length v == expectedCols of
              True
                -> $formatName <$>

";
        $i = 0;
        foreach ($this->getListOfFields() as $f) {
            $r .= "\n                     v .! " . $i . "<*>";
            $i++;
        }

        $r .= "
              False
                -> fail $ \"There are \" ++ show (V.length v) ++ \" columns instead of the expected \" ++ (show expectedCols)

";
        return $r;
    }
}
