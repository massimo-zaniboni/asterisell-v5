<?php

/* $LICENSE 2014:
 *
 * Copyright (C) 2014 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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
 * Read CDRs on a table of a (maybe external) database, convert to CSV files for rating them.
 *
 * Manage other related activities.
 *
 * This is an abstract class, that must be customized in inherited subclasses, for performing the real work.
 *
 * NOTE: the remote server character set must be not specified, because it is converted automatically to UTF8.
 *
 * DEV-NOTE: an imported CDR can not be converted to a standard rating format in this phase, because
 * otherwise the backup of CSV files is not a exact copy of the source table content.
 *
 * DEV-NOTE: storing some info on file-system, and other info on the database can broke some ACID properties
 * of the involved transactions, because the database can not be in sync with file-system, in case of server fault during writings.
 * So special code is added, for discovering and signaling these problems. Then missing CDRs can be manually imported again.
 */
abstract class ImportCDRSFromDatabase extends FixedJobProcessor
{

    const ARCHIVE_JOB_PREFIX = 'old_cdrs_';

    //
    // Methods to customize in subclasses.
    // Make sure to respect the requirements in method headers.
    //

    /**
     * @return string|null null if the connection is the same of the Asterisell instance (same host, same database).
     * Otherwise the name of the connection.
     * The connection params are specified in `app.yml` in the connection settings.
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
     * The name of the field on getLocalTableName() that indicates if the CDR is exported or not.
     *
     * There should be an index on this field, for a speedup of the exporting process.
     * Usual something like:
     *
     * > ALTER TABLE <collector_table> ADD COLUMN is_exported_to_asterisell TINYINT(1) NOT NULL DEFAULT 0;
     * > ALTER TABLE <collector_table> ADD INDEX index_cdr_is_exported_to_asterisell(is_exported_to_asterisell);
     *
     * @return string SQL table field name
     */
    abstract public function getExportedStatusBooleanField();

    /**
     * @return bool true if CDRs to be exported have getExportedStatusBooleanField equal to 1,
     * false if CDRs to be exported have value 0 on this field.
     */
    abstract public function getValueOfCDRToBeExported();

    /**
     * A field in the getLocalTableName() that is progressive on the new generated CDRs.
     * The requirements are that a new generated CDR has a value of this field greater than previous CDRs.
     * Do not use the calldate, because there can be pending calls, started before already finished and added calls.
     * Usually this field is an autoincrement ID.
     *
     * @return string SQL table field name
     */
    abstract public function getProgressiveField();

    /**
     * A CDR can optionally be safely removed, because all the records of the collector are exported in CSV format,
     * and so it contains repeated information.
     *
     * @return int 0 for not removing the CDRs,
     * otherwise the number of the days after an already exported CDR can be removed/deleted from the collector table.
     */
    public function removeExportedCDRSOlderThanDays()
    {
        return 0;
    }

    /**
     * @return int minutes to wait, before checking for old CDRs to remove.
     */
    public function getHowMuchCheckCDRSOlderThanDays()
    {
        return 60 * 2;
    }

    /**
     * @return string the name of the field to use for calldate of CDRs.
     */
    abstract public function getCallDateField();

    /**
     * @return bool true if the getCalldateField is a TIMESTAMP (numeric value),
     * or false if the field is a DATETIME (string value).
     */
    abstract public function isCallDateFieldATimestamp();

    /**
     * @return string additional query conditions for CDRs to be exported. Important: start with an ' AND ' this condition.
     */
    public function getAdditionalQueryConditions()
    {
        return '';
    }

    //
    // Job Processor Interface
    //

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
     * @param bool $filterToBeExported true for selecting the CDRs to be exported, false for selecting the exported CDRs
     * @param bool $useForSet true for use in a set command
     * @return string
     */
    protected function getConditionOnCDRToBeExported($filterToBeExported, $useForSet = false)
    {
        if ($this->getValueOfCDRToBeExported()) {
            $exported = '1';
            $notExported = '0';
        } else {
            $exported = '0';
            $notExported = '1';
        }

        $r = ' ' . $this->getExportedStatusBooleanField() . ' = ';
        if ($filterToBeExported) {
            $r .= $exported;
        } else {
            $r .= $notExported;
        }

        if ($useForSet === FALSE) {
            $r .= ' ' . $this->getAdditionalQueryConditions();
        }

        return $r;
    }

    /**
     * @param string|null $fileName the file containing the result if it is a local connection
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
        CHARACTER SET  'utf8'
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

    /**
     * @return string the query for signaling the CDRs as exported, with a parameter on progressive limit
     */
    protected function getSetExportedCDRSQuery()
    {
        $r = 'UPDATE ' . $this->getCollectorTableName() . ' SET ' . $this->getConditionOnCDRToBeExported(false, true);
        $r .= ' WHERE ' . $this->getConditionOnCDRToBeExported(true);
        $r .= ' AND ' . $this->getProgressiveField() . ' <= ? ;';

        return $r;
    }

    /**
     * @return mixed the max value of progressive limit to consider for current run of exporting process
     */
    protected function getMaxProgressiveLimitQuery()
    {
        $r = 'SELECT MAX(' . $this->getProgressiveField() . ') FROM ' . $this->getCollectorTableName()
            . ' WHERE ' . $this->getConditionOnCDRToBeExported(true);

        return $r;
    }

    protected function getUniqueFileName()
    {
        return 'cdrs_' . get_ordered_timeprefix_with_unique_id() . '.' . $this->getCdrProvider() . '__' . $this->getLogicalType() . '__' . $this->getPhysicalType();
    }

    public function getDevNotes() {
        $r = "### Haskell Rating Engine Code Template\n";
        $r .= $this->getHaskellCodeTemplate();
        $r .= "\n";
        return $r;
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
     * @return PDO
     * @throws ArProblemException
     */
    protected function getConnection() {
        $conf_connectionString = null;

        $conn = null;

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
                    get_class($this),
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
                $conn->setAttribute(PDO::MYSQL_ATTR_USE_BUFFERED_QUERY, false);

                // The client (this app) will send utf8 strings to the remote server.
                // The remote server must convert the results to utf8, before sending to the client.
                // The character set used on the server, is automatically converted to utf8 by the server.
                $conn->exec("SET NAMES 'utf8';");

            } catch (Exception $e) {
                $problemDuplicationKey = get_class($this) . " - no connection - ";
                $p = ArProblemException::createFromGenericExceptionWithGarbageCollection(
                    $e,
                    ArProblemType::TYPE_CRITICAL,
                    $problemDuplicationKey,
                    get_class($this),
                    $this->getGarbageFromDate(),
                    $this->getGarbageToDate(),
                    "Unable to retrieve and process CDRs from provider \"" . $this->getCdrProvider() . "\", using a database connection/table, according the code in class " . get_class($this) . ". There is an error opening the connection with name \"$connectionName\".",
                    "CDRs of the provider \"" . $this->getCdrProvider() . "\", will not be rated.",
                    "Configure correctly the params of the connection, inside file \"apps/asterisell/config/app.yml\"."
                );

                throw($p);
            }
        }

        return $conn;
    }

    /**
     * @return string log
     * @throws ArProblemException
     */
    public
    function importCDRS()
    {
        $prof = new JobProfiler('bytes');

        ArProblemException::garbageCollect(get_class($this), $this->getGarbageFromDate(), $this->getGarbageToDate());

        $conn = $this->getConnection();

        $dstResultFileName = null;
        $tmpResultFileName = null;
        $debugFileName = normalizeFileNamePath(ImportDataFiles::getMySQLAccessibleTmpDirectory(get_class($this)) . '/' . ManageRateEvent::IMPORT_DEBUG_FILE_NAME);

        $fileImportJob = new ImportDataFiles();

        try {
            $conn->beginTransaction();

            // NOTE: I'm using maxId because during query execution new CDRs can be inserted in the collector table,
            // and they will be processed in the next run of the job.
            $query = $this->getMaxProgressiveLimitQuery();
            $stmt = $conn->prepare($query);
            $stmt->execute();
            $maxId = NULL;
            while ((($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false)) {
                $maxId = $rs[0];
            }
            $stmt->closeCursor();

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
                            get_class($this),
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
                    $stmt = $conn->prepare($query);
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
                            get_class($this),
                            $this->getGarbageFromDate(),
                            $this->getGarbageToDate(),
                            $problemDescription,
                            $problemEffect,
                            $problemProposedSolution);
                        throw ($p);
                    }

                    $isOk = TRUE;
                    $stmt = $conn->prepare($query);
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
                        get_class($this),
                        $this->getGarbageFromDate(),
                        $this->getGarbageToDate(),
                        $problemDescription,
                        $problemEffect,
                        $problemProposedSolution);
                    throw ($p);
                }

                $query = $this->getSetExportedCDRSQuery();
                $stmt = $conn->prepare($query);
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
                        get_class($this),
                        $this->getGarbageFromDate(),
                        $this->getGarbageToDate(),
                        $problemDescription,
                        $problemEffect,
                        $problemProposedSolution);
                    throw ($p);
                }

                $isOk = @rename($tmpResultFileName, $dstResultFileName);
                if ($isOk === FALSE) {
                    $problemDuplicationKey = "Can not move file - " . get_class($this);
                    $problemDescription = "Unable to move file \"$tmpResultFileName\" to \"$dstResultFileName\".";
                    $problemEffect = "CDRs of the provider \"" . $this->getCdrProvider() . "\", will not be rated.";
                    $problemProposedSolution = "If the problem persist, contact the assistance.";
                    $p = ArProblemException::createWithGarbageCollection(
                        ArProblemType::TYPE_CRITICAL,
                        ArProblemDomain::APPLICATION,
                        null,
                        $problemDuplicationKey,
                        get_class($this),
                        $this->getGarbageFromDate(),
                        $this->getGarbageToDate(),
                        $problemDescription,
                        $problemEffect,
                        $problemProposedSolution);
                    throw ($p);
                }

                $fileImportJob->processFile($dstResultFileName, $debugFileName);
                // DEV-NOTE: import the file in the database archive.
                // In this way if there is a successfull COMMIT in the database,
                // but the file is not correctly transferred due to some fsync problem,
                // then the application will warn about a missing input file.
                // On the contrary if the file is moved, but the database does not commit,
                // then the CDRs will be exported again, and some space will be lost in the archive, until next garbage collection.
                // DEV-NOTE: when PHP support fsync, the best option is forcing an fsync before committing, in order to having the database and filesystem synchronized

                $prof->addToProcessedUnits(filesize($dstResultFileName));

            }
            $this->commitTransactionOrSignalProblem($conn);
        } catch (ArProblemException $e) {
            if (!is_null($dstResultFileName)) {
                @unlink($dstResultFileName);
            }
            if (!is_null($tmpResultFileName)) {
                @unlink($tmpResultFileName);
            }

            $this->maybeRollbackTransaction($conn);

            throw($e);

        } catch (Exception $e) {
            if (!is_null($dstResultFileName)) {
                @unlink($dstResultFileName);
            }
            if (!is_null($tmpResultFileName)) {
                @unlink($tmpResultFileName);
            }

            $this->maybeRollbackTransaction($conn);

            $problemDuplicationKey = get_class($this) . " - unexpected error - " . $e->getCode();
            $p = ArProblemException::createFromGenericExceptionWithGarbageCollection(
                $e,
                ArProblemType::TYPE_CRITICAL,
                $problemDuplicationKey,
                get_class($this),
                $this->getGarbageFromDate(),
                $this->getGarbageToDate(),
                "Unable to retrieve and process CDRs from provider \"" . $this->getCdrProvider() . "\", using a database connection/table, according the code in class " . get_class($this),
                "CDRs of the provider \"" . $this->getCdrProvider() . "\", will not be rated.",
                "If the problem persist contact the assistance."
            );

            throw($p);
        }

        return $prof->stop();
    }

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
            $garbageKey = "delete-cdrs-" . get_class($this);
            ArProblemException::garbageCollect($garbageKey, null, null);

            $conn = $this->getConnection();

            $checkFile = self::ARCHIVE_JOB_PREFIX . get_class($this);
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
                    $conn->beginTransaction();

                    $stmt = $conn->prepare($query);
                    $stmt->execute(array($dateLimit));
                    $this->commitTransactionOrSignalProblem($conn);

                } catch (ArProblemException $e) {
                    $this->maybeRollbackTransaction($conn);

                    $problemDuplicationKey = get_class($this) . " - unexpected error during delete - " . $e->getCode();
                    $p = ArProblemException::createWithGarbageCollection(
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
                } catch (Exception $e) {
                    $this->maybeRollbackTransaction($conn);

                    $problemDuplicationKey = get_class($this) . " - unexpected error during delete - " . $e->getCode();
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

//
// Utility Functions
//

    protected function strHaskellize($str) {
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
