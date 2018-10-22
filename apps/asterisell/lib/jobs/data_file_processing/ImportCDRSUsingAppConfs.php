<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Import CDRS using the configurations specified on connection params.
 * Assumes that there is an `ID` or `id` auto incrementing field for any table to import.
 * Use the result of `SHOW COLUMN` command for discovering fields.
 * If the format of the table change, then a new version of the format must be configured,
 * so the Haskell specific importer can manage it.
 */
class ImportCDRSUsingAppConfs extends ImportCDRSFromDatabase
{

    /**
     * @return string all the connection params specified in `asterisell_instances.py`,
     * having this prefix will be used for importing data.
     * Doing so one can import from multiple sources without adding explicit jobs,
     * in the configuration, but only adding new connection params.
     *
     * The parameter 'dataSourceFormat' of the connection is used as import method name.
     * The parameter `provider` of the connection is used as data-source-name.
     * The parameter `timeFrameInMinutes` is used for postponing the execution.
     */
    public function getConnectionNamePrefix()
    {
        // DEV-NOTE: if you change this value, update also `asterisell_instances.py` documentation.
        return "import-remote-cdrs-";
    }

    /**
     * @return String log
     * @throws ArProblemException
     */
    public
    function process()
    {
        $prof = new JobProfiler('CDRS providers');

        $log = '';

        $i = -1;

        while (true) {
            $i++;
            $params = getConnectionParams($this->getConnectionNamePrefix(), true, true, $i);
            if (is_null($params)) {
                break;
            } else {
                $log .= $this->processConnection($params) . "\n";
                $prof->incrementProcessedUnits();
            }
        }

        return $prof->stop() . "\nDetails: ";
    }

    /**
     * @param array $params process a connection
     * @return string stats about the computation
     * Signal problems in the error table, without generating an exception.
     */
    public function processConnection($params)
    {
        $jobName = $params['name'] . '-' . $params['provider'] . '-' . get_class($this);
        $timeFrameInMinutes = intval($params['timeFrameInMinutes']);
        $checkLimit = strtotime("-$timeFrameInMinutes minutes");
        $mutex = new Mutex($jobName);

        if ($mutex->maybeTouch($checkLimit)) {
            try {
                $this->initJobParamsUsingConnectionParams($params);
                return $this->importCDRS();
            } catch (ArProblemException $e) {
                return 'Error during processing of ' . $jobName;
            } catch (Exception $e) {
                $problemDuplicationKey = $this->getGarbageKey() . " - unexpected error - " . $e->getCode();
                ArProblemException::createFromGenericExceptionWithGarbageCollection(
                    $e,
                    ArProblemType::TYPE_CRITICAL,
                    $problemDuplicationKey,
                    $this->getGarbageKey(),
                    $this->getGarbageFromDate(),
                    $this->getGarbageToDate(),
                    "Unable to retrieve and process CDRs of the provider " . $this->getCdrProvider(),
                    "CDRs of the provider \"" . $this->getCdrProvider() . "\", will not be rated.",
                    "Fix the connection params, or inspect the reason of the exception."
                );
                return 'Error during processing of ' . $jobName;
            }
        } else {
            return "$jobName will be executed later, every $timeFrameInMinutes minutes.";
        }
    }

    // ------------------------------------------
    // Implement ImportCDRSFromDatabase interface

    // DEV-NOTE: if you add caching fields here, make sure to init them in `initJobParamsUsingConnectionParams`
    protected
        $params = null;

    protected
        $fieldsToExport = null;

    /**
     * @var PDO
     */
    protected
        $remoteConn = null;

    protected
        $progressiveIDField = null;

    /**
     * Init the `ImportCDRSFromDatabase` interface according the content of $params
     * @param array $params connection params
     */
    protected
    function initJobParamsUsingConnectionParams($params)
    {
        $this->params = $params;
        $this->fieldsToExport = null;
        $this->progressiveIDField = null;
        $this->remoteConn = null;

        $provider = ArCdrProviderPeer::retrieveByName($this->getCdrProvider());
        if (is_null($provider)) {
            $provider = new ArCdrProvider();
            $provider->setInternalName($this->getCdrProvider());
            $provider->save();
        }
    }

    protected
    function getGarbageKey()
    {
        // NOTE: call directly for avoiding an infinite loop during error signalation
        return $this->params['name'];
    }

    public
    function getConnectionName()
    {
        // NOTE: call directly for avoiding an infinite loop during error signalation
        return $this->params['name'];
    }

    public
    function getCollectorDatabaseName()
    {
        return $this->getParamValueOrSignalError('dbName');
    }

    public
    function getCollectorTableName()
    {
        return $this->getParamValueOrSignalError('tableName');
    }

    public
    function getCdrProvider()
    {
        return $this->getParamValueOrSignalError('provider');
    }

    public
    function getLogicalType()
    {
        return $this->getParamValueOrSignalError('dataSourceFormat');
    }

    public
    function getPhysicalType()
    {
        return $this->getParamValueOrSignalError('dataSourceVersion');
    }

    /**
     * @param $name
     * @return string
     * @throws ArProblemException
     */
    protected function getParamValueOrSignalError($name)
    {
        if (array_key_exists($name, $this->params)) {
            return $this->params[$name];
        } else {
            $problemDuplicationKey = "Missing param field " . $name . " - " . $this->getGarbageKey();
            $problemDescription = "In params configurations for " . $this->params['name'] . " there is no field " . $name;
            $problemEffect = "CDRs of this connection will not be imported and rated.";
            $problemProposedSolution = "Complete the missing params.";
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
    }

    public
    function getListOfFields()
    {
        if (is_null($this->fieldsToExport)) {
            $query = 'SELECT COLUMN_NAME
                      FROM information_schema.COLUMNS
                      WHERE TABLE_SCHEMA = ?
                      AND TABLE_NAME = ?
                      ORDER BY ORDINAL_POSITION';
            try {
                $stmt = $this->getConnection()->prepare($query);
                $isOk = $stmt->execute(array($this->getCollectorDatabaseName(), $this->getCollectorTableName()));
                if (!$isOk) {
                    throw (new Exception("Error executing query"));
                }
                $this->fieldsToExport = array();
                $this->progressiveIDField = null;
                while ((($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false)) {
                    $fieldName = $rs[0];
                    $this->fieldsToExport[] = $fieldName;
                    if (strcmp('id', strtolower($fieldName)) == 0) {
                        $this->progressiveIDField = $fieldName;
                    }
                }
                $stmt->closeCursor();
            } catch (Exception $e) {
                $problemDuplicationKey = "error in list of fields query " . $this->getGarbageKey();
                $problemDescription = "Can not access the list of fields for " . $this->getCollectorDatabaseName() . "." . $this->getCollectorTableName() . " of provider " . $this->getCdrProvider() . ". The query is " . $query;
                $problemEffect = "CDRs of the provider \"" . $this->getCdrProvider() . "\", will not be imported and rated.";
                $problemProposedSolution = "The MySQL user configured in fabric_data/asterisell_instances.py must have the right priorities, and access to the DBMS.";
                $p = ArProblemException::createFromGenericExceptionWithGarbageCollection(
                    $e,
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

            if (is_null($this->progressiveIDField)) {
                $problemDuplicationKey = "Missing ID field " . $this->getGarbageKey();
                $problemDescription = "There is no ID field in table " . $this->getCollectorDatabaseName() . "." . $this->getCollectorTableName() . " of provider " . $this->getCdrProvider() . ".";
                $problemEffect = "CDRs of the provider \"" . $this->getCdrProvider() . "\", will not be imported and rated.";
                $problemProposedSolution = "The code of service " . get_class($this) . " can only manage tables with an ID field.";
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
        }

        return $this->fieldsToExport;
    }

    public
    function getExportedStatusBooleanField()
    {
        return null;
    }

    public
    function manageExportUsingIdField()
    {
        return true;
    }

    public
    function getValueOfCDRToBeExported()
    {
        return null;
    }

    public
    function getProgressiveField()
    {
        if (is_null($this->progressiveIDField)) {
            $this->getListOfFields();
        }

        return $this->progressiveIDField;
    }

    public
    function getCallDateField()
    {
        return 'NOT_USED';
    }

    public
    function isCallDateFieldATimestamp()
    {
        return true;
    }

    protected
    function getConnection()
    {
        if (is_null($this->remoteConn)) {
            $this->remoteConn = parent::getConnection();
        }

        return $this->remoteConn;
    }

    protected
    function getHaskellCodeTemplate()
    {
        return "NOT-SUPPORTED";
    }
}
