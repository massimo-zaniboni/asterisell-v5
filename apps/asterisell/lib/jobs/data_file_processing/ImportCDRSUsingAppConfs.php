<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Import CDRS from remote databases, using the configurations specified on `ConnectionParams`
 * inside `asterisell_instances.py`.
 *
 * This job is the counterpart of `ImportDataFiles`, but it works on remote databases,
 * instead of local files.
 *
 * It process connections starting with prefix 'import-remote-cdrs-'.
 * They are saved inside `app.yml` from `fabric_data/lib.py` in a way like this
 *
 * > connections:
 * >   -
 * >       name: import-remote-cdrs-sigma-abiliscpx
 * >       user: some-name
 * >       password: some-password
 * >       host: db.example.net
 * >       port: 443
 * >       dbName: some-name
 * >       tableName: some-name
 * >       provider: some-provider
 * >       timeFrameInMinutes: 0
 * >       dataSourceFormat: some-format
 * >       dataSourceVersion: some-version
 * >       fromDate: ''
 * >       removeOlderThanDays: 0
 *
 * DEV-NOTE: each connection is from a logical point of view a distinct job:
 * - generate distinct errors
 * - do not interrupt other jobs
 * - distinct garbage errors
 *
 */
class ImportCDRSUsingAppConfs extends FixedJobProcessor
{

    // --------------------
    // Logical job params

    /**
     * @var array the connection configuration key => values
     */
    protected $params = null;

    protected
    function getGarbageKey()
    {
        // NOTE: call directly for avoiding an infinite loop during error signalation
        return 'ImportCDRSUsingAppConfs - ' . $this->params['name'];
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
                null,
                null,
                $problemDescription,
                $problemEffect,
                $problemProposedSolution);
            throw ($p);
        }
    }

    /**
     * @param $name
     * @param $defaultValue
     * @return string
     * @throws ArProblemException
     */
    protected function getParamValue($name, $defaultValue)
    {
        if (array_key_exists($name, $this->params)) {
            return $this->params[$name];
        } else {
            return $defaultValue;
        }
    }

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

    // -------------------------
    // Job interface

    /**
     * @return String log
     * @throws ArProblemException
     */
    public
    function process()
    {
        $prof = new JobProfiler('Remote CDRS providers');

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

        return $prof->stop() . ' ' . $log;
    }

    /**
     * @param array $params process a connection
     * @return string stats about the computation
     * Signal problems in the error table, without generating an exception.
     */
    public function processConnection($params)
    {
        $this->params = $params;
        $jobDescription = 'Connection ' . $params['name'] . ', provider ' . $params['provider'];
        $jobName = $params['name'] . '-' . $params['provider'];
        $timeFrameInMinutes = intval($params['timeFrameInMinutes']);
        $checkLimit = strtotime("-$timeFrameInMinutes minutes");
        $mutex = new Mutex($jobName);

        if ($mutex->maybeTouch($checkLimit)) {
            try {
                $prof = new JobProfiler("CDRS");

                $this->params = $params;
                ArProblemException::garbageCollect($this->getGarbageKey(), null, null);

                $s = $this->getParamValue('fromDate', null);
                if (isEmptyOrNull($s)) {
                    $importFromDateS = fromUnixTimestampToMySQLTimestamp($this->getGlobalStartingDate());
                } else {
                    $importFromDateS = trim($s);
                }

                $cdrProviderId = CustomCDRServices::getInstance()->getCdrProviderId($this->getCdrProvider());

                if (is_null($cdrProviderId)) {
                    $this->signalError(
                        "cdr_provider - $jobDescription",
                        "Missing CDR provider " . $this->getCdrProvider() . " for $jobDescription",
                        "CDRS will be not imported.",
                        "Define the missing CDR provider inside Asterisell Web UI or improve the configuration of $jobDescription inside \"asterisell_instances.py\"",
                        true, ArProblemDomain::CONFIGURATIONS);
                }

                $logicalTypeId = CustomCDRServices::getInstance()->getLogicalTypeId($this->getLogicalType());
                $versionTypeId = CustomCDRServices::getInstance()->getLogicalTypeAndVersionId($this->getLogicalType(), $this->getPhysicalType());

                if (is_null($logicalTypeId) || is_null($versionTypeId)) {
                    $this->signalError(
                        "logical type - $jobDescription",
                        "Missing type " . $this->getLogicalType() . ", with version " . $this->getPhysicalType() . ",  for $jobDescription",
                        "CDRS will be not imported.",
                        "Fix the configuration of $jobDescription inside \"asterisell_instances.py\"",
                        true, ArProblemDomain::CONFIGURATIONS);
                }


                $cmd = RateEngineService::getToolExecutable()
                    . ' --import-from-db ' . RateEngineService::writeParams($params)
                    . ' --debug-mode 0'
                    . ' --provider ' . $this->getCdrProvider()
                    . ' --provider-id ' . $cdrProviderId
                    . ' --file-logical-type ' . $this->getLogicalType()
                    . ' --file-logical-type-id ' . $logicalTypeId
                    . ' --file-version-type ' . $this->getPhysicalType()
                    . ' --file-version-type-id ' . $versionTypeId
                    . ' --from-date "' . $importFromDateS . '"'
                    . ' '
                    . ManageRateEvent::DONT_TOUCH_DEBUG_MODE_GHC_PARAMS;

                if ($this->getDebugMode()) {
                    echo "\nExecute command: \n$cmd\n";
                }

                $output = array();
                $exitStatus = 0;
                $resultLine = exec($cmd, $output, $exitStatus);

                RateEngineService::signalIfThereAreCDRSAlreadyBilled($jobDescription);

                // NOTE: the file will be signaled as imported, from the Haskell rating engine

                if ($exitStatus == 0) {

                    $results = explode(',', $resultLine);
                    $i = 0;

                    // The result is composed of values separated from a ",":
                    // - minimum date
                    // - maximum date
                    // - total lines
                    // - tot lines with errors

                    if (count($results) < 4) {
                        $this->signalError(
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
                    $prof->addToProcessedUnits($totLines);
                    return "$jobDescription imported " . $prof->stop() . ", from $minDateStr to $maxDateStr. ";

                } else {
                    $this->signalError(
                        "error executing command $cmd",
                        "Error executing command \"$cmd\": " . implode("\n", $output),
                        "CDRS on $jobDescription will be not imported.",
                        "Contact the assistance.",
                        true);

                    // NOTE: never called because the exceeption is raised first
                    return '';
                }
            } catch (ArProblemException $e) {
                // nothing to do, already signaled
                return "Error during processing of $jobDescription";

            } catch (Exception $e) {
                $this->signalError(
                    "675 - $jobDescription",
                    "Unable to process CDRS on $jobDescription Error: " . $e->getMessage(),
                    "CDRS will be not imported, and an attempt will be done at next execution of job processor.",
                    "If the problem persist, contact the assistance.",
                    false // do not throw the error, but only signal
                );
                return "Error during processing of $jobDescription";
            }
        } else {
            return "$jobDescription will be executed later, every $timeFrameInMinutes minutes.";
        }
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
    public function signalError($key, $description, $effect, $solution, $throw = true, $problemDomain = ArProblemDomain::APPLICATION)
    {
        $garbageKey = $this->getGarbageKey();
        $problemDuplicationKey = 'ImportCDRSUsingAppConfs' . ' - ' . $key;
        $problemDescription = $description;
        $problemProposedSolution = $solution;
        $problemEffect = $effect;
        $p = ArProblemException::createWithGarbageCollection(ArProblemType::TYPE_CRITICAL, $problemDomain, null, $problemDuplicationKey, $garbageKey, null, null, $problemDescription, $problemEffect, $problemProposedSolution);
        if ($throw) {
            throw($p);
        }
    }
}
