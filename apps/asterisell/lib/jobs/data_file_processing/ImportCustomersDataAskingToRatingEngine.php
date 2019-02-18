<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Call the rating engine, invoking a `--customers-import` command,
 * for importing customers, extensions/DIDS.
 * Retrieve the all/majority of params from YAML connection params.
 * Retrieve the list of customer providers from the connection params.
 */
class ImportCustomersDataAskingToRatingEngine extends FixedJobProcessor
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
        return "import-remote-customers-";
    }

    /**
     * @return String log
     * @throws ArProblemException
     */
    public
    function process()
    {
        $prof = new JobProfiler('remote imports');

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
        $jobName = $params['name'] . '-' . $params['provider'];
        $timeFrameInMinutes = intval($params['timeFrameInMinutes']);
        $checkLimit = strtotime("-$timeFrameInMinutes minutes");
        $mutex = new Mutex($jobName);

        if ($mutex->maybeTouch($checkLimit)) {

            $prof = new JobProfiler($jobName);
            $prof->addToProcessedUnits(1);

            $garbageKey = $jobName;
            ArProblemException::garbageCollect($garbageKey, null, null);

            $currencyPrecision = sfConfig::get('app_currency_decimal_places');


            list($localDBName, $localDBUser, $localDBPassword) = getDatabaseNameUserAndPassword(true);

            $engineParams = array();
            $engineParams['remote-db-user'] = $params['user'];
            $engineParams['remote-db-name'] = $params['dbName'];
            $engineParams['remote-db-password'] = $params['password'];

            $cmd = RateEngineService::getToolExecutable()
                . ' --customers-import ' . $params['dataSourceFormat']
                . ' --import ' . RateEngineService::writeParams($engineParams)
                . ' --data-source-name ' . $params['provider']
                . ' --organization-to-ignore "' . sfConfig::get('app_organization_to_ignore') . '" '
                . ' --currency-precision ' . $currencyPrecision
                . ' --db-host ' . $params['host']
                . ' --db-port ' . $params['port']
                . ' ' . ManageRateEvent::DONT_TOUCH_DEBUG_MODE_GHC_PARAMS;

            if (JobQueueProcessor::$IS_INTERACTIVE) {
                echo "\nExecuted:\n" . $cmd;
            }

            $output = array();
            $exitStatus = 0;
            $resultLine = exec($cmd, $output, $exitStatus);

            if (JobQueueProcessor::$IS_INTERACTIVE) {
                echo implode("\n", $output);
            }

            if ($exitStatus != 0) {
                ArProblemException::createWithGarbageCollection(
                    ArProblemType::TYPE_CRITICAL,
                    ArProblemDomain::CONFIGURATIONS,
                    ArProblemResponsible::ADMIN,
                    $jobName,
                    $garbageKey,
                    null,
                    null,
                    "There is an error in job $jobName, not allowing importing of remote customers data. " . implode("\n", $output),
                    "Customer data is not correctly imported/updated and some calls can be not rated or wrongly rated.",
                    "Check if the remote server is reachable, and if connection params are correct. This is the issued command: " . $cmd
                );
            }

            return $prof->stop();
        } else {
            return "$jobName will be executed later, every $timeFrameInMinutes minutes.";
        }
    }
}
