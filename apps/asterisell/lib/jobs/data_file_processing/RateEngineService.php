<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2020 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Call the services of the external rate engine tool.
 * This is a simple wrapper between the command line tool, and the PHP world.
 */
class RateEngineService
{
    const PARAMS_FILE = 'rating-params.csv';

    const TOOL_EXECUTABLE = 'scripts/RateEngine';

    const MAX_IMPORT_ERRORS_TO_SHOW = 50;

    /**
     * Here a custom job executed before the rating process,
     * can put specific instance params,
     * that can be read from the rating engine,
     * through the `--params ...` option.
     *
     * Useful for customizing the rating process, without modifying
     * the public accessible code of the rating engine.
     *
     * @var array name => value
     */
    static public $custom_initial_params = array();

    static public function getToolExecutable()
    {
        $opts = " ";
        return normalizeFileNamePath(getAsterisellCompleteRootDirectory() . '/' . self::TOOL_EXECUTABLE) . $opts;
    }

    /**
     * @param string $d
     * @param string $garbageKey
     * @param int $garbageFrom
     * @param int $garbageTo
     * @return int
     * @throws ArProblemException
     */
    protected static function  fromMySQLTimestampToUnixTimestampAndCheck($d, $garbageKey, $garbageFrom, $garbageTo)
    {
        $r = fromMySQLTimestampToUnixTimestamp($d);
        if (is_null($r)) {
            throw(ArProblemException::createWithGarbageCollection(
                ArProblemType::TYPE_CRITICAL,
                ArProblemDomain::APPLICATION,
                null,
                'RateEngineService bad dateformat - ',
                $garbageKey,
                $garbageFrom,
                $garbageTo,
                "Received a bad date format from the RatingEngine: \"$d\"",
                "This Source Data CSV file will be not imported. Stats about not rated CDRs are not updated in this case, and all the CDRs on the CSV file are not rated.",
                "This is an error in application code. Contact the assistance."
            ));
        }
        return $r;
    }

    /**
     * @return bool true in case of success of tests, false otherwise
     */
    static public function executeUpdateAllCachedCDRS()
    {

        $ratingParams = array();
        $cmd = self::getToolExecutable()
            . ' --update-all-cached-cdrs '
            . ' --params ' . self::writeParams($ratingParams);

        // Execute the command
        $discard = array();
        $exitStatus = 0;
        $resultLine = exec($cmd, $discard, $exitStatus);

        if ($exitStatus != 0) {
            return false;
        } else {
            return true;
        }
    }

    /**
     * Execute regression tests on the rating engine.
     *
     * @param int|null $referenceTime null for only testing if organizations are well specified
     * @param int $pass
     * @return bool true in case of success of tests, fals otherwise
     */
    static public function executeOrganizationTests($referenceTime, $pass)
    {

        if (is_null($referenceTime)) {
            $referenceTimeS = 'null';
        } else {
            $referenceTimeS = fromUnixTimestampToMySQLTimestamp($referenceTime);
        }

        $ratingParams = array();
        $cmd = self::getToolExecutable()
            . ' --test-organizations ' . $pass
            . ' --params ' . self::writeParams($ratingParams)
            . ' --from-date "' . $referenceTimeS . '" ';

        // Execute the command
        $discard = array();
        $exitStatus = 0;
        $resultLine = exec($cmd, $discard, $exitStatus);

        if ($exitStatus != 0) {
            return false;
        } else {
            return true;
        }
    }

    /**
     * @return bool true in case of success of tests, fals otherwise
     */
    static public function executeRateEngineTests()
    {

        // Create command

        $cmd = self::getToolExecutable()
            . ' --test';

        $discard = array();
        $exitStatus = 0;
        $resultLine = exec($cmd, $discard, $exitStatus);

        if ($exitStatus != 0) {
            return false;
        } else {
            return true;
        }
    }

    /**
     * Write all the params on a param file, to send to the engine.
     * Write also default params for accessing the database.
     *
     * @param array $params name => value array
     * @return string the name of the written file
     */
    static public function writeParams($params)
    {
        list($dbName, $dbUser, $dbPassword) = getDatabaseNameUserAndPassword(true);
        $params['db-name'] = $dbName;
        $params['db-user'] = $dbUser;
        $params['db-password'] = $dbPassword;

        $ratingParamsFileName = normalizeFileNamePath(ImportDataFiles::getMySQLAccessibleTmpDirectory('rating-services') . '/' . self::PARAMS_FILE);

        // NOTE: the custom params can override the code params
        $params2 = array_merge($params, self::$custom_initial_params);

        $fh = fopen($ratingParamsFileName, 'w');
        foreach ($params2 as $key => $value) {
            $data = array();
            $data[] = $key;
            $data[] = $value;
            safe_fputcsv($fh, $data);
        }
        fclose($fh);

        return $ratingParamsFileName;
    }

    static public function signalIfThereAreCDRSAlreadyBilled($jobSource)
    {
        $p = ArParamsPeer::getDefaultParams();
        $d1 = fromMySQLTimestampToUnixTimestamp($p->getNewImportedCdrsFromCalldate());
        $d2 = fromMySQLTimestampToUnixTimestamp($p->getOfficialCalldate());
        if (!is_null($d1) && !is_null($d2) && $d1 < $d2) {

            ArProblemException::createWithoutGarbageCollection(
                ArProblemType::TYPE_ERROR,
                ArProblemDomain::CONFIGURATIONS,
                null,
                "CDR with old date - " . $jobSource . " - " . get_ordered_timeprefix_with_unique_id(),
                "The CDR importer $jobSource contains new CDRS from date " . fromUnixTimestampToMySQLTimestamp($d1)
                . ", that are in an already billed time-frame. The new billing time-frame starts from date  "
                . fromUnixTimestampToMySQLTimestamp($d2),
                "Only CDRS in the unbilled time-frame will be rated, in order to not modify the already billed time-frame. "
                . "But doing so there can be CDRS not billed, or CDRS billed but having different prices. "
                . "They can be rerated selecting the proper time-frame, and pressing the re-rating button, because they are in the ar_source_cdr table. ",
                "Someone of your VoIP providers sent new CDRS or recalculated them, in a time-frame you already billed. So you had to coordinate better with them in order to bill CDRS only when they are confirmed. "
                . "NOTE: unlikely other error messages you will be informed only one time of this event. If you delete this message, it will be not generated again, except another file with the same problem will be received."
            );

            // Rate only unbilled calls.
            $p->setNewImportedCdrsFromCalldate($d2);
            $p->save();
        }
    }

}
