<?php

// SPDX-License-Identifier: GPL-3.0-or-later

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
    protected static function  fromMySQLTimestampToUnixTimestampAndCheck($d, $garbageKey, $garbageFrom, $garbageTo) {
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
            . self::writeWithDBAccessParams($ratingParams)
            ;

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
            . self::writeWithDBAccessParams($ratingParams)
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
     * @param array $params add to this array the db access params
     */
    static public function addDBAccessToParams(&$params) {
      list($dbName, $dbUser, $dbPassword) = getDatabaseNameUserAndPassword(true);
      $params['db-name'] = $dbName;
      $params['db-user'] = $dbUser;
      $params['db-password'] = $dbPassword;
    }

    /**
     * Write all the params on a param file, to send to the engine.
     * Add also the `self::custom_initial_params`.
     *
     * @param array $params name => value array
     * @return string the command line with the name of the param file to pass to the rating engine
     */
    static public function writeParams($params) {
      $ratingParamsFileName = normalizeFileNamePath(ImportDataFiles::getMySQLAccessibleTmpDirectory('rating-services') . '/' . self::PARAMS_FILE);

      // NOTE: the custom params can override the code params
      $params2 = array_merge($params, self::$custom_initial_params);

      $fh = fopen($ratingParamsFileName, 'w');
      foreach($params2 as $key => $value) {
          $data = array();
          $data[] = $key;
          $data[] = $value;
          safe_fputcsv($fh, $data);
      }
      fclose($fh);

      return ' --params ' . $ratingParamsFileName . ' ';

    }

    static public function writeWithDBAccessParams(&$params) {
        self::addDBAccessToParams($params);
        return self::writeParams($params);
    }

}
