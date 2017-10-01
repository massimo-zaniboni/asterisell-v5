<?php

/* $LICENSE 2013, 2014, 2017:
 *
 * Copyright (C) 2013, 2014, 2017 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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
 * Call the services of the external rate engine tool.
 * This is a simple wrapper between the command line tool, and the PHP world.
 */
class RateEngineService
{

    const TOOL_EXECUTABLE = 'scripts/RateEngine';

    const MAX_IMPORT_ERRORS_TO_SHOW = 50;

    static public function getToolExecutable()
    {

        $cpu_cores = sfConfig::get('app_cpu_cores');
        $opts = " +RTS -N" . $cpu_cores . " -RTS ";

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
     * Execute regression tests on the rating engine.
     *
     * @param int $referenceTime
     * @param int $pass
     * @return bool true in case of success of tests, fals otherwise
     */
    static public function executeOrganizationTests($referenceTime, $pass)
    {

        // Create command

        $cmd = self::getToolExecutable()
            . ' --test-organizations ' . $pass
             . ' --from-date "' . fromUnixTimestampToMySQLTimestamp($referenceTime) . '" '
            . ' --load-extensions ' . ManageRateEvent::getParamsFileCompleteName(ManageRateEvent::EXTENSIONS_FILE_NAME);

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


}
