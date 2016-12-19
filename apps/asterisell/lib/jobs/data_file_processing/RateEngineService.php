<?php

/* $LICENSE 2013, 2014:
 *
 * Copyright (C) 2013, 2014 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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
        return normalizeFileNamePath(getAsterisellCompleteRootDirectory() . '/' . self::TOOL_EXECUTABLE);
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
     * BundleRate produce service-cdrs. Generate the corresponding telephone prefix entries.
     *
     * @param string $rateFileName the file with the list of CDR files to import
     * @param string $resultFileName
     * @return bool true if the resulting file can be imported
     * @param string $garbageKey
     * @param int|null $garbageFrom
     * @param int|null $garbageTo
     * @throws ArProblemException
     */
    static public function compileRate($rateFileName, $resultFileName, $garbageKey, $garbageFrom, $garbageTo)
    {

        // Create command

        $cmd = self::getToolExecutable()
            . ' --compile-rate ' . $rateFileName
            . ' --load-rate-categories ' . ManageRateEvent::getParamsFileCompleteName(ManageRateEvent::RATE_CATEGORY_FILE_NAME)
            . ' --load-vendors ' . ManageRateEvent::getParamsFileCompleteName(ManageRateEvent::VENDORS_FILE_NAME)
            . ' --load-channels-types ' . ManageRateEvent::getParamsFileCompleteName(ManageRateEvent::CHANNEL_TYPES_FILE_NAME)
            . ' --load-channel-domains ' . ManageRateEvent::getParamsFileCompleteName(ManageRateEvent::CHANNEL_DOMAINS)
            . ' --load-telephone-prefixes ' . ManageRateEvent::getParamsFileCompleteName(ManageRateEvent::TELEPHONE_PREFIXES_FILE_NAME)
            . ' --load-services ' .  ManageRateEvent::getParamsFileCompleteName(ManageRateEvent::SERVICE_FILE_NAME)
            . ' --load-service-price-list ' .  ManageRateEvent::getParamsFileCompleteName(ManageRateEvent::SERVICE_PRICE_LIST_FILE_NAME)
            . ' --load-assigned-services ' .  ManageRateEvent::getParamsFileCompleteName(ManageRateEvent::ASSIGNED_SERVICE_FILE_NAME);

        $mask = sfConfig::get('app_mask_for_external_telephone_number');
        if (isEmptyOrNull($mask)) {
            $mask = 0;
        }


        $defaultTelephonePrefix = sfConfig::get('app_not_displayed_telephone_prefix');
        if (isEmptyOrNull($defaultTelephonePrefix) || $defaultTelephonePrefix == '-') {
            $defaultTelephonePrefix = '""';
        }

        $currencyPrecision = sfConfig::get('app_currency_decimal_places');

        $cmd .= ' --digits-to-mask ' . $mask
            . ' --default-telephone-prefix ' . $defaultTelephonePrefix
            . ' --currency-precision ' . $currencyPrecision
            . ' --load-extensions ' . ManageRateEvent::getParamsFileCompleteName(ManageRateEvent::EXTENSIONS_FILE_NAME);

        $cmd .= ' --result-file ' . $resultFileName;

        // Execute the command

        if (JobQueueProcessor::$IS_INTERACTIVE) {
            echo "\nExecuted:\n" . $cmd;
        }

        $output = array();
        $exitStatus = 0;
        exec($cmd, $output, $exitStatus);

        if ($exitStatus != 0) {
            throw(ArProblemException::createWithGarbageCollection(
                ArProblemType::TYPE_CRITICAL,
                ArProblemDomain::APPLICATION,
                null,
                'compile rate - ' . md5($cmd),
                $garbageKey,
                $garbageFrom,
                $garbageTo,
                "Error executing command \"$cmd\": \n" . implode("\n", $output),
                "CDRs rating will produce errors.",
                "This is an error in the application code. Contact the assistance."
            ));
        }
    }

    /**
     * Generate the corresponding telephone prefix entries.
     *
     * @param string $resultFileName
     * @return bool true if the resulting file can be imported
     * @param string $garbageKey
     * @param int|null $garbageFrom
     * @param int|null $garbageTo
     * @throws ArProblemException
     */
    static public function compileServices($resultFileName, $garbageKey, $garbageFrom, $garbageTo)
    {

        // Create command

        $cmd = self::getToolExecutable()
            . ' --compile-services '
            . ' --load-rate-categories ' . ManageRateEvent::getParamsFileCompleteName(ManageRateEvent::RATE_CATEGORY_FILE_NAME)
            . ' --load-vendors ' . ManageRateEvent::getParamsFileCompleteName(ManageRateEvent::VENDORS_FILE_NAME)
            . ' --load-channels-types ' . ManageRateEvent::getParamsFileCompleteName(ManageRateEvent::CHANNEL_TYPES_FILE_NAME)
            . ' --load-channel-domains ' . ManageRateEvent::getParamsFileCompleteName(ManageRateEvent::CHANNEL_DOMAINS)
            . ' --load-telephone-prefixes ' . ManageRateEvent::getParamsFileCompleteName(ManageRateEvent::TELEPHONE_PREFIXES_FILE_NAME)
            . ' --load-services ' .  ManageRateEvent::getParamsFileCompleteName(ManageRateEvent::SERVICE_FILE_NAME)
            . ' --load-service-price-list ' .  ManageRateEvent::getParamsFileCompleteName(ManageRateEvent::SERVICE_PRICE_LIST_FILE_NAME)
            . ' --load-assigned-services ' .  ManageRateEvent::getParamsFileCompleteName(ManageRateEvent::ASSIGNED_SERVICE_FILE_NAME);

        $mask = sfConfig::get('app_mask_for_external_telephone_number');
        if (isEmptyOrNull($mask)) {
            $mask = 0;
        }

        $defaultTelephonePrefix = sfConfig::get('app_not_displayed_telephone_prefix');
        if (isEmptyOrNull($defaultTelephonePrefix) || $defaultTelephonePrefix == '-') {
            $defaultTelephonePrefix = '""';
        }

        $currencyPrecision = sfConfig::get('app_currency_decimal_places');

        $cmd .= ' --digits-to-mask ' . $mask
            . ' --default-telephone-prefix ' . $defaultTelephonePrefix
            . ' --currency-precision ' . $currencyPrecision
            . ' --load-extensions ' . ManageRateEvent::getParamsFileCompleteName(ManageRateEvent::EXTENSIONS_FILE_NAME);

        $cmd .= ' --result-file ' . $resultFileName;

        // Execute the command

        if (JobQueueProcessor::$IS_INTERACTIVE) {
            echo "\nExecuted:\n" . $cmd;
        }

        $output = array();
        $exitStatus = 0;
        exec($cmd, $output, $exitStatus);

        if ($exitStatus != 0) {
            throw(ArProblemException::createWithGarbageCollection(
                ArProblemType::TYPE_CRITICAL,
                ArProblemDomain::APPLICATION,
                null,
                'compile rate - ' . md5($cmd),
                $garbageKey,
                $garbageFrom,
                $garbageTo,
                "Error executing command \"$cmd\": \n" . implode("\n", $output),
                "CDRs rating will produce errors.",
                "This is an error in the application code. Contact the assistance."
            ));
        }
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
            . ' --load-rate-categories ' . ManageRateEvent::getParamsFileCompleteName(ManageRateEvent::RATE_CATEGORY_FILE_NAME)
            . ' --load-vendors ' . ManageRateEvent::getParamsFileCompleteName(ManageRateEvent::VENDORS_FILE_NAME)
            . ' --load-channels-types ' . ManageRateEvent::getParamsFileCompleteName(ManageRateEvent::CHANNEL_TYPES_FILE_NAME)
            . ' --load-channel-domains ' . ManageRateEvent::getParamsFileCompleteName(ManageRateEvent::CHANNEL_DOMAINS)
            . ' --load-telephone-prefixes ' . ManageRateEvent::getParamsFileCompleteName(ManageRateEvent::TELEPHONE_PREFIXES_FILE_NAME);

        $mask = sfConfig::get('app_mask_for_external_telephone_number');
        if (isEmptyOrNull($mask)) {
            $mask = 0;
        }

        $defaultTelephonePrefix = sfConfig::get('app_not_displayed_telephone_prefix');
        if (isEmptyOrNull($defaultTelephonePrefix) || $defaultTelephonePrefix == '-') {
            $defaultTelephonePrefix = '""';
        }

        $currencyPrecision = sfConfig::get('app_currency_decimal_places');

        $cmd .= ' --digits-to-mask ' . $mask
            . ' --default-telephone-prefix ' . $defaultTelephonePrefix
            . ' --currency-precision ' . $currencyPrecision
            . ' --load-extensions ' . ManageRateEvent::getParamsFileCompleteName(ManageRateEvent::EXTENSIONS_FILE_NAME);

        $cmd .= ' --load-rate-plan-changes ' . ManageRateEvent::getParamsFileCompleteName(ManageRateEvent::RATE_PLAN_FILE_NAME)
            . ' --load-rate-plan ' . ManageRateEvent::getParamsFileCompleteName(ManageRateEvent::RATE_PLAN_ID_FILE_NAME);

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
