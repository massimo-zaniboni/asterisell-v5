<?php

/* $LICENSE 2012:
 *
 * Copyright (C) 2012 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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
 * TODO this code generate some error
 *
 * Export CDRs to aggregate server.
 * CDRs to export are recognized using the `is_exported` flag.
 * CDRs are exported using daily sums, and each file contains the CDRs of a month.
 */
class ExportCDRsForAggregateServer extends DailyStatusJob
{

    ////////////
    // PARAMS //
    ////////////

    // TODO problema: e` chiamato DAILY ma lavora sul MONTH, quindi dovrebbe mantenere uno stato
    // e dovrebbe evitare di scrivere MONTH che sono gia` stati processati.
    // TODO assicurarsi che sia chiamato sempre lo stesso job
    // TODO documentare il behaviour sopra

    /**
     * Where put files exported to aggregate server.
     */
    const export_directory = 'data_files/export_to_aggregate_server/';

    const temp_file_name = 'temp_file.tmp';

    const CHANNEL_NAME_FOR_UNRATED_CDRS = 'Rating Error';

    const VENDOR_NAME_FOR_UNRATED_CDRS = 'Rating Error';

    const CONTENT_TYPE_VERSION = 'aggregate-001';

    ////////////////////////////
    // CUSTOMIZABLE BEHAVIOUR //
    ////////////////////////////

    // TODO add init job and init the cache on the month

    protected $processedMonth;

    /**
     * Called before starting processing.
     */
    public function initJob() {
        $this->processedMonth = array();
    }

    /**
     * @param int $fromDate the day where there is changed data,
     * @param int $toDate the last value to process not inclusive (the next day)
     * @param PropelPDO $conn the connection with the transaction to use
     * @return string log message
     */
    public function processChangedDay($fromDate, $toDate, PropelPDO $conn) {
        $this->exportAllCDRsOfMonthToFile($fromDate, $conn);
    }

    protected function waitXMinutes()
    {
        return sfConfig::get('app_check_new_external_files_to_import_after_minutes');
    }


    /**
     * @return string
     */
    static public function getExportDirectory() {
        return getAsterisellRootDirectory() . DIRECTORY_SEPARATOR . self::export_directory;
    }

    protected function getCompleteDir()
    {
        return self::getExportDirectory();
    }

    /**
     * @return string
     */
    protected function getCompleteTempFileName()
    {
        return $this->getCompleteDir() . self::temp_file_name;
    }


    // TODO old code to adapt

    /////////////////
    // DATA ACCESS //
    /////////////////

    /**
     * @param int $timestamp
     * @return string
     */
    protected function getYearPart($timestamp)
    {
        return date('Y', $timestamp);
    }

    /**
     * @param int $timestamp
     * @return string
     */
    protected function getMonthPart($timestamp)
    {
        return date('m', $timestamp);
    }

    /**
     * @param int $timestamp
     * @return string
     */
    protected function getCompleteFileName($timestamp)
    {
        $fileName = 'aggregate_cdrs_of_' . $this->getYearPart($timestamp) . '_' . $this->getMonthPart($timestamp) . '.csv';
        return $this->getCompleteDir() . $fileName;
    }

    /**
     * Export all the CDRs in the month of the $timestamp
     *
     * @param int $timestamp
     * @param PropelPDO $conn
     * @throws ArProblemException
     */
    protected function exportAllCDRsOfMonthToFile($timestamp, PropelPDO $conn)
    {

        if (JobQueueProcessor::$IS_INTERACTIVE) {
          echo "\n      export to " . $this->getCompleteFileName($timestamp);
        }

        // Time interval init

        $serverCode = getInstanceConfigValue('instance_code_name');

        $datePrefix = $this->getYearPart($timestamp) . '-' . $this->getMonthPart($timestamp);
        $timeSuffix = ' 00:00:00';
        $minDate = strtotime($datePrefix . '-01');
        $maxDate = strtotime('+1 month', $minDate);

        $minDateMySQL = fromUnixTimestampToMySQLTimestamp($minDate);
        $maxDateMySQL = fromUnixTimestampToMySQLTimestamp($maxDate);

        // Continue processing only new dates.
        if (array_key_exists($minDate, $this->processedMonth)) {
            return;
        } else {
            $this->processedMonth[$minDate] = true;
        }

        $problemEffect = 'The procedure can not export the data of month ' . $minDate;
        $problemProposedSolution = 'If the problem persist, contact the assistance.';

            $ok = $this->maybeMakeDir($this->getCompleteDir());
            if ($ok === FALSE) {
                $problemDuplicationKey = get_class($this) . " - create directory ";
                $problemDescription = get_class($this) . ' can not create directory "' . $this->getCompleteDir() . '".';
                $p = $this->createError(
                    ArProblemType::TYPE_ERROR,
                    ArProblemDomain::APPLICATION,
                    $problemDuplicationKey,
                    $problemDescription,
                    $problemEffect,
                    $problemProposedSolution);

                throw($p);
            }

            $fh = fopen($this->getCompleteTempFileName(), "w");

            if ($fh === FALSE) {
                $problemDuplicationKey = get_class($this) . " - file open ";
                $problemDescription = get_class($this) . ' can not create file "' . $this->getCompleteTempFileName($timestamp) . '".';
                $p = $this->createError(
                    ArProblemType::TYPE_ERROR,
                    ArProblemDomain::APPLICATION,
                    $problemDuplicationKey,
                    $problemDescription,
                    $problemEffect,
                    $problemProposedSolution);

                throw($p);
            }

            // Write aggregate for correctly rated CDRs

            $query = 'SELECT
                        DAYOFMONTH(ar_cdr.calldate) AS cdr_day,
                        ar_cdr.destination_type AS destination_type,
                        ch.internal_name AS channel_internal_name,
                        ch.name AS channel_name,
                        vendor.internal_name AS vendor_internal_name,
                        vendor_party.name AS vendor_party_name,
                        SUM(ar_cdr.count_of_calls) AS count_of_calls,
                        SUM(ar_cdr.cost) AS cost_of_calls,
                        SUM(ar_cdr.billsec) AS tot_duration,
                        SUM(ar_cdr.cost_saving) AS cost_saving
                      FROM ar_cdr FORCE INDEX (ar_cdr_calldate_index)
                           INNER JOIN ar_communication_channel_type AS ch ON ar_cdr.ar_communication_channel_type_id = ch.id
                           INNER JOIN ar_vendor AS vendor ON ar_cdr.ar_vendor_id = vendor.id
                           LEFT JOIN ar_party AS vendor_party ON vendor.ar_party_id = vendor_party.id
                      WHERE ar_cdr.calldate >= ?
                      AND ar_cdr.calldate < ?
                      AND ar_cdr.destination_type <> ?
                      AND ar_cdr.destination_type <> ?
                      AND ar_cdr.destination_type <> ?
                      GROUP BY DAYOFMONTH(ar_cdr.calldate),
                               ar_cdr.destination_type,
                               ar_cdr.ar_communication_channel_type_id,
                               ar_cdr.ar_vendor_id';

            $stm = $conn->prepare($query);
            $ok = $stm->execute(array(
                $minDateMySQL,
                $maxDateMySQL,
                DestinationType::error,
                DestinationType::ignored)
            );

            if (!$ok) {
                $problemDuplicationKey = get_class($this) . " - query1 ";
                $problemDescription = get_class($this) . ' error executing query "' . $query . '".';
                $p = $this->createError(
                    ArProblemType::TYPE_ERROR,
                    ArProblemDomain::APPLICATION,
                    $problemDuplicationKey,
                    $problemDescription,
                    $problemEffect,
                    $problemProposedSolution);
                throw($p);
            }

            while ($rs = $stm->fetch(PDO::FETCH_ASSOC)) {
                $errorDestinationType = 0; // standard value for correctly rated calls

                $line =   '"' . self::CONTENT_TYPE_VERSION . '",'
                        . '"' . $serverCode . '",'
                        . '"' . $datePrefix . '-' . $rs['cdr_day'] . $timeSuffix . '",'
                        . '"' . $rs['destination_type'] . '",'
                        . '"' . DestinationType::getName($rs['destination_type']) . '",'
                        . '"' . $errorDestinationType . '",'
                        . '"' . DestinationType::getName($errorDestinationType) . '",'
                        . '"' . $rs['channel_internal_name'] . '",'
                        . '"' . $rs['channel_name'] . '",'
                        . '"' . $rs['vendor_internal_name'] . '",'
                        . '"' . $rs['vendor_party_name'] . '",'
                        . '"' . $rs['count_of_calls'] . '",'
                        . '"' . $rs['tot_duration'] . '",'
                        . '"' . $rs['cost_of_calls'] . '",'
                        . '"' . $rs['cost_saving'] . "\"\n";

                fwrite($fh, $line);
            }
            $stm->closeCursor();

            // Write stats for CDRs with errors

            $query = 'SELECT DAYOFMONTH(ar_cdr.calldate) AS cdr_day,
                             ar_cdr.destination_type AS destination_type,
                             SUM(ar_cdr.count_of_calls) AS count_of_calls,
                             SUM(ar_cdr.BILLSEC) AS tot_duration,
                             ar_cdr.error_destination_type AS error_destination_type
                      FROM ar_cdr FORCE INDEX (ar_cdr_calldate_index)
                      WHERE ar_cdr.destination_type = ?
                      AND ar_cdr.calldate >= ?
                      AND ar_cdr.calldate < ?
                      GROUP BY DAYOFMONTH(ar_cdr.calldate), ar_cdr.error_destination_type';

            $stm = $conn->prepare($query);
            $ok = $stm->execute(array(DestinationType::error, $minDateMySQL, $maxDateMySQL));

            if (!$ok) {
                $problemDuplicationKey = get_class($this) . " - query1 ";
                $problemDescription = get_class($this) . ' error executing query "' . $query . '".';
                $p = ArProblemException::createWithoutGarbageCollection(
                    ArProblemType::TYPE_ERROR,
                    ArProblemDomain::APPLICATION,
                    null,
                    $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
                throw($p);
            }

            while ($rs = $stm->fetch(PDO::FETCH_ASSOC)) {
                $line = '"' . self::CONTENT_TYPE_VERSION . '",'
                        . '"' . $serverCode . '",'
                        . '"' . $datePrefix . '-' . $rs['cdr_day'] . $timeSuffix . '",'
                        . '"' . $rs['destination_type'] . '",'
                        . '"' . DestinationType::getName($rs['destination_type']) . '",'
                        . '"' . $rs['error_destination_type'] . '",'
                        . '"' . DestinationType::getName($rs['error_destination_type']) . '",'
                        . '"' . self::CHANNEL_NAME_FOR_UNRATED_CDRS . '",'
                        . '"' . self::CHANNEL_NAME_FOR_UNRATED_CDRS . '",'
                        . '"' . self::VENDOR_NAME_FOR_UNRATED_CDRS . '",'
                        . '"' . self::VENDOR_NAME_FOR_UNRATED_CDRS . '",'
                        . '"' . $rs['count_of_calls'] . '",'
                        . '"' . $rs['tot_duration'] . '",'
                        . '"0",'
                        . '"' . '0' . "\"\n";
                fwrite($fh, $line);
            }
            $stm->closeCursor();

            // Generate final file

            $ok = fclose($fh);
            if (!$ok) {
                $problemDuplicationKey = get_class($this) . " - file save ";
                $problemDescription = get_class($this) . ' can not write to file "' . $this->getCompleteTempFileName($timestamp) . '".';
                $p = $this->createError(
                    ArProblemType::TYPE_ERROR,
                    ArProblemDomain::APPLICATION,
                    $problemDuplicationKey,
                    $problemDescription,
                    $problemEffect,
                    $problemProposedSolution);
                throw($p);
            }

            $ok = $this->myMove($this->getCompleteTempFileName($timestamp), $this->getCompleteFileName($timestamp));
            if (!$ok) {
                $problemDuplicationKey = get_class($this) . " - file move ";
                $problemDescription = get_class($this) . ' can not create file "' . $this->getCompleteFileName($timestamp) . '".';
                $p = $this->createError(
                    ArProblemType::TYPE_ERROR,
                    ArProblemDomain::APPLICATION,
                    $problemDuplicationKey,
                    $problemDescription,
                    $problemEffect,
                    $problemProposedSolution);
                throw($p);
            }
        $this->commitTransactionOrSignalProblem($conn);
    }

    /**
     * @param string $file1
     * @param string $file2
     * @return bool
     */
    protected function myMove($file1, $file2)
    {
        if (@file_exists($file2)) {
            @unlink($file2);
        }
        $ok1 = @rename($file1, $file2);
        $ok2 = @chmod($file2, 0744);

        return $ok1 && $ok2;
    }

    /**
     * @static
     * @param string $dir
     * @return bool
     */
    protected static function maybeMakeDir($dir)
    {
        if (!file_exists($dir)) {
            $ok1 = @mkdir($dir);
            $ok2 = @chmod($dir, 0755);

            return $ok1 && $ok2;
        }
        return true;
    }
}
