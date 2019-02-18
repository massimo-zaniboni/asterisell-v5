<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Unlike `ExportCDRSToReseller` export only services CDRS.
 * The service CDRS are grouped by month. This is only a practical physical grouping.
 * Read all the notes on `ExportCDRSToReseller` because they apply also to this class.
 */
abstract class ExportServiceCDRSToReseller extends ExportCDRSToReseller
{

    /**
     * The date (in localtime format) of the months to process.
     */
    protected $monthsToProcess = null;

    public function initJob()
    {
        $this->testResellerCode();
        $this->createAsterisellProviderDirectoryCheck();

        $this->monthsToProcess = array();

        return true;
    }

    public
    function processChangedDay($fromDate, $toDate, PropelPDO $conn)
    {
        $year = date('Y', $fromDate);
        $month = date('m', $fromDate);
        $month = strtotime("$year-$month-01 00:00:00");
        $this->monthsToProcess[$month] = true;
    }

    public function endJob(PropelPDO $conn) {

        $logicalType = self::SERVICE_LOGICAL_TYPE;
        $formatType = self::SERVICE_FORMAT_TYPE;
        $resellerCallFilter = '%/' . $this->getOrganizationId() . '/%';

        foreach($this->monthsToProcess as $month => $notUsed) {
            $yyyy = intval(date('Y', $month));
            $mm = intval(date('m'), $month);

            $tmpFileName = '/var/tmp/' . ImportDataFiles::createInputStatusDataFileName($this->getResellerCode(), self::CDR_PROVIDER_TYPE, $logicalType, $formatType, $yyyy, $mm, null, true);
            $fileName = basename($tmpFileName);
            @unlink($tmpFileName);
            $query = $this->getExportCDRSQuery(true, $tmpFileName);

            $stmt = $conn->prepare($query);
            $isOk = $stmt->execute(array(
                fromUnixTimestampToMySQLTimestamp($month),
                fromUnixTimestampToMySQLTimestamp($this->getActivationDate()),
                fromUnixTimestampToMySQLTimestamp(strtotime('+1 month', $month)),
                $resellerCallFilter));

            if ($isOk === FALSE) {
                throw $this->createError(
                    ArProblemType::TYPE_ERROR,
                    ArProblemDomain::APPLICATION,
                    'error in query ' . $query,
                    "Error in query \"" . $query . "\"",
                    "CDRs will be not exported to the reseller.",
                    "This is an error in the code. Contact the assistance."
                );
            }
            $stmt->closeCursor();

            // NOTE: send also empty files because:
            // * they can represent removed services on the provider side, and they must be removed also on the reseller side
            // * they are generated only for instances having resellers

            $isOk = ImportDataFiles::moveAtomicallyFileFromMySQLDirToOtherDirectory($tmpFileName, $this->getExportDirectory(), $fileName);
            if ($isOk === FALSE) {
                throw $this->createError(
                    ArProblemType::TYPE_ERROR,
                    ArProblemDomain::CONFIGURATIONS,
                    'unable to move files to directory ' . $this->getExportDirectory(),
                    "During exporting of CDRs to reseller \"" . $this->getResellerCode() . "\", the produced file \"" . $tmpFileName . "\", can not be moved to directory \"" . $this->getExportDirectory() . "\".",
                    "CDRs will be not exported to the reseller.",
                    "The directory can not exists, or not having the right permissions."
                );
            }
        }

        $this->createFilesToExportList();
    }
}
