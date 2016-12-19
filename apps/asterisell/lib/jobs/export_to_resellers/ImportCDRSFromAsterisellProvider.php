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

use ImportDataFiles;

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Import CDRs from a CSV file produced from a Asterisell provider.
 * Create a job of this type for each provider.
 * This code must be take in synchro with `ExportCDRSToReseller`.
 *
 * NOTE: This is a developer-only class,
 * use instead one of subclass from this class: there are Local and Remote variants.
 */
abstract class ImportCDRSFromAsterisellProvider extends FixedJobProcessor
{

    const SHARED_RATES_PREFIX = 'shared_rates';

    //
    // Customizable Interface
    //

    /**
     * @return string the name of the CDR provider to use for importing the files.
     * Each Asterisell provider *must* have a distinct name, otherwise files will be overwritten,
     * CDRs will conflict togheter (without a signal from the application),
     * and calls are associated to the wrong Vendor.
     * In case two servers have the same logical Vendor, you can define two distinct providers
     * associated to the same Vendor in the Asterisell application configurations.
     */
    abstract function getCDRProviderName();

    /**
     * The files sent from the client, can be ordered according the generation date,
     * and so insert first the files that are older. In this case if there are queued
     * status files, are processed first the older status files, and then the new
     * status files overwriting the older versions.
     *
     * @return array list of file path and names to process
     * @throws ArProblemException
     */
    abstract function getSortedListOfFilesToProcess();

    //
    // Fixed Job Interface
    //

    /**
     * @return int the ar_vendor.id
     * @throws ArProblemException
     */
    public function getArVendorId()
    {
        static $id = null;

        if (is_null($id)) {
            $vendor = ArVendorPeer::retrieveByInternalName($this->getCDRProviderName());
            if (is_null($vendor)) {
                $p = ArProblemException::createWithGarbageCollection(
                    ArProblemType::TYPE_CRITICAL,
                    ArProblemDomain::CONFIGURATIONS,
                    null,
                    "not configured vendor - " . $this->getCDRProviderName(),
                    $this->getGarbageKey(),
                    null,
                    null,
                    'There is no vendor with internal name "' . $this->getCDRProviderName() . '".',
                    'CDRs will be not imported from Asterisell provider.',
                    'Add a vendor with the specified internal name. ',
                    null);
                throw($p);
            }

            $id = $vendor->getId();
        }

        return $id;
    }

    /**
     * @return string log message.
     */
    protected function initialProcessPhase() {
        return '';
    }

    /**
     * @return string log message
     */
    protected function endingProcessPhase() {
        return '';
    }

    public function process()
    {
        $prof = new JobProfiler('CSV files with CDRs');

        ArProblemException::garbageCollect($this->getGarbageKey(), null, null);

        $log1 = $this->initialProcessPhase();

        $sourceFiles = $this->getSortedListOfFilesToProcess();

        foreach ($sourceFiles as $sourceFile) {
            $this->processFile($sourceFile);
            // NOTE: in case of an error also only on one file, interrupt all the process, because
            // older files can not be processed after new files.
            // But also if I continue, in any case if there is still some file in the input directory,
            // the server will not add any more new files.

            $prof->incrementProcessedUnits();
        }

        $log2 = $this->endingProcessPhase();

        return $log1 . $prof->stop() . $log2;
    }

    /**
     * @param string $sourceFile file name and path to process
     * @throws ArProblemException
     * @throws Exception
     * @throws ArProblemException
     */
    protected function processRateFile($sourceFile)
    {

        $megaByte = 100;

        $firstDate = null;

        $conn = Propel::getConnection();

        $handle = fopen($sourceFile, 'r');
        if ($handle === FALSE) {
            $p = ArProblemException::createWithGarbageCollection(
                ArProblemType::TYPE_CRITICAL,
                ArProblemDomain::CONFIGURATIONS,
                null,
                "can not process file - " . $sourceFile,
                $this->getGarbageKey(),
                null,
                null,
                'The file "' . $sourceFile . '", of CDR provider "' . $this->getCDRProviderName() . '", can not be opened.',
                'CDRs will be not imported from Asterisell provider.',
                'It is probably a problem on access permissions of directory or files. ',
                null);
            throw($p);
        }

        $conn->beginTransaction();
        try {
            $nrOfCol = 6;
            $ln = 0;
            while (($data = fgetcsv($handle, $megaByte*1024*1024, ",", "\"", "\"")) !== FALSE) {
                $ln++;
                $nr = count($data);
                if ($nr != $nrOfCol) {
                    $p = ArProblemException::createWithGarbageCollection(
                        ArProblemType::TYPE_CRITICAL,
                        ArProblemDomain::APPLICATION,
                        null,
                        "unexpected number of fields - " . $sourceFile . ' - ' . $ln,
                        $this->getGarbageKey(),
                        null,
                        null,
                        'The file "' . $sourceFile . '", of CDR provider "' . $this->getCDRProviderName() . '", can not be processed, because at line ' . $ln . "there are $nr columns instead of the expected $nrOfCol",
                        'CDRs will be not imported from Asterisell provider.',
                        'This is a problem in application code. Contact the assistance. ',
                        null);
                    throw($p);
                }
                $rateTime = fromMySQLTimestampToUnixTimestamp($data[0]);
                $rateInternalName = $data[1];
                $rateFormat = $data[2];
                $rateShortDescription = $data[3];
                $rateSourceDataFile = $data[4];
                $rateHtmlDescription = $data[5];

                $format = ArRateFormatPeer::retrieveByInternalName($rateFormat);
                if (is_null($format)) {
                    $p = ArProblemException::createWithGarbageCollection(
                        ArProblemType::TYPE_CRITICAL,
                        ArProblemDomain::APPLICATION,
                        null,
                        "not configured rate format - " . $rateFormat,
                        $this->getGarbageKey(),
                        null,
                        null,
                        'There is no rate format with internal name "' . $rateFormat . '".',
                        'CDRs will be not imported from Asterisell provider.',
                        'Contact the application assistance, because the rate format is not supported from application code.',
                        null);
                    throw($p);

                }
                $formatId = $format->getId();

                if ($ln === 1) {
                    // The first time, delete all previous rates, before replacing with the new one.

                    $firstDate = $rateTime;

                    $query = 'DELETE
                                  FROM ar_rate
                                  WHERE ar_vendor_id = ?
                                  AND from_time >= ?';

                    $stm = $conn->prepare($query);
                    $stm->execute(array($this->getArVendorId(), fromUnixTimestampToMySQLTimestamp($rateTime)));
                    $stm->closeCursor();
                }

                $rate = new ArRate();
                $rate->setFromTime($rateTime);
                $rate->setInternalName($rateInternalName);
                $rate->setArRateFormatId($formatId);
                $rate->setArVendorId($this->getArVendorId());
                $rate->setShortDescription($rateShortDescription);
                $rate->setSourceDataFile($rateSourceDataFile);
                $rate->setHtmlDescription($rateHtmlDescription);
                $rate->setNote("This rate is automatically imported from vendor " . $this->getCDRProviderName() . '. Every change to this rate will be overwritten, if the vendor change it again.');
                $rate->save($conn);
            }
            fclose($handle);

            $isOk = @unlink($sourceFile);
            if ($isOk === FALSE) {
                $p = ArProblemException::createWithGarbageCollection(
                    ArProblemType::TYPE_CRITICAL,
                    ArProblemDomain::CONFIGURATIONS,
                    null,
                    "can not delete file - " . $sourceFile,
                    $this->getGarbageKey(),
                    null,
                    null,
                    "The file \"$sourceFile\", of CDR provider \"" . $this->getCDRProviderName() . "\", can not be processed, because it can not be removed from processing directory.",
                    'CDRs will be not imported from Asterisell provider.',
                    'This is a problem in application code. Contact the assistance. ',
                    null);
                throw($p);
            }

            $this->commitTransactionOrSignalProblem($conn);
        } catch (ArProblemException $e) {
            $this->maybeRollbackTransaction($conn);
            throw($e);
        } catch (Exception $e) {
            $this->maybeRollbackTransaction($conn);
            $p = ArProblemException::createWithGarbageCollection(
                ArProblemType::TYPE_CRITICAL,
                ArProblemDomain::APPLICATION,
                null,
                'error importing rates ' . $e->getCode(),
                $this->getGarbageKey(),
                null,
                null,
                "The file \"$sourceFile\", of CDR provider \"" . $this->getCDRProviderName() . "\", can not be processed. " . $e->getMessage(),
                'CDRs will be not imported from Asterisell provider.',
                'This is a problem in application code. Contact the assistance. ',
                null);
            throw($p);
        }

        ArRateFormatPeer::clearInstancePool();

        // Inform the administrator of new rates
        if (!is_null($firstDate)) {
            ArProblemException::createWithoutGarbageCollection(
                ArProblemType::TYPE_INFO,
                ArProblemDomain::RATES,
                null,
                "updated rates from vendor - " . $this->getCDRProviderName() . ' - ' . $firstDate,
                "The vendor \"" . $this->getCDRProviderName() . "\", changed the shared rates from date " . fromUnixTimestampToMySQLTimestamp($firstDate),
                "The rates of the vendor, are replaced with a new version.",
                'Check if the rates are correct, and compatbile with the agreement with the Vendor. In case rerate CDRs',
                null);

        }
    }

    /**
     * @param string $sourceFile complete file name and path to import.
     * @return null
     * @throws ArProblemException
     */
    public function processFile($sourceFile)
    {
        // Retrieve the elements of the file name

        $sourceFileName = basename($sourceFile);

        $m = array();

        $statusYYYY = null;
        $statusMM = null;
        $statusDD = null;

        if ($sourceFileName === ImportDataFiles::FILES_TO_EXPORT_LIST) {
            return null;
        } else if ($sourceFileName === ImportDataFiles::IS_ASTERISELL_PROVIDER_DIRECTORY_CHECK) {
            return null;
        } else if (isPrefixOf(self::SHARED_RATES_PREFIX, $sourceFileName)) {
            $this->processRateFile($sourceFile);
            return null;
        } else if (preg_match('/^([^.]+)\.(\d\d\d\d)-(\d\d)-(\d\d)\.([^.]+)__([^.]+)__([^.]+)(\.gz)?$/i', $sourceFileName, $m)) {

            $baseName = $m[1];
            $statusYYYY = $m[2];
            $statusMM = $m[3];
            $statusDD = $m[4];

            $logicalType = $m[6];
            $versionType = $m[7];

            $suffix = '';
            if (array_key_exists(8, $m)) {
                $suffix = $m[8];
            }

            // replace the source provider with the configured CDR provider.
            $destFileName = $baseName . '.' . $statusYYYY . '-' . $statusMM . '-' . $statusDD . '.' . $this->getCDRProviderName() . '__' . $logicalType . '__' . $versionType . $suffix;

            // the file can be imported later, using normal Asterisell procedures.
            ImportDataFiles::moveAtomicallyFileFromMySQLDirToOtherDirectory($sourceFile, ImportDataFiles::getAbsoluteInputDirectory(), $destFileName);

            return null;
        } else {
            $p = ArProblemException::createWithGarbageCollection(
                ArProblemType::TYPE_CRITICAL,
                ArProblemDomain::CONFIGURATIONS,
                null,
                "can not process file - " . $sourceFileName,
                $this->getGarbageKey(),
                null,
                null,
                'The file "' . $sourceFileName . '", of CDR provider "' . $this->getCDRProviderName() . '", has an unknown format in its name.',
                'CDRs will be not imported from Asterisell provider.',
                'Advise the Asterisell CDR provider that is sending files in a bad format. ',
                null);
            throw($p);
        }
    }
}
