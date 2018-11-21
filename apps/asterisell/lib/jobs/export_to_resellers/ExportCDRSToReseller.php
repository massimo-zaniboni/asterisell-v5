<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Create a job of this type for each reseller.
 *
 * Configure the reseller with a Job ImportCDRSFromReseller, using the right directory.
 *
 * This job export all the calls of each day with changes, so it can be rather heavy to execute
 * often for small changes on the same day.
 *
 * Normal CDRS and ServiceCDRS are exported using two distinct logical file types,
 * because they are managed and rated in a different way from the reseller receiving them.
 *
 * NOTE: up to date support external resellers with the same precision.
 * In the future export to fractional cost (using a MySQL function),
 * switch to format v2, and convert into the reseller to the reseller precision.
 *
 * NOTE: this job must be exectude after SignalRatesToExportToResellers, because
 * it generates a list of files to export.
 *
 * NOTE: the communication protocol must be mantained in synchro with ImportCDRSFromReseller job,
 * and ImportCDRSFromRemoteAsterisellProvider.
 *
 * The requirements are:
 * - no files are generated for locked remote servers,
 * - list of files is sent, if the remote server processed it
 *
 */
abstract class ExportCDRSToReseller extends DailyStatusJob
{

    const LOGICAL_TYPE = 'asterisell-provider';
    const FORMAT_TYPE = 'v1';
    const CDR_PROVIDER_TYPE = 'unspecified';

    /**
     * IMPORTANT: this type *must* be used only for exporting services from a provider
     * to resellers, because they have a specific way to be managed, and they are
     * recognized by the (hard-coded) usage of this export data type.
     *
     * IMPORTANT: do not change the name of this type, without updating also the corresponding
     * name on Haskell Rate Engine code.
     */
    const SERVICE_LOGICAL_TYPE = 'asterisell-provider-services';
    const SERVICE_FORMAT_TYPE = 'v1';

    //
    // Customizable Interface
    //

    /**
     * @return string
     */
    abstract function getResellerCode();

    /**
     * @return string the directory where export CDR status files.
     */
    public function getExportDirectory()
    {
        return '/var/opt/asterisell/' . getInstanceCodeName() . '/' . $this->getResellerCode();
    }

    /**
     * return int the minutes to wait after each execution of the job.
     * 0 for always executing the job.
     */
    protected function waitXMinutes()
    {
        return 0;
    }

    /**
     * Allows exporting info about used communication channels,
     * in case they must be known from the Reseller, for applying different rates on them.
     *
     * The used name, is `ar_communication_channel_type.internal_name`.
     *
     * @return array a map between channel name on provider, and name to use when exporting to the reseller.
     * So something like
     *
     * > $arr['some-source-channel'] = 'dest-channel';
     *
     * Channels that are not matching will be exported to the reseller using the default channel name.
     * Channel Names are exported in this way:
     * - empty string when there is no channel info exported
     * - the channel name otherwise
     * Channel Names are imported on the reseller side in this way:
     * - "provider-name" when there is no channel info exported
     * - "provider-name-<exported-channel-name>" otherwise
     * By default (without specifying nothing) the services are exported like 'system-service-cdr'
     */
    public function exportedCommunicationChannels()
    {
        return array();
    }

    //
    // DailyStatusJob interface.
    //

    public function initJob()
    {
        $this->testResellerCode();

        // In case there are still files to process, do not generate rates and CDRs:
        // * accumulation of daily files can use a lot of disk space
        // * accumulation of daily files can use a lot of bandwidth and resources for the client
        // * there can be times when the client is temporary disabled, or there can be different upgrade speeds
        // * new rates must overwrite old rates, and not viceversa

        $this->createAsterisellProviderDirectoryCheck();

        if (!$this->isRemoteServerReadyToReceiveFiles()) {
            return false;
        }

        //
        // Export Rates
        //

        $conn = Propel::getConnection();
        $conn->beginTransaction();
        try {

            // Get the minimum date of the rate to export.

            $minimumRateDate = null;
            $query = '
              SELECT MIN(ar_rate.from_time)
              FROM ar_rate_shared_with_reseller
              INNER JOIN ar_rate ON ar_rate_shared_with_reseller.ar_rate_id = ar_rate.id
              WHERE ar_rate_shared_with_reseller.is_exported = 0
              AND ar_rate_shared_with_reseller.ar_reseller_id = ?';

            $stm = $conn->prepare($query);
            $stm->execute(array($this->getArResellerId()));

            while ((($rs = $stm->fetch(PDO::FETCH_NUM)) !== false)) {
                $minimumRateDate = fromMySQLTimestampToUnixTimestamp($rs[0]);
            }
            $stm->closeCursor();

            if (!is_null($minimumRateDate)) {
                // Export all the rates from this date, also if they are already exported,
                // because the reseller must have a complete list of rates.

                $fileName = ImportCDRSFromLocalAsterisellProvider::SHARED_RATES_PREFIX . '_' . get_ordered_timeprefix_with_unique_id();
                $tmpFileName = normalizeFileNamePath(ImportDataFiles::getAbsoluteTmpDirectory() . '/' . $fileName);
                @unlink($tmpFileName);

                $handle = fopen($tmpFileName, 'w');
                if ($handle === FALSE) {
                    throw $this->createError(
                        ArProblemType::TYPE_ERROR,
                        ArProblemDomain::CONFIGURATIONS,
                        'error in temporary file to write',
                        "The file \"" . $tmpFileName . "\", can not be created.",
                        "CDRs will be not exported to the reseller.",
                        "It can be a problem in files and directories permissions."
                    );
                }

                $query = '
        SELECT ar_rate.from_time,
               ar_rate.internal_name,
               ar_rate_format.internal_name,
               ar_rate.short_description,
               ar_rate.source_data_file,
               ar_rate.html_description
        FROM  ar_rate_shared_with_reseller
        INNER JOIN ar_rate ON ar_rate_shared_with_reseller.ar_rate_id = ar_rate.id
        INNER JOIN ar_rate_format ON ar_rate_format.id = ar_rate.ar_rate_format_id
        WHERE ar_rate.from_time >= ?
        AND   ar_rate_shared_with_reseller.ar_reseller_id = ?
        ORDER BY ar_rate.from_time;';

                $stm = $conn->prepare($query);
                $stm->execute(array(fromUnixTimestampToMySQLTimestamp($minimumRateDate)
                , $this->getArResellerId()));

                // Genera e CSV file with the list of source data files to load/rate

                while ((($rs = $stm->fetch(PDO::FETCH_NUM)) !== false)) {
                    $data = array();
                    $data[] = $rs[0];
                    $data[] = $rs[1];
                    $data[] = $rs[2];
                    $data[] = $rs[3];
                    $data[] = $rs[4];
                    $data[] = $rs[5];
                    safe_fputcsv($handle, $data);
                }
                $stm->closeCursor();
                $isOk = fclose($handle);
                if (!$isOk) {
                    throw $this->createError(
                        ArProblemType::TYPE_ERROR,
                        ArProblemDomain::CONFIGURATIONS,
                        'error in temporary file to write closing',
                        "The file \"" . $tmpFileName . "\", can not be closed correctly.",
                        "CDRs will be not exported to the reseller.",
                        "It can be a problem in files and directories permissions."
                    );
                }

                // Move atomically the file in the export directory.
                // It is an atomic move because they are by construction on the same file system.
                $destFile = normalizeFileNamePath($this->getExportDirectory() . '/' . $fileName);
                $isOk = @rename($tmpFileName, $destFile);
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

                // Signal the rates as exported.
                // It is safe doing this, because there can not other jobs inserting info in the meantime.

                $query = 'UPDATE ar_rate_shared_with_reseller
                          SET is_exported = 1
                          WHERE is_exported <> 1
                          AND ar_reseller_id = ?';
                $stm = $conn->prepare($query);
                $stm->execute(array($this->getArResellerId()));
                $stm->closeCursor();
            }

            $this->commitTransactionOrSignalProblem($conn);
        } catch (ArProblemException $e) {
            $this->maybeRollbackTransaction($conn);
            throw($e);
        } catch (Exception $e) {
            $this->maybeRollbackTransaction($conn);

            $msg = "Error during exporting of rates to reseller \"" . $this->getResellerCode() . "\"." . $e->getMessage();
            if (isset($query) && (!isEmptyOrNull($query))) {
                $msg .= "\n\nAs possible hint, the last executed query is: " . $query;
            }

            throw $this->createError(
                ArProblemType::TYPE_ERROR,
                ArProblemDomain::APPLICATION,
                'error exporting rates ' . $e->getCode(),
                $msg,
                "CDRs and rates will be not exported to the reseller.",
                "This is probably an error in the application. Contact the assistance."
            );
        }

        return true;
    }

    public function endJob(PropelPDO $conn)
    {
        // NOTE: also if this job fails, the list will be created at next execution
        $this->createFilesToExportList();
    }

    /**
     * @param string $resellerCode
     * @return int|null ar_reseller.id or null in case of error
     */
    static public function fromResellerCodeToId($resellerCode)
    {
        $id = null;
        $conn = Propel::getConnection();

        $query = '
        SELECT ar_reseller.id
        FROM ar_reseller
        WHERE ar_reseller.internal_name = ?';

        $stmt = $conn->prepare($query);

        $isOk = $stmt->execute(array($resellerCode));
        if ($isOk === FALSE) {
            return null;
        }

        $isError = false;
        while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
            if (is_null($id)) {
                $id = $rs[0];
            } else {
                $isError = true;
            }
        }
        $stmt->closeCursor();

        if (is_null($id)) {
            $isError = true;
        }

        if ($isError) {
            return null;
        } else {
            return $id;
        }

    }

    /**
     * @param string $resellerCode
     * @return int|null ar_organization_unit.id or null in case of error
     */
    static public function fromResellerCodeToUnitId($resellerCode)
    {
        $id = null;

        $conn = Propel::getConnection();

        $resellerId = self::fromResellerCodeToId($resellerCode);
        if (is_null($resellerId)) {
            return null;
        }

        $query = '
        SELECT DISTINCT ar_organization_unit.id
        FROM ar_party
        INNER JOIN ar_organization_unit_has_structure
        ON ar_organization_unit_has_structure.ar_party_id = ar_party.id
        INNER JOIN ar_organization_unit
        ON ar_organization_unit_has_structure.ar_organization_unit_id = ar_organization_unit.id
        WHERE ar_party.ar_reseller_id = ?';

        $stmt = $conn->prepare($query);

        $isOk = $stmt->execute(array($resellerId));
        if ($isOk === FALSE) {
            return null;
        }

        $isError = false;
        while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
            if (is_null($id)) {
                $id = $rs[0];
            } else {
                $isError = true;
            }
        }
        $stmt->closeCursor();

        if (is_null($id)) {
            $isError = true;
        }

        if ($isError) {
            return null;
        } else {
            return $id;
        }
    }

    /**
     * @return int the ar_reseller.id
     * @throws ArProblemException
     */
    public function getArResellerId()
    {
        $id = self::fromResellerCodeToId($this->getResellerCode());
        if (is_null($id)) {
            throw $this->createError(
                ArProblemType::TYPE_ERROR,
                ArProblemDomain::CONFIGURATIONS,
                'misconfigured reseller code ' . $this->getResellerCode(),
                "There is no or two or more organizations with the same reseller code \"" . $this->getResellerCode() . "\", defined in the party section.",
                "CDRs will be not exported to the reseller.",
                "Specify the organization associated to the reseller, compiling the code inside party section, and making sure to use a distinct and unique reseller code."
            );
        } else {
            return $id;
        }
    }

    /**
     * @return int the ar_organization_unit.id of the reseller.
     * @throws ArProblemException
     */
    public
    function getOrganizationId()
    {
        $id = self::fromResellerCodeToUnitId($this->getResellerCode());
        if (is_null($id)) {
            throw $this->createError(
                ArProblemType::TYPE_ERROR,
                ArProblemDomain::CONFIGURATIONS,
                'misconfigured reseller code ' . $this->getResellerCode(),
                "There is no or two or more organizations with the same reseller code \"" . $this->getResellerCode() . "\", defined in the party section.",
                "CDRs will be not exported to the reseller.",
                "Specify the organization associated to the reseller, compiling the code inside party section, and making sure to use a distinct and unique reseller code."
            );
        } else {
            return $id;
        }
    }

    protected
    function communicationChannelExportSQLCode()
    {

        // Create a SQL IF expression exporting the correct channel.

        $arr = $this->exportedCommunicationChannels();
        if (!array_key_exists(ConfigureCommunicationChannels::SERVICE_CDR_CHANNEL, $arr)) {
            $arr[ConfigureCommunicationChannels::SERVICE_CDR_CHANNEL] = ConfigureCommunicationChannels::SERVICE_CDR_CHANNEL;
        }

        $open = 0;
        $str = '';
        foreach ($arr as $source => $dest) {
            $open++;
            $str .= ",IF(ar_communication_channel_type.internal_name LIKE '"
                . mysql_escape_string($source)
                . "', '"
                . mysql_escape_string($dest) . "'";
        }

        $str .= ",''"; // by default export the empty string
        $str .= str_repeat(')', $open);

        return $str;
    }

    /**
     * Generate error messages for extensions of the reseller, without an export code.
     */
    public function generateErrorMessageForMissingExportCodes()
    {
        $garbageKey = 'export-code-' . $this->getResellerCode();
        ArProblemException::garbageCollect($garbageKey, $this->getGlobalStartingDate(), null);

        $info = OrganizationUnitInfo::getInstance();
        $resellerId = $this->getOrganizationId();
        $children = $info->getAllChildren($resellerId);

        foreach ($children as $unitId => $nothing) {
            $data = $info->getDataInfo($unitId, null);
            if (isEmptyOrNull($data[OrganizationUnitInfo::DATA_EXPORT_CODE])) {
                $isExtension = $data[OrganizationUnitInfo::DATA_UNIT_TYPE_IS_LEAF];

                if ($isExtension) {
                    $problemEffect = 'Calls associated to this extension are not exported to reseller.';
                } else {
                    $problemEffect = 'Services and bundle rates associated to this extension are not exported to reseller.';
                }

                ArProblemException::createWithGarbageCollection(
                    ArProblemType::TYPE_CRITICAL,
                    ArProblemDomain::VOIP_ACCOUNTS,
                    null,
                    'export-code-for-unit-id-' . $unitId,
                    $garbageKey,
                    $this->getGlobalStartingDate(),
                    null,
                    'The customer/organization with id ' . $unitId
                    . ', and name "' . $info->getFullNameAtDate($unitId, null, false, false) . '"'
                    . ', for reseller "' . $this->getResellerCode() . '"'
                    . ', has an empty export code',
                    $problemEffect,
                    'Complete the export code with a unique name, that will be used for identifying the extension/account on the reseller side.'
                );
            }
        }
    }

    /**
     * @param bool $isServiceCDRS true for exporting only service CDRS,
     * false for exporting only normal CDRS
     * @param string $tmpFileName where writing the result
     *
     * @return string the SQL exporting code, accepting these params:
     * - starting rate frame (inclusive)
     * - the activation date of this job
     * - ending rate farme (not inclusive)
     * - filter on ar_cdr.cached_parent_id_hierarchy
     */
    protected function getExportCDRSQuery($isServiceCDRS, $tmpFileName) {

        // NOTE: is_service_cdr is set from the receiver if to_calldate IS NOT NULL
        // NOTE: this form is used because:
        // * only with this form the strings are CSV escaped into double " and recognized on the Haskell side
        // * the IFNULL is used for telling to Haskell that there is a NULL field, because it is not automatically escaped to \N by MySQL
        // * in case of service-CDRS export the masked telephone number, because it is a human readable description of the service, instead of the internal code of the number
        // This form must be not used for LOADING files, but another form must be used.
        // NOTE: only values with an export-code are sent.
        $query = <<<'NOWDOC'
        SELECT     ar_cdr.calldate,
                   IFNULL(ar_cdr.to_calldate, '\\N'),
                   ar_cdr.count_of_calls,
                   ar_cdr.destination_type,
                   ar_cdr.is_redirect,
                   ar_cdr.duration,
                   ar_cdr.billsec,
                   ar_cdr.income,
                   IFNULL(ar_organization_unit.export_code, '\\N'),
                   IF(ar_cdr.to_calldate IS NULL, IFNULL(ar_cdr.cached_external_telephone_number, '\\N'), IFNULL(ar_cdr.cached_masked_external_telephone_number, '\\N')),
                   IFNULL(ar_cdr.external_telephone_number_with_applied_portability, '\\N')

NOWDOC;
        $query .= $this->communicationChannelExportSQLCode();
        $query .= " INTO OUTFILE '$tmpFileName' ";

        $query .= <<<'NOWDOC'
        CHARACTER SET 'utf8'
        FIELDS TERMINATED BY ','
        OPTIONALLY ENCLOSED BY '"'
        ESCAPED BY '"'
        LINES TERMINATED BY '\r\n'
        FROM ar_cdr
        INNER JOIN ar_organization_unit
        ON ar_cdr.ar_organization_unit_id = ar_organization_unit.id
        INNER JOIN ar_communication_channel_type
        ON ar_cdr.ar_communication_channel_type_id = ar_communication_channel_type.id
        WHERE calldate >= ? AND calldate >= ? AND calldate < ?
        AND ar_cdr.cached_parent_id_hierarchy LIKE ?
        AND NOT destination_type = 5
        AND NOT destination_type = 4
        AND ar_organization_unit.export_code IS NOT NULL
NOWDOC;

        if ($isServiceCDRS) {
            $query .= ' AND is_service_cdr = 1 ';
        } else {
            $query .= ' AND is_service_cdr = 0 ';
        }

        return $query;
    }

    public
    function processChangedDay($fromDate, $toDate, PropelPDO $conn)
    {
        $year = date('Y', $fromDate);
        $month = date('m', $fromDate);
        $day = date('d', $fromDate);

        $logicalType = self::LOGICAL_TYPE;
        $formatType = self::FORMAT_TYPE;

        //NOTE: use this directory because the DB has write permissions
        $tmpFileName = '/var/tmp/' . ImportDataFiles::createInputStatusDataFileName($this->getResellerCode(), self::CDR_PROVIDER_TYPE, $logicalType, $formatType, $year, $month, $day, true);
        $fileName = basename($tmpFileName);
        @unlink($tmpFileName);

        $query = $this->getExportCDRSQuery(false, $tmpFileName);
        $stmt = $conn->prepare($query);

        $resellerCallFilter = '%/' . $this->getOrganizationId() . '/%';

        $isOk = $stmt->execute(array(
            fromUnixTimestampToMySQLTimestamp($fromDate),
            fromUnixTimestampToMySQLTimestamp($this->getActivationDate()),
            fromUnixTimestampToMySQLTimestamp($toDate),
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

    /**
     * Create an error that can be garbage collected according the values of the CDR.
     * @pre called only if the values of CDR fields are reliable
     *
     * @param int $errorType ArProblemType
     * @param int $problemDomain ArProblemDomain
     * @param string $problemDuplicationKey
     * @param string $problemDescription
     * @param string $problemEffect
     * @param string $problemProposedSolution
     * @return ArProblemException
     */
    protected
    function createError($errorType, $problemDomain, $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution)
    {
        $p = ArProblemException::createWithGarbageCollection(
            $errorType,
            $problemDomain,
            null,
            get_class($this) . '-' . $problemDuplicationKey,
            $this->getGarbageKey(),
            $this->garbageFrom,
            $this->garbageTo,
            'CDRs will not be exported to reseller  "' . $this->getResellerCode() . '".' . $problemDescription,
            'CDRs will be not received from the reseller, and it can not see new calls, or correction to calls of the past. ' . $problemEffect,
            $problemProposedSolution . ' If the problem persist contact the assistance.',
            null);

        return $p;
    }

    /**
     * A remote client can receive new data, only if it has already processed current info.
     * @return bool true if the remote server can receive files
     * @throws ArProblemException
     */
    protected function isRemoteServerReadyToReceiveFiles()
    {
        $fileName = normalizeFileNamePath($this->getExportDirectory() . '/' . ImportDataFiles::FILES_TO_EXPORT_LIST);
        return !file_exists($fileName);
    }

    protected function createAsterisellProviderDirectoryCheck()
    {
        $fileName = normalizeFileNamePath($this->getExportDirectory() . '/' . ImportDataFiles::IS_ASTERISELL_PROVIDER_DIRECTORY_CHECK);
        if (!file_exists($fileName)) {
            file_put_contents($fileName, "this is an Asterisell provider directory");
        }
    }

    /**
     * @return bool true if there are files to export
     * @throws ArProblemException
     */
    protected function createFilesToExportList()
    {

        $exportDir = $this->getExportDirectory();
        $sourceFiles = @scandir($exportDir);

        // NOTE: export also in case of empty directories, generating an empty list of files.
        // Then the remote server will download the file content that is empty

        $tmpFileName = normalizeFileNamePath(ImportDataFiles::getAbsoluteTmpDirectory() . '/' . ImportDataFiles::FILES_TO_EXPORT_LIST);
        @unlink($tmpFileName);
        $handle = fopen($tmpFileName, 'w');
        if ($handle === FALSE) {
            throw $this->createError(
                ArProblemType::TYPE_ERROR,
                ArProblemDomain::CONFIGURATIONS,
                'error in temporary file to write',
                "The file \"" . $tmpFileName . "\", can not be created.",
                "CDRs will be not exported to the reseller.",
                "It can be a problem in files and directories permissions."
            );
        }

        $createFile = false;
        foreach ($sourceFiles as $sourceFile) {
            if ($sourceFile === '.'
                || $sourceFile === '..'
                || $sourceFile === ImportDataFiles::FILES_TO_EXPORT_LIST
                || $sourceFile === ImportDataFiles::IS_ASTERISELL_PROVIDER_DIRECTORY_CHECK
            ) {
                continue;
            }
            $completeSourceFile = normalizeFileNamePath($exportDir . '/' . $sourceFile);
            if (is_file($completeSourceFile)) {
                $createFile = true;
                fwrite($handle, $sourceFile . "\n");
            }
        }
        fclose($handle);


        if ($createFile) {
            $destFileName = normalizeFileNamePath($exportDir . '/' . ImportDataFiles::FILES_TO_EXPORT_LIST);
            if (file_exists($destFileName)) {
                @unlink($destFileName);
            }
            $isOk = rename($tmpFileName, $destFileName);
            if ($isOk === FALSE) {
                throw $this->createError(
                    ArProblemType::TYPE_ERROR,
                    ArProblemDomain::CONFIGURATIONS,
                    'error in temporary file to write',
                    "The file \"" . $destFileName . "\", can not be created.",
                    "CDRs will be not exported to the reseller.",
                    "It can be a problem in files and directories permissions."
                );
            }
            return true;
        } else {
            return false;
        }
    }

    protected function testResellerCode() {
        if (is_null($this->getOrganizationId())) {
            throw $this->createError(
                ArProblemType::TYPE_ERROR,
                ArProblemDomain::CONFIGURATIONS,
                'misconfigured reseller code ' . $this->getResellerCode(),
                "There is no reseller with code \"" . $this->getResellerCode() . "\", defined in the party section of an organization.",
                "CDRs will be not exported to the reseller.",
                "Specify the organizaton associated to the reseller, compiling the code inside party section."
            );
        }

        $this->generateErrorMessageForMissingExportCodes();
    }


}
