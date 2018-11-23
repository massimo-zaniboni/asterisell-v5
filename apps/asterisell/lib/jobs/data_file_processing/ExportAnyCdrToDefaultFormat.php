<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Used for exporting any CDR in `ar_cdr` to a default format (the same used for resellers).
 */
class ExportAnyCdrToDefaultFormat extends FixedJobProcessor
{

    public function process() {
        return '';
    }

    /**
     * @param string $sourceFileBaseName base file name to use
     * @param int $year
     * @param int|null $month
     * @param int|null $day
     * @return string the name of the created file
     * @throws ArProblemException
     */
    public function exportStatusFile($sourceFileBaseName, $year, $month, $day)
    {
        $conn = Propel::getConnection();

        list($statusPrefix, $fromDate, $toDate) = ImportDataFiles::getTimeFrame($year, $month, $day);
        $cdrProviderName = ArCdrProvider::MANUAL_IMPORTING;
        $logicalTypeName = 'asterisell-provider';
        $formatName = 'v1';

        $cdrProviderId = CustomCDRServices::getInstance()->getCdrProviderId($cdrProviderName);
        $logicalTypeId = CustomCDRServices::getInstance()->getLogicalTypeId($logicalTypeName);
        $versionTypeId = CustomCDRServices::getInstance()->getLogicalTypeAndVersionId($logicalTypeName, $formatName);

        if (is_null($cdrProviderId) || is_null($logicalTypeId) || is_null($versionTypeId)) {
            throw(ArProblemException::createWithoutGarbageCollection(
                ArProblemType::TYPE_ERROR,
               ArProblemDomain::CONFIGURATIONS,
               null,
               "export-any-cdr",
               "Missing configurations for $cdrProviderName, $logicalTypeName, $formatName",
               "CDRS will be not exported",
               "Check if the application is correctly installed, or contact the assistance."
               ));
        }

        // NOTE: write to directory accesible from MySQL proces
        $tmpFileName = '/var/tmp/' . ImportDataFiles::createInputStatusDataFileName(getInstanceCodeName() . '-' . $sourceFileBaseName, $cdrProviderName, $logicalTypeName, $formatName, $year, $month, $day);
        @unlink($tmpFileName);
        $query = $this->getExportCDRSQuery($tmpFileName);
        $stmt = $conn->prepare($query);

        $isOk = $stmt->execute(array(fromUnixTimestampToMySQLTimestamp($fromDate), fromUnixTimestampToMySQLTimestamp($toDate)));
        $stmt->closeCursor();

        if ($isOk === FALSE) {
            throw(ArProblemException::createWithoutGarbageCollection(
               ArProblemType::TYPE_ERROR,
               ArProblemDomain::APPLICATION,
               null,
               "export-any-cdr",
               'error in query ' . $query,
               "Error in query \"" . $query . "\"",
               "CDRs will be not exported.",
               "This is an error in the code. Contact the assistance."
            ));
        }

        return $tmpFileName;
     }

    /**
     * @param string $tmpFileName where writing the result
     *
     * @return string the SQL exporting code, accepting these params:
     * - starting rate frame (inclusive)
     * - the activation date of this job
     * - ending rate farme (not inclusive)
     * - filter on ar_cdr.cached_parent_id_hierarchy
     */
    protected function getExportCDRSQuery($tmpFileName) {

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
                   IFNULL(ar_cdr.external_telephone_number_with_applied_portability, '\\N'),
                   ar_communication_channel_type.internal_name
NOWDOC;
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
        WHERE calldate >= ? AND calldate < ?
        AND NOT destination_type = 5
        AND NOT destination_type = 4
        ORDER BY calldate
NOWDOC;

        return $query;
    }

}
