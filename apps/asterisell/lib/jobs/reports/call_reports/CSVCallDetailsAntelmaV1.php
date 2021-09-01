<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2020 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Export all calls in time-frame to a CSV file, using AntelmaV1 format.
 * The report advise only of the presence of the file, but the content
 * will be in `self::getOutFileName()` file.
 */
class CSVCallDetailsAntelmaV1 extends ReportGenerator
{

    /**
     * 
     * @return string
     */
    public function getOutFileName() {
        return getAsterisellCompleteDataFileOutputFile('calls_to_bill.csv');   
    }
    
    ///////////////////////////////
    // ReportGenerator Interface //
    ///////////////////////////////

    public function getReportUserReadableName() {
        return 'Rated calls in CSV format';
    }
    
    protected function calcStore($schedulerId) {
        return new ReportCalcStore();
    }
    
    public function deriveReportParams() {
        $report = $this->getArReport();
        $report->setParamShowCallDetails(true);
        $report->setProducedReportMimeType('text/plain');
        $report->setProducedReportFileTypeSuffix('txt');
    }

    protected function internalGenerateReport()
    {
      $info = OrganizationUnitInfo::getInstance();
      
      $report = $this->getArReport();

      $reportQuery = '
          SELECT
            ar_cdr.id
          , ar_cdr.calldate
          , ar_cdr.destination_type
          , ar_cdr.billsec
          , ar_cdr.cached_external_telephone_number
          , ar_cdr.external_telephone_number_with_applied_portability
          , ar_cdr.cost
          , ar_cdr.income
          , ar_cdr.imported_info
          , ar_cdr.cached_parent_id_hierarchy
          , ch.name
          , ch.internal_name
          , tp.geographic_location
          , tp.operator_type
          , ar_cdr.billable_ar_organization_unit_id 
       
  FROM ar_cdr 
  
  INNER JOIN ar_telephone_prefix AS tp 
  ON ar_cdr.ar_telephone_prefix_id = tp.id
  
  INNER JOIN ar_communication_channel_type AS ch
  ON ar_cdr.ar_communication_channel_type_id = ch.id

  WHERE ar_cdr.destination_type <> ' . DestinationType::error . '
  AND ar_cdr.destination_type <> ' . DestinationType::ignored . '
  AND ar_cdr.destination_type <> ' . DestinationType::known_error . '
  AND ar_cdr.calldate >= ?
  AND ar_cdr.calldate < ?
  
  ORDER BY ar_cdr.billable_ar_organization_unit_id, calldate
        ';

        try {
          $stmt = FixedJobProcessor::prepareFetchStmt($reportQuery);
          $stmt->execute(array($this->getArReport()->getFromDate(), $this->getArReport()->getToDate()));

          $out = fopen($this->getOutFileName(), 'w');
          
          // Set UTF-8 encoding
          fwrite($out, "\xEF\xBB\xBF");

          // Write header
          $line =
                  csv_field('calldate', true)
                . csv_field('cli', false)
                . csv_field('external_telephone_number', false)
                . csv_field('external_telephone_number_with_applied_portability', false)
                . csv_field('external_telephone_prefix_with_applied_portability', false)
                . csv_field('external_operator_description', false)
                . csv_field('billsec', false)
                . csv_field('cost', false)
                . csv_field('income', false)
                . csv_field('channel', false)
                . csv_field('customer_internal_id', false)
                . csv_field('customer_name', false)
                . csv_field('customer_crm', false);
          fwrite($out, $line);
          
          while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
            // Some info of the CDR useful to Antelma is exported in the imported_info field
            $cdrInfo = str_getcsv($rs[8], $delimeter = "\t");
            $cli = '<missing>';
            $originalPrefix = '<missing>';
            $portedPrefix = '<missing>';
            $operatorDescr = '<missing>';
            if (strcmp($cdrInfo[0],'twt-cps-v1') == 0) {
                $cli = $cdrInfo[1];
                $originalPrefix = $cdrInfo[2];
                $portedPrefix = $cdrInfo[3];
                $operatorDescr = $cdrInfo[4];
            }
            
            $callDate = $rs[1];
            $callDateU = fromMySQLTimestampToUnixTimestamp($callDate);
            $billableUnitId = $rs[14];
           
            $customerName = $info->getFullNameAtDate($billableUnitId, $callDateU);
            
            $customerCRM = null;
            $unitData = $info->getDataInfo($billableUnitId, $callDateU);
            if(!is_null($unitData)) {
              $customerCRM = $unitData[OrganizationUnitInfo::DATA_PARTY_CRM];
            }
            if (isEmptyOrNull($customerCRM)) {
                $customerCRM = '<missing>';
            }
            
            $line =
              "\r\n"
              . csv_field($callDate, true)  // calldate
              . csv_field($cli, false)  //  CLI
              . csv_field($rs[4], false)  // external_telephone_number
              . csv_field($rs[5], false)  // external_telephone_number_with_applied_portability
              . csv_field($portedPrefix, false)  // external_telephone_prefix_with_applied_portability
              . csv_field($operatorDescr, false)  // external_operator_description 
              . csv_field($rs[3], false)  // billsec
              . csv_field(from_db_decimal_to_php_decimal($rs[6]), false)  // cost
              . csv_field(from_db_decimal_to_php_decimal($rs[7]), false)  // income
              . csv_field($rs[11], false)  // channel
              . csv_field($billableUnitId, false) // customer_internal_id
              . csv_field($customerName, false)  // customer_name
              . csv_field($customerCRM, false)  // customer_crm
              ;
                      
            fwrite($out, $line);
          }
          $stmt->closeCursor();
          fclose($out);
  
        } catch (ArProblemException $e) {
            throw($e);
        } catch (Exception $e) {
            $report->setDocumentContent("Error during the generation of file " . $this->getOutFileName());

            throw(ArProblemException::createFromGenericExceptionWithoutGarbageCollection(
                $e
                , get_class($this)
                , get_class($this)
                , "Report is not generated."
                , "If the error persist contact the assistance."
            ));
        }
        $report->setDocumentContent("The calls from " . $report->getFromDate() . " to " . $report->getToDate() . " are saved on file " . $this->getOutFileName());
    }
}
