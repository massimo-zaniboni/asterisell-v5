<?php

/* $LICENSE 2017:
 *
 * Copyright (C) 2017 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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
 * Generate a summary CSV report, with the list of all invoices.
 */
class GenerateCSVInvoiceSummaryReport extends JobProcessor
{

    //
    // Customizable Methods
    //

    public function getEmailSubject(ArReportSet $reportSet) {
        return "Invoices of report-set " . $reportSet->getId() . " with calls from  "
            . fromUnixTimestampToMySQLDate(fromMySQLTimestampToUnixTimestamp($reportSet->getFromDate()));
    }

    public function getEmailBody(ArReportSet $reportSet) {
        return "The attachment contains the list of invoices, in CSV format, of the report-set "
            . $reportSet->getId()
            . " with calls from " . $reportSet->getFromDate()
            . " to " . $reportSet->getToDate()
            . " (excluded). Note: invoices containing postponed calls in the past, can have different billing time frames."
            ;
    }

    public function getEmailAttachmentName(ArReportSet $reportSet) {
        return "invoices_" . fromUnixTimestampToMySQLDate(fromMySQLTimestampToUnixTimestamp($reportSet->getFromDate()));
    }

    //
    // Implementation
    //

    public function processEvent(JobData $jobData, $parentJobId)
    {

        if (!($jobData instanceof GenerateSummaryReportEvent)) {
            return null;
        }

        /**
         * @var GenerateSummaryReportEvent $jobData
         */

        $reportSetId = $jobData->reportSetId;
        if (is_null($reportSetId)) {
            return null;
        }

        $report = $this->generateReport($reportSetId);

        return 'Generated summary report ' . $report->getId();
    }

    /**
     * Calculate and return a compact CSV list of reports inside the set.
     * @param int $reportSetId
     * @return ArReport
     * @throws ArProblemException
     */
    public function generateReport($reportSetId)
    {

        $allTagsQuery = 'SELECT id, internal_name FROM ar_tag';

        $partyTagQuery = 'SELECT ar_tag_id FROM ar_party_has_tag WHERE ar_party_id = ?';

        $allReportsQuery = 'SELECT id, ar_organization_unit_id, legal_date
                            FROM ar_report
                            WHERE ar_report_set_id = ?
                            ORDER BY legal_consecutive_nr, legal_receiver_name';

        $reportQuery = '
          SELECT
            r.legal_nr_prefix AS legal_prefix
          , r.legal_consecutive_nr AS legal_nr
          , r.legal_date AS legal_date
          , r.legal_receiver_name AS legal_receiver_name
          , r.legal_receiver_vat AS legal_vat
          , r.ar_organization_unit_id AS organization_id
          , p.external_crm_code AS crm_code
          , p.email AS client_email
          , p.payment_iban AS client_iban
          , p.payment_bic AS client_bic
          , p.payment_sepa AS client_sepa
          , p.legal_address AS legal_address
          , p.legal_city AS legal_city
          , p.legal_zipcode AS legal_zipcode
          , p.legal_state_province AS legal_state_province
          , p.legal_country AS legal_country
          , p.email AS email
          , p.phone AS phone
          , p.migration_field_for_telephone AS migration_field_for_telephone
          , p.migration_field_for_adsl AS migration_field_for_adsl
          , r.total_without_tax AS total_without_tax
          , r.applied_vat AS vat_perc
          , r.tax AS vat_tax
          , r.total_with_tax AS total_with_tax
          , r.from_date AS from_date
          , r.to_date AS to_date
        FROM ar_report as r, ar_party as p
        WHERE r.id = ? AND p.id = ?
        ';

        $info = OrganizationUnitInfo::getInstance();
        $conn = Propel::getConnection();
        $conn->beginTransaction();

        try {

            // Retrieve party tags

            $stmt = $conn->prepare($allTagsQuery);
            $stmt->execute();

            /** @var array $tags id => tag_name */
            $tags = array();

            $nrOfTags = 0;
            while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
                $nrOfTags++;
                $id = $rs[0];
                $tagName = $rs[1];

                $tags[$id] = $tagName;
            }
            $stmt->closeCursor();

            // Retrieve the list of reports:
            // * they are usually not very much
            // * I load them one time, because I can not open multiple queries
            // * I can use business-logic code for retrieving the correct party info associated to the report

            $stmt = $conn->prepare($allReportsQuery);
            $stmt->execute(array($reportSetId));

            /** @var array $reports report_id => ar_party_id */
            $reports = array();

            /** @var array $orderedReports report_id ordred by legal number */
            $orderedReports = array();

            $nrOfReports = 0;
            while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
                $nrOfReports++;
                $id = $rs[0];
                $unitId = $rs[1];
                $reportDate = fromMySQLTimestampToUnixTimestamp($rs[2]);

                $orderedReports[] = $id;
                $reports[$id] = $info->getArPartyId($unitId, $reportDate);
            }
            $stmt->closeCursor();

            /**
             * @var string $content the content of the report
             * NOTE: it assumes that all the report content can be stored in the RAM of the server,
             * before serializating the result.
             */
            $content = '';

            // Set UTF-8 encoding
            $content .= "\xEF\xBB\xBF";

            //
            // Header
            //

            // TODO update here
            $line =
                csv_field(mytr('document_nr'), true)
                . csv_field(mytr('document_date'), false)
                . csv_field(mytr('calls_from'), false)
                . csv_field(mytr('calls_to'), false)
                . csv_field(mytr('customer'), false)
                . csv_field(mytr('customer_vat'), false)
                . csv_field(mytr('customer_relationship_code'), false)
                . csv_field(mytr('customer_IBAN'), false)
                . csv_field(mytr('customer_BIC'), false)
                . csv_field(mytr('customer_SEPA'), false)
                . csv_field(mytr('address'), false)
                . csv_field(mytr('email'), false)
                . csv_field(mytr('telephone'), false)
                . csv_field(mytr('migration_field_for_telephone'), false)
                . csv_field(mytr('migration_field_for_adsl'), false)
                . csv_field(mytr('total_without_tax'), false)
                . csv_field(mytr('vat_perc'), false)
                . csv_field(mytr('total_tax'), false)
                . csv_field(mytr('total_with_tax'), false);

            foreach ($tags as $tagId => $tagName) {
                $line .= csv_field(mytr("customer_tag_") . $tagName, false);
            }

            $content .= $line;

            //
            // Show data
            //

            $totAmount = 0;
            $totTax = 0;
            $totWithTax = 0;
            $taxPerc = 0;

            $partyTagStmt = $conn->prepare($partyTagQuery);
            $reportStmt = $conn->prepare($reportQuery);

            foreach ($orderedReports as $reportId) {
                $partyId = $reports[$reportId];

                // Retrieve party tags

                /** @var array $partyTags tag_id => true */
                $partyTags = array();

                $partyTagStmt->execute(array($partyId));
                while (($rs = $partyTagStmt->fetch(PDO::FETCH_NUM)) !== false) {
                    $tagId = $rs[0];
                    $partyTags[$tagId] = true;
                }
                $partyTagStmt->closeCursor();

                $reportStmt->execute(array($reportId, $partyId));
                while (($rs = $reportStmt->fetch(PDO::FETCH_ASSOC)) !== false) {
                    $totAmount += $rs['total_without_tax'];
                    $totTax += $rs['vat_tax'];
                    $totWithTax += $rs['total_with_tax'];
                    $taxPerc = $rs['vat_perc'];
                    // TODO if the tax is not the same for all invoices, it can be a problem

                    $line =
                        "\r\n"
                        . csv_field($rs['legal_prefix'] . $rs['legal_nr'], true)
                        . csv_field(fromUnixTimestampToSymfonyStrDate(fromMySQLTimestampToUnixTimestamp($rs['legal_date'])), false)
                        . csv_field(fromUnixTimestampToSymfonyStrTimestamp(fromMySQLTimestampToUnixTimestamp($rs['from_date'])), false)
                        . csv_field(fromUnixTimestampToSymfonyStrTimestamp(fromMySQLTimestampToUnixTimestamp($rs['to_date'])), false)
                        . csv_field($rs['legal_receiver_name'], false)
                        . csv_field($rs['legal_vat'], false)
                        . csv_field($rs['crm_code'], false)
                        . csv_field($rs['client_iban'], false)
                        . csv_field($rs['client_bic'], false)
                        . csv_field($rs['client_sepa'], false)
                        . csv_field(getCustomerAddressAccordingCulture(
                              $rs['legal_country']
                            , $rs['legal_state_province']
                            , $rs['legal_city']
                            , $rs['legal_address']
                            , $rs['legal_zipcode']
                            , $rs['legal_vat']), false)
                        . csv_field($rs['email'], false)
                        . csv_field($rs['phone'], false)
                        . csv_field($rs['migration_field_for_telephone'], false)
                        . csv_field($rs['migration_field_for_adsl'], false)
                        . csv_numeric_field(from_db_decimal_to_php_decimal($rs['total_without_tax']), false)
                        . csv_numeric_field(from_db_decimal_to_php_decimal($rs['vat_perc']), false)
                        . csv_numeric_field(from_db_decimal_to_php_decimal($rs['vat_tax']), false)
                        . csv_numeric_field(from_db_decimal_to_php_decimal($rs['total_with_tax']), false);

                    foreach ($tags as $tagId => $tagName) {
                        if (array_key_exists($tagId, $partyTags)) {
                            $v = 1;
                        } else {
                            $v = 0;
                        }
                        $line .= csv_numeric_field($v, false);
                    }

                    $content .= $line;
                }
                $reportStmt->closeCursor();
            }


             // Generate the report

            $reportSet = ArReportSetPeer::retrieveByPK($reportSetId);

            $report = new ArReport();

            $report->setReportName($this->getEmailSubject($reportSet));
            $report->setProducedReportShortDescription($this->getEmailSubject($reportSet));
            $report->setProducedReportAdditionalDescription($this->getEmailBody($reportSet));

            $report->setTotalWithoutTax($totAmount);
            $report->setTotalWithTax($totWithTax);
            $report->setTax($totTax);
            $report->setAppliedVat($taxPerc);

            $report->setAboutArReportSetId($reportSetId);
            $report->setProducedReportGenerationDate(time());
            $report->setProducedReportAlreadyReviewed(false);
            $report->setProducedReportIsDraft(false);
            $report->setCachedParentIdHierarchy(null);
            $report->setFromDate($reportSet->getFromDate());
            $report->setToDate($reportSet->getToDate());

            $report->setParamShowCallDetails(false);
            $report->setParamIsLegal(false);
            $report->setParamShowCallIncome(true);
            $report->setParamShowAlsoIncomingCalls(true);
            $report->setParamShowAlsoInternalCalls(true);
            $report->setParamShowAlsoOutgoingCalls(true);
            $report->setParamShowAlsoSystemCalls(true);
            $report->setParamShowCallCost(true);
            $report->setParamShowConnectionType(true);

            $report->setReportMailBody($this->getEmailBody($reportSet));
            $report->setReportMailSubject($this->getEmailSubject($reportSet));
            $report->setReportAttachmentFileName($this->getEmailAttachmentName($reportSet));
            $report->setReportAttachmentFileNameAddReportDate(false);

            $report->setProducedReportMimeType('text/csv');
            $report->setDocumentContent($content);
            $report->setProducedReportFileTypeSuffix('csv');

            $report->setProducedReportMustBeRegenerated(false);

            $report->save($conn);

            $permission = new ArReportAlsoFor();
            $permission->setArRoleId(ArRolePeer::retrieveByInternalName(ArRole::ADMIN)->getId());
            $permission->setArReportId($report->getId());
            $permission->save($conn);

            $permission = new ArReportAlsoFor();
            $permission->setArRoleId(ArRolePeer::retrieveByInternalName(ArRole::ACCOUNTANT)->getId());
            $permission->setArReportId($report->getId());
            $permission->save($conn);

            ArReportPeer::publishReportToUsers($report->getId(), $report->getProducedReportAlreadyReviewed(), true, $conn);

            $conn->commit();
            return $report;

        } catch (ArProblemException $e) {
            $this->maybeRollbackTransaction($conn);
            throw($e);
        } catch (Exception $e) {
            $this->maybeRollbackTransaction($conn);
            throw(ArProblemException::createFromGenericExceptionWithoutGarbageCollection(
                $e
                , get_class($this)
                , get_class($this)
                , "Summary report is not generated."
                , "If the error persist contact the assistance."
            ));
        }
    }

}
