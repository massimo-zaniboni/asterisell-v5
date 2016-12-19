<?php

/* $LICENSE 2012, 2013:
 *
 * Copyright (C) 2012, 2013 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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
 * Debug the behaviour of a Billing Report
 */
class DebugBillingReport extends BaseBillingReport
{

    public function deriveReportParams() {

        $this->checkParamArOrganizationUnitIdExists();
        $this->checkParamFromDateExists();

        $report = $this->getArReport();

        $report->setParamShowAlsoOutgoingCalls(true);
        $report->setParamShowAlsoIncomingCalls(false);
        $report->setParamShowAlsoInternalCalls(false);
        $report->setParamShowCallCost(true);
        $report->setParamShowVoipProvider(true);
        $report->setParamShowMaskedTelephoneNumbers(true);
        $report->setParamShowCallDetails(false);
        $report->setParamShowCommunicationChannel(false);
        $report->setParamExpandToLevel(0);

        // Use the month as reference

        $paramDate = fromMySQLTimestampToUnixTimestamp($report->getFromDate());

        $reportDate1 = strtotime(date('Y', $paramDate) .'-' . date('m', $paramDate) . '-' . '01');
        $reportDate2 = strtotime('+1 month', $reportDate1);

        $report->setFromDate($reportDate1);
        $report->setToDate($reportDate2);
    }

    ///////////////////////////////////////
    // ReportGenerator Interface Support //
    ///////////////////////////////////////

    public function getReportUserReadableName()
    {
        return mytr('Debug Billing Report');
    }



    ///////////////////////
    // Report Generation //
    ///////////////////////

    protected function internalGenerateReport()
    {
        $this->makeCallsByOrganizationReport();
    }

    protected function makeCallsByOrganizationReport()
    {
        $conn = Propel::getConnection();

        $report = $this->getArReport();
        $isDraftReport = $report->getProducedReportIsDraft();
        $startOrganizationId = $this->getArReport()->getArOrganizationUnitId();
        $maxChildrenLevel = $this->getArReport()->getParamExpandToLevel();
        if (is_null($maxChildrenLevel)) {
            $maxChildrenLevel = 0;
        }

        $orderCode = $this->getArReport()->getArReportOrderOfChildrenId();

        $fromDate = fromMySQLTimestampToUnixTimestamp($report->getFromDate());
        if (!is_null($report->getToDate())) {
            $toDate = fromMySQLTimestampToUnixTimestamp($report->getToDate());
        } else {
            $toDate = null;
        }

        if (is_null($this->getStore())) {
            $store = new BaseBillingReportCalcStore();
            $store->process($fromDate, $toDate, $conn);
            $this->setStore($store);
        }

        if (!OrganizationUnitInfo::getInstance()->getExists($startOrganizationId, $fromDate)) {
            $problemDuplicationKey = get_class($this) . ' - ' . rand();
            $problemDescription = "Report " . get_class($this) . " can not be generated, because there is no info about the subjet organization. ";
            $problemEffect = 'The report can not be genereted.';
            $problemProposedSolution = 'The problem can be in report specification, or organization hierarchy. If it is not the case, contact the assistance.';
            $p = ArProblemException::createWithoutGarbageCollection(
                ArProblemType::TYPE_ERROR,
                ArProblemDomain::REPORTS,
                null,
                $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
            throw($p);
        }

        /////////////////
        // Write Stats //
        /////////////////

        $title = self::convertString($this->getReportDescription());

        $pdf = new BillingReportPDFTemplate();
        $pdf->setIsDraft($isDraftReport);
        $pdf->SetTitle($title);
        $pdf->setReportHeaderLine1($title);
        $pdf->AliasNbPages();
        $pdf->AddPage();

        $errorInfo = $this->getInfoAboutUnratedCDRs();
        if (!isEmptyOrNull($errorInfo)) {
            $pdf->addErrorInfo(self::convertString($errorInfo));
        }

        /**
         * These are the userAndOrganizationKey to display in the report. It is a queue, used for implementing
         * a breadth first display of data.
         * @var string[int] $userAndOrganizationKeyToDisplayQueue
         */
        $idsToDisplayQueue = array();
        $idsToDisplayQueue[] = $this->getOrganizationUnitIds();

        if ($maxChildrenLevel > 0) {
            $maxChildrenLevel = OrganizationUnitInfo::getFullIdsNestingLevel($this->getOrganizationUnitIds()) + $maxChildrenLevel;
        }

        $rootLevel = OrganizationUnitInfo::getFullIdsNestingLevel($this->getOrganizationUnitIds());

        $pdf->addDebugInfo("maxChildrenLevel: $maxChildrenLevel");
        $pdf->addDebugInfo($this->convertString("getCachedParentIdHierarchy: " . $this->getOrganizationUnitIds()));
        $pdf->addDebugInfo($this->convertString("rootLevel: $rootLevel"));

        // show organizations in the report
        $pdf->addDebugInfo('Show Organizations');
        foreach($this->getStore()->values as $ids => $ignored) {
            list($count, $duration, $cost, $income, $savings) = $this->getStoreValues($ids, null, null, null, null, null);

            $lastId = OrganizationUnitInfo::getLastId($ids);

            $pdf->addDebugInfo($this->convertString(


                    'ids: ' . $ids
                    . ", lastId: $lastId"
                    . ' name using hierarchy: ' . OrganizationUnitInfo::getInstance()->getFullNameAtDate($lastId, fromMySQLTimestampToUnixTimestamp($this->getArReport()->getFromDate()), false, false, null, false)
                    . ", name using store method 1: '")
                    . $this->getStore()->getCompleteNameInAFastWay($rootLevel, $ids, true) . "'"
                    . ", name using store method 2: '" . $this->getStore()->getCompleteNameInAFastWay($rootLevel, $ids, false) . "'"
                    . $this->convertString(", calls: $count, duration:" . format_minute($duration)
                    . ", cost: " . from_db_decimal_to_pdf_txt_decimal($cost)
                    . ", income: " . from_db_decimal_to_pdf_txt_decimal($income)
                    . ", savings: " . from_db_decimal_to_pdf_txt_decimal($savings)

            ));
        }

        // show the vendors
        $pdf->addDebugInfo('Show Vendors');
        $ids = $this->getOrganizationUnitIds();
        foreach($this->getOrderedVendors($ids) as $vendorId => $ignored) {
            list($count, $duration, $cost, $income, $savings) = $this->getStoreValues($ids, null, $vendorId, null, null, null);
            $pdf->addDebugInfo($this->convertString('vendor: ' . ArVendorPeer::retrieveByPK($vendorId)->getName()
                    . ", calls: $count, duration:" . format_minute($duration)
                    . ", cost: " . from_db_decimal_to_pdf_txt_decimal($cost)
                    . ", income: " . from_db_decimal_to_pdf_txt_decimal($income)
                    . ", savings: " . from_db_decimal_to_pdf_txt_decimal($savings)
            ));
        }

        // show the channels
        $pdf->addDebugInfo('Show Channels');
        $ids = $this->getOrganizationUnitIds();
        foreach($this->getOrderedCommunicationChannels($ids) as $channelId => $ignored) {
            list($count, $duration, $cost, $income, $savings) = $this->getStoreValues($ids, null, null, $channelId, null, null);
            $pdf->addDebugInfo($this->convertString('channel: ' . ArCommunicationChannelTypePeer::retrieveByPK($channelId)->getName()
                        . ", calls: $count, duration:" . format_minute($duration)
                        . ", cost: " . from_db_decimal_to_pdf_txt_decimal($cost)
                        . ", income: " . from_db_decimal_to_pdf_txt_decimal($income)
                        . ", savings: " . from_db_decimal_to_pdf_txt_decimal($savings)
            ));
        }

        // Test values if the different sums are 0
        $pdf->addDebugInfo('Show Organizations Sums');
        foreach($this->getStore()->values as $ids => $ignored) {
            list($count, $duration, $cost, $income, $savings) = $this->getStoreValues($ids, null, null, null, null, null);

            // TODO fa la somma dei VENDORS e dei CHANNELS e controlla se i dati corrispondono

            $lastId = OrganizationUnitInfo::getLastId($ids);

            $pdf->addDebugInfo($this->convertString(


                'ids: ' . $ids
                        . ", lastId: $lastId"
                        . ' name using hierarchy: ' . OrganizationUnitInfo::getInstance()->getFullNameAtDate($lastId, fromMySQLTimestampToUnixTimestamp($this->getArReport()->getFromDate()), false, false, null, false)
                        . ", name using store method 1: '")
                    . $this->getStore()->getCompleteNameInAFastWay($rootLevel, $ids, true) . "'"
                    . ", name using store method 2: '" . $this->getStore()->getCompleteNameInAFastWay($rootLevel, $ids, false) . "'"
                    . $this->convertString(", calls: $count, duration:" . format_minute($duration)
                                . ", cost: " . from_db_decimal_to_pdf_txt_decimal($cost)
                                . ", income: " . from_db_decimal_to_pdf_txt_decimal($income)
                                . ", savings: " . from_db_decimal_to_pdf_txt_decimal($savings)

                    ));
        }



        // Save the PDF document
        $pdfReportContent = $pdf->Output("", "S");
        $report->setProducedReportAlreadyReviewed(false);
        $report->setProducedReportIsDraft($isDraftReport);
        $report->setProducedReportMimeType('application/pdf');
        $report->setDocumentContent($pdfReportContent);
        $report->setProducedReportFileTypeSuffix('pdf');
        $report->save();

    }
}
