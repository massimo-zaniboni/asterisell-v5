<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Common operations for generating an invoice.
 */
abstract class GenerateInvoicesFromTemplate extends ReportGenerator
{

    //
    // Abstract Methods
    //

    /**
     * @return InvoiceTemplate
     */
    abstract public function newInvoiceTemplate();

    //
    // Configuration Methods
    //

    /**
     * @var bool true for legal invoices, false for call reports similar to an invoice.
     * NOTE: this value is not related to isBilling report param.
     */
    protected $isLegal = true;

    /**
     * @param bool $v
     */
    public function setIsLegal($v)
    {
        $this->isLegal = $v;
    }

    /**
     * @return bool
     */
    public function getIsLegal()
    {
        return $this->isLegal;
    }

    //
    // Processing Methods
    //

    public function deriveReportParams()
    {

        $this->checkParamArOrganizationUnitIdExists();
        $this->checkParamFromDateExists();
        $this->checkParamToDateExists();

        if ($this->getIsLegal()) {

            if (is_null($this->getArReport()->getLegalConsecutiveNr())) {
                $this->signalProblem('Specify also the legal number of the report.');
            }

            if (is_null($this->getArReport()->getLegalDate())) {
                $this->signalProblem('Specify also the legal date of the report.');
            }
        }

        $report = $this->getArReport();

        $report->setParamShowCallCost(false);
        $report->setParamShowCallIncome(true);

        $report->setParamShowVoipProvider(false);
        $report->setParamShowMaskedTelephoneNumbers(true);

        $report->setParamShowCallDetails(false);
        $report->setParamShowCommunicationChannel(false);
        $report->setParamExpandToLevel(1);
    }

    public function getCharacterSet()
    {
        return "windows-1252";
    }

    public function calcStore($schedulerId)
    {
        /**
         * @var PropelPDO $conn
         */
        $conn = Propel::getConnection();

        $report = $this->getArReport();

        $fromDate = fromMySQLTimestampToUnixTimestamp($report->getFromDate());
        $toDate = fromMySQLTimestampToUnixTimestamp($report->getToDate());

        $store = new InvoiceCalcStore();
        $store->setCharacterSet($this->getCharacterSet());
        $store->process($fromDate, $toDate, $report, $schedulerId, $conn);
        return $store;
    }

    public function getReportUserReadableName()
    {
        if ($this->getIsLegal()) {
            return mytr('Invoice');
        } else {
            return mytr('Call Report');
        }
    }

    protected function internalGenerateReport()
    {

        $report = $this->getArReport();
        $startOrganizationId = $this->getArReport()->getArOrganizationUnitId();
        $isDraftReport = $report->getProducedReportIsDraft();

        /**
         * @var InvoiceCalcStore $calcStore
         */
        $calcStore = $this->getStore();

        $fromDate = $calcStore->getOrganizationFromDate($startOrganizationId);
        $report->setFromDate($fromDate);
        $toDate = fromMySQLTimestampToUnixTimestamp($report->getToDate());

        if (!OrganizationUnitInfo::getInstance()->getExists($startOrganizationId, $fromDate)) {
            $problemDuplicationKey = get_class($this) . ' - ' . rand();
            $problemDescription = "Report " . get_class($this) . " can not be generated, because there is no info about the subject organization. ";
            $problemEffect = 'The report can not be genereted.';
            $problemProposedSolution = 'The problem can be in report specification, or organization hierarchy. If it is not the case, contact the assistance.';
            $p = ArProblemException::createWithoutGarbageCollection(
                ArProblemType::TYPE_ERROR,
                ArProblemDomain::VOIP_ACCOUNTS,
                null,
                $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
            throw($p);
        }

        // Retrieve data
        //
        $partyId = OrganizationUnitInfo::getInstance()->getArPartyId($startOrganizationId, $fromDate);
        assert(!is_null($partyId));

        /**
         * @var ArParty $buyerParty
         */
        $buyerParty = ArPartyPeer::retrieveByPK($partyId);
        assert(!is_null($buyerParty));

        $params = ArParamsPeer::getDefaultParams();

        $pdf = $this->newInvoiceTemplate();
        $pdf->setBuyer($buyerParty);
        $pdf->setIsLegalInvoice($this->getIsLegal());
        $pdf->setArReport($report);
        $pdf->setCharacterSet($this->getStore()->getCharacterSet());
        $pdf->init();

        $descriptionColName = mytr("DESCRIPTION");

        $senderName = $params->getLegalName();
        $senderAddress = getCustomerAddressAccordingCulture(
            $params->getLegalCountry(),
            $params->getLegalStateProvince(),
            $params->getLegalCity(),
            $params->getLegalAddress(),
            $params->getLegalZipcode(),
            $params->getVat());
        $senderVat = $params->getVat();

        $receiverName = $buyerParty->getName();
        $receiverAddress = getCustomerAddressAccordingCulture(
            $buyerParty->getLegalCountry(),
            $buyerParty->getLegalStateProvince(),
            $buyerParty->getLegalCity(),
            $buyerParty->getLegalAddress(),
            $buyerParty->getLegalZipcode(),
            $buyerParty->getVat());
        $receiverVat = $buyerParty->getVat();

        $thereIsMoreSpace = false;
        $allDetails = $calcStore->getDetails($startOrganizationId, $report);
        $leftDetails = count($allDetails);
        $writtenDetailsInLastPage = 0;
        foreach ($allDetails as $lineDetails) {
            $writtenDetailsInLastPage++;
            $leftDetails--;
            list($detailName, $calls, $seconds, $cost, $income) = $lineDetails;

            // sfContext::getInstance()->getLogger()->info('INVOICE REPORT LINE: ' . $detailName);
            if (! $thereIsMoreSpace) {
                // Start a new page with header and footer

                $pdf->addNewPage();
                $pdf->writeSellerInfo(
                    $this->convertString($senderName),
                    $this->convertString($senderAddress
                        . maybeAddIfExistsCentral("\nemail: ", $params->getLegalEmail(), "")
                        . maybeAddIfExistsCentral("\n" . mytr("tel:"), $params->getLegalPhone(), "")
                        . maybeAddIfExistsCentral("\nfax: ", $params->getLegalFax(), "")
                    ));

                $pdf->writePageNumber();
                $pdf->writeDate(format_invoice_date_according_config($this->getArReport()->getLegalDate()));

                if ($this->getIsLegal()) {
                    $pdf->writeInvoiceNr($this->getArReport()->getLegalNrPrefix() . $this->getArReport()->getLegalConsecutiveNr());
                } else {
                }

                $pdf->writeBuyer(
                    $this->convertString($receiverName),
                    $this->convertString($receiverAddress)
                );

                $pdf->writeTimeframe($this->convertString(getUserReadableTimeFrame($fromDate, $toDate, true, true)));
                $pdf->writeDetailsHeader();
            }

            $line = array($descriptionColName => $this->convertString($this->convertString($detailName)),
                mytr("QUANTITY") => $this->convertString($calls),
                mytr("DURATION") => $this->convertString(format_minute_or_empty($seconds)),
                mytr("COST") => $this->convertString(from_db_decimal_to_pdf_txt_decimal($income)),
                "V_QUANTITY" => $calls,
                "V_DURATION_SECONDS" => $seconds,
                "V_COST" => $income
            );

            list($detailSpace, $totalSpace) = $pdf->writeDetailLine($line);
            $thereIsMoreSpace = True;

            if (!$detailSpace) {
                $thereIsMoreSpace = False;
                $writtenDetailsInLastPage = 0;
            } else if (!$totalSpace) {
               // We have space for more detail lines, but not for the totals.
               // In case the next page is probably the last page, we split details between this page and next,
               // otherwise we fill this page entirely.
               if ($leftDetails < ($writtenDetailsInLastPage - 2)) {
                   $thereIsMoreSpace = False;
                   $writtenDetailsInLastPage = 0;
               }
            }
        }

        // Print totals only in the last page of invoice

        list($setVatPerc,
            $setTotalCalls,
            $setTotalDuration,
            $setTotalCostsWithoutTax,
            $setTotalCostsTax,
            $setTotalCostsWithTax,
            $setTotalIncomesWithoutTax,
            $setTotalIncomesTax,
            $setTotalIncomesWithTax) = $calcStore->getInvoiceTotals($startOrganizationId, $params, $report);

        $pdf->writeTotals(
            $this->convertString(from_db_decimal_to_pdf_txt_decimal($setTotalIncomesWithoutTax))
            , $this->convertString(from_db_decimal_to_vat_perc_according_culture($setVatPerc) . '%')
            , $this->convertString(from_db_decimal_to_pdf_txt_decimal($setTotalIncomesTax))
            , $this->convertString(from_db_decimal_to_pdf_txt_decimal($setTotalIncomesWithTax))
        );

        // Print invoice comments only in the last page
        //
        $txt1 = $params->getInvoiceNotes();
        $txt2 = $params->getInvoicePaymentTerms();
        $txt = '';
        if (!is_null($txt1)) {
            $txt .= $txt1 . "\n\n";
        }
        if (!is_null($txt2)) {
            $txt .= $txt2;
        }
        if (strlen($txt) > 0) {
            $pdf->writeRemarque($txt);
        }

        // Save the PDF document
        $pdfReportContent = $pdf->Output("", "S");
        $report->setProducedReportAlreadyReviewed(0);
        $report->setProducedReportIsDraft($isDraftReport);
        $report->setProducedReportMimeType('application/pdf');
        $report->setDocumentContent($pdfReportContent);
        $report->setProducedReportFileTypeSuffix('pdf');

        $report->setTotalWithoutTax($setTotalIncomesWithoutTax);
        $report->setTax($setTotalIncomesTax);
        $report->setTotalWithTax($setTotalIncomesWithTax);
        $report->setAppliedVat($setVatPerc);

        $report->setLegalSenderName($senderName);
        $report->setLegalSenderVat($senderVat);
        $report->setLegalSenderAddress($senderAddress);

        $report->setLegalReceiverName($receiverName);
        $report->setLegalReceiverVat($receiverVat);
        $report->setLegalReceiverAddress($receiverAddress);

        $report->save();

    }

    ///////////////////////
    // Utility Functions //
    ///////////////////////

    /**
     * @param mixed $value a string in UTF-8 format
     * @return string in PDF format
     */
    public function convertString($value)
    {
        return $this->getStore()->convertString($value);
    }
}
