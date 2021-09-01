<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2021 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Export all the invoices details to a CSV file.
 */
class CSVInvoices extends ReportGenerator {

    ///////////////////////////////
    // ReportGenerator Interface //
    ///////////////////////////////

    public function getReportUserReadableName() {
        return 'CSV invoincing data ';
    }

    public function deriveReportParams() {
        $report = $this->getArReport();
        $report->setProducedReportMimeType('text/csv');
        $report->setProducedReportFileTypeSuffix('csv');
    }

    public function getCharacterSet() {
        return "UTF-8";
    }

    public function calcStore($schedulerId) {
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

    protected function internalGenerateReport() {
        $info = OrganizationUnitInfo::getInstance();

        $report = $this->getArReport();
        $isDraftReport = $report->getProducedReportIsDraft();
        $params = ArParamsPeer::getDefaultParams();
        $totalsGroup = 'totals';

        // Write on a stream, because it is more similar to the BLOB content

        $stream = fopen('php://memory', 'r+');

        // Set UTF-8 encoding
        fwrite($stream, "\xEF\xBB\xBF");

        // Header

        $line = csv_field('customer name', true) .
                csv_field('customer VAT code', false) .
                csv_field('customer CRM', false) .
                csv_field('customer internal ID', false) .
                csv_field('contract', false) .
                csv_field('contract', false) .
                csv_field('complete address', false) .
                csv_field('country', false) .
                csv_field('state or province', false) .
                csv_field('city', false) .
                csv_field('address', false) .
                csv_field('ZIP code', false) .
                csv_field('details level', false) .
                csv_field('details name', false) .
                csv_field('calls', false) .
                csv_field('duration seconds', false) .
                csv_field('duration human readable', false) .
                csv_field('VAT perc', false) .
                csv_field('cost without tax', false) .
                csv_field('cost tax', false) .
                csv_field('cost with tax', false) .
                csv_field('income without tax', false) .
                csv_field('income tax', false) .
                csv_field('income with tax', false) .
                csv_field('earn without tax', false) .
                csv_field('earn with tax', false);

        fwrite($stream, $line);

        // Write totals and details for each customer

        $organizationIds = $this->getStore()->getAllOrganizationIds();
        foreach ($organizationIds as $organizationId) {
            $organizationFromDate = $this->getStore()->getOrganizationFromDate($organizationId);

            $partyId = $info->getArPartyId($organizationId, $organizationFromDate);
            $buyerParty = ArPartyPeer::retrieveByPK($partyId);

            if (!is_null($buyerParty)) {
                $name = $buyerParty->getName();
                $vat = $buyerParty->getVat();
                $crm = $buyerParty->getExternalCrmCode();
                $contract = $buyerParty->getContractNumber();
                $email = $buyerParty->getEmail();
                $country = $buyerParty->getLegalCountry();
                $stateProvince = $buyerParty->getLegalStateProvince();
                $city = $buyerParty->getLegalCity();
                $address = $buyerParty->getLegalAddress();
                $zipCode = $buyerParty->getLegalZipcode();
                $completAddress = getCustomerAddressAccordingCulture($country, $stateProvince, $city, $address, $zipCode, $vat);
            } else {
                $name = "";
                $vat = "";
                $crm = "";
                $contract = "";
                $email = "";
                $country = "";
                $stateProvince = "";
                $city = "";
                $address = "";
                $zipCode = "";
                $completAddress = "";
            }

            if (isEmptyOrNull($crm)) {
                $crm = "customer-id-" . $organizationId;
            }

            $lineForCustomer = "\r\n" .
                    csv_field($name, true) .
                    csv_field($vat, false) .
                    csv_field($crm, false) .
                    csv_field($organizationId, false) .
                    csv_field($contract, false) .
                    csv_field($email, false) .
                    csv_field($completAddress, false) .
                    csv_field($country, false) .
                    csv_field($stateProvince, false) .
                    csv_field($city, false) .
                    csv_field($address, false) .
                    csv_field($zipCode, false);

            fwrite($stream, $lineForCustomer);

            list($setVatPerc,
                    $setTotalCalls,
                    $setTotalDurationSec,
                    $setTotalCostsWithoutTax,
                    $setTotalCostsTax,
                    $setTotalCostsWithTax,
                    $setTotalIncomesWithoutTax,
                    $setTotalIncomesTax,
                    $setTotalIncomesWithTax) = $this->getStore()->getInvoiceTotals($organizationId, $params, $report);

            $setVatPercDecimal = from_db_decimal_to_php_decimal($setVatPerc);
            $setVatPercH = from_db_decimal_to_vat_perc_according_culture($setVatPerc);
            
            $setTotalEarnsWithoutTax = $setTotalIncomesWithoutTax - $setTotalCostsWithoutTax;
            $setTotalEarnsWithTax = $setTotalIncomesWithTax - $setTotalCostsWithTax;

            $setTotalDurationH = format_minute($setTotalDurationSec);

            $line = csv_field('0', false) .
                    csv_field($totalsGroup, false) .
                    csv_field($setTotalCalls, false) .
                    csv_field($setTotalDurationSec, false) .
                    csv_field($setTotalDurationH, false) .
                    csv_field($setVatPercH, false) .
                    csv_field(from_db_decimal_to_invoice_decimal($setTotalCostsWithoutTax), false) .
                    csv_field(from_db_decimal_to_invoice_decimal($setTotalCostsTax), false) .
                    csv_field(from_db_decimal_to_invoice_decimal($setTotalCostsWithTax), false) .
                    csv_field(from_db_decimal_to_invoice_decimal($setTotalIncomesWithoutTax), false) .
                    csv_field(from_db_decimal_to_invoice_decimal($setTotalIncomesTax), false) .
                    csv_field(from_db_decimal_to_invoice_decimal($setTotalIncomesWithTax), false) .
                    csv_field(from_db_decimal_to_invoice_decimal($setTotalEarnsWithoutTax), false) .
                    csv_field(from_db_decimal_to_invoice_decimal($setTotalEarnsWithTax), false);

            fwrite($stream, $line);

            // Write details

            $lineForCustomerDetails = "\r\n" .
                    csv_field('', true) .
                    csv_field('', false) .
                    csv_field($crm, false) .
                    csv_field($organizationId, false) .
                    csv_field('', false) .
                    csv_field('', false) .
                    csv_field('', false) .
                    csv_field('', false) .
                    csv_field('', false) .
                    csv_field('', false) .
                    csv_field('', false) .
                    csv_field('', false);

            $details = $this->getStore()->getDetails($organizationId, $report);
            foreach ($details as $detail) {

                list($groupDescr, $setTotalCalls, $setTotalDurationSec, $setTotalCostsWithoutTax, $setTotalIncomesWithoutTax) = $detail;

                list($setTotalCostsTax, $setTotalCostsWithTax) = invoice_amount_with_vat($setTotalCostsWithoutTax, $setVatPercDecimal);
                list($setTotalIncomesTax, $setTotalIncomesWithTax) = invoice_amount_with_vat($setTotalIncomesWithoutTax, $setVatPercDecimal);

                $setTotalEarnsWithoutTax = $setTotalIncomesWithoutTax - $setTotalCostsWithoutTax;
                $setTotalEarnsWithTax = $setTotalIncomesWithTax - $setTotalCostsWithTax;

                $setTotalDurationH = format_minute($setTotalDurationSec);

                $line = csv_field('1', false) .
                        csv_field($groupDescr, false) .
                        csv_field($setTotalCalls, false) .
                        csv_field($setTotalDurationSec, false) .
                        csv_field($setTotalDurationH, false) .
                        csv_field($setVatPercH, false) .
                        csv_field(from_db_decimal_to_invoice_decimal($setTotalCostsWithoutTax), false) .
                        csv_field(from_db_decimal_to_invoice_decimal($setTotalCostsTax), false) .
                        csv_field(from_db_decimal_to_invoice_decimal($setTotalCostsWithTax), false) .
                        csv_field(from_db_decimal_to_invoice_decimal($setTotalIncomesWithoutTax), false) .
                        csv_field(from_db_decimal_to_invoice_decimal($setTotalIncomesTax), false) .
                        csv_field(from_db_decimal_to_invoice_decimal($setTotalIncomesWithTax), false) .
                        csv_field(from_db_decimal_to_invoice_decimal($setTotalEarnsWithoutTax), false) .
                        csv_field(from_db_decimal_to_invoice_decimal($setTotalEarnsWithTax), false);

                fwrite($stream, $lineForCustomerDetails);
                fwrite($stream, $line);
            }

            ArPartyPeer::clearInstancePool();
        }

        // Save the report

        rewind($stream);
        $content = stream_get_contents($stream);
        fclose($stream);
        // NOTE: this is not optimal because a stream is converted to a string,
        // and then it is converted again in a stream during BLOB saving.

        $report->setProducedReportAlreadyReviewed(0);
        $report->setProducedReportIsDraft($isDraftReport);
        $report->setProducedReportMimeType('text/csv');
        $report->setDocumentContent($content);
        $report->setProducedReportFileTypeSuffix('csv');
        $report->save();
    }

}
