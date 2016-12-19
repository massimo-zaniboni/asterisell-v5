<?php

/* $LICENSE 2009, 2010:
 *
 * Copyright (C) 2009, 2010 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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
 * Read InvoiceDetailsData, calculate other queries and
 * generate a PDFInvoiceCenteredTemplate.
 *
 * Store it in ar_invoice.pdf_invoice
 */
class GenerateInvoiceAsCenteredTemplate extends JobProcessor
{

    public function processEvent(JobData $jobData, $parentId)
    {

        if (!($jobData instanceof InvoiceDetailsData)) {
            return null;
        }

        $showOutgoingCalls = sfConfig::get('app_show_outgoing_calls');
        $showIncomingCalls = sfConfig::get('app_show_incoming_calls');

        // Retrieve data
        //
        $invoice = $jobData->getArInvoice();
        $party = $invoice->getArParty();
        $params = $party->getArParams();
        $limit = GenerateCallReportAsPDF::CALLS_TO_DISPLAY;

        // Work only on invoice to customers, that are no revenue sharing
        //
        if (!($invoice->getType() == "C")) {
            return null;
        }

        if ($invoice->getIsRevenueSharing() == 1) {
            return null;
        }

        // Query for listing call details grouped by destination
        //
        $c = new Criteria();
        $c2 = $c->getNewCriterion(ArCdrPeer::CALLDATE, $invoice->getArCdrFrom(), Criteria::GREATER_EQUAL);
        $c2->addAnd($c->getNewCriterion(ArCdrPeer::CALLDATE, $invoice->getArCdrTo(), Criteria::LESS_THAN));
        $c->add($c2);

        $c->addJoin(ArCdrPeer::AR_ASTERISK_ACCOUNT_ID, ArAsteriskAccountPeer::ID);
        $c->addJoin(ArAsteriskAccountPeer::AR_OFFICE_ID, ArOfficePeer::ID);
        $c->add(ArOfficePeer::AR_PARTY_ID, $party->getId());
        $c->addJoin(ArCdrPeer::AR_TELEPHONE_PREFIX_ID, ArTelephonePrefixPeer::ID);

        $c->addGroupByColumn(ArTelephonePrefixPeer::OPERATOR_TYPE);

        $c->clearSelectColumns();
        $c->addSelectColumn('SUM(' . ArCdrPeer::COUNT_OF_CALLS . ') as nr_of_calls');
        $c->addSelectColumn('SUM(' . ArCdrPeer::BILLSEC . ')');
        $c->addSelectColumn('SUM(' . ArCdrPeer::INCOME . ')');
        $c->addSelectColumn(ArTelephonePrefixPeer::OPERATOR_TYPE);

        $c->addDescendingOrderByColumn(ArTelephonePrefixPeer::OPERATOR_TYPE);

        $lines = array();

        if ($showOutgoingCalls) {
            $c2 = clone($c);
            $c2->add(ArCdrPeer::DESTINATION_TYPE, DestinationType::outgoing);
            $this->addDetailsAboutCalls($lines, $c2, true, $params, $invoice, $party);
        }

        if ($showIncomingCalls) {
            $c2 = clone($c);
            $c2->add(ArCdrPeer::DESTINATION_TYPE, DestinationType::incoming);
            $this->addDetailsAboutCalls($lines, $c2, false, $params, $invoice, $party);
        }

        // PDF report.
        //
        $pdf = new PDFInvoiceCenteredTemplate($invoice);
        $pdf->AddPage();
        $pdf->draw($lines);
        $pdfInvoice = $pdf->Output("", "S");
        $invoice->setPdfInvoice($pdfInvoice);
        $invoice->save();

        return '';
    }

    protected function addDetailsAboutCalls(&$lines, $query, $isOutcoming, $params, $invoice, $party)
    {
        $index_countCalls = 1;
        $index_sumBillsec = 2;
        $index_sumIncome = 3;
        $index_operatorType = 4;

        $rs = BasePeer::doSelect($query);
        while ($rs->next()) {
            $costDb = $rs->get($index_sumIncome);
            if ($costDb != 0) {
                $descr = $rs->get($index_operatorType);
                $cost = from_db_decimal_to_pdf_txt_decimal($costDb);

                if (!$isOutcoming) {
                    $descr .= " (" . mytr("incoming") . ")";
                }

                $line = array($descr, $cost);
                $lines[] = $line;
            }
        }
    }
}

?>