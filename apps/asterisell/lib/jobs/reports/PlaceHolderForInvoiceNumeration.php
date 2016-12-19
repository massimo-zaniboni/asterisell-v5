<?php

/* $LICENSE 2012:
 *
 * Copyright (C) 2012 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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

class PlaceHolderForInvoiceNumeration extends ReportGenerator
{

    /**
     * Return a calculated store.
     *
     * @return ReportCalcStore
     */
    protected function calcStore() {
      return new EmptyReportCalcStore();
    }

    public function getReportUserReadableName() {
      return "Placeholder for invoice numeration";
    }

    protected function deriveReportParams() {

        if (is_null($this->getArReport()->getLegalDate())) {
            $this->signalProblem('Specify also the legal/fiscal date of the report.');
        }

        if (is_null($this->getArReport()->getLegalConsecutiveNr())) {
            $this->signalProblem('Specify also the legal/fiscal number of the report.');
        }

        if (! is_null($this->getArReport()->getFromDate())) {
            $this->signalProblem('For this type of report, leave empty the initial report date, and specify only the legal/fiscal date.');
        }

        $report = $this->getArReport();

        $report->setParamShowAlsoOutgoingCalls(false);
        $report->setParamShowAlsoIncomingCalls(false);
        $report->setParamShowAlsoInternalCalls(false);
        $report->setParamShowCallCost(false);
        $report->setParamShowVoipProvider(false);
        $report->setParamShowMaskedTelephoneNumbers(false);
        $report->setParamShowCallDetails(false);
        $report->setParamShowCommunicationChannel(false);
        $report->setParamExpandToLevel(1);

        // Use the legal date as reference

        $paramDate = fromMySQLTimestampToUnixTimestamp($report->getLegalDate());

        $reportDate1 = strtotime(date('Y', $paramDate) .'-' . date('m', $paramDate) . '-' . date('d', $paramDate));
        $reportDate2 = strtotime(intval(date('Y', $paramDate)) + 1 . '-01-01');

        $report->setFromDate($reportDate1);
        $report->setToDate($reportDate2);
        $report->setLegalDate($reportDate1);
    }

    protected function internalGenerateReport() {
        $report = $this->getArReport();

        $report->setProducedReportAdditionalDescription('This is a dummy report, used as placeholder for the numeration of legal/fiscal reports. All legal/fiscal reports produced after date '
                . fromUnixTimestampToSymfonyStrDate(fromMySQLTimestampToUnixTimestamp($report->getLegalDate()))
                . ' will be numbered starting from ' . ($report->getLegalConsecutiveNr() + 1)
        );

        $report->setProducedReportAlreadyReviewed(true);
        $report->setProducedReportIsDraft(false);
        $report->setProducedReportMimeType('');
        $report->setDocumentContent(null);
        $report->setProducedReportFileTypeSuffix('');
        $report->save();
    }
}
