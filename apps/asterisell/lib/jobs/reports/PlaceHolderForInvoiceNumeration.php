<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

class PlaceHolderForInvoiceNumeration extends ReportGenerator
{

    protected function calcStore($schedulerId) {
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

        $report->setProducedReportAlreadyReviewed(1);
        $report->setProducedReportIsDraft(false);
        $report->setProducedReportMimeType('');
        $report->setDocumentContent(null);
        $report->setProducedReportFileTypeSuffix('');
        $report->save();
    }
}
