<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

class BillingReport_Organization extends BaseBillingReport
{

    ///////////////////////////////
    // ReportGenerator Interface //
    ///////////////////////////////

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
        $report->setParamExpandToLevel(2);

        // Use the month as reference

        $paramDate = fromMySQLTimestampToUnixTimestamp($report->getFromDate());

        $reportDate1 = strtotime(date('Y', $paramDate) .'-' . date('m', $paramDate) . '-' . '01');
        $reportDate2 = strtotime('+1 month', $reportDate1);

        $report->setFromDate($reportDate1);
        $report->setToDate($reportDate2);

    }
}
