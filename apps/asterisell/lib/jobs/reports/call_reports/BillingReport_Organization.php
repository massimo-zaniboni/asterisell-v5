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
