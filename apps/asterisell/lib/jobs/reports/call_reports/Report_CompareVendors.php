<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

class Report_CompareVendors extends Report_CompareVendorsOrChannels
{

    ///////////////////////////////////////
    // ReportGenerator Interface Support //
    ///////////////////////////////////////

    public function deriveReportParams() {
        $report = $this->getArReport();

        $report->setParamShowCallCost(true);

        $report->setParamShowVoipProvider(true);
        $report->setParamShowCommunicationChannel(false);
        $report->setParamShowCostSaving(false);
        $report->setArReportOrderOfChildrenId(ArReportOrderOfChildren::ORDER_BY_COUNT_OF_CALLS);

        parent::deriveReportParams();
    }

}