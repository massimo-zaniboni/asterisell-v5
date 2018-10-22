<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

class Report_CompareChannels extends Report_CompareVendorsOrChannels
{

    ///////////////////////////////////////
    // ReportGenerator Interface Support //
    ///////////////////////////////////////

    public function deriveReportParams() {
        $report = $this->getArReport();

        $report->setParamShowCallCost(true);

        $report->setParamShowVoipProvider(false);
        $report->setParamShowCommunicationChannel(true);
        $report->setParamShowCostSaving(false);
        $report->setArReportOrderOfChildrenId(ArReportOrderOfChildren::ORDER_BY_COUNT_OF_CALLS);

        parent::deriveReportParams();
    }

}