<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));


class InitDefaultReportsForCallReporting extends AdminJobProcessor
{

    ////////////////////////////////////////
    // SPECIFIC VENDORS NAMES AND DOMAINS //
    ////////////////////////////////////////

    const ID_FOR_DEFAULT_REPORT = "default-report-for-call-report";

    public function isCDRTableModified()
    {
        return false;
    }

    public function process() {
        $this->disableReports();
        $this->scheduleReportsInThePast(2);
    }

    public function disableReports() {

        $c = new Criteria();
        $c->add(ArReportSchedulerPeer::INTERNAL_NAME, self::ID_FOR_DEFAULT_REPORT);
        $c->add(ArReportSchedulerPeer::IS_ACTIVE, true);
        $rs = ArReportSchedulerPeer::doSelect($c);

        foreach($rs as $r) {
            /**
             * @var ArReportScheduler $r
             */
            $r->setIsActive(false);
            $r->save();
        }
    }

    /**
     * @param int $monthInThePast when start scheduling in the past,
     *        0 for not generate reports in the past, but only starting from today,
     *        1 for generating also the reports of last month,
     *        2 for generating also the reports of last two months, etc.
     * @return string
     */
    public function scheduleReportsInThePast($monthInThePast = 0)
    {

        // A report with summary of calls, to customers

        $r = new ArReport();
        $r->setPhpClassName('BillingReport_UserDefined');
        $r->setReportName('Summary of Calls');
        $r->setIsTemplate(true);
        $r->setArOrganizationUnitId(null);
        $r->setParamExpandToLevel(2);

        if (sfConfig::get('app_show_incoming_calls')) {
            $r->setParamShowAlsoIncomingCalls(true);
        }

        if (sfConfig::get('app_show_outgoing_calls')) {
            $r->setParamShowAlsoOutgoingCalls(true);
        }

        if (sfConfig::get('app_show_internal_calls')) {
            $r->setParamShowAlsoInternalCalls(true);
        }
        $r->setArReportOrderOfChildrenId(ArReportOrderOfChildren::ORDER_BY_NAME);
        $r->setParamShowVoipProvider(true);
        $r->setParamShowCommunicationChannel(true);
        $r->setParamShowMaskedTelephoneNumbers(true);
        $r->setParamShowCallCost(true);
        $r->setParamShowCostSaving(false);
        $r->setParamShowCallDetails(false);
        $r->setParamShowCallIncome(false);
        $r->setParamShowGeographicLocation(true);
        $r->setParamShowConnectionType(true);
        $r->setReportAttachmentFileName('calls_cost_summary_');
        $r->setReportAttachmentFileNameAddReportDate(true);
        $r->setParamIsLegal(true);
        $r->save();

        $s = new ArReportScheduler();
        $s->setInternalName(self::ID_FOR_DEFAULT_REPORT);
        $s->setArReportId($r->getId());
        $s->setIsActive(true);
        $s->setArOrganizationUnitId(null);
        $s->setArReportGenerationId(ArReportGeneration::GENERATE_FOR_ALL_CHILDREN_ORGANIZATIONS_WITH_A_RESPONSIBLE);
        $s->setNote('Calls Summary sent to every Customer, and to every department with a responsible.');
        $s->setProducedReportMustBeReviewed(true);
        $s->setStartGenerationAfterXHours(2);
        $s->setScheduleEveryXMonths(1);
        $s->initForMonthly(1, $monthInThePast);
        $s->save();

    }
}
