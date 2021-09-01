<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));


class InitDefaultReportsForVoIPReseller extends AdminJobProcessor
{

    ////////////////////////////////////////
    // SPECIFIC VENDORS NAMES AND DOMAINS //
    ////////////////////////////////////////

    const ID_FOR_DEFAULT_REPORT = "default-report-for-reseller";

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

        // Generate invoices, and generate summary report.

        $r = new ArReport();
        $r->setPhpClassName('GenerateLegalInvoiceUsingTemplate01PDF');
        $r->setReportName(mytr('Invoice'));
        $r->setIsTemplate(true);
        $r->setArOrganizationUnitId(null);
        $r->setParamExpandToLevel(1);
        $r->setParamShowAlsoOutgoingCalls(true);
        $r->setArReportOrderOfChildrenId(ArReportOrderOfChildren::ORDER_BY_NAME);
        $r->setParamShowVoipProvider(false);
        $r->setParamShowCommunicationChannel(false);
        $r->setParamShowMaskedTelephoneNumbers(true);
        $r->setParamShowCallCost(false);
        $r->setParamShowCostSaving(false);
        $r->setParamShowCallDetails(false);
        $r->setParamShowCallIncome(true);
        $r->setParamShowGeographicLocation(true);
        $r->setParamShowConnectionType(true);
        $r->setParamIsLegal(true);

        $r->setReportAttachmentFileName(mytr('invoice_'));
        $r->setReportAttachmentFileNameAddReportDate(true);
        $r->save();

        $p = new ArReportAlsoFor();
        $p->setArReportId($r->getId());
        $p->setArRoleId(ArRolePeer::retrieveByInternalName(ArRole::USER)->getId());
        $p->save();

        $s = new ArReportScheduler();
        $s->setInternalName(self::ID_FOR_DEFAULT_REPORT);
        $s->setArReportId($r->getId());
        $s->setIsActive(true);
        $s->setSendCompactReportListToAccountant(true);
        $s->setArOrganizationUnitId(null);
        $s->setArReportGenerationId(ArReportGeneration::GENERATE_FOR_ALL_BILLABLE_CHILDREN_ORGANIZATIONS);
        $s->setNote('Invoices to Customers.');
        $s->setProducedReportMustBeReviewed(true);
        $s->setStartGenerationAfterXHours(2);
        $s->setScheduleEveryXMonths(1);
        $s->initForMonthly(1, $monthInThePast);
        $s->setArLegalDateGenerationMethodId(ArLegalDateGenerationMethod::USE_END_DATE_OF_THE_REPORT_RANGE);
        $s->setDaysToAddToLegalDateGenerationMethod(1);
        $s->setIsYearlyLegalNumeration(true);
        $s->setGenerateOnlyIfThereIsCost(true);
        $s->save();

        // A CSV report with all data of all customers, that can be used
        // for exporting the billing process to external software.
        
        $r = new ArReport();
        $r->setPhpClassName('CSVInvoices');
        $r->setReportName(mytr('All invoices details in CSV format.'));
        $r->setIsTemplate(true);
        $r->setArOrganizationUnitId(null);
        $r->setParamExpandToLevel(1);

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
        $r->setParamShowVoipProvider(false);
        $r->setParamShowCommunicationChannel(false);
        $r->setParamShowMaskedTelephoneNumbers(true);
        $r->setParamShowCallCost(true);
        $r->setParamShowCostSaving(false);
        $r->setParamShowCallDetails(false);
        $r->setParamShowCallIncome(true);
        $r->setParamShowGeographicLocation(true);
        $r->setParamShowConnectionType(true);
        $r->setReportAttachmentFileName(mytr('invoicing_details_'));
        $r->setReportAttachmentFileNameAddReportDate(true);
        $r->save();

        $p = new ArReportAlsoFor();
        $p->setArReportId($r->getId());
        $p->setArRoleId(ArRolePeer::retrieveByInternalName(ArRole::ACCOUNTANT)->getId());
        $p->save();

        $p = new ArReportAlsoFor();
        $p->setArReportId($r->getId());
        $p->setArRoleId(ArRolePeer::retrieveByInternalName(ArRole::ADMIN)->getId());
        $p->save();

        $s = new ArReportScheduler();
        $s->setInternalName(self::ID_FOR_DEFAULT_REPORT);
        $s->setArReportId($r->getId());
        $s->setIsActive(true);
        $s->setArOrganizationUnitId(null);
        $s->setArReportGenerationId(ArReportGeneration::GENERATE_ONLY_FOR_SPECIFIED_ORGANIZATION);
        $s->setNote('All invoices details in CSV format.');
        $s->setProducedReportMustBeReviewed(true);
        $s->setStartGenerationAfterXHours(2);
        $s->setScheduleEveryXMonths(1);
        $s->initForMonthly(1, $monthInThePast);
        $s->save();
        
        // A report with summary of calls, to each customers

        $r = new ArReport();
        $r->setPhpClassName('BillingReport_UserDefined');
        $r->setReportName(mytr('Summary of Calls'));
        $r->setIsTemplate(true);
        $r->setArOrganizationUnitId(null);
        $r->setParamExpandToLevel(0);

        if (sfConfig::get('app_show_incoming_calls')) {
            $r->setParamShowAlsoIncomingCalls(true);
        }

        if (sfConfig::get('app_show_outgoing_calls')) {
            $r->setParamShowAlsoOutgoingCalls(true);
        }

        if (sfConfig::get('app_show_internal_calls')) {
            $r->setParamShowAlsoInternalCalls(true);
        }
        $r->setArReportOrderOfChildrenId(ArReportOrderOfChildren::ORDER_BY_CALL_INCOME);
        $r->setParamShowVoipProvider(false);
        $r->setParamShowCommunicationChannel(false);
        $r->setParamShowMaskedTelephoneNumbers(true);
        $r->setParamShowCallCost(false);
        $r->setParamShowCostSaving(false);
        $r->setParamShowCallDetails(false);
        $r->setParamShowCallIncome(true);
        $r->setParamShowGeographicLocation(true);
        $r->setParamShowConnectionType(true);
        $r->setReportAttachmentFileName(mytr('calls_cost_summary_'));
        $r->setReportAttachmentFileNameAddReportDate(true);
        $r->save();

        $p = new ArReportAlsoFor();
        $p->setArReportId($r->getId());
        $p->setArRoleId(ArRolePeer::retrieveByInternalName(ArRole::USER)->getId());
        $p->save();

        $s = new ArReportScheduler();
        $s->setInternalName(self::ID_FOR_DEFAULT_REPORT);
        $s->setArReportId($r->getId());
        $s->setIsActive(true);
        $s->setSendCompactReportListToAccountant(false);
        $s->setArOrganizationUnitId(null);
        $s->setArReportGenerationId(ArReportGeneration::GENERATE_FOR_ALL_CHILDREN_ORGANIZATIONS_WITH_A_RESPONSIBLE);
        $s->setNote('Calls Summary sent to every Customer, and to every department with a responsible.');
        $s->setProducedReportMustBeReviewed(true);
        $s->setStartGenerationAfterXHours(2);
        $s->setScheduleEveryXMonths(1);
        $s->initForMonthly(1, $monthInThePast);
        $s->save();

        // A single report with the summary of calls made from Customers, to the admin and to the accountant.
        // This report contains more details, and it is for analyzing the calls.

        $r = new ArReport();
        $r->setPhpClassName('BillingReport_UserDefined');
        $r->setReportName(mytr('Analysis of Billed Calls'));
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
        $r->setArReportOrderOfChildrenId(ArReportOrderOfChildren::ORDER_BY_CALL_INCOME);
        $r->setParamShowVoipProvider(true);
        $r->setParamShowCommunicationChannel(true);
        $r->setParamShowMaskedTelephoneNumbers(true);
        $r->setParamShowCallCost(true);
        $r->setParamShowCostSaving(false);
        $r->setParamShowCallDetails(false);
        $r->setParamShowCallIncome(true);
        $r->setParamShowGeographicLocation(true);
        $r->setParamShowConnectionType(true);
        $r->setReportAttachmentFileName(mytr('summary_of_invoiced_calls_'));
        $r->setReportAttachmentFileNameAddReportDate(true);
        $r->save();

        $p = new ArReportAlsoFor();
        $p->setArReportId($r->getId());
        $p->setArRoleId(ArRolePeer::retrieveByInternalName(ArRole::ACCOUNTANT)->getId());
        $p->save();

        $p = new ArReportAlsoFor();
        $p->setArReportId($r->getId());
        $p->setArRoleId(ArRolePeer::retrieveByInternalName(ArRole::ADMIN)->getId());
        $p->save();

        $s = new ArReportScheduler();
        $s->setInternalName(self::ID_FOR_DEFAULT_REPORT);
        $s->setArReportId($r->getId());
        $s->setIsActive(true);
        $s->setArOrganizationUnitId(null);
        $s->setArReportGenerationId(ArReportGeneration::GENERATE_ONLY_FOR_SPECIFIED_ORGANIZATION);
        $s->setNote('Summary of customers invoices.');
        $s->setProducedReportMustBeReviewed(true);
        $s->setStartGenerationAfterXHours(2);
        $s->setScheduleEveryXMonths(1);
        $s->initForMonthly(1, $monthInThePast);
        $s->save();
    }
}
