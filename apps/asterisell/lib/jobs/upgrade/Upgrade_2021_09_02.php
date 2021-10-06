<?php

class Upgrade_2021_09_02 extends AdminJobProcessor
{

    public function isCDRTableModified()
    {
        return true;
    }

    public function isDBUpgradeJob()
    {
        return true;
    }

    public function process()
    {
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
        $s->setInternalName(InitDefaultReportsForVoIPReseller::ID_FOR_DEFAULT_REPORT);
        $s->setArReportId($r->getId());
        $s->setIsActive(true);
        $s->setArOrganizationUnitId(null);
        $s->setArReportGenerationId(ArReportGeneration::GENERATE_ONLY_FOR_SPECIFIED_ORGANIZATION);
        $s->setNote('All invoices details in CSV format.');
        $s->setProducedReportMustBeReviewed(true);
        $s->setStartGenerationAfterXHours(2);
        $s->setScheduleEveryXMonths(1);
        $s->setMinimumCost(0);
        $s->initForMonthly(1, 1);
        $s->save();
        
        return '';
   }
}
