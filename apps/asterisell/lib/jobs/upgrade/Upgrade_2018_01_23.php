<?php

class Upgrade_2018_01_23 extends AdminJobProcessor
{

    public function isCDRTableModified()
    {
        return false;
    }

    public function isDBUpgradeJob()
    {
        return true;
        // use true also if it is not correct, otherwise the job is not invoked
    }

    public function process()
    {
        $r = ArReportGenerationPeer::retrieveByPK(ArReportGeneration::GENERATE_ONLY_FOR_SPECIFIED_ORGANIZATION);
        $r->setName('Generate a unique cumulative report');
        $r->save();

        $r = ArReportGenerationPeer::retrieveByPK( ArReportGeneration::GENERATE_FOR_ALL_BILLABLE_CHILDREN_ORGANIZATIONS);
        $r->setName('Generate a distinct report for each billable organization');
        $r->save();

        $r = ArReportGenerationPeer::retrieveByPK( ArReportGeneration::GENERATE_FOR_ALL_CHILDREN_ORGANIZATIONS_THAT_ARE_NOT_VOIP_ACCOUNTS);
        $r->setName('Generate a distinct report for each main and children organization, but not extensions');
        $r->save();

        $r = ArReportGenerationPeer::retrieveByPK( ArReportGeneration::GENERATE_FOR_ALL_CHILDREN_ORGANIZATIONS_AND_VOIP_ACCOUNTS);
        $r->setName('Generate a distinct report for each main and children organization and extension');
        $r->save();

        $r = ArReportGenerationPeer::retrieveByPK( ArReportGeneration::GENERATE_FOR_ALL_CHILDREN_ORGANIZATIONS_WITH_A_RESPONSIBLE);
        $r->setName('Generate a distinct report for each main and children organization with a responsible');
        $r->save();

        $conn = Propel::getConnection();
        $conn->exec('UPDATE ar_report_scheduler '
                    . ' SET ar_report_generation_id = ' . ArReportGeneration::GENERATE_FOR_ALL_BILLABLE_CHILDREN_ORGANIZATIONS
                    . ' WHERE ar_report_generation_id IS NULL');

        return '';
    }
}
