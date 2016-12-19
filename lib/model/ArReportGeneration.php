<?php

require 'lib/model/om/BaseArReportGeneration.php';


/**
 * Skeleton subclass for representing a row from the 'ar_report_generation' table.
 *
 * 
 *
 * You should add additional methods to this class to meet the
 * application requirements.  This class will only be generated as
 * long as it does not already exist in the output directory.
 *
 * @package    lib.model
 */
class ArReportGeneration extends BaseArReportGeneration {

    // NOTE: if you add new type of reports, update:
    // - ConfigureDefaultParamsAndSettings
    // - SchedulerReportGenerator

    // NOTE: the const value is used as ID of the created reports,
    // so do not change them.

    const GENERATE_ONLY_FOR_SPECIFIED_ORGANIZATION = 10;

    const GENERATE_FOR_ALL_CHILDREN_ORGANIZATIONS_AND_VOIP_ACCOUNTS = 20;

    const GENERATE_FOR_ALL_CHILDREN_ORGANIZATIONS_THAT_ARE_NOT_VOIP_ACCOUNTS = 30;

    const GENERATE_FOR_ALL_BILLABLE_CHILDREN_ORGANIZATIONS = 40;

    const GENERATE_FOR_ALL_CHILDREN_ORGANIZATIONS_WITH_A_RESPONSIBLE = 50;


    public function __toString() {
        return $this->getName();
    }

} // ArReportGeneration
