<?php

require 'lib/model/om/BaseArLegalDateGenerationMethod.php';


/**
 * Skeleton subclass for representing a row from the 'ar_legal_date_generation_method' table.
 *
 * 
 *
 * You should add additional methods to this class to meet the
 * application requirements.  This class will only be generated as
 * long as it does not already exist in the output directory.
 *
 * @package    lib.model
 */
class ArLegalDateGenerationMethod extends BaseArLegalDateGenerationMethod {

    //////////////////////////
    // Default permissions. //
    //////////////////////////
    //
    // NOTE: they are created inside
    //
    // > apps/asterisell/lib/jobs/admin/ConfigureDefaultParamsAndSettings.php
    //
    // IMPORTANT: do not change these values for systems already in production, because they are used as
    // constant for created IDs. Doing so you will alter the meaning of assigned permissions, for already
    // created users.

    const USE_END_DATE_OF_THE_REPORT_RANGE  = 10;

    const USE_THE_FIRST_MONDAY_AFTER_THE_REPORT_RANGE = 30;

    const USE_THE_FIRST_DAY_OF_THE_MONTH_OF_THE_END_OF_REPORT_RANGE = 40;

    const USE_THE_FIRST_DAY_OF_THE_MONTH_AFTER_THE_END_OF_REPORT_RANGE = 50;

    const USE_THE_DATE_OF_WHEN_THE_REPORT_IS_GENERATED = 60;

    /**
     * Used mainly during database initialization for loading values.
     *
     * @static
     * @return string[] names associated to types
     */
    public static function getConstNames() {
        return array(
            self::USE_END_DATE_OF_THE_REPORT_RANGE  => "Use the end date of the report range"
        , self::USE_THE_FIRST_MONDAY_AFTER_THE_REPORT_RANGE  => "The first Monday, following the end date of report range"
        , self::USE_THE_FIRST_DAY_OF_THE_MONTH_OF_THE_END_OF_REPORT_RANGE => "The 1st of the month of the end date of report range"
        , self::USE_THE_FIRST_DAY_OF_THE_MONTH_AFTER_THE_END_OF_REPORT_RANGE => "The 1st of the next month after the end date of report range"
        , self::USE_THE_DATE_OF_WHEN_THE_REPORT_IS_GENERATED => "Use the date of when the report is generated"
        );
    }

    public function __toString() {
        return $this->getName();
    }


} // ArLegalDateGenerationMethod
