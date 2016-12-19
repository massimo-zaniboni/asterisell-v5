<?php

require 'lib/model/om/BaseArPeriodOfTime.php';


/**
 * Skeleton subclass for representing a row from the 'ar_period_of_time' table.
 *
 * 
 *
 * You should add additional methods to this class to meet the
 * application requirements.  This class will only be generated as
 * long as it does not already exist in the output directory.
 *
 * @package    lib.model
 */
class ArPeriodOfTime extends BaseArPeriodOfTime {

    // NOTE: add these constants to `getDescriptions()` function

    const SPECIFIC_TIMEFRAME = 10;

    const LAST_MONTH = 20;

    const LAST_30_DAYS = 30;

    const LAST_WEEK = 40;

    const LAST_7_DAYS = 50;

    const THIS_MONTH = 60;

    const THIS_WEEK = 70;

    /**
     * Used mainly during database initialization for loading values.
     *
     * @static
     * @return string[] description associated to types
     */
    public static function getDescriptions() {
        return array(  self::SPECIFIC_TIMEFRAME => "User defined"
                     , self::LAST_MONTH => "Last month"
                     , self::LAST_30_DAYS => "Last 30 days"
                     , self::LAST_WEEK => "Last week"
                     , self::LAST_7_DAYS => "Last 7 days"
                     , self::THIS_MONTH => "This month"
                     , self::THIS_WEEK => "This week"
            );
    }

    public function __toString() {
        return $this->getName();
    }

} // ArPeriodOfTime
