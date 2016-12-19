<?php

require 'lib/model/om/BaseArReportOrderOfChildren.php';


/**
 * Skeleton subclass for representing a row from the 'ar_report_order_of_children' table.
 *
 * 
 *
 * You should add additional methods to this class to meet the
 * application requirements.  This class will only be generated as
 * long as it does not already exist in the output directory.
 *
 * @package    lib.model
 */
class ArReportOrderOfChildren extends BaseArReportOrderOfChildren {

    const ORDER_BY_NAME = 10;

    const ORDER_BY_CALL_DURATION = 20;

    const ORDER_BY_CALL_COST = 30;

    const ORDER_BY_CALL_INCOME = 40;

    const ORDER_BY_COUNT_OF_CALLS = 50;

    public function __toString()
    {
        return ($this->getName());
    }

    static
    public function getDefaultOrder() {
        return self::ORDER_BY_CALL_DURATION;
    }

}
