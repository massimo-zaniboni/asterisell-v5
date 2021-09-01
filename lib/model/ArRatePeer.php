<?php

/**
 * Subclass for performing query and update operations on the 'ar_rate' table.
 *
 * 
 *
 * @package lib.model
 */ 
class ArRatePeer extends BaseArRatePeer
{
    const MAIN_COST_RATE = 'main-cost-rate';
    const MAIN_INCOME_RATE = 'main-income-rate';

    /**
     * @static
     * @param string $internalName
     * @return ArRate|null consider the most recent rate
     */
    public static function retrieveByInternalName($internalName)
    {
        $c = new Criteria();
        $c->add(ArRatePeer::INTERNAL_NAME, $internalName);
        $c->addDescendingOrderByColumn(ArRatePeer::FROM_TIME);
        $c->setLimit(1);
        return ArRatePeer::doSelectOne($c);
    }
   
}
