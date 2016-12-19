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
     * @return ArRate|null
     */
    public static function retrieveByInternalName($internalName)
    {
        $criteria = new Criteria();
        $criteria->add(ArRatePeer::INTERNAL_NAME, $internalName);

        return ArRatePeer::doSelectOne($criteria);
    }

}
