<?php

/**
 * Subclass for performing query and update operations on the 'ar_rate_category' table.
 *
 * 
 *
 * @package lib.model
 */ 
class ArRateCategoryPeer extends BaseArRateCategoryPeer
{

    /**
     * @static
     * @param string $internalName
     * @return ArRateCategory|null
     */
    public static function retrieveByInternalName($internalName)
    {
        $criteria = new Criteria();
        $criteria->add(ArRateCategoryPeer::INTERNAL_NAME, $internalName);

        return ArRateCategoryPeer::doSelectOne($criteria);
    }

    /**
     * @static
     * @param string $internalName
     * @return int|null
     */
    public static function retrieveIdByInternalName($internalName)
    {
        $criteria = new Criteria();
        $criteria->add(ArRateCategoryPeer::INTERNAL_NAME, $internalName);

        $p = ArRateCategoryPeer::doSelectOne($criteria);
        if (is_null($p)) {
            return null;
        } else {
            return $p->getId();
        }
    }
}
