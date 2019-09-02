<?php

require 'lib/model/om/BaseArWholesaleCarrierPeer.php';


/**
 * Skeleton subclass for performing query and update operations on the 'ar_wholesale_carrier' table.
 *
 * 
 *
 * You should add additional methods to this class to meet the
 * application requirements.  This class will only be generated as
 * long as it does not already exist in the output directory.
 *
 * @package    lib.model
 */
class ArWholesaleCarrierPeer extends BaseArWholesaleCarrierPeer {

    /**
     * @static
     * @param string $internalName
     * @return ArWholesaleCarrier|null
     */
    public static function retrieveByInternalName($internalName)
    {
        $criteria = new Criteria();
        $criteria->add(ArWholesaleCarrierPeer::INTERNAL_NAME, $internalName);
        return ArWholesaleCarrierPeer::doSelectOne($criteria);
    }

    /**
     * As `retrieveByInternalName` but uses a cache
     * @static
     * @param string $internalName
     * @return ArWholesaleCarrier|null
     */
    public static function retrieveByInternalNameCached($internalName)
    {
        static $cache = array();

        if (array_key_exists($internalName, $cache)) {
            return $cache[$internalName];
        } else {
            $r = self::retrieveByInternalName($internalName);
            $cache[$internalName] = $r;
            return $r;
        }
    }

} // ArWholesaleCarrierPeer
