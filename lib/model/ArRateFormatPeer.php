<?php

require 'lib/model/om/BaseArRateFormatPeer.php';


/**
 * Skeleton subclass for performing query and update operations on the 'ar_rate_format' table.
 *
 * 
 *
 * You should add additional methods to this class to meet the
 * application requirements.  This class will only be generated as
 * long as it does not already exist in the output directory.
 *
 * @package    lib.model
 */
class ArRateFormatPeer extends BaseArRateFormatPeer {

    /**
     * @static
     * @param string $internalName
     * @return ArRateFormat|null
     */
    public static function retrieveByInternalName($internalName)
    {
        $criteria = new Criteria();
        $criteria->add(ArRateFormatPeer::INTERNAL_NAME, $internalName);

        return ArRateFormatPeer::doSelectOne($criteria);
    }


} // ArRateFormatPeer
