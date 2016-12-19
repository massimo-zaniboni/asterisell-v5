<?php

require 'lib/model/om/BaseArVendorPeer.php';


/**
 * Skeleton subclass for performing query and update operations on the 'ar_vendor' table.
 *
 * 
 *
 * You should add additional methods to this class to meet the
 * application requirements.  This class will only be generated as
 * long as it does not already exist in the output directory.
 *
 * @package    lib.model
 */
class ArVendorPeer extends BaseArVendorPeer {

    /**
     * @static
     * @param string $internalName
     * @return ArVendor|null
     */
    public static function retrieveByInternalName($internalName)
    {
        $criteria = new Criteria();
        $criteria->add(ArVendorPeer::INTERNAL_NAME, $internalName);

        return ArVendorPeer::doSelectOne($criteria);
    }

} // ArVendorPeer
