<?php

require 'lib/model/om/BaseArPhysicalFormatPeer.php';


/**
 * Skeleton subclass for performing query and update operations on the 'ar_physical_format' table.
 *
 * 
 *
 * You should add additional methods to this class to meet the
 * application requirements.  This class will only be generated as
 * long as it does not already exist in the output directory.
 *
 * @package    lib.model
 */
class ArPhysicalFormatPeer extends BaseArPhysicalFormatPeer {

    /**
     * @static
     * @param int $logicalTypeId
     * @param string $name
     * @return ArPhysicalFormat|null
     */
    public static function retrieveByName($logicalTypeId, $name)
    {
        $criteria = new Criteria();
        $criteria->add(ArPhysicalFormatPeer::AR_LOGICAL_SOURCE_ID, $logicalTypeId);
        $criteria->add(ArPhysicalFormatPeer::NAME, $name);
        return ArPhysicalFormatPeer::doSelectOne($criteria);
    }

} // ArPhysicalFormatPeer
