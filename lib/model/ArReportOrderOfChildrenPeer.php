<?php

require 'lib/model/om/BaseArReportOrderOfChildrenPeer.php';


/**
 * Skeleton subclass for performing query and update operations on the 'ar_report_order_of_children' table.
 *
 * 
 *
 * You should add additional methods to this class to meet the
 * application requirements.  This class will only be generated as
 * long as it does not already exist in the output directory.
 *
 * @package    lib.model
 */
class ArReportOrderOfChildrenPeer extends BaseArReportOrderOfChildrenPeer {

    /**
     * @static
     * @param string $internalName
     * @return ArReportOrderOfChildren|null
     */
    public static function retrieveByInternalName($internalName)
    {
        $criteria = new Criteria();
        $criteria->add(ArReportOrderOfChildrenPeer::INTERNAL_CODE, $internalName);

        return ArReportOrderOfChildrenPeer::doSelectOne($criteria);
    }


} // ArReportOrderOfChildrenPeer
