<?php

require 'lib/model/om/BaseArCdrProviderPeer.php';


/**
 * Skeleton subclass for performing query and update operations on the 'ar_cdr_provider' table.
 *
 * 
 *
 * You should add additional methods to this class to meet the
 * application requirements.  This class will only be generated as
 * long as it does not already exist in the output directory.
 *
 * @package    lib.model
 */
class ArCdrProviderPeer extends BaseArCdrProviderPeer {

    /**
     * @static
     * @param string $internalName
     * @return ArCdrProvider|null
     */
    public static function retrieveByName($internalName)
    {
        $criteria = new Criteria();
        $criteria->add(ArCdrProviderPeer::INTERNAL_NAME, $internalName);
        return ArCdrProviderPeer::doSelectOne($criteria);
    }
} // ArCdrProviderPeer
