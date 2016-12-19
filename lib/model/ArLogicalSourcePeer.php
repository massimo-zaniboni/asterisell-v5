<?php

require 'lib/model/om/BaseArLogicalSourcePeer.php';


/**
 * Skeleton subclass for performing query and update operations on the 'ar_logical_source' table.
 *
 * 
 *
 * You should add additional methods to this class to meet the
 * application requirements.  This class will only be generated as
 * long as it does not already exist in the output directory.
 *
 * @package    lib.model
 */
class ArLogicalSourcePeer extends BaseArLogicalSourcePeer {


    /**
     * @static
     * @param string $internalName
     * @return ArLogicalSource|null
     */
    public static function retrieveByName($internalName)
    {
        $criteria = new Criteria();
        $criteria->add(ArLogicalSourcePeer::NAME, $internalName);
        return ArLogicalSourcePeer::doSelectOne($criteria);
    }

} // ArLogicalSourcePeer
