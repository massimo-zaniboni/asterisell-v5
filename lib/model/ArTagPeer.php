<?php

require 'lib/model/om/BaseArTagPeer.php';


/**
 * Skeleton subclass for performing query and update operations on the 'ar_tag' table.
 *
 * 
 *
 * You should add additional methods to this class to meet the
 * application requirements.  This class will only be generated as
 * long as it does not already exist in the output directory.
 *
 * @package    lib.model
 */
class ArTagPeer extends BaseArTagPeer {

    /**
     * @static
     * @param string $internalName
     * @return ArTag|null
     */
    public static function retrieveByInternalName($internalName)
    {
        $criteria = new Criteria();
        $criteria->add(ArTagPeer::INTERNAL_NAME, $internalName);

        return ArTagPeer::doSelectOne($criteria);
    }

} // ArTagPeer
