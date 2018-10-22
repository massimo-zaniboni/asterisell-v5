<?php

require 'lib/model/om/BaseArLocalFileToDeletePeer.php';

class ArLocalFileToDeletePeer extends BaseArLocalFileToDeletePeer {

    /**
     * @static
     * @param string $internalName
     * @return ArRate|null
     */
    public static function retrieveByInternalName($internalName)
    {
        $criteria = new Criteria();
        $criteria->add(ArLocalFileToDeletePeer::NAME, $internalName);

        return ArRatePeer::doSelectOne($criteria);
    }
} // ArLocalFileToDeletePeer
