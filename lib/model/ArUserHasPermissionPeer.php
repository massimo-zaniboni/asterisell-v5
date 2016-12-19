<?php

require 'lib/model/om/BaseArUserHasPermissionPeer.php';


/**
 * Skeleton subclass for performing query and update operations on the 'ar_user_has_permission' table.
 *
 * 
 *
 * You should add additional methods to this class to meet the
 * application requirements.  This class will only be generated as
 * long as it does not already exist in the output directory.
 *
 * @package    lib.model
 */
class ArUserHasPermissionPeer extends BaseArUserHasPermissionPeer {

    /**
     * @static
     * @param int $userId
     * @return ArUserHasPermission[]
     */
    public static function getPermissionsOfUser($userId) {
        $c = new Criteria();
        $c->add(ArUserHasPermissionPeer::AR_USER_ID, $userId);
        return ArUserHasPermissionPeer::doSelect($c);
    }

} // ArUserHasPermissionPeer
