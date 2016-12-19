<?php

require 'lib/model/om/BaseArViewAllUserPermissionsPeer.php';


/**
 * Skeleton subclass for performing query and update operations on the 'ar_view_all_user_permissions' table.
 *
 * 
 *
 * You should add additional methods to this class to meet the
 * application requirements.  This class will only be generated as
 * long as it does not already exist in the output directory.
 *
 * @package    lib.model
 */
class ArViewAllUserPermissionsPeer extends BaseArViewAllUserPermissionsPeer {

    /**
     * @static
     * @param int $userId
     * @return ArViewAllUserPermissions[]
     */
    public static function getPermissionsOfUser($userId) {
        $c = new Criteria();
        $c->add(ArViewAllUserPermissionsPeer::AR_USER_ID, $userId);
        return ArViewAllUserPermissionsPeer::doSelect($c);
    }

    /**
     * @param int $userId
     * @param int $permissionId
     * @return bool true if the user has the permission
     */
    public static function haveUserPermission($userId, $permissionId) {
        $c = new Criteria();
        $c->add(ArViewAllUserPermissionsPeer::AR_USER_ID, $userId);
        $c->add(ArViewAllUserPermissionsPeer::AR_PERMISSION_ID, $permissionId);

        $count = ArViewAllUserPermissionsPeer::doCount($c);

        if ($count > 0) {
            return true;
        } else {
            return false;
        }
    }


} // ArViewAllUserPermissionsPeer
