<?php

/**
 * Subclass for performing query and update operations on the 'ar_permission' table.
 *
 * 
 *
 * @package lib.model
 */ 
class ArPermissionPeer extends BaseArPermissionPeer
{

    /**
     * @var ArPermission[]|null
     */
    protected static $cachedArPermissions = null;

    /**
     * @return ArPermission[] cached list of all `ArPermission`
     *
     */
    public static function getAll() {
        if (is_null(self::$cachedArPermissions)) {
            self::$cachedArPermissions = ArPermissionPeer::doSelect(new Criteria());
        }
        return self::$cachedArPermissions;
    }
}
