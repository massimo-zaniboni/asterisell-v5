<?php

/**
 * Subclass for performing query and update operations on the 'ar_role' table.
 *
 * 
 *
 * @package lib.model
 */ 
class ArRolePeer extends BaseArRolePeer
{
    /**
     * @var ArPermission[]|null
     */
    protected static $cachedArRoles = null;

    /**
     * @return ArRole[] cached list of all `ArRole`
     *
     */
    public static function getAll() {
        if (is_null(self::$cachedArRoles)) {
            self::$cachedArRoles = ArRolePeer::doSelect(new Criteria());
        }
        return self::$cachedArRoles;
    }

    /**
     * @static
     * @param string $internalName
     * @return ArRole|null
     */
    public static function retrieveByInternalName($internalName)
    {
        $criteria = new Criteria();
        $criteria->add(ArRolePeer::INTERNAL_NAME, $internalName);

        return ArRolePeer::doSelectOne($criteria);
    }
}
