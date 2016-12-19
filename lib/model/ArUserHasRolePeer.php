<?php

require 'lib/model/om/BaseArUserHasRolePeer.php';


/**
 * Skeleton subclass for performing query and update operations on the 'ar_user_has_role' table.
 *
 * 
 *
 * You should add additional methods to this class to meet the
 * application requirements.  This class will only be generated as
 * long as it does not already exist in the output directory.
 *
 * @package    lib.model
 */
class ArUserHasRolePeer extends BaseArUserHasRolePeer {

    /**
     * @static
     * @param int $userId
     * @return ArUserHasRole[]
     */
    public static function getRolesOfUser($userId) {
        $c = new Criteria();
        $c->add(ArUserHasRolePeer::AR_USER_ID, $userId);
        return ArUserHasRolePeer::doSelect($c);
    }

} // ArUserHasRolePeer
