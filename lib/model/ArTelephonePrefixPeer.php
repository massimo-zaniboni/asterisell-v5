<?php

/**
 * Subclass for performing query and update operations on the 'ar_telephone_prefix' table.
 *
 * 
 *
 * @package lib.model
 */ 
class ArTelephonePrefixPeer extends BaseArTelephonePrefixPeer
{

    /**
     * Retrieve a single object by pkey.
     *
     * @param      int $pk the exact prefix.
     * @param      PropelPDO $con the connection to use
     * @return     ArTelephonePrefix
     */
    public static function retrieveByPrefix($pk, PropelPDO $con = null)
    {

        if ($con === null) {
            $con = Propel::getConnection(ArTelephonePrefixPeer::DATABASE_NAME, Propel::CONNECTION_READ);
        }

        $criteria = new Criteria(ArTelephonePrefixPeer::DATABASE_NAME);
        $criteria->add(ArTelephonePrefixPeer::PREFIX, $pk);

        $v = ArTelephonePrefixPeer::doSelect($criteria, $con);

        return !empty($v) > 0 ? $v[0] : null;
    }

}
