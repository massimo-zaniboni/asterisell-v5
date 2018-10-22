<?php

// SPDX-License-Identifier: GPL-3.0-or-later

/**
 * Subclass for performing query and update operations on the 'cdr' table.
 *
 * @package lib.model
 */ 
class ArCdrPeer extends BaseArCdrPeer
{
    /**
     * Update $c with the Criteria used from self::doSelectJoinAllExceptVendor.
     * It adds only join conditions.
     */
    public static function addAllJoinsExceptVendorCondition(Criteria $c) {

        // Add joins
        //
        // (do not join with VENDOR_ID) but they can be cached efficiently)
        //
        $c->addJoin(ArCdrPeer::AR_TELEPHONE_PREFIX_ID, ArTelephonePrefixPeer::ID);

        return $c;
    }

    /**
     * @return int|null the max call date in the database
     */
    public static function getLastCallDate() {
        $conn = Propel::getConnection();
        $query = 'SELECT MAX(calldate) FROM ar_cdr';

        $r = null;
        $stmt = $conn->prepare($query);
        $stmt->execute();
        while (($rs = $stmt->fetch(PDO::FETCH_COLUMN)) !== false) {
            $r = $rs[0];
        }
        $stmt->closeCursor();

        if (is_null($r)) {
            return null;
        } else {
            return strtotime($r);
        }
    }

  	/**
	 * Retrieve object using using composite pkey values.
	 * @param      int $calldate
	 * @param      int $is_service_cdr
	 * @param      int $id
	 * @param      PropelPDO $con
	 * @return     ArCdr
	 */
	public static function retrieveByPK1($calldate, $is_service_cdr, $id, PropelPDO $con = null) {
     	if ($con === null) {
			$con = Propel::getConnection();
		}
		$criteria = new Criteria();
		$criteria->add(ArCdrPeer::CALLDATE, fromUnixTimestampToMySQLTimestamp($calldate));
		$criteria->add(ArCdrPeer::IS_SERVICE_CDR, $is_service_cdr);
		$criteria->add(ArCdrPeer::ID, $id);
		return ArCdrPeer::doSelectOne($criteria, $con);
	}
}