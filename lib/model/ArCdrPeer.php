<?php

/*
* Copyright (C) 2010 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
*
*   This file is part of Asterisell.
*
*   Asterisell is free software; you can redistribute it and/or modify
*   it under the terms of the GNU General Public License as published by
*   the Free Software Foundation; either version 3 of the License, or
*   (at your option) any later version.
*
*   Asterisell is distributed in the hope that it will be useful,
*   but WITHOUT ANY WARRANTY; without even the implied warranty of
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*   GNU General Public License for more details.
*
*   You should have received a copy of the GNU General Public License
*   along with Asterisell. If not, see <http://www.gnu.org/licenses/>.
*    
*/

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

}