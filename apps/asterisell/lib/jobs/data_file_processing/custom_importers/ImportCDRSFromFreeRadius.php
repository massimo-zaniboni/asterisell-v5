<?php

/* $LICENSE 2014:
 *
 * Copyright (C) 2014 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
 *
 * This file is part of Asterisell.
 *
 * Asterisell is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * Asterisell is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Asterisell. If not, see <http://www.gnu.org/licenses/>.
 * $
 */

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Read CDRS from FreeRadius.
 */
abstract class ImportCDRSFromFreeRadius extends ImportCDRSFromDatabase
{

    public function getLogicalType()
    {
        return 'free-radius';
    }

    public function getPhysicalType()
    {
        return 'v1';
    }

    public function getExportedStatusBooleanField()
    {
        return 'is_exported_to_asterisell';
    }

    public function getValueOfCDRToBeExported()
    {
        return false;
    }

    public function getCallDateField()
    {
        return 'h323_setup_time';
    }

    public function isCallDateFieldATimestamp()
    {
        return false;
    }

    public function getProgressiveField()
    {
        return 'id';
    }

    public function removeExportedCDRSOlderThanDays()
    {
        return 0;
    }

    public function getListOfFields()
    {
        static $r = null;

        if (is_null($r)) {
            $r = array(
                'id',
                'Unique_Id',
                'Calling_Station_Id',
                'Called_Station_Id',
                'h323_setup_time',
                'Acct_Session_Time',
                'in_intrfc_desc',
                'out_intrfc_desc',
                'h323_remote_address_in',
                'h323_remote_address_out',
                'rerouted',
                'h323_disconnect_cause',
                'h323_gateway_id',
                'h323_conf_id',
                'Acct_Session_Id'
            );
        }

        return $r;
    }
}
