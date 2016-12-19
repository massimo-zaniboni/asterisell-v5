<?php

/* $LICENSE 2015:
 *
 * Copyright (C) 2015 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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
 * Read CDRS from Asterisk
 */
abstract class ImportCDRSFromAsterisk extends ImportCDRSFromDatabase
{

    public function getLogicalType()
    {
        return 'asterisk';
    }

    public function getPhysicalType()
    {
        return 'generic';
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
        return 'calldate';
    }

    public function isCallDateFieldATimestamp()
    {
        return false;
    }

    public function getProgressiveField()
    {
        return 'uniqueid';
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
                'calldate',
                'clid',
                'src',
                'dst',
                'dcontext',
                'channel',
                'dstchannel',
                'lastapp',
                'lastdata',
                'duration',
                'billsec',
                'disposition',
                'amaflags',
                'accountcode',
                'uniqueid',
                'userfield',
                'did',
                'recordingfile',
                'cnum',
                'cnam',
                'outbound_cnum',
                'outbound_cnam',
                'dst_cnam'
            );
        }
        return $r;
    }
}
