<?php

// SPDX-License-Identifier: GPL-3.0-or-later

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
