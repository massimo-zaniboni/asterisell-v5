<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Import CPS files from TWT FTP server.
 * This is an abstract class. Every TWT account must be mapped to a distinct job,
 * with a distinct CDR provider.
 * The connection params are named "twt-ftp-account-<getTWTAccount>".
 * The CDR provider is "twt-ftp-account-<getTWTAccount>".
 */
abstract class ImportTWT_CPS extends ImportFromTWT_FTP_Server
{

    public function getLogicalType() {
        return 'twt-cps';
    }

    public function getPhysicalType() {
        return 'v1';
    }

}
