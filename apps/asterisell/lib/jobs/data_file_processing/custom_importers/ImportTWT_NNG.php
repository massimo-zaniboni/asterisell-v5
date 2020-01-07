<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Import NNG files from TWT FTP server.
 * This is an abstract class. Every TWT account must be mapped to a distinct job,
 * with a distinct CDR provider.
 * The connection params are named "twt-ftp-account-<getTWTAccount>".
 * The CDR provider is "twt-ftp-account-<getTWTAccount>".
 */
abstract class ImportTWT_NNG extends ImportFromTWT_FTP_Server
{

    public function getLogicalType() {
        return 'twt-nng';
    }

    public function getPhysicalType() {
        return 'v1';
    }
   
    public function isNNG() {
        return true;
    }

    /**
     * @return bool true for ignoring other additional files, false for signaling problems.
     * NOTE: it is rather risky to ignore files,
     * because we can not import data files.
     */
    public function thereCanBeOtherFilesToIgnore() {
        return true;
    }

}
