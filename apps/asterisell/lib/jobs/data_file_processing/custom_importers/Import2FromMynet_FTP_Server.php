<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2022 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * This is an abstract class. Every Mynet account must be mapped to a distinct job,
 * with a distinct CDR provider.
 * The connection params are named "mynet-ftp-account-<getAccount>".
 * The CDR provider is "mynet-ftp-account-<getAccount>".
 */
abstract class Import2FromMynet_FTP_Server extends ImportCSVFilesFromFTPServer {

    // ----------------------------------------------------------    
    // Methods to customize in subclasses.
    // Make sure to respect the requirements in method headers.
    //

    /**
     * @return string the name of the account from which retrieve files.
     */
    abstract public function getAccount();

    //
    // Specific Implementation.
    //

    public function getRemoteDirectory() {
        return '';
    }
    
    // MAYBE
    public function getSourceCharacterEncoding() {
        return 'UTF8';
    }

    /**
     * @return string the name of the connection params to use, and defined in app.yml under connection settings.
     */
    public function getConnectionName() {
        return 'mynet-ftp-account-' . $this->getAccount();
    }

    public function getCdrProvider() {
        return $this->getConnectionName();
    }

    /**
     * File names like "2020_05_26.csv"
     * @param string $n
     * @return boolean|string
     */
    public function canAcceptFileName($n) {
        $m = array();
        $logicalType = null;

        $reg = "/^(\\d\\d\\d\\d)_(\\d\\d)_(\\d\\d)\\.csv$/";
        
        if (preg_match($reg, $n, $m)) {
            return ImportDataFiles::createInputDataFileName(null, $this->getCdrProvider(), "mynet", "v1");
        } else {
            return false;
        }
   }
}
