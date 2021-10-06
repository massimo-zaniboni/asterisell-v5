<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2021 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Import CSV files from TWT FTP server.
 * This is a better version of ImportFromTWT_FTP_Server, because it recognizes all files
 * and signal missing files.
 * This code must recognize all files in FTP directory.
 * 
 * This is an abstract class. Every TWT account must be mapped to a distinct job,
 * with a distinct CDR provider.
 * The connection params are named "twt-ftp-account-<getTWTAccount>".
 * The CDR provider is "twt-ftp-account-<getTWTAccount>".
 */
abstract class Import2FromTWT_FTP_Server extends ImportCSVFilesFromFTPServer {
    // ----------------------------------------------------------    
    // Methods to customize in subclasses.
    // Make sure to respect the requirements in method headers.
    //

    /**
     * @return string the name of the TWT account from which retrieve files.
     */
    abstract public function getTWTAccount();

    /**
     * @return string optional owner account
     */
    public function getOwnerAccount() {
        return '';
    }

    /**
     * @return string default logical type
     */
    abstract public function getDefaultLogicalType();

    /**
     * @param string $logicalType 
     * @return string|null the corresponding physical type
     */
    abstract public function getPhysicalType($logicalType);

    /**
     * @return bool true for ignoring other additional files, false for signaling problems.
     * NOTE: it is rather risky to ignore files,
     * because we can not import data files.
     */
    public function thereCanBeOtherFilesToIgnore() {
        return false;
    }

    //
    // Specific Implementation.
    //

    public function fileArchiveName($sourceFileName) {
        return $sourceFileName;
    }

    public function getSourceCharacterEncoding() {
        return 'ISO-8859-1';
    }

    /**
     * @return string the name of the connection params to use, and defined in app.yml under connection settings.
     */
    public function getConnectionName() {
        return 'twt-ftp-account-' . $this->getTWTAccount();
    }

    public function getCdrProvider() {
        return $this->getConnectionName();
    }

    /**
     * @return string the TWT account used in `createFileNamePREG` and from TWT
     * for creating file names.
     */
    public function getTWTAccountInFileName() {
        return $this->getTWTAccount();
    }
    
    /**
     * @return string the remote directory where there are the files to download. Empty for default directory.
     */
    public function getRemoteDirectory() {
        if (isEmptyOrNull($this->getOwnerAccount())) {
            return $this->getTWTAccount();
        } else {
            return normalizeFileNamePath($this->getOwnerAccount() . '/' . $this->getTWTAccount());
        }
    }

    /**
     * @return a PREG expression in a default format.
     */
    public function createFileNamePREG($prefixPREG) {
        $account = ltrim($this->getTWTAccountInFileName(), "0");

        // NOTE: the digits are the date in YYYYMMDD format, "N" for a new data, and a progressive number.
        // "X" is used for additional data, not included in the first send.
        $reg = '/^'
                . $prefixPREG
                . '0*' . $account
                . "\\d\\d\\d\\d\\d\\d\\d\\d([NXM])\\d+"
                . "[.](zip|cdr)/i";

        return $reg;
    }

    /**
     * Recognize common data files provided by TWT.
     * The correct physical type can be configured in `getPhysicalType()`.
     */
    public function canAcceptFileName($n) {
        $m = array();
        $logicalType = null;

        if (strcmp($n, "Domini") == 0 ||
                strcmp($n, "Fax2Mail") == 0 ||
                strcmp($n, "NNG") == 0 ||
                strcmp($n, "PEC") == 0 ||
                strcmp($n, "Telegrammi") == 0 ||
                strcmp($n, "vBackup") == 0 ||
                strcmp($n, "vServer") == 0 ||
                strcmp($n, "WEB") == 0 ||
                strcmp($n, "WLR") == 0 ||
                strcmp($n, "xDSL") == 0) {
            return true;
            // Ignore known directories with the activation of customers lines
        }

        if (preg_match($this->createFileNamePREG('NNG'), $n, $m)) {
            if ($m[1] == "M") {
                return true;
                // ignore the file containing the calls of the entire month, because they are repetaed calls
            } else {
                $logicalType = 'twt-nng';
            }
        }

        if (preg_match($this->createFileNamePREG('DQ'), $n, $m)) {
            if ($m[1] == "M") {
                return true;
                // ignore the file containing the calls of the entire month, because they are repetaed calls
            } else {
                $logicalType = 'twt-dq';
            }
        }

        if (preg_match($this->createFileNamePREG(''), $n, $m)) {
            if ($m[1] == "M") {
                return true;
                // ignore the file containing the calls of the entire month, because they are repetaed calls
            } else {
                $logicalType = $this->getDefaultLogicalType();
            }
        }

        if (is_null($logicalType)) {
            return $this->thereCanBeOtherFilesToIgnore();
        }

        $physicalType = $this->getPhysicalType($logicalType);

        if (is_null($physicalType)) {
            $physicalType = 'v1';
        }

        return ImportDataFiles::createInputDataFileName(null, $this->getCdrProvider(), $logicalType, $physicalType);
    }

    /**
     * Change (eventually) the content of the source file name. Usually it is a decompression operation.
     * @param string $remoteFileName
     * @param string $sourceFileName
     * @param string $destFileName
     * @return bool true if the content of the file was changed, false if $sourceFile must be used
     */
    public function normalizeFileContent($remoteFileName, $sourceFileName, $destFileName) {
        $m = array();
        $reg = "/\\A.*[.]ZIP\\z/";

        if (preg_match($reg, strtoupper($remoteFileName), $m)) {
            $this->unzipFileWithOnlyOneFile($remoteFileName, $sourceFileName, $destFileName);
            return true;
        }

        return false;
    }

}
