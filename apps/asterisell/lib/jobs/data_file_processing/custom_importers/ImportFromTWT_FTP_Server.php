<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Import CSV files from TWT FTP server.
 * This is an abstract class. Every TWT account must be mapped to a distinct job,
 * with a distinct CDR provider.
 * The connection params are named "twt-ftp-account-<getTWTAccount>".
 * The CDR provider is "twt-ftp-account-<getTWTAccount>".
 */
abstract class ImportFromTWT_FTP_Server extends ImportCSVFilesFromFTPServer {

    // ----------------------------------------------------------    
    // Methods to customize in subclasses.
    // Make sure to respect the requirements in method headers.
    //

    /**
     * @return boolean true for processing NNG files
     */
    public function isNNG() {
        return false;
    }

    abstract public function getLogicalType();

    abstract public function getPhysicalType();

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
     * @return bool true for ignoring other additional files, false for signaling problems.
     * NOTE: it is rather risky to ignore files,
     * because we can not import data files.
     */
    public function thereCanBeOtherFilesToIgnore() {
        return true;
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
     * @return string the remote directory where there are the files to download. Empty for default directory.
     */
    public function getRemoteDirectory() {
        if (isEmptyOrNull($this->getOwnerAccount())) {
            return $this->getTWTAccount();
        } else {
            return normalizeFileNamePath($this->getOwnerAccount() . '/' . $this->getTWTAccount());
        }
    }

    public function canAcceptFileName($n) {
        if ($this->isNNG()) {
            $account = str_pad($this->getTWTAccount(), 7, '0', STR_PAD_LEFT);

            $m = array();
            
            // NOTE: the digits are the date in YYYYMMDD format, "N" for a new data, and a progressive number.
            // "X" is used for additional data, not included in the first send.
            $reg = '/^NNG' . $account . "\\d\\d\\d\\d\\d\\d\\d\\d([NXM])\\d\\d\\d[.]zip/i";
            if (preg_match($reg, $n, $m)) {
                if ($m[1] == "M") {
                    return true;
                    // ignore the file containing the calls of the entire month, because they are repetaed calls
                } else {
                    return ImportDataFiles::createInputDataFileName(null, $this->getCdrProvider(), $this->getLogicalType(), $this->getPhysicalType());
                }
            } else {
              return $this->thereCanBeOtherFilesToIgnore();
            }
        } else {
            $account = str_pad($this->getTWTAccount(), 10, '0', STR_PAD_LEFT);

            $m = array();

            // NOTE: the digits are the date in YYYYMMDD format, "N" for a new data, and a progressive number.
            // "X" is used in case of additional data
            // NOTE: do not use status files, because the timeframe of the file name is not the same of the CDRs inside it, and it can be error prone.
            $reg = '/^' . $account . "(\\d)+([NXM])(\\d)+[.]zip/i";
            if (preg_match($reg, $n, $m)) {
                if ($m[2] == "M") {
                    return true;
                    // ignore the file containing the calls of the entire month, because they are repetaed calls
                } else {
                  return ImportDataFiles::createInputDataFileName(null, $this->getCdrProvider(), $this->getLogicalType(), $this->getPhysicalType());
                }
            } else {
              return $this->thereCanBeOtherFilesToIgnore();
            }
        }
    }

    /**
     * Change (eventually) the content of the source file name. Usually it is a decompression operation.
     * @param string $remoteFileName
     * @param string $sourceFileName
     * @param string $destFileName
     * @return bool true if the content of the file was changed, false if $sourceFile must be used
     */
    public function normalizeFileContent($remoteFileName, $sourceFileName, $destFileName) {
        $this->unzipFileWithOnlyOneFile($remoteFileName, $sourceFileName, $destFileName);
        return true;
    }

}
