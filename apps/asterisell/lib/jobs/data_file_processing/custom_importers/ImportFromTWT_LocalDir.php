<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Similar to ImportFromTWT_FTP_Server but work on local directory.
 */
abstract class ImportFromTWT_LocalDir extends ImportCSVFilesFromLocalServer
{

    // ------------------------------------------------------------------
    // Methods to customize in subclasses.
    // Make sure to respect the requirements in method headers.
    //

    /**
     * @return boolean true for processing NNG files
     */
    public function isNNG() {
        return false;
    }
        
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
        return false;
    }

    //
    // Specific Implementation.
    //

    public function getSourceCharacterEncoding() {
        return 'ISO-8859-1';
    }

    
    public function canAcceptFileName($n) {
        if ($this->isNNG()) {
            $account = str_pad($this->getTWTAccount(), 7, '0', STR_PAD_LEFT);

            // NOTE: the digits are the date in YYYYMMDD format, "N" for a new data, and a progressive number.
            // "X" is used for additional data, not included in the first send.
            $reg = '/^NNG' . $account . "\\d\\d\\d\\d\\d\\d\\d\\d[NX]\\d\\d\\d[.](zip|cdr)/i";
            if (preg_match($reg, $n)) {
                return ImportDataFiles::createInputDataFileName(null, $this->getCdrProvider(), $this->getLogicalType(), $this->getPhysicalType());
            }

            return $this->thereCanBeOtherFilesToIgnore();
        } else {
            $account = str_pad($this->getTWTAccount(), 10, '0', STR_PAD_LEFT);

            // NOTE: the digits are the date in YYYYMMDD format, "N" for a new data, and a progressive number.
            // "X" is used in case of additional data
            // NOTE: do not use status files, because the timeframe of the file name is not the same of the CDRs inside it, and it can be error prone.
            $reg = '/^' . $account . "(\\d)+[NX](\\d)+[.](zip|cdr)/i";
            if (preg_match($reg, $n)) {
                return ImportDataFiles::createInputDataFileName(null, $this->getCdrProvider(), $this->getLogicalType(), $this->getPhysicalType());
            }

            return $this->thereCanBeOtherFilesToIgnore();
        }
    }
    
}
