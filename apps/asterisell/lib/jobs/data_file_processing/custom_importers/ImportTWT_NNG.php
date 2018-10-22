<?php

// SPDX-License-Identifier: GPL-3.0-or-later

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

    /**
     * @return bool true for ignoring other additional files, false for signaling problems.
     * NOTE: it is rather risky to ignore files,
     * because we can not import data files.
     */
    public function thereCanBeOtherFilesToIgnore() {
        return true;
    }

    /**
     * @param string $n file name on remote server
     * @return int 0 if the file can be ignored, 1 if the file can be imported, 2 if the name is unexpected, and it must be signaled to the user
     */
    public function canAcceptFileName($n)
    {
        $account = str_pad($this->getTWTAccount(), 7, '0', STR_PAD_LEFT);

        // NOTE: the digits are the date in YYYYMMDD format, "N" for a new data, and a progressive number.
        // "X" is used for additional data, not included in the first send.
        $reg = '/^NNG' . $account . "\\d\\d\\d\\d\\d\\d\\d\\d[NX]\\d\\d\\d[.]zip/i";
        if (preg_match($reg, $n)) {
            return ImportDataFiles::createInputDataFileName(null, $this->getCdrProvider(), $this->getLogicalType(), $this->getPhysicalType());
        }

        return $this->thereCanBeOtherFilesToIgnore();
    }

}
