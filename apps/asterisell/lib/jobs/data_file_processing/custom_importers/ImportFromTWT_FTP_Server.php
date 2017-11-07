<?php

/* $LICENSE 2012:
 *
 * Copyright (C) 2012 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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
 * Import CSV files from TWT FTP server.
 * This is an abstract class. Every TWT account must be mapped to a distinct job,
 * with a distinct CDR provider.
 * The connection params are named "twt-ftp-account-<getTWTAccount>".
 * The CDR provider is "twt-ftp-account-<getTWTAccount>".
 */
abstract class ImportFromTWT_FTP_Server extends ImportCSVFilesFromFTPServer
{

    //
    // Methods to customize in subclasses.
    // Make sure to respect the requirements in method headers.
    //

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

    //
    // Specific Implementation.
    //

    public function fileArchiveName($sourceFileName)
    {
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

    public function canAcceptFileName($n)
    {
        $account = str_pad($this->getTWTAccount(), 10, '0', STR_PAD_LEFT);

        // NOTE: the digits are the date in YYYYMMDD format, "N" for a new data, and a progressive number.
        // "X" is used in case of data deleting previous info, but it is not managed up to date, but only signaled.
        // NOTE: do not use status files, because the timeframe of the file name is not the same of the CDRs inside it, and it can be error prone.
        $reg = '/^' . $account . "(\\d)+N(\\d)+[.]zip/i";
        if (preg_match($reg, $n)) {
            return ImportDataFiles::createInputDataFileName(null, $this->getCdrProvider(), $this->getLogicalType(), $this->getPhysicalType());
        }

        // this type of file must be taken in consideration, but it is not processed in an automatic way.
        // So signal the problem.
        $reg = '/^' . $account . "(\\d)+X(\\d)+[.]zip/i";
        if (preg_match($reg, $n)) {
            return false;
        }

        // there can be other files, and they are ignored.
        return true;
    }

    /**
     * Change (eventually) the content of the source file name. Usually it is a decompression operation.
     * @param string $remoteFileName
     * @param string $sourceFileName
     * @param string $destFileName
     * @return bool true if the content of the file was changed, false if $sourceFile must be used
     */
    public function normalizeFileContent($remoteFileName, $sourceFileName, $destFileName)
    {
        $this->unzipFileWithOnlyOneFile($remoteFileName, $sourceFileName, $destFileName);
        return true;
    }
}
