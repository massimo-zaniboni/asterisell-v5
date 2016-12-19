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
     * @param string $n file name on remote server
     * @return int 0 if the file can be ignored, 1 if the file can be imported, 2 if the name is unexpected, and it must be signaled to the user
     */
    public function canAcceptFileName($n)
    {
        $account = str_pad($this->getTWTAccount(), 7, '0', STR_PAD_LEFT);

        // NOTE: the digits are the date in YYYYMMDD format, "N" for a new data, and a progressive number.
        // "X  " is used in case of data deleting previous info, but it is not managed up to date, but only signaled.
        $reg = '/^NNG' . $account . "\\d\\d\\d\\d\\d\\d\\d\\dN\\d\\d\\d[.]zip/i";
        if (preg_match($reg, $n)) {
            return ImportDataFiles::createInputDataFileName(null, $this->getCdrProvider(), $this->getLogicalType(), $this->getPhysicalType());
        }

        // NOTE: the digits are the date in YYYYMMDD format, "N" for a new data, and a progressive number.
        // "X" is used in case of data deleting previous info, but it is not managed up to date, but only signaled.
        $reg = '/^NNG' . $account . "\\d\\d\\d\\d\\d\\d\\d\\dX\\d\\d\\d[.]zip/i";
        if (preg_match($reg, $n)) {
            // this file must be managed in a correct way
            return false;
        }

        // in this directory there are other files that can be ignored.
        return true;
    }

}
