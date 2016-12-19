<?php

/* $LICENSE 2015:
 *
 * Copyright (C) 2015 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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
 * Support for backup of files.
 */
abstract class DailyBackupJob extends DailyStatusJob
{

    /**
     * Execute usually at the specified time.
     * TODO up to date this does not work as expected.
     * @return string 'hh:mm:ss'
     */
    abstract function getAtSpecificTime();

    /**
     * @return int execute every specified hours.
     */
    abstract function getHoursExecutionTimeFrame();

    /**
     * @return string the name of a logical format, identifying the type of backup
     */
    abstract function getLogicalType();

    /**
     * Restore information.
     * @return null|string null if it is all ok, the error message instead.
     */
    abstract function restoreFromBackup();

    //
    // Implement DailyJob interface
    //

    public function generateUniqueChangedDayEvent() {
        return true;
    }

    public function getActivationDate() {
        return $this->getGlobalStartingDate();
    }

    public function initJob()
    {
        // this is called only if waitXMinutes test passed. Then check if it is the correct time of the day.

        $time = time();
        $mm = date('m', $time);
        $yy = date('Y', $time);
        $dd = date('d', $time);

        $backupTime = strtotime("$yy-$mm-$dd " . $this->getAtSpecificTime());

        if (time() >= $backupTime) {
            return true;
        } else {
            return false;
        }
    }

    protected function waitXMinutes()
    {
        return $this->getHoursExecutionTimeFrame() * 60;
    }

    //
    // Directory and File Management
    //

    const ARCHIVE_DIRECTORY = 'data_files/messages/backup/';

    static public function getCompleteBackupDirectory() {
      return normalizeFileNamePath(getAsterisellCompleteAdminDirectory() . '/' . self::ARCHIVE_DIRECTORY);
    }

    /**
     * @return string the preferred tmp directory to use, because it is saved on a file system, without
     * using the RAM, and if Asterisell is mounted on the same partition, move operations are atomic.
     * Use a distinct directory for every instance, so multiple instances can work on the same server.
     * @throws ArProblemException
     */
    public function getMySQLAccessibleTmpDirectory() {
        return ImportDataFiles::getMySQLAccessibleTmpDirectory(get_class($this));
    }

    /**
     * Create and get the directory where writing
     * @param int|null $time
     * @return string the directory where writing
     * @throws ArProblemException
     */
    public function createAndGetAbsoluteArchiveDirectory($time)
    {
        $completeDestDir = normalizeFileNamePath(getAsterisellCompleteAdminDirectory() . '/' . self::getRelativeArchiveDirectory($time));

        if (!file_exists($completeDestDir)) {
            $isOk = mkdir($completeDestDir, 0777, true);
            if ($isOk === FALSE) {
                throw $this->createBackupError(
                    'unable to create directory ' . $completeDestDir,
                    "Unable to create directory \"$completeDestDir\""
                    );
            }
        }

        return $completeDestDir;
    }

    /**
     * @param int|null $time
     * @return string
     */
    public function getRelativeArchiveDirectory($time)
    {
        $r = self::ARCHIVE_DIRECTORY . '/' . $this->getLogicalType() . '/';

        if (!is_null($time)) {
            $r .= 'y_' . date('Y', $time) . '/' . 'm_' . date('m', $time) . '/';
        }

        return normalizeFileNamePath($r);
    }

    /**
     * @param string $key
     * @param string $msg a message
     * @return ArProblemException
     */
    protected function createBackupError($key, $msg) {
        return $this->createError(
            ArProblemType::TYPE_ERROR,
            ArProblemDomain::CONFIGURATIONS,
            $key . ' - ' . $this->getLogicalType(),
            "Error during backup of " . $this->getLogicalType() . ": " . $msg,
            $this->getLogicalType() . " can not be backup.",
            "Check the directory have the correct access permissions. If the problem persists contact the assistance."
        );
    }
}
