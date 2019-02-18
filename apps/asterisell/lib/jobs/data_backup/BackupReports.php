<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Backup application reports.
 *
 * DEV-NOTE: this code is not optimal, because it saves backup only if there are some change events in the application.
 * I organized in this way for having code reuse.
 *
 * DEV-NOTE: This job save all reports of all the time. It can be rather big with time. A better method is saving
 * the reports of last 6-12 months, assuming that reports older than 6-12 months are not any more changed.
 * It can write info on separated files, grouped by month, and then not generate any more old months.
 */
class BackupReports extends DailyBackupJob
{

    function getAtSpecificTime()
    {
        return '00:30:00';
    }

    function getHoursExecutionTimeFrame()
    {
        return 24;
    }

    function getLogicalType()
    {
        return 'reports';
    }

    public function initJob()
    {
        $continue = parent::initJob();
        if ($continue) {
            list($database, $user, $password) = getDatabaseNameUserAndPassword();

            // require: all the tables in this list must:
            // - be regenerated automatically after restore
            // - or saved explicitely in other backup jobs
            $tablesToIgnore = array_merge(BackupConfigurations::getConfigurationTablesToIgnore());

            $options = ' ';
            foreach ($tablesToIgnore as $t) {
                $options .= " --ignore-table=$database.$t ";
            }

            $fileName = $this->getLogicalType() . '.sql';
            $tmpFileName = normalizeFileNamePath($this->getMySQLAccessibleTmpDirectory() . '/' . $fileName);
            @unlink($tmpFileName);

            $cmd = "mysqldump -u $user --password=$password $database --single-transaction $options > $tmpFileName";
            $isOk = system($cmd);
            if ($isOk === FALSE) {
                throw $this->createError(
                    ArProblemType::TYPE_ERROR,
                    ArProblemDomain::APPLICATION,
                    'unable to backup configuration files ' . $tmpFileName,
                    "Unable to generate $tmpFileName, during backup of configurations.",
                    "Configurations can not be backup. In case of restore from a backup, there will be missing or no updated information.",
                    "If the problem persists contact the assistance."
                );
            }

            $exportDirectory = $this->createAndGetAbsoluteArchiveDirectory(null);
            $dstFile = normalizeFileNamePath($exportDirectory . '/' . $fileName);
            @unlink($dstFile);

            $isOk = rename($tmpFileName, $dstFile);
            if ($isOk === FALSE) {
                throw $this->createError(
                    ArProblemType::TYPE_ERROR,
                    ArProblemDomain::CONFIGURATIONS,
                    'unable to backup report files ' . $dstFile,
                    "Unable to move the file $tmpFileName to $dstFile, during backup of reports.",
                    "Reports can not be backup. In case of restore from a backup, there will be missing or no updated information.",
                    "Check the directory have the correct access permissions. If the problem persists contact the assistance."
                );
            }

            return true;
        } else {
            return false;
        }
    }

    function restoreFromBackup()
    {
        list($database, $user, $password) = getDatabaseNameUserAndPassword();

        $exportDirectory = $this->createAndGetAbsoluteArchiveDirectory(null);
        $fileName = $this->getLogicalType() . '.sql';
        $dstFile = normalizeFileNamePath($exportDirectory . '/' . $fileName);

        $cmd = "mysql -u${user} -p${password} $database < $dstFile";
        $isOk = system($cmd);
        if ($isOk === FALSE) {
            return "Error executing command: $cmd";
        }

        return null;
    }

    public function processChangedDay($fromDate, $toDate, PropelPDO $conn)
    {
        // nothing to do because the work is done from the init method only one time, for all change events.
    }
}
