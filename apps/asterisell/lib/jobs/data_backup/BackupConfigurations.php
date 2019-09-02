<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Backup application configurations.
 * Configurations is data that do not grow in a linear way day after day, and that can be backup as a unique chunk.
 *
 * DEV-NOTE: this code is not optimal, because it saves backup only if there are some change events in the application.
 * I organized in this way for having code reuse.
 */
class BackupConfigurations extends DailyBackupJob
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
        return 'configurations';
    }

    /**
     * @return array the tables to not load
     */
    static public function getConfigurationTablesToIgnore()
    {
        return array(
          'ar_cdr'                // saved from other backup job
        , 'ar_source_cdr'         // saved from other backup job
        , 'ar_cached_grouped_cdr' // regenerated
        , 'ar_cached_errors'      // regenerated
        , 'ar_current_problem'    // regenerated
        , 'ar_new_problem'        // regenerated
        , 'ar_bundle_state'       // regenerated. In any case the rerating is never done for old CDRs
        , 'ar_organization_backup_of_changes' // can be retrieved from past backups
        , 'ar_report_to_read' // consider all reports as already read
        , 'ar_user_can_view_report' // regenerated
        , 'ar_job_queue' // regenerated
        , 'ar_lock' // regenerated
        , 'ar_temp_source_cdr' // regenerated
        , 'ar_temp_source_cdr_to_dest_cdr' // regenerated
        , 'ar_temp_problem' // regenerated
        , 'ar_daily_status_change' // regenerated
        , 'ar_cached_organization_info' // regenerated
        , 'ar_wholesale_number_transaction_to_update' // regenerated
        , 'ar_wholesale_number_transaction' // regenerated
        );
    }

    /**
     * @return array the tables to not load
     */
    static public function getConfigurationTablesToIgnoreInOnlyConfigExport()
    {
        $arr = self::getConfigurationTablesToIgnore();
        array_push($arr, 'ar_source_csv_file', 'ar_remote_file');
        return $arr;
   }

    /**
     * @return array the tables to not load
     */
    static public function getSpecificTablesForReports()
    {
        return array(
          'ar_report' // saved from other backup jobs
        , 'ar_report_also_for' // saved from other backup jobs
        , 'ar_report_only_for' // saved from other backup jobs
        , 'ar_report_scheduler' // saved from other backup jobs
        , 'ar_report_set' // saved from other backup jobs
        , 'ar_report_generation' // saved from other backup jobs
        , 'ar_report' // saved from other backup jobs
        , 'ar_report' // saved from other backup jobs
        );
    }

    public function initJob()
    {
        $continue = parent::initJob();
        if ($continue) {
            list($database, $user, $password) = getDatabaseNameUserAndPassword();

            // require: all the tables in this list must:
            // - be regenerated automatically after restore
            // - or saved explicitely in other backup jobs
            $tablesToIgnore = array_merge(self::getConfigurationTablesToIgnore(), self::getSpecificTablesForReports());

            $options = ' ';
            foreach ($tablesToIgnore as $t) {
                $options .= " --ignore-table=$database.$t ";
            }

            $fileName = $this->getLogicalType() . '.sql';
            $tmpFileName = normalizeFileNamePath($this->getMySQLAccessibleTmpDirectory() . '/' . $fileName);
            @unlink($tmpFileName);

            $cmd = "mysqldump -u $user --password=$password $database --opt $options > $tmpFileName";
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
                    'unable to backup configuration files ' . $dstFile,
                    "Unable to move the file $tmpFileName to $dstFile, during backup of configurations.",
                    "Configurations can not be backup. In case of restore from a backup, there will be missing or no updated information.",
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

        $fileName = $this->getLogicalType() . '.sql';
        $exportDirectory = $this->createAndGetAbsoluteArchiveDirectory(null);
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
