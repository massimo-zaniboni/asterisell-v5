<?php
/* $LICENSE 2014:
 *
 * Copyright (C) 2014 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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
 * Save on table ar_organization_backup_of_changes the last changes on the organization info..
 */
class BackupOrganizationInfo extends JobProcessor
{
    ////////////
    // PARAMS //
    ////////////

    const ENABLE_DEBUG_MESSAGES = false;

    const GARBAGE_KEY = 'BackupOrganizationInfo';

    const EXECUTION_INTERVAL_IN_MINUTES = 60;

    const DELETE_BACKUPS_OLDER_THAN_DAYS = 180;

    ///////////////////
    // JOB INTERFACE //
    ///////////////////

    public function processEvent(JobData $jobData, $jobId)
    {
        if (!($jobData instanceof BackupOrganizationInfoEvent)) {
            return null;
        }

        /**
         * @var ChangeOrganizationInfoEvent $jobData
         */
        return $this->saveBackup();
    }

    /**
     * This is the best way to execute this process.
     */
    public static function scheduleProcess()
    {
        $event = new BackupOrganizationInfoEvent();
        ArJobQueuePeer::addNew($event, null, self::GARBAGE_KEY);
    }

    public function process()
    {
        $timeFrameInMinutes = self::EXECUTION_INTERVAL_IN_MINUTES;
        $checkFile = self::GARBAGE_KEY;
        $checkLimit = strtotime("-$timeFrameInMinutes minutes");
        $mutex = new Mutex($checkFile);

        if ($mutex->maybeTouch($checkLimit)) {
            return $this->saveBackup();
        }

        return '';
    }


    /**
     * Save a backup.
     */
    public function saveBackup()
    {
        $outFileName = tempnam(ImportDataFiles::getMySQLAccessibleTmpDirectory(self::GARBAGE_KEY), "asterisell");
        try {
            list($databaseName, $user, $password) = getDatabaseNameUserAndPassword();

            $rootDir = getAsterisellCompleteRootDirectory();
            $cmd = "cd $rootDir && mysqldump --opt -u$user -p$password $databaseName ar_user ar_party ar_organization_unit ar_organization_unit_type ar_organization_unit_has_structure ar_user_has_role ar_user_has_permission ar_itc_organizations > $outFileName ";
            system($cmd, $result);

            // Check if this is a new backup

            $md5 = md5_file($outFileName);

            $c = new Criteria();
            $c->add(ArOrganizationBackupOfChangesPeer::ID, 0, Criteria::GREATER_EQUAL);
            $c->addDescendingOrderByColumn(ArOrganizationBackupOfChangesPeer::BACKUP_AT_DATE);
            $lastBackup = ArOrganizationBackupOfChangesPeer::doSelectOne($c);

            if (is_null($lastBackup) || $lastBackup->getMd5Sum() != $md5) {
                // the backup is different from last backup

                $version = file_get_contents(getAsterisellCompleteRootDirectory() . '/VERSION');

                $lastBackup = null;
                ArOrganizationBackupOfChangesPeer::clearInstancePool();

                $backup = new ArOrganizationBackupOfChanges();
                $backup->setBackupAtDate(time());
                $backup->setApplicationVersion($version);

                $sqlContent = file_get_contents($outFileName);
                $backup->setSqlTablesFromPlainText($sqlContent);

                $yamlJob = new ChangeOrganizationInfo();
                $yaml = $yamlJob->getYAMLContent(null, null, null);

                $backup->setYamlExportAtDateFromPlainText($yaml);

                // DEV-NOTE force the known good value...
                $backup->setMd5Sum($md5);

                $backup->save();

                ArOrganizationBackupOfChangesPeer::clearInstancePool();
                @unlink($outFileName);

                // Delete backups older than some days / months

                $limit = strtotime('-' . self::DELETE_BACKUPS_OLDER_THAN_DAYS . ' days', time());

                $stm = Propel::getConnection()->prepare('DELETE FROM ar_organization_backup_of_changes WHERE backup_at_date < ?');
                $stm->execute(array(fromUnixTimestampToMySQLTimestamp($limit)));
                $stm->closeCursor();


                @unlink($outFileName );
                return 'Saved last changes, and deleted backups older than ' . self::DELETE_BACKUPS_OLDER_THAN_DAYS . ' days.';

            } else {

                @unlink($outFileName );
                return 'No changes to save, respect previous backup.';
            }

        } catch (ArProblemException $e) {
            @unlink($outFileName );
            throw($this->createError($e->getLastErrorDescription()));
        } catch (Exception $e) {
            @unlink($outFileName );
            throw($this->createError($e->getMessage()));
        }
    }

    protected function createError($message)
    {
        return ArProblemException::createWithGarbageCollection(
            ArProblemType::TYPE_WARNING,
            ArProblemDomain::VOIP_ACCOUNTS,
            ArProblemResponsible::APPLICATION_ASSISTANCE,
            'error saving units -' . get_class($this) . '- ' . md5($message),
            self::GARBAGE_KEY,
            null, null,
            "Error during saving backup version of organizations info. Organization Hierarchy will not be saved in the backup. " . $message,
            'In case the organization info will be corrupted, there is no recent backup from which restore.',
            'If the problem persist, contact the assistance.');
    }

}
