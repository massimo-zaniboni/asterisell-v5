<?php
// SPDX-License-Identifier: GPL-3.0-or-later

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

        $yamlJob = new ChangeOrganizationInfo();
        $yaml = $yamlJob->getYAMLContent(null, null, null);
        $md5 = md5($yaml);

        $outFileName = tempnam(ImportDataFiles::getMySQLAccessibleTmpDirectory(self::GARBAGE_KEY), "asterisell");

        try {

            $lastBackup = null;
            ArOrganizationBackupOfChangesPeer::clearInstancePool();
            $c = new Criteria();
            $c->add(ArOrganizationBackupOfChangesPeer::ID, 0, Criteria::GREATER_EQUAL);
            $c->addDescendingOrderByColumn(ArOrganizationBackupOfChangesPeer::BACKUP_AT_DATE);
            $lastBackup = ArOrganizationBackupOfChangesPeer::doSelectOne($c);

            if (is_null($lastBackup) || $lastBackup->getMd5Sum() != $md5) {
                // The backup is different from last backup.

                // NOTE: I use the md5 of the YAML export, because the mysql dump can contain
                // info about the date of dump, and so every time it seems there is a change in the data.
                // I have full control of YAML content, but some changes can not be detected (in particular users).

                // Backup of MySQL

                list($databaseName, $user, $password) = getDatabaseNameUserAndPassword();
                $version = file_get_contents(getAsterisellCompleteRootDirectory() . '/VERSION');

                $rootDir = getAsterisellCompleteRootDirectory();
                $cmd = "cd $rootDir && mysqldump --opt -u$user -p$password $databaseName ar_user ar_party ar_organization_unit ar_organization_unit_type ar_organization_unit_has_structure ar_user_has_role ar_user_has_permission > $outFileName ";
                system($cmd, $result);

                // Save info

                $backup = new ArOrganizationBackupOfChanges();
                $backup->setBackupAtDate(time());
                $backup->setApplicationVersion($version);
                $backup->setYamlExportAtDateFromPlainText($yaml);
                $sqlContent = file_get_contents($outFileName);
                $backup->setSqlTablesFromPlainText($sqlContent);
                $backup->setMd5Sum($md5);
                $backup->save();

                ArOrganizationBackupOfChangesPeer::clearInstancePool();
                @unlink($outFileName);

                $conn = Propel::getConnection();

                // Delete backups older than some days / months
                $oldMonths = getInstanceConfigValue('months_after_removing_a_job_log_entry');
                $limit = strtotime("-$oldMonths month");
                $stm = $conn->prepare('DELETE FROM ar_organization_backup_of_changes WHERE backup_at_date < ?');
                $stm->execute(array(fromUnixTimestampToMySQLTimestamp($limit)));
                $stm->closeCursor();

                // Check if there are too much backups.
                $query = "SELECT count(id) FROM ar_organization_backup_of_changes";
                $stm = $conn->prepare($query);
                $stm->execute();
                $num = 0;
                while ($rs = $stm->fetch(PDO::FETCH_NUM)) {
                    $num = $rs[0];
                }
                $stm->closeCursor();
                $x = (int) $oldMonths;
                $limit = $x * 31 * 5;
                if ($num > $limit) {
                    $problemDuplicationKey = get_class($this) . " - too much backup of organization info";
                    $problemDescription = 'There are ' . $num . ' hourly backup of organization info in last ' . $oldMonths . ' months, and this is an unexpected high number. There can be an errors in the code, or importing procedures.';
                    $problemEffect = "Too much space is used on the DBMS and on the backups.";
                    $problemProposedSolution = 'Contact the assistance.';
                    ArProblemException::createWithoutGarbageCollection(
                        ArProblemType::TYPE_WARNING
                        , ArProblemDomain::CONFIGURATIONS
                        , null
                        , $problemDuplicationKey
                        , $problemDescription
                        , $problemEffect
                        , $problemProposedSolution);
                }

                return 'Saved last changes, and deleted backups older than ' . $oldMonths . ' months. There are ' . $num . ' left backups on the table.';

            } else {
                @unlink($outFileName);
                return 'No changes to save, respect previous backup.';
            }

        } catch (ArProblemException $e) {
            @unlink($outFileName);
            throw($this->createError($e->getLastErrorDescription()));
        } catch (Exception $e) {
            @unlink($outFileName);
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
