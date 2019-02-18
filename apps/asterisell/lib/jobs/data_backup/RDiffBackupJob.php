<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Copy using RDiff the backup sensible directories to an external server.
 * Make sure to read the manual, about correct backup configurations.
 */
abstract class RDiffBackupJob extends FixedJobProcessor
{

    //
    // Configurable Params
    //

    /**
     * Execute usually at the specified time.
     * TODO up to date this does not work as expected.
     * @return string 'hh:mm:ss'
     */
    function getAtSpecificTime() {
        return '02:15:00';
    }

    /**
     * @return int execute every specified hours.
     */
    function getHoursExecutionTimeFrame() {
        return 24;
    }

    /**
     * @return string
     */
    abstract function getRemoteBackupServer();

    /**
     * @return string
     */
    abstract function getRemoteUser();

    public function getRemoteBackupDirectory() {
        return '';
    }

    /**
     * @return int for how many days maintains a copy of old files
     */
    public function getRemoveOlderThanDays() {
        return 7;
    }

    //
    // Implementation
    //

    protected function getGarbageKey() {
        return get_class($this);
    }

    /**
     * @param string $backupType the name of backup content. It should be used also as directory name.
     * @param string $sourceDir the directory to backup
     * @param bool $isDeleteCommand true for generating a delete/remove command. false for a normal command
     * @throws ArProblemException
     */
    protected function executeRDiffBackupCommand($backupType, $sourceDir, $isDeleteCommand) {

        if ($isDeleteCommand) {
          $deleteCommand = " --remove-older-than " . $this->getRemoveOlderThanDays() . "D --force ";
        } else {
            $deleteCommand = " --create-full-path $sourceDir";
        }

        $dstDir1 = $this->getRemoteBackupDirectory();
        if (isEmptyOrNull($dstDir1)) {
            $dstDir1 = '';
        } else {
            $dstDir1 .= '/';
        }
        $dstDir = normalizeFileNamePath($dstDir1 . getInstanceCodeName() . '/' . $backupType);

        $rdiffDst = $this->getRemoteUser() . '@' . $this->getRemoteBackupServer() . '::' . $dstDir;
        $cmd = "rdiff-backup $deleteCommand $rdiffDst";

        $output = array();
        $exitStatus = 0;

        try {
          exec($cmd, $output, $exitStatus);

        if ($exitStatus != 0) {
            throw(ArProblemException::createWithGarbageCollection(
                ArProblemType::TYPE_ERROR,
                ArProblemDomain::CONFIGURATIONS,
                null,
                $cmd,
                $this->getGarbageKey(),
                null,
                null,
                "Error executing backup command \"$cmd\": \n" . implode("\n", $output),
                "Data Backup is not executed.",
                "If the error persist check the backup server."
            ));
        }
        } catch(ArProblemException $e) {
            // already signaled
        } catch (Exception $e) {
             throw(ArProblemException::createWithGarbageCollection(
                ArProblemType::TYPE_ERROR,
                ArProblemDomain::CONFIGURATIONS,
                null,
                $cmd,
                $this->getGarbageKey(),
                null,
                null,
                "Error executing backup command \"$cmd\": \n" . $e->getMessage(),
                "Data Backup is not executed.",
                "If the error persist check the backup server."
            ));
        }
    }


    protected function waitXMinutes()
    {
        return $this->getHoursExecutionTimeFrame() * 60;
    }

    public function process()
    {
        $timeFrameInMinutes = $this->waitXMinutes();

        if ($timeFrameInMinutes == 0) {
            $executeNow = true;
        } else {
            $checkFile = get_class($this);
            $checkLimit = strtotime("-$timeFrameInMinutes minutes");
            $mutex = new Mutex($checkFile);
            $executeNow = $mutex->maybeTouch($checkLimit);
        }

        if ($executeNow) {
            $prof = new JobProfiler('backup directories');
            ArProblemException::garbageCollect($this->getGarbageKey(), null, null);

            $prof->incrementProcessedUnits();

            $removeDir = array(false, true);
            foreach($removeDir as $removeDir1) {

                $prof->incrementProcessedUnits();
                $this->executeRDiffBackupCommand('data', DailyBackupJob::getCompleteBackupDirectory(), $removeDir1);

                $prof->incrementProcessedUnits();
                $this->executeRDiffBackupCommand('etc', '/etc', $removeDir1);

                $prof->incrementProcessedUnits();
                $webUploadsDir = normalizeFileNamePath(getAsterisellCompleteAdminDirectory() . '/web/uploads');
                $this->executeRDiffBackupCommand('uploads', $webUploadsDir, $removeDir1);
            }

            return $prof->stop();
        } else {
            return "It will be executed later.";
        }
    }
}
