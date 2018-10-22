<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));


/**
 * Delete files inside `ar_local_file_to_delete`.
 * Doing so:
 * - files will be not processed more than one time
 * - files will be deleted only if the processing transaction will commit
 */
class DeleteProcessedFiles extends FixedJobProcessor
{

    const GARBAGE_KEY = 'DeleteProcessedFiles';

    public function process()
    {
        $conn = Propel::getConnection();
        $prof = new JobProfiler('Delete already processed files');

        $allOk = true;
        $notDeletedFiles = '';
        $query = 'SELECT name FROM ar_local_file_to_delete ORDER BY id';
        $stmt = $conn->prepare($query);
        $stmt->execute();
        while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
            $completeSourceFile = $rs[0];
            $isOk = unlink($completeSourceFile);
            if (!$isOk) {
                $allOk = false;
                $notDeletedFiles .= $completeSourceFile . ', ';
            }
            $prof->incrementProcessedUnits();
        }
        $stmt->closeCursor();

        $query = 'TRUNCATE ar_local_file_to_delete';
        $conn->exec($query);

        if (! $allOk) {
            ArProblemException::createWithoutGarbageCollection(
                ArProblemType::TYPE_CRITICAL
                , ArProblemDomain::CONFIGURATIONS
                , null
                , get_class($this) . ' - ' . md5($notDeletedFiles)
                , "The job " . get_class($this) . " was unable to delete these already processed files $notDeletedFiles"
                , "The content of these files will be processed more than one time, and so there can be duplicated data inside Asterisell database."
                , "Check the rights of directories and users. The Asterisell user is \"apache\"");
        }

        return $prof->stop();
    }


}
