<?php

/* $LICENSE 2013:
 *
 * Copyright (C) 2013 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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
 * Store message in files inside directories.
 *
 * Return messages in alphabetical order.
 *
 * Uses fast atomic move operation of the operating system for moving messages
 * between queues and state.
 *
 * TODO review alla ricerca di errori dato che e` un bel po' di codice
 * TODO usato in LEGACY, probabilmente si puo` cancellare
 */
class FileMessageQueue
{

    //////////
    // INIT //
    //////////

    const INPUT_DIR_NAME = 'input';

    const PROCESSING_DIR_NAME = 'processing';

    const META_INFO_DIR_NAME = 'meta_info';

    /**
     * @var string
     */
    protected $queueName;

    protected $queueAbsolutePath;

    /**
     * @param string $queueName it must be a valid directory name, without spaces inside
     */
    function __construct($queueName)
    {
        $this->queueName = $queueName;

        $this->queueAbsolutePath = getAsterisellCompleteRootDirectory() . '/' . 'data_files/' . $this->queueName;

        $this->maybeCreateDirectory($this->getQueueInputDir());
        $this->maybeCreateDirectory($this->getQueueProcessingDir());
        $this->maybeCreateDirectory($this->getQueueMetaInfoDir());
    }

    /**
     * @return string the directory where there are messages to process
     */
    public function getQueueInputDir()
    {
        return $this->queueAbsolutePath . '/' . self::INPUT_DIR_NAME;
    }

    /**
     * @return string the directory where there are messages in processing state, or with problem in processing
     */
    public function getQueueProcessingDir()
    {
        return $this->queueAbsolutePath . '/' . self::PROCESSING_DIR_NAME;
    }

    public function getQueueMetaInfoDir()
    {
        return $this->queueAbsolutePath . '/' . self::META_INFO_DIR_NAME;
    }

    ////////////////////////
    // POPULATE THE QUEUE //
    ////////////////////////

    /**
     * @param string $fullFilePath the complete file name
     * @param string|null $newName null for using the same file name
     */
    public function moveFileToQueue($fullFilePath, $newName)
    {
        $this->addFileToQueue($fullFilePath, $newName, true);
    }

    /**
     * @param string $fullFilePath the complete file name
     * @param string|null $newName null for using the same file name
     */
    public function copyFileToQueue($fullFilePath, $newName)
    {
        $this->addFileToQueue($fullFilePath, $newName, false);
    }

    /**
     * @param string $fullFilePath the complete file name
     * @param string|null $newName null for using the same file name
     * @param bool $isMove
     * @throws ArProblemException
     */
    protected function addFileToQueue($fullFilePath, $newName, $isMove)
    {

        $destName = $newName;

        if (is_null($destName)) {
            $destName = basename($fullFilePath);
        }

        $this->myMove($fullFilePath, $this->getQueueInputDir() . '/' . $destName, $isMove);
    }

    //////////////////////
    // ACCESS THE QUEUE //
    //////////////////////

    /**
     * @var array fileName => true
     */
    protected $filesToProcess;

    const MANIFEST_PREVIOUS_FILES_TO_PROCESS = 'MANIFEST_PREVIOUS_FILES_TO_PROCESS.php.serialization';

    /**
     * @return Void
     * @throws ArProblemException
     */
    public function open()
    {

        $dirHandle = @opendir($this->getQueueInputDir());

        if ($dirHandle === FALSE) {
            $problemDuplicationKey = get_class($this) . " - " . $this->getQueueInputDir();
            $problemDescription = "Error during opening of directory \"" . $this->getQueueInputDir() . "\" containing the files to process.";
            $problemEffect = "Information in the directory is not processed from Asterisell.";
            $problemProposedSolution = "Fix the problem in the directory. Probably there are file access rights problems. These files will be processed automatically at the next iteration of job processor.";

            $p = ArProblemException::createWithoutGarbageCollection(
                ArProblemType::TYPE_ERROR,
                ArProblemDomain::APPLICATION,
                null,
                $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
            throw ($p);
        }

        $this->filesToProcess = array();
        $count = 0;
        while ((false !== ($filename = readdir($dirHandle)))) {
            if ($filename !== '.' && $filename !== '..') {
                $this->filesToProcess[$filename] = true;
                $count++;
            }
        }
        closedir($dirHandle);

        // NOTE: enforce alphabetical order
        ksort($this->filesToProcess);

        // See if there are problems in the input queue
        $fileNameForMetaInfo = $this->getQueueMetaInfoDir() . '/' . self::MANIFEST_PREVIOUS_FILES_TO_PROCESS;
        if (file_exists($fileNameForMetaInfo)) {
            $d = file_get_contents($fileNameForMetaInfo);
            if ($d !== null) {
                $previousFiles = unserialize($d);

                $errorMsg = '';
                $countErrors = 0;
                $maxErrors = 10;
                foreach ($previousFiles as $previousFileName => $ignore) {
                    if (isset($this->filesToProcess[$previousFileName])) {
                        $countErrors++;

                        if ($countErrors < $maxErrors) {
                            if ($countErrors > 0) {
                                $errorMsg .= ', ';
                            }
                            $errorMsg .= $previousFileName;
                        }
                    }
                }

                if ($countErrors > 0) {
                    $problemDuplicationKey = get_class($this) . " - errors of files not processed in queue";
                    $problemDescription = 'In the queue ' . $this->getQueueInputDir() . ' there are ' . $countErrors . ' files that were not processed in the previous processing phase. The first ' . $maxErrors . ' are ' . $errorMsg;
                    $problemEffect = "Asterisell is not able to process all the info of this queue.";
                    $problemProposedSolution = "Delete this error message. If the problem appears again, then it is a constant problem of the application. Otherwise it was a transient problem. There should be a more specific error message, in the error log, about the job that halted, and stopped processing the messages. This error message is only an additional safety measure.";
                    ArProblemException::createWithoutGarbageCollection(
                        ArProblemType::TYPE_ERROR,
                        ArProblemDomain::APPLICATION,
                        null,
                        $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
                }
            }
        }

        // Save the new meta-info
        file_put_contents($fileNameForMetaInfo, serialize($this->filesToProcess));

        $this->warnIfThereAreProblemsInProcessingQueue();

        // start from the beginning
        reset($this->filesToProcess);
    }


    /**
     * Close the queue processing.
     *
     * @throws ArProblemException
     */
    public function close()
    {
        $e = $this->warnIfThereAreProblemsInProcessingQueue();
        if (!is_null($e)) {
            throw($e);
        }

        $this->filesToProcess = array();
    }

    /**
     * return ArProblemException|null
     */
    protected function warnIfThereAreProblemsInProcessingQueue()
    {
        $dirHandle = @opendir($this->getQueueProcessingDir());

        if ($dirHandle !== FALSE) {
            $errorMsg = '';
            $countErrors = 0;
            $maxErrors = 10;
            $fileName = '';
            while ((false !== ($filename = readdir($dirHandle)))) {
                if ($filename !== '.' && $filename !== '..') {
                    $countErrors++;

                    if ($countErrors < $maxErrors) {
                        if ($countErrors > 0) {
                            $errorMsg .= ', ';
                        }
                        $errorMsg .= $fileName;
                    }
                }
            }
            closedir($dirHandle);

            if ($countErrors > 0) {
                $problemDuplicationKey = get_class($this) . " - errors of files not processed in queue";
                $problemDescription = 'In the queue ' . $this->getQueueProcessingDir() . ' there are ' . $countErrors . ' files that were not processed in the previous processing phase. The first ' . $maxErrors . ' are ' . $errorMsg;
                $problemEffect = "Asterisell is not able to process all the info of this queue.";
                $problemProposedSolution = "There should be a more specific error message, in the error log, about the job that halted, and stopped processing the messages. This error message is only an additional safety measure.";
                return ArProblemException::createWithoutGarbageCollection(
                    ArProblemType::TYPE_ERROR,
                    ArProblemDomain::APPLICATION,
                    null,
                    $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
            }
        }

        return null;
    }

    /**
     * @param FileMessageQueue $outQueue
     * @param string|null $newName null for using the same file name
     * @throws ArProblemException
     */
    public function moveCurrentFileToQueue(FileMessageQueue $outQueue, $newName)
    {
        $this->maybeCloseFileHandle();

        $outQueue->moveFileToQueue($this->getCurrentFileName(), $newName);
    }

    /**
     * @throws ArProblemException
     */
    public function deleteCurrentFile()
    {
        $this->maybeCloseFileHandle();
        $r = unlink($this->getCurrentFileName());

        if ($r === FALSE) {
            $problemDuplicationKey = get_class($this) . " my delete - " . $this->getCurrentFileName();
            $problemDescription = "Error deleting file \"" . $this->getCurrentFileName() . "\"";
            $problemEffect = "The file can not be processed in the correct way from Asterisell.";
            $problemProposedSolution = "Probably there are file access rights problems.";
            $p = ArProblemException::createWithoutGarbageCollection(
                ArProblemType::TYPE_ERROR,
                ArProblemDomain::APPLICATION,
                null,
                $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
            throw ($p);
        }
    }

    protected $currentFileHandle = null;

    protected $currentFileName = null;

    /**
     * @return string
     * @throws ArProblemException
     */
    public function getCurrentFileName()
    {
        $this->maybeOpenFileHandle();
        return $this->currentFileName;
    }

    /**
     * @return int
     * @throws ArProblemException
     */
    public function getCurrentFileHandle()
    {
        $this->maybeOpenFileHandle();
        return $this->currentFileHandle;
    }

    /**
     * @throws ArProblemException
     */
    protected function maybeOpenFileHandle()
    {

        if (is_null($this->currentFileName)) {

            $this->currentFileName = null;
            $this->currentFileHandle = null;

            $fileName = key($this->filesToProcess);

            $completeSourceFileName = $this->getQueueInputDir() . '/' . $fileName;
            $completeProcessingFileName = $this->getQueueProcessingDir() . "/" . $fileName;

            $this->myMove($completeSourceFileName, $completeProcessingFileName, true);

            $fileHandle = fopen($completeProcessingFileName, "r");
            if ($fileHandle === FALSE) {
                $problemDuplicationKey = get_class($this) . " - error opening " . $completeProcessingFileName . time();

                $problemDescription = "File \"" . $completeProcessingFileName . "\" can not be open.";

                $problemEffect = "The file is not processed.";
                $problemProposedSolution = "This file will be not processed automatically. So it must be put again in the input queue, from assistance. ";

                $p = ArProblemException::createWithoutGarbageCollection(
                    ArProblemType::TYPE_ERROR,
                    ArProblemDomain::APPLICATION,
                    null,
                    $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
                throw($p);
            }

            $this->currentFileHandle = $fileHandle;
            $this->currentFileName = $completeProcessingFileName;

        }
    }

    public function maybeCloseFileHandle()
    {
        $handle = $this->currentFileHandle;

        $this->currentFileHandle = null;

        // close current file
        try {
            if (!is_null($handle)) {
                @fclose($handle);
            }
        } catch (Exception $e) {
            // does nothing, some time other jobs close the file themself.
        }
    }


    /**
     * @throws ArProblemException
     */
    public function leaveCurrentFileAsUnprocessedWithError()
    {
        $this->maybeCloseFileHandle();
    }

    /**
     * @return bool true if there is a file to process
     */
    public function isThereFileToProcess()
    {
        $v = current($this->filesToProcess);
        if ($v === false) {
            return false;
        } else {
            return true;
        }
    }

    /**
     * Advance to the next file to process.
     * @throws ArProblemException
     */
    public function next()
    {
        $this->maybeCloseFileHandle();

        // advance the iterator
        next($this->filesToProcess);
    }

    ////////////////////////
    // UTILITY FUNCTIONS  //
    ////////////////////////

    /**
     * @param string $file1
     * @param string $file2
     * @param bool $isMove false for a copy
     * @throws ArProblemException
     */
    protected function myMove($file1, $file2, $isMove)
    {
        if ($isMove) {

            if (@file_exists($file2)) {
                @unlink($file2);
            }
            $status = @rename($file1, $file2);
        } else {
            $status = copy($file1, $file2);
        }

        if ($status === FALSE) {
            $problemDuplicationKey = get_class($this) . " my move - " . $file1 . ' - ' . $file2;
            $problemDescription = "Error moving or copying \"$file1\" to \"$file2\".";
            $problemEffect = "The file can not be processed in the correct way from Asterisell.";
            $problemProposedSolution = "Probably there are file access rights problems.";
            $p = ArProblemException::createWithoutGarbageCollection(
                ArProblemType::TYPE_ERROR,
                ArProblemDomain::APPLICATION,
                null,
                $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
            throw ($p);
        }
    }

    /**
     * @param string $directory absolute path
     * @return Void
     * @throws ArProblemException
     */
    protected function maybeCreateDirectory($directory)
    {
        if (!file_exists($directory)) {
            $r = @mkdir($directory, 0755, true);
            if ($r === FALSE) {
                $problemDuplicationKey = get_class($this) . " create directory " . $directory;
                $problemDescription = "Error creating directory \"$directory\", in job " . get_class($this);
                $problemEffect = "Asterisell can not create the corresponding message queue, and can not process information.";
                $problemProposedSolution = "Probably there are file access rights problems.";
                $p = ArProblemException::createWithoutGarbageCollection(
                    ArProblemType::TYPE_ERROR,
                    ArProblemDomain::APPLICATION,
                    null,
                    $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
                throw ($p);
            }
        }
    }
}
