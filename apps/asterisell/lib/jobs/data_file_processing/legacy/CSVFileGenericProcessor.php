<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Read data from CSV Files inside a directory,
 * process them, and move them to a processed diretory queue.
 *
 * This is an abstract/generic class used from other Job
 * to implement useful features.
 *
 * It guarantee that files are processed in alphabetically order.
 * This can be usefull in some circumstances, for example for applying
 * updates that can be overlap.
 *
 * TODO consider to substitute with CSVFileQueueProcessor
 */
abstract class CSVFileGenericProcessor extends FixedJobProcessor
{

    /**
     * Absolute directory where read CSV files.
     * Subdirectory are not processed.
     *
     * @return string
     */
    abstract public function get_csv_input_directory();

    /**
     * Absolute directory were move/put processing CSV files.
     * This processing directory is used in order to prevent
     * the re-execution of jobs on already processed files
     * interrupted for some reason.
     *
     * @return string
     */
    abstract public function get_csv_processing_directory();

    /**
     * Absolute directory were move/put processed CSV files.
     *
     * @return string
     */
    abstract public function get_csv_processed_directory();

    /**
     * @param string $fileName a file to process
     * @return bool TRUE if the file name can be accepted and processed,
     *         FALSE if it must be ignored.
     */
    abstract public function accept_file_name($fileName);

    /**
     * @return bool TRUE if duplicated lines in imported CSV files are recognized,
     *           and they are imported again according the new code, but without duplication.
     *           FALSE otherwise.
     */
    public function recognizeDuplicatedCSVLines()
    {
        return FALSE;
    }

    /**
     * @static
     * @param int $fileTime
     * @return string the directory associated to the file timestamp, something like "y2012/m03"
     */
    static public function fileProcessedDirectoryAccordingDate($fileTime)
    {
        return 'y' . date('Y', $fileTime) . '/' . 'm' . date('m', $fileTime);
    }

    /**
     * @return string
     */
    protected function getDuplicatedCSVLinesMesage()
    {
        if ($this->recognizeDuplicatedCSVLines()) {
            return "NOTE: CSV lines already inserted in CDR table, will not be duplicated, but simply replaced with a new CDR version, according changes in the code. ";
        } else {
            return "If the file was partially processed, remove from it already processed parts, in order to avoid duplication of imported CDRs.";
        }
    }

    /**
     * Delete (if exists) a CDR with the same $sourceId.
     * @param string $sourceId
     * @return void
     */
    protected function deleteDuplicatedCDR($sourceId)
    {
        $connection = Propel::getConnection();

        $query = 'DELETE FROM ar_source_cdr WHERE source_id = ?';
        $stm = $connection->prepare($query);
        $stm->execute(array($sourceId));
        // NOTE: the corresponding ar_cdr will be deleted on cascade.
    }

    /**
     * @param string $sourceId
     * @return bool
     */
    protected function thereIsDuplicateCDR($sourceId)
    {
        $connection = Propel::getConnection();
        $query = 'SELECT count(id) FROM ar_source_cdr WHERE source_id = ?';
        $stm = $connection->prepare($query);
        $stm->execute(array($sourceId));
        $count = intval($stm->fetchColumn());
        $stm->closeCursor();

        if ($count > 0) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * @return int the max number of files to load for each job exeution.
     */
    public function max_nr_of_files_per_session()
    {
        return 500;
    }

    /**
     * Use a MD5 based approach for calculating and setting a unique sourceId.
     *
     * @param ArSourceCdr $cdr common call related fields must be setted
     * @param string $prefix a (short) prefix to add the calculated MD5
     * @return  void
     * @pre $cdr common call related fields must be setted
     */
    protected function calcAndUpdateSourceId($cdr, $prefix)
    {
        $sourceId = fromMySQLTimestampToUnixTimestamp($cdr->getCalldate()) . '-' . md5($cdr->getSourceData());
        $cdr->setSourceId($sourceId);
    }

    /**
     * Process a file.
     *
     * @param string $fileName
     * @param resource $handle a correctly open file to process.
     * @return mixed[] list(TRUE, $logMessage) if the job was successfull,
     *                 list(FALSE, $errorMessage) otherwise.
     * @post $handle must be closed at the end.
     */
    abstract public function process_file($fileName, $handle);

    protected $logMessage;

    /**
     * @param string $msg
     */
    protected function addLogMessage($msg)
    {
        $this->logMessage .= "\n" . $msg;
    }

    protected $thereWereError;

    /**
     * @return mixed
     * @throws ArProblemException
     */
    public function process()
    {
        $this->thereWereError = FALSE;
        $this->logMessage = "";

        $this->maybeCreateDirectory($this->get_csv_input_directory());
        $this->maybeCreateDirectory($this->get_csv_processing_directory());
        $this->maybeCreateDirectory($this->get_csv_processed_directory());

        $dirHandle = @opendir($this->get_csv_input_directory());

        if ($dirHandle === FALSE) {
            $problemDuplicationKey = "CSVFileGenericProcessor open directory " . $this->get_csv_input_directory();
            $problemDescription = "Error during opening of directory \"" . $this->get_csv_input_directory() . "\" containing the files to process, in job " . get_class($this);

            $problemEffect = "The files inside the directory are not processed.";
            $problemProposedSolution = "Fix the problem in the directory. Probably there are file access rights problems. These files will be processed automatically at the next iteration of job processor.";

            $p = ArProblemException::createWithoutGarbageCollection(
                ArProblemType::TYPE_ERROR,
                ArProblemDomain::APPLICATION,
                null,
                $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
            throw ($p);
        }

        // Retrieve all the files inside the directory,
        // and order them alphabetically.
        //
        $files = array();
        $count = 0;
        while ((false !== ($filename = readdir($dirHandle))) && $count < $this->max_nr_of_files_per_session()) {
            if ($this->accept_file_name($filename)) {
                $files[] = $filename;
                $count++;
            }
        }
        sort($files);

        // Process every file
        //
        $atLeastOneProcessedFile = FALSE;
        $countFiles = 0;
        foreach ($files as $file) {
            $completeSourceFileName = $this->get_csv_input_directory() . "/" . $file;
            $completeProcessingFileName = $this->get_csv_processing_directory() . "/" . $file;

            if (is_dir($completeSourceFileName)) {
                // skip directories

                continue;
            }

            $countFiles++;
            $atLeastOneProcessedFile = TRUE;

            $status = $this->myMove($completeSourceFileName, $completeProcessingFileName);
            if ($status === FALSE) {
                $this->thereWereError = TRUE;
                $this->addLogMessage("Error signaled on the problem table during processing of $completeSourceFileName.");


                $problemDuplicationKey = "CSVFileGenericProcessor moving " . $completeSourceFileName;
                $problemDescription = "Error during moving " . $completeSourceFileName . " to " . $completeProcessingFileName . ' in job ' . get_class($this);

                $problemEffect = "The file is not processed and it is skipped. Other input files are correctly processed (this error is not a blocking error).";
                $problemProposedSolution = "Fix the problem in the file, probably there are file access rights problems. The file will be processed automatically at the next iteration of job processor.";
                ArProblemException::createWithoutGarbageCollection(
                    ArProblemType::TYPE_ERROR,
                    ArProblemDomain::APPLICATION,
                    null,
                    $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);

                continue;
            }

            $this->addLogMessage("Input file \"$file\" was moved from input directory \"" . $this->get_csv_input_directory() . "\" to processing directory \"" . $this->get_csv_processing_directory() . "\"");

            $fileHandle = fopen($completeProcessingFileName, "r");
            if ($fileHandle === FALSE) {
                $this->thereWereError = TRUE;
                $this->addLogMessage("Error signaled on the problem table.");


                $problemDuplicationKey = "CSVFileGenericProcessor opening " . $completeProcessingFileName . time();
                $problemDescription = "File \"" . $completeProcessingFileName . "\" can not be open.";

                $problemEffect = "The file is not processed and it is skipped. Other input files are correctly processed (this error is not a blocking error).";
                $problemProposedSolution = "Fix the problem in the file and move it agains in input directory \"" . $this->get_csv_input_directory() . "\". The file will be processed automatically at the next iteration of job processor.";
                // NOTE: set Mantain to TRUE because CSV file errors are lost if error-table is deleted
                ArProblemException::createWithoutGarbageCollection(
                    ArProblemType::TYPE_ERROR,
                    ArProblemDomain::APPLICATION,
                    null,
                    $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution, true);

                continue;
            }

            try {
                list($jobIsOk, $jobMessage) = $this->process_file($file, $fileHandle);

                try {
                    @fclose($fileHandle);
                } catch (Exception $e) {
                    // does nothing, some time other jobs close the file themself.
                }

                if ($jobIsOk === FALSE) {
                    $this->thereWereError = TRUE;
                    $this->addLogMessage("Error signaled on the problem table.");


                    $problemDuplicationKey = "CSVFileGenericProcessor processing " . $completeProcessingFileName . time();
                    $problemDescription = "Error during processing of " . $completeProcessingFileName . ": " . $jobMessage;

                    $problemEffect = "The file is not processed and it is skipped. Other files are correctly processed.";
                    $problemProposedSolution = "Fix the problem in the file. Delete this problem from the table. Move the file again in input directory \"" . $this->get_csv_input_directory() . "\". The file will be processed automatically at the next iteration of job processor. " . $this->getDuplicatedCSVLinesMesage();
                    // NOTE: set Mantain to TRUE because CSV file errors are lost if error-table is deleted
                    ArProblemException::createWithoutGarbageCollection(
                        ArProblemType::TYPE_ERROR,
                        ArProblemDomain::APPLICATION,
                        null,
                        $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution, true);

                    continue;
                }

                $this->addLogMessage($jobMessage);

            } catch (Exception $e) {
                $this->thereWereError = TRUE;
                $this->addLogMessage("Error signaled on the problem table.");


                $problemDuplicationKey = "CSVFileGenericProcessor processing " . $completeProcessingFileName . time();
                $problemDescription = "Error during processing of " . $completeProcessingFileName . ": " . $e->getMessage();

                $problemEffect = "The file is not processed and it is skipped. Other files are correctly processed.";
                $problemProposedSolution = "Fix the problem in the file. Delete this problem from the table. Move the file again in input directory \"" . $this->get_csv_input_directory() . "\". The file will be processed automatically at the next iteration of job processor. " . $this->getDuplicatedCSVLinesMesage();
                // NOTE: set Mantain to TRUE because CSV file errors are lost if error-table is deleted
                ArProblemException::createWithoutGarbageCollection(
                    ArProblemType::TYPE_ERROR,
                    ArProblemDomain::APPLICATION,
                    null,
                    $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution, true);

                continue;
            }

            $fileTime = filemtime($completeProcessingFileName);
            $completeProcessedFilePath = $this->get_csv_processed_directory() . "/" . self::fileProcessedDirectoryAccordingDate($fileTime);
            @mkdir($completeProcessedFilePath, 0777, true);
            $completeProcessedFileName = $completeProcessedFilePath . '/' . $file;
            $status = $this->myMove($completeProcessingFileName, $completeProcessedFileName);
            if ($status === FALSE) {
                $this->thereWereError = TRUE;
                $this->addLogMessage("Error signaled on the problem table.");


                $problemDuplicationKey = "CSVFileGenericProcessor moving " . $completeProcessedFileName;
                $problemDescription = "Error during moving of " . $completeProcessingFileName . " to " . $completeProcessedFileName;

                $problemEffect = "The file was correctly processed but it can not be moved to processed directory \"" . $this->get_csv_processed_directory() . "\"";
                $problemProposedSolution = "Move manually the file from \"" . $this->get_csv_processing_directory() . "\" to \"" . $this->get_csv_processed_directory() . "\"";
                // NOTE: set Mantain to TRUE because CSV file errors are lost if error-table is deleted
                ArProblemException::createWithoutGarbageCollection(
                    ArProblemType::TYPE_ERROR,
                    ArProblemDomain::APPLICATION,
                    null,
                    $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution, true);

                continue;
            }
        }
        closedir($dirHandle);

        if ($atLeastOneProcessedFile === FALSE) {
            $this->addLogMessage("There were no files to process inside \"" . $this->get_csv_input_directory() . "\"");
        }

        return $this->logMessage;
    }

    protected function myMove($file1, $file2)
    {
        if (@file_exists($file2)) {
            @unlink($file2);
        }
        $r = @rename($file1, $file2);
        return $r;
    }

    /**
     * @param string $directory
     * @throws ArProblemException
     */
    protected function maybeCreateDirectory($directory)
    {
        if (!file_exists($directory)) {
            $r = @mkdir($directory, 0755, true);
            if ($r === FALSE) {
                $problemDuplicationKey = get_class($this) . " create directory " . $directory;
                $problemDescription = "Error creating directory \"$directory\", in job " . get_class($this);
                $problemEffect = "The files inside the directory are not processed.";
                $problemProposedSolution = "Fix the problem in the directory. Probably there are file access rights problems. These files will be processed automatically at the next iteration of job processor.";
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