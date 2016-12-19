<?php

/* $LICENSE 2009, 2010, 2011, 2012, 2013:
 *
 * Copyright (C) 2009, 2010, 2011, 2012, 2013 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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
 * Process data files, inside a queue.
 *
 * It guarantee that files are processed in alphabetically order.
 * This can be useful in some circumstances, for example for applying
 * updates that can be overlap.
 *
 * ensure: all files in the input queue must be processed from the job.
 */
abstract class FileMessageQueueProcessor extends FixedJobProcessor
{

    //////////////////////////
    // METHODS TO CUSTOMIZE //
    //////////////////////////

    /**
     * @return string the input queue name, where the job consume the messages.
     *
     * NOTE: each specific job, must define the queue, that is a static property of the job.
     */
    abstract public function getInputQueueName();

    /**
     * @return string|null the queue where are put by default processed messages. null for no output queue.
     * NOTE: a job can use more than a queue as output. In any case every job has a default/common output queue,
     * so for convenience it is part of the API.
     */
    abstract public function getOutputQueueName();

    protected function initBeforeProcessing()
    {

    }

    protected function endAfterProcessing()
    {

    }

    /**
     * Process message accessible using $this->getInputQueue()->getCurrentFileHandle()
     *
     * @param PDO $conn the connection to use for acessing the database
     * @return Void
     * @throws ArProblemException
     *
     * ensure: the current message is put in the output queue, or managed in the correct way (deleted and so on).
     * If the message is not moved from processing queue, then an error is signaled to application users.
     *
     * ensure: use $this->addLogMessage($message) for logging the status of the work
     */
    abstract protected function processCurrentMessage(PDO $conn);

    /////////////////
    // LOG SUPPORT //
    /////////////////

    protected $logMessage;

    /**
     * @param string $msg
     */
    protected function addLogMessage($msg)
    {
        $this->logMessage .= "\n" . $msg;
    }

    protected $thereWereError;

    ///////////////////////////
    // DATA ACCESS FUNCTIONS //
    ///////////////////////////

    /**
     * @var FileMessageQueue
     */
    protected $inputQueue = null;


    /**
     * @return FileMessageQueue
     */
    protected function getInputQueue()
    {
        if (is_null($this->inputQueue)) {
            $this->inputQueue = new FileMessageQueue($this->getInputQueueName());
        }

        return $this->inputQueue;
    }

    /**
     * @var FileMessageQueue
     */
    protected $outputQueue = null;

    /**
     * @return FileMessageQueue|null
     */
    protected function getOutputQueue()
    {
        if (is_null($this->outputQueue)) {
            if (!is_null($this->getOutputQueueName())) {
                $this->outputQueue = new FileMessageQueue($this->getOutputQueueName());
            }
        }

        return $this->outputQueue;
    }

    ///////////////////
    // JOB EXECUTION //
    ///////////////////

    /**
     * @return mixed
     * @throws ArProblemException
     */
    public function process()
    {
        $this->getInputQueue()->open();

        $this->thereWereError = FALSE;
        $this->logMessage = "";

        $this->initBeforeProcessing();

        // Process every file in the queue
        $atLeastOneProcessedFile = FALSE;
        $atLeastOneError = FALSE;
        $countFiles = 0;

        while ($this->getInputQueue()->isThereFileToProcess()) {

            $countFiles++;
            $atLeastOneProcessedFile = TRUE;

            $conn = Propel::getConnection();
            $conn->beginTransaction();
            try {
                $this->processCurrentMessage($conn);
                $this->commitTransactionOrSignalProblem($conn);

            } catch (Exception $e) {
                $conn->rollBack();
                $this->thereWereError = TRUE;

                if (!$atLeastOneError) {
                    $this->addLogMessage("Error signaled on the problem table.");
                    $atLeastOneError = true;
                }
                $completeProcessingFileName = $this->getInputQueue()->getCurrentFileName();

                if ($e instanceof ArProblemException) {
                    $errorMessage = ArProblemException::getLastErrorDescription();
                } else {
                    $errorMessage = $e->getMessage();
                }

                $problemDuplicationKey = get_class($this) . ' - processing of ' . $completeProcessingFileName . ' - ' . time();
                $problemDescription = get_class($this) . ": error during processing of " . $completeProcessingFileName . ": " . $errorMessage;
                $problemEffect = "The file is not processed and it is skipped. Other files can be correctly processed.";
                $problemProposedSolution = "Fix the problem in the file. Delete this problem from the table. Resubmit the file again. ";
                ArProblemException::createWithoutGarbageCollection(
                    ArProblemType::TYPE_ERROR,
                    ArProblemDomain::APPLICATION,
                    null,
                    $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);

                continue;
            }

            $this->getInputQueue()->next();
        }

        $this->endAfterProcessing();

        if ($atLeastOneProcessedFile === FALSE) {
            $this->addLogMessage("There were no files to process inside \"" . $this->getInputQueue()->getQueueProcessingDir() . "\"");
        }

        $this->inputQueue->close();
        return $this->logMessage;
    }
}
