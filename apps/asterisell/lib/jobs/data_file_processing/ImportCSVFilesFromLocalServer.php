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
 * Import CSV files from a local server directory, using a logic similar to FTP server.
 * This is an abstract class, that must be customized in inherited subclasses, for performing the real work.
 * Obviously the job must be added to the `import_cdrs_job` parameter.
 */
abstract class ImportCSVFilesFromLocalServer extends ImportCSVFilesFromRemoteServer
{

    //
    // Not used params.
    //

    public function getConnectionName() {
        return null;
    }

    /**
     * @return int minutes of check frequency
     */
    public function checkNewFilesEveryMinutes() {
        return 5;
        // NOTE: this is a local server, so frequency can be high
    }

    //
    // Utils Functions
    //

    /**
     * @param string $key
     * @param string $problem
     * @param string|null $solution
     * @param string|null $remoteFileName null for an error for all CDRs, the file name otherwise
     * @return ArProblemException
     */
    protected function createProblem($key, $problem, $solution, $remoteFileName = null)
    {

        $problemDuplicationKey = $key . '-' . get_class($this);

        if (is_null($remoteFileName)) {
            $problemDescription = "The CDR importing procedure \"" . get_class($this) . "\", has this problem: " . $problem;
            $problemEffect = "CDRs on the directory " . $this->getRemoteDirectory() . " will not be processed.";
            $problemProposedSolution = $solution;
        } else {
            $problemDescription = "The CDR importing procedure \"" . get_class($this) . "\", can not process the remote file \"" . $remoteFileName . "\". " . $problem;
            $problemEffect = "This file will be not processed. ";
            $problemProposedSolution = $solution;
        }

        if (isEmptyOrNull($problemProposedSolution)) {
            $problemProposedSolution = "If the problem persist check the connection parameters.";
        }

        $p = ArProblemException::createWithGarbageCollection(
            ArProblemType::TYPE_CRITICAL,
            ArProblemDomain::CONFIGURATIONS,
            null,
            $problemDuplicationKey,
            get_class($this),
            $this->getGarbageFromDate(),
            $this->getGarbageToDate(),
            $problemDescription,
            $problemEffect,
            $problemProposedSolution);
        return $p;
    }

    const TEMP_FILE_NAME = 'temp_processing.tmp';

    public function process()
    {

        // check if the job can be started
        $timeFrameInMinutes = $this->checkNewFilesEveryMinutes();
        $checkFile = get_class($this);
        $checkLimit = strtotime("-$timeFrameInMinutes minutes");
        $mutex = new Mutex($checkFile);

        if ($mutex->maybeTouch($checkLimit)) {
            $profiler = new JobProfiler("imported CSV files");
            $log = '';

            ArProblemException::garbageCollect(get_class($this), $this->getGarbageFromDate(), $this->getGarbageToDate());

            $sourceFiles = scandir($this->getRemoteDirectory());
            if ($sourceFiles === FALSE) {
                $this->createProblem(
                    '1 - ' . $this->getRemoteDirectory(),
                    "Unable to access the data files directory \"" . $this->getRemoteDirectory() . "\".",
                    "Data files will be not imported.",
                    "Contact the assistance."
                );
            }

            sort($sourceFiles);
            foreach ($sourceFiles as $sourceFile) {
                if ($sourceFile === '.'
                    || $sourceFile === '..'
                ) {
                    continue;
                }

                $remoteFileName = normalizeFileNamePath($this->getRemoteDirectory() . '/' . $sourceFile);
                try {
                    // NOTE: call first this function, because the provider can change
                    $fileName = $this->canAcceptFileName($sourceFile);
                    if ($this->isNewfile($remoteFileName)) {

                        if ($fileName === false) {
                            throw $this->createProblem(
                                "not valid filename $remoteFileName"
                                , "The remote file \"$remoteFileName\" has an invalid name."
                                , "If the error persist, contact the assistance."
                                , $remoteFileName
                            );

                        } else if ($fileName === true) {
                            // can skip the file
                            $log .= "skip file $remoteFileName, ";
                        } else if (is_string($fileName)) {

                            $log .= "import $remoteFileName renaming into $fileName, ";

                            $tmpResultFileName = normalizeFileNamePath(ImportDataFiles::getAbsoluteTmpDirectory() . '/' . $fileName);
                            $tmpResultFileName2 = normalizeFileNamePath(ImportDataFiles::getAbsoluteTmpDirectory() . '/' . self::TEMP_FILE_NAME);

                            @unlink($tmpResultFileName);
                            @unlink($tmpResultFileName2);

                            $isOk = copy($remoteFileName, $tmpResultFileName);
                            if ($isOk === FALSE) {
                                throw $this->createProblem(
                                    "not processable file $remoteFileName"
                                    , "Can not copy file \"$remoteFileName\", into local file \"$tmpResultFileName\"."
                                    , "If the error persist, contact the assistance."
                                    , $remoteFileName
                                );
                            }

                            if ($this->normalizeFileContent($remoteFileName, $tmpResultFileName, $tmpResultFileName2)) {
                                // the $tmpResultFileName was replaced with a normalized version
                                @unlink($tmpResultFileName);
                                $isOk = rename($tmpResultFileName2, $tmpResultFileName);

                                if (!$isOk) {
                                    throw $this->createProblem(
                                        "not processable file $remoteFileName"
                                        , "Can not process file \"$tmpResultFileName2\", and write normalized content into file \"$tmpResultFileName\"."
                                        , "If the error persist, contact the assistance."
                                        , $remoteFileName
                                    );
                                }
                            }

                            if ($this->getSourceCharacterEncoding() !== 'UTF8') {
                                $cmd = 'recode ' . $this->getSourceCharacterEncoding() . '..UTF8 ' . $tmpResultFileName;
                                $isOk = system($cmd);
                                if ($isOk === FALSE) {
                                        throw $this->createProblem(
                                              "not character encoding for file $remoteFileName"
                                            , "Can not convert the character encoding of file \"$tmpResultFileName\", using the command \"$cmd\"."
                                            , "Install the recode utiliy with command \"yum install recode\". If the error persist, contact the assistance."
                                            , $remoteFileName
                                        );
                                }
                            }

                            $archive = $this->processFile($tmpResultFileName);
                            if ($archive) {

                                $dstResultFileName = normalizeFileNamePath(ImportDataFiles::getAbsoluteInputDirectory() . '/' . $fileName);
                                @unlink($dstResultFileName);
                                $isOk = rename($tmpResultFileName, $dstResultFileName);
                                if ($isOk === FALSE) {
                                    throw $this->createProblem(
                                        "not processable file $remoteFileName"
                                        , "Can not move file \"$tmpResultFileName\", into \"$dstResultFileName\"."
                                        , "If the error persist, contact the assistance."
                                        , $remoteFileName
                                    );
                                }
                            } else {
                                $isOk = @unlink($tmpResultFileName);
                                if ($isOk === FALSE) {
                                    $this->createProblem(
                                        "not deletable file $remoteFileName"
                                        , "Can not delete file \"$tmpResultFileName\"."
                                        , "It can be a problem of directory permissions."
                                        , $remoteFileName
                                    );
                                }
                            }

                            $this->signalFileAsProcessed($remoteFileName);
                            $profiler->incrementProcessedUnits();
                        } else {
                            throw $this->createProblem(
                                "error in code on filenname $remoteFileName"
                                , "The file \"$remoteFileName\" can not be processed."
                                , "This is an error in application code. Contact the assistance."
                                , $remoteFileName
                            );
                        }
                    }
                } catch (ArProblemException $e) {
                    // nothing to do: problem already signaled, process next file

                } catch (Exception $e) {
                    $this->createProblem(
                        "generic exception " . $e->getCode()
                        , "Excetption: " . $e->getMessage()
                        , "If the error persist, contact the assistance."
                        , $remoteFileName
                    );
                }
            }

            return $profiler->stop() . "\n" . $log;

        } else {
            return 'Job will be executed later.';
        }
    }

}
