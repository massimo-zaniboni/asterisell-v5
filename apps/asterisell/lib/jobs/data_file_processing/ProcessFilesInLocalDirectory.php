<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Process files inside a `getRemoteDirectory()` that must be local respect the Asterisell file system,
 * and remove them only if correctly processed.
 * Contrary to other methods, the file names are not stored and considered uniques.
 *
 * The `processFile()` method is called for each file,
 * and the source file is archived if it returns true, deleted if it returns false,
 * and left in directory if an exception is raised.
 *
 * `canAcceptFileName()` must return a string, for indicating that the file can be processed,
 * but the return value is ignored.
 */
abstract class ProcessFilesInLocalDirectory extends ImportCSVFilesFromRemoteServer
{


    public function checkNewFilesEveryMinutes()
    {
        // local files are fast to check
        return 0;
    }


    protected function signalFileAsProcessed($fileName)
    {
    }

    public function process()
    {

        // check if the job can be started
        $timeFrameInMinutes = $this->checkNewFilesEveryMinutes();
        $checkFile = get_class($this);
        $checkLimit = strtotime("-$timeFrameInMinutes minutes");
        $mutex = new Mutex($checkFile);

        if ($mutex->maybeTouch($checkLimit)) {

            $prof = new JobProfiler("files");
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
                    $fileName = $this->canAcceptFileName($sourceFile);
                    if ($fileName === false) {
                        throw $this->createProblem(
                            "not valid filename $remoteFileName"
                            , "The file \"$remoteFileName\" has an invalid name."
                            , "If the error persist, contact the assistance."
                            , $remoteFileName
                        );
                    } else if ($fileName === true) {
                        // can skip the file, but do not delete because it can be processed from other jobs
                    } else if (is_string($fileName)) {
                        $prof->incrementProcessedUnits();

                        $archive = $this->processFile($remoteFileName);
                        $this->signalFileAsProcessed($remoteFileName);
                        if ($archive) {
                            $isOk = $this->maybeArchiveFile($sourceFile, $remoteFileName, true);
                        } else {
                            $isOk = @unlink($remoteFileName);
                        }

                        if ($isOk === FALSE) {
                            $this->createProblem(
                                "not deletable file $remoteFileName"
                                , "Can not delete file \"$remoteFileName\"."
                                , "It can be a problem of directory permissions."
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

            return $prof->stop();

        } else {
            return 'Job will be executed later.';
        }
    }

    // Usually not to change

    public function getConnectionName()
    {
        // it is not needed for connecting local directories
        return 'not_used';
    }

    protected function isNewFile($fileName)
    {
        return true;
    }

}