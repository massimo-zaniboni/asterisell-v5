<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Import CSV CDRS files from a local directory into input directory.
 * Contrary to other methods, the file names are not stored and considered uniques.
 * Files are always removed from `getRemoteDirectory()` and put into input directory.
 * This is an abstract class, that must be customized in inherited subclasses, for performing the real work.
 * Obviously the job must be added to the `import_cdrs_job` parameter.
 */
abstract class ImportCSVFilesFromLocalDirectory extends ImportCSVFilesFromRemoteServer
{

    public function getConnectionName() {
        // it is not needed for connecting local directories
        return 'not_used';
    }

    public function checkNewFilesEveryMinutes()
    {
        // local files are fast to check
        return 0;
    }

    public function processFile($completeFileName)
    {
        return true;
    }

    protected function isNewFile($fileName)
    {
        return true;
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
                    // NOTE: call first this function because the provider can change
                    $fileName = $this->canAcceptFileName($sourceFile);
                    if ($this->isNewfile($remoteFileName)) {
                        if ($fileName === false) {
                            throw $this->createProblem(
                                "not valid filename $remoteFileName"
                                , "The file \"$remoteFileName\" has an invalid name."
                                , "If the error persist, contact the assistance."
                                , $remoteFileName
                            );
                        } else if ($fileName === true) {
                            // can skip the file
                        } else if (is_string($fileName)) {

                            $tmpResultFileName = normalizeFileNamePath(ImportDataFiles::getAbsoluteTmpDirectory() . '/' . $fileName);
                            @unlink($tmpResultFileName);

                            if ($this->normalizeFileContent($remoteFileName, $remoteFileName, $tmpResultFileName)) {
                                // nothing to do: the file was written
                            } else {
                                // move the file in a local directory, so subsequent move operation is atomic
                                $isOk = rename($remoteFileName, $tmpResultFileName);

                                if (!$isOk) {
                                    throw $this->createProblem(
                                        "not processable file $remoteFileName"
                                        , "Can not process file \"$tmpResultFileName\", and write normalized content into file \"$tmpResultFileName\"."
                                        , "If the error persist, contact the assistance."
                                        , $remoteFileName
                                    );
                                }
                            }

                            $this->maybeRecode($this->getSourceCharacterEncoding(), $tmpResultFileName, get_class($this));
                            $this->maybeArchiveFile($sourceFile, $tmpResultFileName);
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
                        } else {
                            throw $this->createProblem(
                                "error in code on filenname $remoteFileName"
                                , "The remote file \"$remoteFileName\" can not be processed."
                                , "This is an error in application code. Contact the assistance."
                                , $remoteFileName
                            );
                        }
                    }
                } catch
                (ArProblemException $e) {
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

            return '';

        } else {
            return 'Job will be executed later.';
        }
    }

}
