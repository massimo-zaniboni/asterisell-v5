<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Import CDRs from a CSV file produced from a Asterisell provider,
 * residing on the same local machine. So files are transferred using shared directories.
 */
abstract class ImportCDRSFromLocalAsterisellProvider extends ImportCDRSFromAsterisellProvider
{

    //
    // Customizable Interface
    //

    /**
     * @return string the directory where there are status files to import.
     */
    function getInputDirectory()
    {
        return '/var/opt/asterisell/' . $this->getCDRProviderName() . '/' . getInstanceCodeName();
    }

    protected function endingProcessPhase()
    {
        // At the end of file processing, delete the file, so new files can be imported.
        $n = normalizeFileNamePath($this->getInputDirectory() . '/' . ImportDataFiles::FILES_TO_EXPORT_LIST);
        if (file_exists($n)) {
            $isOk = unlink($n);
            if ($isOk === FALSE) {
                $p = ArProblemException::createWithGarbageCollection(
                    ArProblemType::TYPE_CRITICAL,
                    ArProblemDomain::CONFIGURATIONS,
                    null,
                    "can not delete file - " . $n,
                    $this->getGarbageKey(),
                    null,
                    null,
                    'Asterisell can not delete the file \"$n\".',
                    'New CDRs will be not imported from the Asterisell provider.',
                    'Check directory write permissions.',
                    null);
                throw($p);
            }
        }

        return '';
    }

    //
    // Implemented Methods
    //

    function getSortedListOfFilesToProcess()
    {
        $files = array();
        $dir = $this->getInputDirectory();

        $dh = opendir($dir);
        if ($dh === FALSE) {
            $p = ArProblemException::createWithGarbageCollection(
                ArProblemType::TYPE_CRITICAL,
                ArProblemDomain::CONFIGURATIONS,
                null,
                "missing directory - " . $this->getInputDirectory(),
                $this->getGarbageKey(),
                null,
                null,
                'The input directory  "' . $this->getInputDirectory() . '" is not accessible. ',
                'CDRs will be not imported from Asterisell provider.',
                'Check if the directory exists, and it has the right permissions.',
                null);
            throw($p);
        }

        while (false !== ($filename = readdir($dh))) {
            if ($filename == '.' || $filename === '..' || $filename === ImportDataFiles::IS_ASTERISELL_PROVIDER_DIRECTORY_CHECK) {
                continue;
            }

            $files[] = normalizeFileNamePath($dir . '/' . $filename);
        }

        sort($files);
        return $files;
    }
}
