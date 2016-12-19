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
