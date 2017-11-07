<?php

/* $LICENSE 2017:
 *
 * Copyright (C) 2017 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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
 * Import CSV files from an external FTPS server, with a wrong SSL certificate, or other problems.
 * This job only mirrors/imports files inside a local directory, using Linux `lftp` utility.
 * Then other jobs had to process the content of the directory.
 * The content of the directory could be deleted at every run of this job, so it must be moved to a safer position.
 */
abstract class ImportCSVFilesFromNotPerfectRemoteServer extends FixedJobProcessor
{

    /**
     * @return string the name of the connection params to use, and defined in app.yml under connection settings.
     * The files will be loaded inside directory `/var/tmp/connection-name/remote-directory/`.
     */
    abstract public function getConnectionName();

    /**
     * @return string the remote directory where there are the files to download.
     */
    abstract public function getRemoteDirectory();

    /**
     * @return int minutes of check frequency
     */
    public function checkNewFilesEveryMinutes()
    {
        return 60 * 12;
    }

    /**
     * @param string $key
     * @param string $problem
     * @param string|null $solution
     * @return ArProblemException
     */
    protected function createProblem($key, $problem, $solution)
    {

        $problemDuplicationKey = $key . '-' . get_class($this);

        $problemDescription
            = "The CDR importing procedure \""
            . get_class($this) . "\", associated to the remote server connection params \""
            . $this->getConnectionName()
            . "\", and remote directory \""
            . $this->getRemoteDirectory()
            . "\" can not download CDRs. "
            . $problem;
        $problemEffect = "These CDRS will be not rated.";
        $problemProposedSolution = $solution;

        if (isEmptyOrNull($problemProposedSolution)) {
            $problemProposedSolution = "It can be a temporary problem on the remote host. If the problem persist check the connection parameters.";
        }

        $p = ArProblemException::createWithGarbageCollection(
            ArProblemType::TYPE_CRITICAL,
            ArProblemDomain::CONFIGURATIONS,
            null,
            $problemDuplicationKey,
            get_class($this),
            null,
            null,
            $problemDescription,
            $problemEffect,
            $problemProposedSolution);
        return $p;
    }

    public function process()
    {
        $timeFrameInMinutes = $this->checkNewFilesEveryMinutes();
        $checkFile = get_class($this);
        $checkLimit = strtotime("-$timeFrameInMinutes minutes");
        $mutex = new Mutex($checkFile);
        if ($timeFrameInMinutes > 0 && $mutex->maybeTouch($checkLimit)) {
            ArProblemException::garbageCollect(get_class($this), null, null);
            $prof = new JobProfiler("FTP download");
            $prof->addToProcessedUnits(1);

            $connectionName = $this->getConnectionName();
            $r = getConnectionParams($connectionName);

            if (is_null($r)) {
                throw $this->createProblem(
                    "configurations"
                    , "Unable to find the settings for connection \"$connectionName\""
                    , "Complete configuration settings"
                );
            }

            list($conf_host, $conf_user, $conf_password, $conf_port) = $r;

            $remoteDir = $this->getRemoteDirectory();
            $localDir = normalizeFileNamePath('/var/tmp/' . $this->getConnectionName() . '/' . $this->getRemoteDirectory());
            @mkdir($localDir, true);

            $cmd = "lftp ftps://$conf_user:$conf_password@$conf_host:$conf_port -e \"set ssl:verify-certificate false; mirror $remoteDir $localDir ; quit\"";

            $output = array();
            $exitStatus = 0;
            exec($cmd, $output, $exitStatus);

            if ($exitStatus != 0) {
                throw $this->createProblem('ftp', implode("\n", $output), "Check connection params.");
            }

            return $prof->stop();
        } else {
            return get_class($this) . " will be checked later, every $timeFrameInMinutes minutes, according application settings.";
        }
    }
}
