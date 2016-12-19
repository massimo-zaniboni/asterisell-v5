<?php

/* $LICENSE 2009, 2010, 2013, 2015:
 *
 * Copyright (C) 2009, 2010, 2013, 2015 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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
 * Check if there are new problems, and advise the administrator by email.
 * Set the signaled errors as already sent/signaled, in order to send only new problems.
 * This must be the last job to call, otherwise there can be problems noticing already sent problems.
 */
class AdviseAdminOfNewProblems extends FixedJobProcessor
{

    /**
     * Max errors to signal on each email
     *
     * @const int
     */
    const MAX_ERRORS = 25;

    /**
     * Max number of error reports for each type of error, to mantain on the system.
     * Error reports are not much important, and they can be removed, because the list of current errors,
     * is always the most important reference.
     *
     * @const int
     */
    const MAX_REPORTS = 5;

    /**
     * @param int $errorType
     * @return string
     */
    protected function getReportInternalIdentifier($errorType)
    {
        return 'error-report-internal-identifier-' . $errorType;
    }

    public function process()
    {
        ArProblemException::updateSentToAdminStateInNewProblems(ArProblemException::getLogConnection());

        $log = $this->processErrorType(ArProblemType::TYPE_CRITICAL, ArRole::NOTIFIED_FOR_CRITICAL_ERRORS, "critical errors")
            . "\n"
            . $this->processErrorType(ArProblemType::TYPE_ERROR, ArRole::NOTIFIED_FOR_ERRORS, 'errors')
            . "\n"
            . $this->processErrorType(ArProblemType::TYPE_WARNING, ArRole::NOTIFIED_FOR_WARNINGS, 'warnings');

        return $log;
    }

    /**
     * @param int $errorType
     * @param int $roleName
     * @param string $errorUserName
     * @return string
     *
     * @pre ` ArProblemException::updateSentToAdminStateInNewProblems` is called before calling this function
     */
    public function processErrorType($errorType, $roleName, $errorUserName)
    {
        $logConnection = ArProblemException::getLogConnection();

        // Create a new report only if there are the double of new problems, respect the old problems,
        // in order to reduce the number of warnings. Take in consideration each responsible,
        // because I don't want that many application errors, elide normal errors.

        /**
         * @var ArProblemResponsible $responsible
         */

        $c = new Criteria();
        $c->add(ArNewProblemPeer::SIGNALED_TO_ADMIN, true);
        $c->add(ArNewProblemPeer::AR_PROBLEM_TYPE_ID, $errorType);
        $oldProblems = ArNewProblemPeer::doCount($c, false, $logConnection);

        $c = new Criteria();
        $c->add(ArNewProblemPeer::SIGNALED_TO_ADMIN, false);
        $c->add(ArNewProblemPeer::AR_PROBLEM_TYPE_ID, $errorType);
        $newProblems = ArNewProblemPeer::doCount($c, false, $logConnection);
        $totProblems = $newProblems + $oldProblems;

        $newProblemsToAdd = $totProblems;
        if ($newProblemsToAdd == 0) {
            $newProblemsToAdd++;
        }

        if ($newProblems > $oldProblems) {
            // if there are 5 old problems, signal the report only when there are other 5 new problems, so 10 problems in total.
            // The next time signal when there are 20 total problems, then 40 and so on.
            // This for each type of different error.

            $newProblemNotifications = $newProblemsToAdd . ' new ' . $errorUserName;

            // Calculate all the errors

            $c = new Criteria();
            $c->add(ArNewProblemPeer::SIGNALED_TO_ADMIN, true);
            $c->add(ArNewProblemPeer::AR_PROBLEM_TYPE_ID, $errorType);
            $oldProblems = ArNewProblemPeer::doCount($c, false, $logConnection);

            $c = new Criteria();
            $c->add(ArNewProblemPeer::SIGNALED_TO_ADMIN, false);
            $c->add(ArNewProblemPeer::AR_PROBLEM_TYPE_ID, $errorType);
            $newProblems = ArNewProblemPeer::doCount($c, false, $logConnection);

            $totProblems = $newProblems + $oldProblems;

            $params = ArParamsPeer::getDefaultParams();

            $eol = "\r\n";

            $d = "There are $totProblems $errorUserName on " . $params->getServiceProviderWebsite()
                . $eol;

            // Show a summary of errors by domain and responsible

            $problemSummary = ArCurrentProblemPeer::getProblemSummaryByType($logConnection, true);

            $d .= $eol . 'There are: ' . $eol;
            foreach ($problemSummary as $summary) {
                list($severityLevelName, $numberOfProblems, $responsibleName, $problemDomainName, $severityLevelId, $responsibleId, $domainId) = $summary;

                if ($severityLevelId == $errorType) {
                    $d .= '* ' . $numberOfProblems
                        . ' '
                        . $problemDomainName
                        . ' '
                        . $errorUserName . '(s)'
                        . ', with responsible '
                        . $responsibleName
                        . $eol;
                }
            }
            $d .= $eol;

            $d .= "A new error report will be produced only when there will be $newProblemNotifications"
                . $eol
                . $eol;

            $d .= "Only the first " . self::MAX_ERRORS . " *new problems* will be included into this notification."
                . $eol
                . $eol;

            // Describe problems

            $c = new Criteria();
            $c->add(ArNewProblemPeer::SIGNALED_TO_ADMIN, false);
            $c->add(ArNewProblemPeer::AR_PROBLEM_TYPE_ID, $errorType);
            $c->addDescendingOrderByColumn(ArNewProblemPeer::CREATED_AT);
            $c->setLimit(self::MAX_ERRORS);
            $problems = ArNewProblemPeer::doSelect($c, $logConnection);

            $countProblems = 0;
            foreach ($problems as $problem) {
                /**
                 * @var ArNewProblem $problem
                 */
                $countProblems++;

                $problemTitle = "Problem no. " . $countProblems . " - " . fromUnixTimestampToSymfonyStrDate($problem->getCreatedAt());
                $problemTitleUnderline = str_repeat('=', strlen($problemTitle));

                $countAffectedCDRS = $problem->getCountOfCdrs();
                if ($countAffectedCDRS > 0) {
                    $countAffectedCDRSStr = $countAffectedCDRS;
                } else {
                    $countAffectedCDRSStr = '-';
                }

                $d .= $problemTitle . $eol . $problemTitleUnderline
                    . $eol
                    . $eol
                    . "Domain: " . $problem->getArProblemDomain()->getName()
                    . $eol
                    . $eol
                    . "Affected Unbilled CDRs: " . $countAffectedCDRSStr
                    . $eol
                    . $eol
                    . "Responsible: " . $problem->getArProblemResponsible()->getName()
                    . $eol
                    . $eol
                    . "Description: " . $problem->getDescription()
                    . $eol
                    . $eol
                    . "Effect: " . $problem->getEffect()
                    . $eol
                    . $eol
                    . "Suggested Solution: " . $problem->getProposedSolution()
                    . $eol
                    . $eol;
            }


            // Set problems as already signaled
            $stmt = $logConnection->prepare('UPDATE ar_new_problem SET signaled_to_admin = 1 WHERE ar_problem_type_id = ?');
            $stmt->execute(array($errorType));

            // Remove old reports
            $c = new Criteria();
            $c->add(ArReportPeer::INTERNAL_NAME, $this->getReportInternalIdentifier($errorType));
            $totalReports = ArReportPeer::doCount($c);
            if ($totalReports > self::MAX_REPORTS) {
                $toDelete = $totalReports - self::MAX_REPORTS;
                $stmt = $logConnection->prepare('DELETE FROM ar_report WHERE internal_name= ? ORDER BY from_date LIMIT ' . $toDelete);
                $stmt->execute(array($this->getReportInternalIdentifier($errorType)));
            }

            // Create the report

            $reportDescr = "There are $totProblems $errorUserName";
            // NOTE: this will be also the subject of the email

            $report = new ArReport();
            $report->setFromDate(time());
            $report->setArOrganizationUnitId(null);
            $report->setDocumentContent($d);
            $report->setInternalName($this->getReportInternalIdentifier($errorType));
            $report->setReportName($reportDescr);
            $report->setProducedReportShortDescription($reportDescr);
            $report->setProducedReportAdditionalDescription($reportDescr);
            $report->setProducedReportAlreadyReviewed(true);
            $report->setProducedReportFileTypeSuffix('');
            $report->setProducedReportMimeType("text/plain");
            $report->setProducedReportIsDraft(false);
            $report->save($logConnection);

            $permission = new ArReportAlsoFor();
            $permission->setArRoleId(ArRolePeer::retrieveByInternalName($roleName)->getId());
            $permission->setArReportId($report->getId());
            $permission->save($logConnection);

            ArReportPeer::publishReportToUsers($report->getId(), true, false, $logConnection);

            return "Report generated for $errorUserName";

        } else {
            return "No new report generated for $errorUserName";
        }

    }

}
