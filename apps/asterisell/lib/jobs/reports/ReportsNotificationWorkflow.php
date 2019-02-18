<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Manage report workflow, from initial state,
 * to final state, that it is when reports are sent to users.
 */
class ReportsNotificationWorkflow extends FixedJobProcessor
{

    /**
     * How many months checks in the past for reports not reviewed.
     *
     * @const int
     */
    const MONTHS_TO_CHECK_IN_THE_PAST = 3;

    public function process()
    {
        $msg = '';

        $msg .= "\n" . $this->checkForReportsToReview();
        $msg .= "\n" . $this->sendReportsByEmail();

        return $msg;
    }

    protected function getGarbageKey()
    {
        return get_class($this) . '-sendreport';
    }

    /**
     * @return string log message
     */
    public function sendReportsByEmail()
    {
        ArProblemException::garbageCollect($this->getGarbageKey(), null, null);

        $c = new Criteria();
        $c->add(ArReportToReadPeer::MUST_BE_SENT_TO_EMAIL, true);
        $reports = ArReportToReadPeer::doSelect($c);
        // NOTE: usually there are not so many reports to send, so I can store them in RAM
        // and it is a list of reports IDS, not complete content

        /**
         * @var array $sentEmails associate to each email (used as key) an array with an index of sentReportId (used as key)
         */
        $sentEmails = array();

        $count = 0;
        foreach ($reports as $report) {
            /**
             * @var ArReportToRead $report
             */

            $count++;

            $this->sendReportByEmail($report, $sentEmails);
            // NOTE: I'm using this, because in this case PHP performs better with RAM usage
        }

        ArReportToReadPeer::clearInstancePool();
        ArReportToReadPeer::clearRelatedInstancePool();

        return "Sent $count reports by email.";
    }

    /**
     * Send the email only if the same report was not already sent to the same email address.
     *
     * @param ArReportToRead $reportToRead
     * @param &array $sentEmails complete with the list of sent emails.
     * @return bool true if the email was sent sucesfully
     */
    public function sendReportByEmail(ArReportToRead $reportToRead, &$sentEmails)
    {
        $report = $reportToRead->getArReport();
        if (is_null($report)) {

            $reportToRead->delete();

            $problemDuplicationKey = get_class($this) . ' - no report ';
            $problemDescription = 'There are generated reports that are empty.';
            $problemProposedSolution = "Regenerate the reports.";
            $problemEffect = "Some Reports will not be sent to interested users.";
            ArProblemException::createWithGarbageCollection(ArProblemType::TYPE_ERROR, ArProblemDomain::REPORTS, null, $problemDuplicationKey, $this->getGarbageKey(), null, null, $problemDescription, $problemEffect, $problemProposedSolution);
            return false;
        }

        $user = $reportToRead->getArUser();
        if (is_null($user)) {
            // in this case the report is associated to the first billable party, and the legal email is used
            $party = $this->getFirstBillableParty($report->getArOrganizationUnitId(), fromMySQLTimestampToUnixTimestamp($report->getFromDate()));

            if (is_null($party) || isEmptyOrNull($party->getEmail())) {
                $problemDuplicationKey = get_class($this) . ' - no party for organization ' . $report->getArOrganizationUnitId();
                $problemDescription = 'It is not possible associating a billable party with a valid email, to organization ' . $report->getArOrganizationUnitId() . ', referenced in report ' . $report->getId();
                $problemProposedSolution = "Associate a party to the organization, or specify the email.";
                $problemEffect = "Report will not be sent to email.";
                ArProblemException::createWithGarbageCollection(ArProblemType::TYPE_ERROR, ArProblemDomain::REPORTS, null, $problemDuplicationKey, $this->getGarbageKey(), null, null, $problemDescription, $problemEffect, $problemProposedSolution);
                return false;
            }
        } else {
            $party = $user->getArParty();

            if (is_null($party) || isEmptyOrNull($party->getEmail())) {
                $problemDuplicationKey = get_class($this) . ' - no party for user ' . $user->getId();
                $problemDescription = 'It is not possible sending emails to account ' . $user->getName() . ' with id ' . $user->getId();
                $problemProposedSolution = "Associate a party to the account, and specify the email.";
                $problemEffect = "Web Account will not receive emails.";
                ArProblemException::createWithGarbageCollection(ArProblemType::TYPE_ERROR, ArProblemDomain::REPORTS, null, $problemDuplicationKey, $this->getGarbageKey(), null, null, $problemDescription, $problemEffect, $problemProposedSolution);
                return false;
            }
        }

        $partyMail = trim($party->getEmail());
        $partyName = $party->getName();

        $alreadySentEmail = false;
        if (array_key_exists($partyMail, $sentEmails)) {
            if (array_key_exists($report->getId(), $sentEmails[$partyMail])) {
                $alreadySentEmail = true;
            } else {
                $sentEmails[$partyMail][$report->getId()] = true;
            }
        } else {
            $sentEmails[$partyMail] = array();
            $sentEmails[$partyMail][$report->getId()] = true;
        }

        if (!$alreadySentEmail) {

            $params = ArParamsPeer::getDefaultParams();

            $subject = $report->getReportMailSubject();
            if (isEmptyOrNull($subject)) {
                $subject = '[' . $params->getServiceProviderWebsite() . '] ' . $report->getProducedReportShortDescription();
            }

            $message = $report->getReportMailBody();
            if (isEmptyOrNull($message)) {
                $message = $report->getProducedReportAdditionalDescription();
            }

            // Replace the place holder in the email message

            $info = OrganizationUnitInfo::getInstance();
            $reportUnitId = $report->getArOrganizationUnitId();
            if (is_null($reportUnitId)) {
                $reportPartyName = $partyName;
            } else {

                $reportPartyId = $info->getArPartyId($reportUnitId, fromMySQLTimestampToUnixTimestamp($report->getFromDate()));
                if (is_null($reportPartyId)) {
                    $reportPartyName = mytr('customer');
                } else {
                    $reportParty = ArPartyPeer::retrieveByPK($reportPartyId);
                    $this->assertCondition(!is_null($reportParty));
                    $reportPartyName = $reportParty->getName();
                }
            }

            $message = str_replace('${ORGANIZATION}', $reportPartyName, $message);

            // set the format for email

            $locale = mb_internal_encoding();
            mb_internal_encoding('UTF-8');
            $message = str_replace("\n", "\n\r", $message);
            mb_internal_encoding($locale);

            $attach = array();

            if ($report->getProducedReportMimeType() == 'text/plain') {
                $message .= $report->getDocumentContent();
            } else {
                $c = $report->getDocumentContent();
                if (!is_null($c)) {
                    $attach[] = array($report->getProducedReportFileName(), $report->getProducedReportMimeType(), $c);
                } else {
                    $problemDuplicationKey = get_class($this) . ' - no attach for report - ' . $report->getId();
                    $problemDescription = 'There is no PDF document attached to report with id ' . $report->getId();
                    $problemProposedSolution = "Regenerate the report.";
                    $problemEffect = "Emails will be sent after the report is regenerated.";
                    ArProblemException::createWithGarbageCollection(ArProblemType::TYPE_ERROR, ArProblemDomain::REPORTS, null, $problemDuplicationKey, $this->getGarbageKey(), null, null, $problemDescription, $problemEffect, $problemProposedSolution);
                    return false;
                }
            }

            $isOk = $this->sendEmail($partyMail, $partyName, array(), $subject, $message, $attach);

        } else {
            $isOk = true;
        }

        if ($isOk) {
            $reportToRead->setSentToEmailAtDate(time());
            $reportToRead->setMustBeSentToEmail(false);
            $reportToRead->setSeenOrReceivedFromUser(true);
            $reportToRead->save();
        }

        return $isOk;
    }

    /**
     * @param int $unitId the unitId interested to the info
     * @param int $time
     * @return ArParty|null the first billable party
     */
    protected function getFirstBillableParty($unitId, $time)
    {

        $info = OrganizationUnitInfo::getInstance();

        $billableUnitId = null;
        $billableDataInfo = null;
        while (!is_null($unitId)) {
            $data = $info->getDataInfo($unitId, $time);
            if ($data[OrganizationUnitInfo::DATA_UNIT_IS_BILLABLE]) {
                $billableUnitId = $unitId;
                $billableDataInfo = $data;

                // stop at the first billable organization
                $unitId = null;
            } else {
                $unitId = $info->getParentId($unitId, $time);
            }
        }

        if (is_null($billableUnitId)) {
            return null;
        }

        $partyId = $billableDataInfo[OrganizationUnitInfo::DATA_PARTY_ID];
        if (is_null($partyId)) {
            return null;
        }

        $party = ArPartyPeer::retrieveByPK($partyId);
        $this->assertCondition(!is_null($party));

        return $party;
    }

    public function checkForReportsToReview()
    {
        $fromDate = strtotime('-' . self::MONTHS_TO_CHECK_IN_THE_PAST . ' month');

        $conn = Propel::getConnection();
        $query = 'select count(id) as nr from ar_report where produced_report_already_reviewed = 0 and from_date >= \'' . fromUnixTimestampToMySQLTimestamp($fromDate) . '\'';
        $stmt = $conn->prepare($query);
        $stmt->execute();
        $result = $stmt->fetchColumn();
        $processed = intval($result);
        if ($processed > 0) {
            $garbageKey = get_class($this) . '-checkForReportsToReview';
            ArProblemException::garbageCollect($garbageKey, null, null);

            $msg = 'There are ' . $processed . ' reports that are not yet reviewed, in the last ' . self::MONTHS_TO_CHECK_IN_THE_PAST . ' months. ';

            $problemDuplicationKey = "reports in not reviewed state - " . floor($processed / 10);
            $problemDescription = $msg;
            $problemEffect = "Not reviewed reports are not visible to all users, and they are not sent by email.";
            $problemProposedSolution = "Review the reports, and confirm them.";
            ArProblemException::createWithGarbageCollection(ArProblemType::TYPE_ERROR, ArProblemDomain::REPORTS, null, $problemDuplicationKey, $garbageKey, null, null, $problemDescription, $problemEffect, $problemProposedSolution);
        } else {
            $msg = 'All reports are reviewed.';
        }

        return $msg;
    }
}
