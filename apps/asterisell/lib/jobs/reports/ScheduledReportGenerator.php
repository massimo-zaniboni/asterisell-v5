<?php
/* $LICENSE 2012, 2013, 2017:
 *
 * Copyright (C) 2012, 2013, 2017 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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
 * When called decide if must generate a batch of reports (ar_report_set), according its schedule.
 *
 * It has no persistent state, and all its values are retrieved from the associated
 * ArReportScheduler.
 *
 * It is associated to a ArReportScheduler.
 */
class ScheduledReportGenerator
{

    /**
     * Init report generator, and return the associated report set for the run.
     *
     * @param PDO $conn
     * @param ArReportScheduler $report
     * @param int $fromDate the starting date of the calls of the report
     * @return ArReportSet report set associated to the run of the scheduler. It must be saved later.
     * @throws ArProblemException
     * @throws Exception
     */
    protected function initGeneratorAndGetOrCreateReportSet(PDO $conn, ArReportScheduler $report, $fromDate)
    {
        $this->setArReportScheduler($report);
        $this->checkParams();

        $this->generateOnlyNames();

        /**
         * @var PropelPDO $conn
         */
        $this->getArReportScheduler()->save($conn);

        $reportSetId = ArReportSetPeer::getReportSetIdAssociatedToReportScheduler($this->getArReportScheduler()->getId(), $fromDate, $conn);

        if (is_null($reportSetId)) {
            $r = new ArReportSet();
            $r->setArReportSchedulerId($this->getArReportScheduler()->getId());
            $r->setMustBeReviewed(true);
            $r->setFromDate($fromDate);
            $r->setToDate($this->getReportRangeToDate($fromDate));

            return $r;
        } else {
            return ArReportSetPeer::retrieveByPK($reportSetId);
        }
    }

    public function generateOnlyNames()
    {
        $this->getArReportScheduler()->setAdditionalDescription($this->calcCompleteDescription(false));
        $this->getArReportScheduler()->setShortDescription($this->calcCompleteDescription(true));
    }

    ///////////////////////
    // REPORT MANAGEMENT //
    ///////////////////////

    /**
     * @param ArReportScheduler $report
     * @param int $fromDate when the reports must be generated, null for current time
     * @return void
     * @throws ArProblemException
     */
    public function deleteAssociatedReports(ArReportScheduler $report, $fromDate)
    {
        /**
         * @var PropelPDO $conn
         */
        $conn = Propel::getConnection();
        $conn->beginTransaction();
        try {
            $reportSet = $this->initGeneratorAndGetOrCreateReportSet($conn, $report, $fromDate);

            if (!is_null($reportSet->getId())) {
                $reportSet->deleteAssociatedReports($conn);
                // NOTE: due to conflicts in constraints, first must delete associated reports.

                $stmt = $conn->prepare('DELETE FROM ar_report_set WHERE id = ?');
                $isOk = $stmt->execute(array($reportSet->getId()));

                if ($isOk === FALSE) {
                    throw(ArProblemException::createWithoutGarbageCollection(
                        ArProblemType::TYPE_ERROR,
                        ArProblemDomain::APPLICATION,
                        null,
                        get_class($this), "Unable to delete old Reports, associated to the report scheduler " . $this->getName(), "Reports are not generated.", "Contact the assistance."));
                }
            }
            $conn->commit();

        } catch (ArProblemException $e) {
            $conn->rollBack();

            throw($e);
        } catch (Exception $e) {
            $conn->rollBack();
            throw(ArProblemException::createFromGenericExceptionWithoutGarbageCollection($e, get_class($this), 'Scheduled report generation: ' . $this->getName(), "Scheduled reports are not generated", "If the error persist contact the assistance."));
        }
    }

    /**
     * @param ArReportScheduler $report
     * @param int $fromDate when the reports must be generated, null for current time
     * @return void
     * @throws ArProblemException
     */
    public function confirmAssociatedReports(ArReportScheduler $report, $fromDate)
    {
        $conn = Propel::getConnection();
        $conn->beginTransaction();
        try {

            $reportSet = $this->initGeneratorAndGetOrCreateReportSet($conn, $report, $fromDate);

            if (!is_null($reportSet->getId())) {
                ArReportSetPeer::confirmAssociatedReports($reportSet->getId(), $conn);
            }

            $conn->commit();

        } catch (ArProblemException $e) {
            $conn->rollBack();

            throw($e);
        } catch (Exception $e) {
            $conn->rollBack();

            throw(ArProblemException::createFromGenericExceptionWithoutGarbageCollection($e, get_class($this), 'Scheduled report generation: ' . $this->getName(), "Scheduled reports are not generated", "If the error persist contact the assistance."));
        }
    }

    ///////////////////////
    // REPORT GENERATION //
    ///////////////////////


    /**
     * @param ArReportScheduler $scheduler
     * @return int the generated reports
     * @throws ArProblemException
     */
    public function maybeGenerateAssociatedReportsAccordingScheduling(ArReportScheduler $scheduler)
    {
        $this->setArReportScheduler($scheduler);

        $lastTime = $this->getArReportScheduler()->getLastToDate();

        if (!is_null($lastTime)) {
            $lastTime = fromMySQLTimestampToUnixTimestamp($lastTime);
            $this->checkParams();

            if ($this->getCanBeExecuted(time())) {
                return $this->generateAssociatedReports($scheduler, $lastTime);
            } else {
                return 0;
            }
        } else {
            return 0;
        }
    }

    /**
     * Generate reports without checking the scheduler frequency.
     *
     * @param ArReportScheduler $report
     * @param int $fromDate when the reports must be generated, null for current time
     * @param int|null $parentJobId in case the method was called from a job
     * @return int the generated reports
     * @throws ArProblemException
     */
    public function generateAssociatedReports(ArReportScheduler $report, $fromDate, $parentJobId = null)
    {
        /**
         * @var PropelPDO $conn
         */
        $count = 0;
        $conn = Propel::getConnection();
        $conn->beginTransaction();
        try {

            $reportSet = $this->initGeneratorAndGetOrCreateReportSet($conn, $report, $fromDate);

            $count = $this->createAllOrganizationReportsAccordingReferenceDate($conn, $fromDate, $reportSet);
            // NOTE: this command delete also associated reports (but not the report-set)

            $stmt = $conn->prepare('CALL proc_update_postponed_reportset_amounts(?)');
            $stmt->execute(array($reportSet->getId()));

            if ($report->getSendCompactReportListToAccountant() === true) {
              $d = new GenerateSummaryReportEvent();
              $d->reportSetId = $reportSet->getId();
              ArJobQueuePeer::addNew($d, $parentJobId, null);
            }

            $conn->commit();
       } catch (ArProblemException $e) {
            $conn->rollBack();

            throw($e);
        } catch (Exception $e) {
            $conn->rollBack();

            throw(ArProblemException::createFromGenericExceptionWithoutGarbageCollection($e, get_class($this), 'Scheduled report generation: ' . $this->getName(), "Scheduled reports are not generated", "If the error persist contact the assistance."));
        }

        if (!is_null($report->getMinimumCost()) && $report->getMinimumCost() > 0) {
                //NOTE: if a report-set has no minimum cost, then it is correct because the code
                // generating it is simple.
                //
                // Otherwise if it has minimun cost then 99% of the times it is used for generating
                // customer invoices, and the code for postponing them can be error-prone,
                // so it is tested that it is all ok.
                // It is an heuristic respect an explicit flag on scheduled reports saying if
                // a check must be performed, but it is good enough.
                $this->checkPostponedReportSet($report, $reportSet->getId());
        }
        return $count;
    }

    /**
     * @var ReportCalcStore
     */
    protected $sharedReportCalcStore;

    /**
     * Set also $this->sharedReportCalcStore
     *
     * @param PDO $conn
     * @param ArReport $template
     * @param int $reportSetId
     * @param int|null $arOrganizationId
     * @param int $from
     * @param int $to
     * @param int|null $legalDate
     * @param int|null $legalNr
     * @param bool $onlyIfThereIsCost true if the report must be generated only if there is cost
     * @param int $minimumCost if the total is < of this value, the report is postponed
     *
     * @return bool|null true if the report is already reviewed and can be sent to users,
     *         false if the report must be reviewed, for example after generation it is in draft mode,
     *         null if the report must be not generated (no totals, or postponed)
     *
     * @throws ArProblemException
     */
    protected function createReportAccordingReferenceDateOrganizationAndRange(PDO $conn, ArReport $template, $reportSetId, $arOrganizationId, $from, $to, $legalDate = null, $legalNr = null, $onlyIfThereIsCost = false, $minimumCost = 0)
    {
        /**
         * @var PropelPDO $conn
         */
        $report = $template->copy();
        $report->setIsTemplate(false);
        $report->setArReportSetId($reportSetId);
        $report->setFromDate($from);
        $report->setToDate($to);
        $report->setLegalDate($legalDate);
        $report->setLegalConsecutiveNr($legalNr);
        $report->setArOrganizationUnitId($arOrganizationId);

        // generate the report reusing the shared store
        $this->sharedReportCalcStore = $report->generateDocument($conn, $this->sharedReportCalcStore, $this->getArReportScheduler()->getId());

        if ($report->getProducedReportIsDraft()) {
            $report->setProducedReportAlreadyReviewed(false);
        } else {
            if ($this->getArReportScheduler()->getProducedReportMustBeReviewed()) {
                $report->setProducedReportAlreadyReviewed(false);
            } else {
                $report->setProducedReportAlreadyReviewed(true);
            }
        }

        if (($onlyIfThereIsCost || $minimumCost > 0) && (is_null($report->getTotalWithTax()) || $report->getTotalWithTax() == 0)) {
            if (!$report->isDeleted() && !$report->isNew()) {
                $report->delete($conn);
            }
            return null;
        } else if ($minimumCost > 0 && $report->getTotalWithoutTax() < $minimumCost) {
            if (!$report->isDeleted() && !$report->isNew()) {
                $report->delete($conn);
            }

            // NOTE: use a REPLACE stmt because it does not issue an error if the two primary keys already exists
            $postStmt = $conn->prepare('
             REPLACE INTO ar_postponed_report(ar_report_set_id, ar_organization_unit_id)
             VALUES(?,?);');
            $postStmt->execute(array($reportSetId, $arOrganizationId));

            return null;

        } else {
            $report->save($conn);
            $stmt = $conn->prepare('CALL create_reports_from_other_report(?,?)');
            $stmt->execute(array($template->getId(), $report->getId()));
            $stmt->closeCursor();
            return $report->getProducedReportAlreadyReviewed();
        }
    }

    /**
     * For each organization, generate the report, calculating the range.
     * Delete previous created reports.
     *
     * @param PropelPDO $conn
     * @param int $fromDate
     * @param ArReportSet $reportSet
     * @return int
     * @throws ArProblemException
     */
    public function createAllOrganizationReportsAccordingReferenceDate(PropelPDO $conn, $fromDate, ArReportSet $reportSet)
    {

        $responsibleRoleId = null;

        $count = 0;

        $this->checkParams();

        $toDate = $this->getReportRangeToDate($fromDate);

        $organizationId = $this->getArReportScheduler()->getArOrganizationUnitId();

        $generationMethod = $this->getArReportScheduler()->getArReportGenerationId();

        $templateReport = $this->getArReportScheduler()->getArReport();

        $onlyIfThereIsCost = $this->getArReportScheduler()->getGenerateOnlyIfThereIsCost();
        if (is_null($onlyIfThereIsCost)) {
            $onlyIfThereIsCost = false;
        }

        $minimumCost = $this->getArReportScheduler()->getMinimumCost();
        if (is_null($minimumCost)) {
            $minimumCost = 0;
        }

        $reportSet->deleteAssociatedReports($conn);

        $reportSet->setArReportSchedulerId($this->getArReportScheduler()->getId());
        $reportSet->setFromDate($fromDate);
        $reportSet->setToDate($toDate);
        $reportSet->setMustBeReviewed(true);
        // NOTE: initially start pessimistic, then only if there are no draft reports, set as already reviewed
        $reportSet->save($conn);

        $isAllReviewed = true;

        $this->sharedReportCalcStore = null;

        $legalDate = null;
        $legalNr = null;
        if (!is_null($this->getArReportScheduler()->getArLegalDateGenerationMethodId())) {
            // Calculate the legal date associated to the reports

            $legalDateMethod = $this->getArReportScheduler()->getArLegalDateGenerationMethodId();
            if ($legalDateMethod == ArLegalDateGenerationMethod::USE_END_DATE_OF_THE_REPORT_RANGE) {
                $legalDate = $toDate;
            } else if ($legalDateMethod == ArLegalDateGenerationMethod::USE_THE_FIRST_DAY_OF_THE_MONTH_AFTER_THE_END_OF_REPORT_RANGE) {
                if (intval(date('j')) == 1) {
                    $legalDate = $toDate;
                } else {
                    $legalDate = strtotime(date('Y', $toDate) . '-' . (intval(date('n', $toDate)) + 1) . '-01');
                }
            } else if ($legalDateMethod == ArLegalDateGenerationMethod::USE_THE_DATE_OF_WHEN_THE_REPORT_IS_GENERATED) {
                $legalDate = startWith00Timestamp(time());
            } else if ($legalDateMethod == ArLegalDateGenerationMethod::USE_THE_FIRST_DAY_OF_THE_MONTH_OF_THE_END_OF_REPORT_RANGE) {
                $legalDate = strtotime(date('Y', $toDate) . '-' . date('m', $toDate) . '-01');
            } else if ($legalDateMethod == ArLegalDateGenerationMethod::USE_THE_FIRST_MONDAY_AFTER_THE_REPORT_RANGE) {
                if (intval(date('N')) == 1) {
                    $legalDate = $toDate;
                } else {
                    $legalDate = strtotime('next Monday', $toDate);
                }
            } else {
                $p = ArProblemException::createWithoutGarbageCollection(
                    ArProblemType::TYPE_ERROR,
                    ArProblemDomain::APPLICATION,
                    null,
                    get_class($this) . ' - unknown legal method type ' . $legalDateMethod,
                    "Reports generator with id " . $this->getArReportScheduler()->getId() . " generate reports with a legal date, and legal number, but the specified method for retrieving the legal date, is not supported/known from the application. ",
                    "The related reports will not be generated.",
                    "This is an error in the application code. Contact the assistance."
                );
                throw($p);
            }

            assert(!is_null($legalDate));

            // Search the first legal number to use
            // NOTE: first order by date, because at change of year it must start with minor numbers
            $stm = $conn->prepare('
            SELECT legal_date, legal_consecutive_nr
            FROM ar_report
            WHERE legal_date IS NOT NULL
            ORDER BY legal_date DESC, legal_consecutive_nr DESC
            LIMIT 1');
            $stm->execute(array());

            $rs = $stm->fetch(PDO::FETCH_NUM);
            if ($rs !== false) {
                $lastLegalDate = fromMySQLTimestampToUnixTimestamp($rs[0]);
                $lastLegalNr = $rs[1];
            } else {
                $lastLegalDate = $legalDate;
                $lastLegalNr = 0;
            }

            $stm->closeCursor();

            if ($this->getArReportScheduler()->getIsYearlyLegalNumeration()) {
                if (date('Y', $lastLegalDate) < date('Y', $legalDate)) {
                    // when there is a change of date, start with a new enumeration

                    $lastLegalDate = $legalDate;
                    $lastLegalNr = 0;
                }
            }

            assert(!is_null($lastLegalNr));

            $legalNr = $lastLegalNr + 1;

        }

        // Start with initial organizations, according report params

        $organizations = array();

        if (!is_null($organizationId)) {
            array_push($organizations, $organizationId);
        } else {

            if ($generationMethod == ArReportGeneration::GENERATE_ONLY_FOR_SPECIFIED_ORGANIZATION) {
                // generate a single report, associated to the super root organization
                array_push($organizations, null);

            } else {
                // generate a report for every root organization
                foreach (OrganizationUnitInfo::getInstance()->getRootOrganizationsIds($fromDate, $toDate) as $rootId => $ignore) {
                    array_push($organizations, $rootId);
                }
            }
        }

        // Process organizations

        while (!empty($organizations)) {
            $organizationId = array_pop($organizations);

            $generateThisReport = false;
            $exploreChildren = false;

            /**
             * @var ArOrganizationUnit $organizationUnit
             */
            $info = OrganizationUnitInfo::getInstance()->getDataInfo($organizationId, $fromDate);

            // Check if the party has the specified TAG
            $partyId = OrganizationUnitInfo::getInstance()->getArPartyId($organizationId, $fromDate);
            $tagId = $templateReport->getArTagId();
            if (is_null($tagId)) {
                $isTagFilterRespected = true;
            } else {
                if (is_null($partyId)) {
                    $isTagFilterRespected = false;
                } else {
                    $isTagFilterRespected = ArPartyPeer::hasTag($partyId, $tagId);
                }
            }

            if (is_null($organizationId)) {
                if ($generationMethod == ArReportGeneration::GENERATE_ONLY_FOR_SPECIFIED_ORGANIZATION) {
                    $exploreChildren = false;
                    $generateThisReport = true;
                } else {
                    $this->signalProblem('Error in the code: 1023. Contact the assistance.');
                }
            } else {


                if (!is_null($info)) {

                    if ($generationMethod == ArReportGeneration::GENERATE_ONLY_FOR_SPECIFIED_ORGANIZATION) {
                        $exploreChildren = false;
                        $generateThisReport = true;
                    } else if ($generationMethod == ArReportGeneration::GENERATE_FOR_ALL_CHILDREN_ORGANIZATIONS_AND_VOIP_ACCOUNTS) {
                        $exploreChildren = true;
                        $generateThisReport = true;
                    } else if ($generationMethod == ArReportGeneration::GENERATE_FOR_ALL_CHILDREN_ORGANIZATIONS_THAT_ARE_NOT_VOIP_ACCOUNTS) {
                        $isLeaf = $info[OrganizationUnitInfo::DATA_UNIT_TYPE_IS_LEAF];
                        if (is_null($isLeaf)) {
                            $isLeaf = false;
                        }
                        $generateThisReport = !$isLeaf;
                        $exploreChildren = true;
                    } else if ($generationMethod == ArReportGeneration::GENERATE_FOR_ALL_BILLABLE_CHILDREN_ORGANIZATIONS) {
                        $isBillable = $info[OrganizationUnitInfo::DATA_UNIT_IS_BILLABLE];
                        if (is_null($isBillable)) {
                            $isBillable = false;
                        }
                        $generateThisReport = $isBillable;
                        $exploreChildren = true;
                    } else if ($generationMethod == ArReportGeneration::GENERATE_FOR_ALL_CHILDREN_ORGANIZATIONS_WITH_A_RESPONSIBLE) {

                        if (is_null($responsibleRoleId)) {
                            $responsibleRoleId = ArRolePeer::retrieveByInternalName(ArRole::USER)->getId();
                        }

                        $usersWithRoles = OrganizationUnitInfo::getInstance()->getDirectUsersWithRoles($organizationId);
                        $isThereResponsible = false;
                        foreach ($usersWithRoles as $userId => $roles) {
                            foreach ($roles as $role) {
                                if ($role == $responsibleRoleId) {
                                    $isThereResponsible = true;
                                }
                            }
                        }

                        $generateThisReport = $isThereResponsible;
                        $exploreChildren = true;
                    } else {
                        $this->signalProblem('Error in the code: not supported generation method. Contact the assistance.');
                    }
                } else {
                    $this->signalProblem('Error in the code: 7575. Contact the assistance.');
                }
            }

            if ($generateThisReport && $isTagFilterRespected) {
                $isReviewed = $this->createReportAccordingReferenceDateOrganizationAndRange($conn, $templateReport, $reportSet->getId(), $organizationId, $fromDate, $toDate, $legalDate, $legalNr, $onlyIfThereIsCost, $minimumCost);
                if (is_null($isReviewed)) {
                    // the report has no cost, and it can be skipped
                } else {
                    if (!is_null($legalNr)) {
                        $legalNr++;
                    }

                    $isAllReviewed = $isAllReviewed && $isReviewed;
                    $count++;
                }
            }

            if ($exploreChildren) {
                $children = OrganizationUnitInfo::getInstance()->getDirectChildrenAtDate($organizationId, $fromDate);
                foreach ($children as $child => $ignore) {
                    array_push($organizations, $child);
                }
            }
        }

        $reportSet->setMustBeReviewed(!$isAllReviewed);
        $reportSet->save($conn);
        ArReportSetPeer::publishReportToUsers($reportSet->getId(), $isAllReviewed, false, $conn);

        $this->getArReportScheduler()->setLastExecutionDate(time());
        $this->getArReportScheduler()->setLastFromDate($fromDate);
        $this->getArReportScheduler()->setLastToDate($toDate);

        $this->getArReportScheduler()->save($conn);

        return $count;
    }

    ///////////////////
    // ACCESS REPORT //
    ///////////////////

    /**
     * @var ArReportScheduler
     */
    protected $report;

    /**
     * @param ArReportScheduler $report
     */
    public function setArReportScheduler($report)
    {
        $this->report = $report;
    }

    /**
     * @return ArReportScheduler|null
     */
    public function getArReportScheduler()
    {
        return $this->report;
    }

    //////////////////////
    // DESCRIBE REPORTS //
    //////////////////////

    /**
     * @return null|string
     */
    public function getName()
    {
        if (!is_null($this->getArReportScheduler())) {
            return 'Generate ' . $this->getArReportScheduler()->getName();
        } else {
            return 'Not defined';
        }
    }

    /**
     * @param bool $shortVersion
     * @return string
     */
    public function calcCompleteDescription($shortVersion)
    {
        try {
            $this->checkParams();

            /**
             * @var ArReport $template
             */
            $template = $this->getArReportScheduler()->getArReport();
            if (is_null($template)) {
                return 'Missing report template.';
            } else {

                if ($shortVersion) {
                    $d = $template->getProducedReportShortDescription();
                } else {
                    $long = $template->getProducedReportAdditionalDescription();
                    if (isEmptyOrNull($long)) {
                        $long = $template->getProducedReportShortDescription();
                    }

                    $d = "\"" . $long . "\", scheduled every ";

                    $isThereMonth = false;
                    $x = $this->getArReportScheduler()->getScheduleEveryXMonths();
                    if ((!is_null($x)) && $x > 0) {
                        $d .= "$x month(s) ";
                        $isThereMonth = true;
                    }

                    $x = $this->getArReportScheduler()->getScheduleEveryXDays();
                    if ((!is_null($x)) && $x > 0) {
                        if ($isThereMonth) {
                            $d .= 'and ';
                        }

                        $d .= "$x day(s) ";

                    }
                }

                return $d;

            }
        } catch (Exception $e) {
            return 'Incomplete Params';
        }
    }

    /**
     * @return string
     * @throws ArProblemException
     */
    protected function getReportSubjectsDescription()
    {

        $organization = ArOrganizationUnitPeer::retrieveByPK($this->getArReportScheduler()->getArOrganizationUnitId());
        $d = "organization \"" . $organization->getHumanReadableName() . "\"";

        $generationMethod = $this->getArReportScheduler()->getArReportGenerationId();

        if ($generationMethod == ArReportGeneration::GENERATE_ONLY_FOR_SPECIFIED_ORGANIZATION) {
        } else if ($generationMethod == ArReportGeneration::GENERATE_FOR_ALL_CHILDREN_ORGANIZATIONS_AND_VOIP_ACCOUNTS) {
            $d .= ", and each child organization and VoIP account";
        } else if ($generationMethod == ArReportGeneration::GENERATE_FOR_ALL_CHILDREN_ORGANIZATIONS_THAT_ARE_NOT_VOIP_ACCOUNTS) {
            $d .= ", and each proper child organization, that is not a VoIP account";
        } else if ($generationMethod == ArReportGeneration::GENERATE_FOR_ALL_BILLABLE_CHILDREN_ORGANIZATIONS) {
            $d .= ", and each child organization that is set as billable";
        } else if ($generationMethod == ArReportGeneration::GENERATE_FOR_ALL_CHILDREN_ORGANIZATIONS_WITH_A_RESPONSIBLE) {
            $d .= ", and each child organization with an associated responsible user";
        } else {
            $this->signalProblem('Error in the code: not supported generation method. Contact the assistance.');
        }

        return $d;
    }

    ///////////////////////////////////////
    // ERROR REPORTING UTILITY FUNCTIONS //
    ///////////////////////////////////////

    /**
     * @param string $description
     * @throws ArProblemException
     */
    protected function signalProblem($description)
    {
        $report = $this->getArReportScheduler();
        if (is_null($report)) {
            $reportId = '<unspecified>';
        } else {
            $reportId = $report->getId();
        }

        $p = ArProblemException::createWithoutGarbageCollection(
            ArProblemType::TYPE_ERROR,
            ArProblemDomain::REPORTS,
            null,
            get_class($this) . ' - report id ' . $reportId,
            "Reports generator with id $reportId: " . $description,
            "The report with id $reportId is not generated.",
            "Fix the report params according the description, and regenerate the report."
        );

        throw($p);
    }

    /**
     * @throws ArProblemException
     */
    public function checkParams()
    {
        $report = $this->getArReportScheduler();
        if (is_null($report)) {
            $this->signalProblem('The report generator is not associated to any report specification. This is an internal and unexpected error of the application. Contact the assistance.');
        }

        if (is_null($this->getArReportScheduler()->getArReportId())) {
            $this->signalProblem('Unspecified report template.');
        }

        // test for date range

        $isThereOneDateRange = false;

        $x = $this->getArReportScheduler()->getScheduleEveryXMonths();
        if ((!is_null($x)) && $x > 0) {
            $isThereOneDateRange = true;
        }

        $x = $this->getArReportScheduler()->getScheduleEveryXDays();
        if ((!is_null($x)) && $x > 0) {
            $isThereOneDateRange = true;
        }

        if (!$isThereOneDateRange) {
            $this->signalProblem('Unspecified date range.');
        }
    }

    /////////////////////
    // CHECK FUNCTIONS //
    /////////////////////

    /**
     * @param ArReportScheduler $scheduler
     * @param int $reportSetId
     * @return bool true if totals are corract, false otherwise. Signal also problems on the error table.
     *
     * @require report set has a limit (optionally) and reports have a cost and their sum
     * is equal to the sum of cdrs in the time-frame, except the postponed costs.
     *
     */
    public function checkPostponedReportSet(ArReportScheduler $scheduler, $reportSetId)
    {
        $conn = Propel::getConnection();

        // Retrieve the default time-frame of report-set,
        // without taking in consideration postponed/included reports.

        // Get more recent report-sets
        $queryReportSets = '
          SELECT DISTINCT ss.id AS id
           FROM ar_report_set AS s1
           ,    ar_report_set AS ss
           WHERE s1.id = ?
           AND   ss.ar_report_scheduler_id = s1.ar_report_scheduler_id
           AND   ss.from_date <= s1.from_date
           ORDER BY ss.from_date DESC LIMIT 3
        ';

        // Get the time-frame to consider for checking recent report-sets
        $queryMinMaxDate = '
        SELECT MIN(rp.from_date)
        ,      MAX(rp.to_date)
        FROM (' . $queryReportSets . ') AS rs
        , ar_report AS rp
        WHERE rp.ar_report_set_id = rs.id
        ';

        $stmt = $conn->prepare($queryMinMaxDate);
        $stmt->execute(array($reportSetId));

        $minFromDate = null;
        $maxToDate = null;

        while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
            $fromDate = fromMySQLTimestampToUnixTimestamp($rs[0]);
            $toDate = fromMySQLTimestampToUnixTimestamp($rs[1]);

            if (is_null($minFromDate) || $fromDate < $minFromDate) {
                $minFromDate = $fromDate;
            }

            if (is_null($maxToDate) || $toDate > $maxToDate) {
                $maxToDate = $toDate;
            }
        }
        $stmt->closeCursor();

        // Execute totals by reports

        $sql1 = '
        SELECT rp.ar_organization_unit_id
        ,      SUM(rp.total_without_tax)
        FROM (' . $queryReportSets . ') AS rs
        , ar_report AS rp
        WHERE rp.ar_report_set_id = rs.id
        GROUP BY rp.ar_organization_unit_id
        ';

        $stmt = $conn->prepare($sql1);
        $stmt->execute(array($reportSetId));

        $group = array();

        while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
            $id = $rs[0];
            $tot = $rs[1];
            $group[$id] = $tot;
        }
        $stmt->closeCursor();

        // Now execute totals by CDRs in the nominal time-frame and check if they are the
        // same totals of effective reports, with also postponed/included values
        // minus/plus the minimum postponed income.

        $minimumCost = $scheduler->getMinimumCost();
        if (is_null($minimumCost)) {
            $minimumCost = 0;
        }

        $sql2 = '
        SELECT billable_ar_organization_unit_id AS unit_id
        ,      SUM(income)
        FROM ar_cdr FORCE INDEX (ar_cdr_calldate_index)
        WHERE calldate >= ?
        AND   calldate < ?
        GROUP BY billable_ar_organization_unit_id
        ';

        $stmt = $conn->prepare($sql2);
        $stmt->execute(array(fromUnixTimestampToMySQLTimestamp($minFromDate), fromUnixTimestampToMySQLTimestamp($maxToDate)));

        $isAllOk = true;
        while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
            $id = $rs[0];
            $cdrTot = $rs[1];

            if (array_key_exists($id, $group)) {
                $groupTot = $group[$id];
            } else {
                // in case of postponed organizations
                $groupTot = 0;
            }

            if (abs($groupTot - $cdrTot) > ($minimumCost * 1.1)) {
                $isAllOk = false;

                $errorInfo = "The report sets from " . fromUnixTimestampToMySQLTimestamp($minFromDate)
                . " to " . fromUnixTimestampToMySQLTimestamp($maxToDate)
                . ", generated by the report scheduler " . $scheduler->getId() . " "
                . " have a total of " . from_db_decimal_to_monetary_txt_according_locale($groupTot)
                . " for the organization with id " . $id . ", and name " . OrganizationUnitInfo::getInstance()->getFullNameAtDate($id, null, false, false)
                . ", that is different from the totals of incomes of CDRs that is " . from_db_decimal_to_monetary_txt_according_locale($cdrTot)
                . ". Difference is " . from_db_decimal_to_monetary_txt_according_locale(abs($cdrTot - $groupTot))
                . ", and it is superior to the minimum invoice postpone of "
                . from_db_decimal_to_monetary_txt_according_locale($minimumCost)
                . ".";
                $errorSolution = "If you have modified the parameters of the scheduler, or of organizations you need to regenerate the reports. Otherwise contact the assistance, because it is an error of the application.";
                $errorEffect = "You are billing more or less money respect the totals of rated CDRs.";

                ArProblemException::createWithoutGarbageCollection(
                    ArProblemType::TYPE_ERROR,
                    ArProblemDomain::APPLICATION,
                    null,
                    get_class($this),
                    $errorInfo,
                    $errorEffect,
                    $errorSolution);
            }
        }
        $stmt->closeCursor();

        return $isAllOk;

    }

//////////////////////
// UTILITY FUNCTION //
//////////////////////

    /**
     * @param int $fromDate the starting date of calls in the report
     * @return int the ending date (exclusive) of the calls in the report
     */
    public
    function getReportRangeToDate($fromDate)
    {

        $scheduler = $this->getArReportScheduler();

        $everyXMonths = $scheduler->getScheduleEveryXMonths();
        $everyXDays = $scheduler->getScheduleEveryXDays();

        $toDate = $fromDate;

        if ((!is_null($everyXMonths)) && ($everyXMonths > 0)) {
            $toDate = strtotime('+' . $everyXMonths . ' months', $toDate);
        }

        if ((!is_null($everyXDays)) && ($everyXDays > 0)) {
            $toDate = strtotime('+' . $everyXDays . ' days', $toDate);
        }

        return $toDate;
    }

    /**
     * @param int $atDate
     * @return bool true if the report can be executed
     */
    public
    function getCanBeExecuted($atDate)
    {
        // avoid the execution if there is no data because it is the first installation
        if (is_null(ArCdrPeer::doSelectOne(new Criteria()))) {
            return false;
        }

        // this scheduler is not already initializated
        if (is_null($this->getArReportScheduler()->getLastToDate())) {
            return false;
        }

        $lastToDate = fromMySQLTimestampToUnixTimestamp($this->getArReportScheduler()->getLastToDate());

        $nextFromDate = $lastToDate;
        $nextToDate = $this->getReportRangeToDate($nextFromDate);
        $nextRunDate = $nextToDate;

        $addHours = $this->getArReportScheduler()->getStartGenerationAfterXHours();
        if (is_null($addHours)) {
            $addHours = 0;
        }
        if ($addHours > 0) {
            $nextRunDate = strtotime('+' . $addHours . ' hours', $nextRunDate);
        }

        if ($atDate > $nextRunDate) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * @param int $date
     * @return string a date in "YYYY-MM-DD" format, without the time part
     */
    static public function getYYYYMMDD($date)
    {
        $yyyy = date('Y', $date);
        $mm = date('m', $date);
        $dd = date('d', $date);

        return "$yyyy-$mm-$dd";
    }

}
