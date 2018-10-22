<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell', 'CustomLocaleConversions'));


/**
 * Generate a report.
 * Objects of this class have no persistent state, because they are not saved after their execution.
 * So all the params for generating a report, must be retrieved from the report database object.
 */
abstract class ReportGenerator
{

    //////////////
    // ArReport //
    //////////////

    protected $schedulerId = null;

    /**
     * @return null|int
     */
    public function getSchedulerId() {
        return $this->schedulerId;
    }

    /**
     * @param int|null $id
     * @return void
     */
    public function setSchedulerId($id) {
        $this->schedulerId = $id;
    }

    /**
     * @var ArReport
     */
    protected $report;

    /**
     * @param ArReport $report
     */
    public function setArReport($report)
    {
        $this->report = $report;
    }

    /**
     * @return ArReport|null
     */
    public function getArReport()
    {
        return $this->report;
    }

    protected $rootoOrganizationLevel = null;

    /**
     * @return int 0 for reports associated to all root organizations/customer,
     * 1 for reports associated to some root customer,
     * 2 for reports associated to a child of a root customer,
     * and so on...
     */
    protected function getRootOrganizationLevel()
    {
        if (is_null($this->rootoOrganizationLevel)) {
            if (is_null($this->getOrganizationUnitIds())) {
                return 0;
            } else {
                $this->rootoOrganizationLevel = OrganizationUnitInfo::getFullIdsNestingLevel($this->getOrganizationUnitIds());
            }
        }
        return $this->rootoOrganizationLevel;
    }

    protected $directions = null;

    /**
     * @return int[]
     */
    protected function getAllowedDestinationTypes()
    {

        if (is_null($this->directions)) {

            $this->directions = array();
            if ($this->getArReport()->getParamShowAlsoOutgoingCalls()) {
                $this->directions[] = DestinationType::outgoing;
            }
            if ($this->getArReport()->getParamShowAlsoSystemCalls()) {
                $this->directions[] = DestinationType::system;
            }
            if ($this->getArReport()->getParamShowAlsoIncomingCalls()) {
                $this->directions[] = DestinationType::incoming;
            }
            if ($this->getArReport()->getParamShowAlsoInternalCalls()) {
                $this->directions[] = DestinationType::internal;
            }
        }

        return $this->directions;
    }

    /**
     * @var string|null the full ids
     */
    protected $organizationUnitIds = null;

    /**
     * @return null|string the full ids for the organization associated to the report,
     * null if the report is a root report, associated to all the root organizations/customers.
     */
    public function getOrganizationUnitIds()
    {
        if (is_null($this->organizationUnitIds)) {
            if (is_null($this->getArReport()->getArOrganizationUnitId())) {
                return null;
            } else {
                $this->organizationUnitIds = OrganizationUnitInfo::getInstance()->getFullIds($this->getArReport()->getArOrganizationUnitId(), fromMySQLTimestampToUnixTimestamp($this->getArReport()->getFromDate()));
            }
        }

        return $this->organizationUnitIds;
    }

    /////////////////////
    // ReportCalcStore //
    /////////////////////

    /**
     * @var ReportCalcStore|null $store
     */
    protected $store = null;

    /**
     * Set store with calcs.
     *
     * NOTE: if store is not set, it will be calculated from the report.
     * Setting the store explicitely is useful for sharing common calcs
     * between reports.
     *
     * @param ReportCalcStore|null $store
     *
     * precondition: $store timeframe is the same of report timeframe
     */
    public function setStore($store)
    {
        $this->store = $store;
    }

    /**
     * @return ReportCalcStore|null
     */
    public function getStore()
    {
        if (is_null($this->store)) {
            $this->store = $this->calcStore($this->getSchedulerId());
        }
        return $this->store;
    }

    /**
     * Return a calculated store.
     *
     * @param int|null $schedulerId
     * @return ReportCalcStore
     */
    abstract protected function calcStore($schedulerId);

    /////////////////////////
    // SHORT NAME SUPPORT //
    ////////////////////////

    /**
     * Read the params from app.yml configuration file.
     *
     * @param string $className a php class name for a report
     * @return string|null the report description
     */
    static public function getReportShortDescriptionByName($className)
    {
        $phpClassOptions = self::getAllReportsClassNameAndDescription();

        if (array_key_exists($className, $phpClassOptions)) {
            return $phpClassOptions[$className];
        } else {
            return null;
        }
    }

    /**
     * @return array class name, and short description
     */
    static public function getAllReportsClassNameAndDescription()
    {
        return sfConfig::get('app_available_phpReports');
    }

    /**
     * @return string
     */
    static public function getNameOfReportAssociatedToAllRootOrganizations() {
        if (sfConfig::get('app_is_voip_reseller')) {
            return mytr('all organizations');
        } else {
            return mytr('all customers');
        }
    }

    ///////////////
    // INTERFACE //
    ///////////////

    /**
     * The title of the report, except the info added from `getReportDescription()`.
     *
     * @return string
     */
    abstract public function getReportUserReadableName();

    /**
     * @return null|string the short name associated to the report
     */
    public function getReportShortDescription()
    {
        // assign the user specified name, or if it does not exists,
        // the standard report name.

        $reportName = $this->getArReport()->getReportName();
        if (isEmptyOrNull($reportName)) {
            return $this->getReportUserReadableName();
        } else {
            return $reportName;
        }
    }

    /**
     * A detailed description that depends from the params.
     * The short name for the report is associated in the configuration file.
     *
     * @return string empty string in case of report not yet initializated
     */
    public function getReportDescription()
    {
        // TODO questo nome di report deve diventare generico e divido in diverse parti

        $report = $this->getArReport();

        $startOrganizationId = $report->getArOrganizationUnitId();

        if (is_null($report->getFromDate())) {
            return '';
        }

        $fromDate = fromMySQLTimestampToUnixTimestamp($report->getFromDate());
        if (is_null($report->getToDate())) {
            $toDate = time();
        } else {
            $toDate = fromMySQLTimestampToUnixTimestamp($report->getToDate());
        }

        $s = array();
        if ($this->getShowAlsoOutgoingCalls()) {
            $s[] = DestinationType::outgoing;
        }

        if ($this->getShowAlsoIncomingCalls()) {
            $s[] = DestinationType::incoming;
        }

        if ($this->getShowAlsoInternalCalls()) {
            $s[] = DestinationType::internal;
        }


        if (!is_null($this->getArReport()->getArVendorId())) {
            $vendor = $this->getArReport()->getArVendor();
            if (!is_null($vendor)) {
                $vendorName = $vendor->getName();
            } else {
                $vendorName = '???';
            }
        } else {
            $vendorName = '';
        }

        if (is_null($startOrganizationId)) {
            $reportSubjectName = self::getNameOfReportAssociatedToAllRootOrganizations();
        } else {
            $reportSubjectName =  OrganizationUnitInfo::getInstance()->getFullNameAtDate($startOrganizationId, $fromDate, false, false);
        }

        return translateFilterOnReportHedaer($this->getReportShortDescription(), $s, $reportSubjectName, $vendorName, $fromDate, $toDate);
    }

    /**
     * @return string|null
     */
    public function getInfoAboutUnratedCDRs()
    {
        $report = $this->getArReport();

        list($correctCDRSByDirection, $errorsByDirection) = CustomCDRServices::getInstance()->getRatedCDRStats(fromMySQLTimestampToUnixTimestamp($report->getFromDate()), fromMySQLTimestampToUnixTimestamp($report->getToDate()), Propel::getConnection());

        // Signal the errror only if it is relevant to the data displayed in the report

        $showErrors = false;

        if (getFromStats($errorsByDirection, DestinationType::error) > 0) {
            $showErrors = true;
        }

        if (getFromStats($errorsByDirection, DestinationType::outgoing) > 0) {
            $showErrors = $showErrors || $this->getShowAlsoOutgoingCalls();
        }

        if (getFromStats($errorsByDirection, DestinationType::incoming) > 0) {
            $showErrors = $showErrors || $this->getShowAlsoIncomingCalls();
        }

        if (getFromStats($errorsByDirection, DestinationType::internal) > 0) {
            $showErrors = $showErrors || $this->getShowAlsoInternalCalls();
        }

        if ($showErrors) {
            $problemMsg = 'In the selected time frame ' . CustomCDRServices::getInstance()->getErrorDescription($errorsByDirection, null, null);
        } else {
            $problemMsg = null;
        }

        return $problemMsg;
    }

    /**
     * Set in $this->getArReport() the ProducedReport final params,
     * according the input params values.
     *
     * @return void
     */
    abstract protected function deriveReportParams();

    /**
     * Generate the report, after the report parameters were derived.
     *
     * Precondition: it is called only if the store contains some data
     * @return void
     */
    abstract protected function internalGenerateReport();

    /**
     * @param ArReport $report
     * @param PropelPDO $conn
     * @return void
     */
    public function generateReport(ArReport $report, PropelPDO $conn)
    {
        // first reset the report, because in case of errors,
        // the old report must not be considered as the new generated report
        $report->setProducedReportDocument(null);
        $report->setTotalWithoutTax(0);
        $report->setTotalWithTax(0);
        $report->setTax(0);
        $report->setAppliedVat(0);
        $report->save($conn);

        $fromDate = fromMySQLTimestampToUnixTimestamp($report->getFromDate());

        // process the report request
        $this->setArReport($report);
        $this->deriveReportParams();
        $this->generateOnlyNames();
        $report->setProducedReportGenerationDate(time());
        $report->setProducedReportAlreadyReviewed(0);
        $report->setProducedReportIsDraft($this->calcIfReportIsDraft($conn));
        $report->setCachedParentIdHierarchy(OrganizationUnitInfo::getInstance()->getFullIds($report->getArOrganizationUnitId(), $fromDate));
        if (!$this->getStore()->isEmpty()) {
            $this->internalGenerateReport();
        }
        $report->save($conn);

        ArReportPeer::publishReportToUsers($report->getId(), $report->getProducedReportAlreadyReviewed(), true, $conn);
    }

    /**
     * @return void
     */
    public function generateOnlyNames()
    {
        $this->getArReport()->setProducedReportShortDescription($this->getReportShortDescription());
        $this->getArReport()->setProducedReportAdditionalDescription($this->getReportDescription());
    }

    //////////////////////////
    // Access Report Params //
    //////////////////////////

    public function getShowCallDetails()
    {
        return $this->getArReport()->getParamShowCallDetails();
    }

    public function getShowAlsoOutgoingCalls()
    {
        return $this->getArReport()->getParamShowAlsoOutgoingCalls();
    }

    public function getShowAlsoIncomingCalls()
    {
        return $this->getArReport()->getParamShowAlsoIncomingCalls();
    }

    public function getShowAlsoInternalCalls()
    {
        return $this->getArReport()->getParamShowAlsoInternalCalls();
    }

    public function getShowCallCost()
    {
        return $this->getArReport()->getParamShowCallCost()
                || $this->getArReport()->getParamShowCallIncome()
                || $this->getArReport()->getParamShowCostSaving();
    }

    public function getShowVoipProvider()
    {
        return $this->getArReport()->getParamShowVoipProvider();
    }

    public function getShowMaskedTelephoneNumbers()
    {
        return $this->getArReport()->getParamShowMaskedTelephoneNumbers();
    }

    public function getShowCostSaving()
    {
        return $this->getArReport()->getParamShowCostSaving();
    }

    public function getShowCommunicationChannel()
    {
        return $this->getArReport()->getParamShowCommunicationChannel();
    }

    const SHOW_COST = 1;
    const SHOW_INCOME = 2;
    const SHOW_COST_SAVING = 3;
    const SHOW_EARN = 4;

    protected $cached_showType = null;

    /**
     * @return int[] self::SHOW_COST and so on...
     */
    public function getShowType()
    {

        if (is_null($this->cached_showType)) {


            $r = array();

            if ($this->getArReport()->getParamShowCallIncome()) {
                $r[] = self::SHOW_INCOME;
            }

            if ($this->getArReport()->getParamShowCallCost()) {
                $r[] = self::SHOW_COST;
            }

            if ($this->getArReport()->getParamShowCostSaving()) {
                $r[] = self::SHOW_COST_SAVING;
            }

            if ($this->getArReport()->getParamShowCallCost() && $this->getArReport()->getParamShowCallIncome()) {
                $r[] = self::SHOW_EARN;
            }

            $this->cached_showType = $r;
        }

        return $this->cached_showType;
    }

    const SHOW_NO_VENDOR = 0;
    const SHOW_VENDOR = 1;
    const SHOW_CHANNEL = 2;
    const SHOW_GEOGRAPHIC_LOCATION = 3;
    const SHOW_CONNECTION_TYPE = 4;
    const SHOW_DIRECTION = 5;

    /**
     * @return int self::SHOW_VENDOR and so on...
     */
    public function getShowVendorOrChannel()
    {
        if ($this->getArReport()->getParamShowCommunicationChannel() == true) {
            return self::SHOW_CHANNEL;
        } else if ($this->getArReport()->getParamShowVoipProvider() == true) {
            return self::SHOW_VENDOR;
        } else {
            return self::SHOW_NO_VENDOR;
        }
    }

    /**
     * @return bool true if both outgoing and incoming calls are displayed
     */
    public function getBothOutgoingAndIncoming()
    {
        return ($this->getArReport()->getParamShowAlsoIncomingCalls() && $this->getArReport()->getParamShowAlsoOutgoingCalls());
    }

    protected $reportIsDraftCachedResult = null;

    /**
     * @param PropelPDO $conn
     * @return bool true if there are unprocessed CDRs in the report timeframe
     */
    public function calcIfReportIsDraft(PropelPDO $conn)
    {

        if (is_null($this->reportIsDraftCachedResult)) {

            $report = $this->getArReport();

            $dateFilter = getArCdrCalldateFilter(fromMySQLTimestampToUnixTimestamp($report->getFromDate()), fromMySQLTimestampToUnixTimestamp($report->getToDate()));

            $query = 'select count(id) as nr from ar_cdr where destination_type = ' . DestinationType::error . ' and ' . $dateFilter;
            $stmt = $conn->prepare($query);
            $stmt->execute();
            $result = $stmt->fetchColumn();
            $processed = intval($result);
            if ($processed > 0) {
                $this->reportIsDraftCachedResult = true;
            } else {
                $this->reportIsDraftCachedResult = false;
            }
        }

        return $this->reportIsDraftCachedResult;
    }

    ///////////////////////
    // UTILITY FUNCTIONS //
    ///////////////////////

    /**
     * @param string $description
     * @throws ArProblemException
     */
    protected function signalProblem($description)
    {
        $report = $this->getArReport();
        if (is_null($report)) {
            $reportId = '<unspecified>';
        } else {
            $reportId = $report->getId();
        }

        $p = ArProblemException::createWithoutGarbageCollection(
            ArProblemType::TYPE_ERROR,
            ArProblemDomain::REPORTS,
            null,
                get_class($this) . ' - ' . rand(),
                "Error during generation of report $reportId. , in class " . get_class($this) . ', ' . $description,
            "The report with ID $reportId is not generated.",
            "Fix the report params according the description, and regenerate the report."
        );

        throw($p);
    }

    /**
     * @throws ArProblemException
     */
    protected function checkArReportExists()
    {
        $report = $this->getArReport();
        if (is_null($report)) {
            $this->signalProblem('Specify also the type of report to produce.');
        }
    }

    protected function checkParamArOrganizationUnitIdExists()
    {
        $this->checkArReportExists();
        if (is_null($this->getArReport()->getArOrganizationUnitId())) {
            $this->signalProblem('Specify also the organization associated to the report.');
        }
    }

    protected function checkParamArUserIdExists()
    {
        $this->checkArReportExists();
        if (is_null($this->getArReport()->getArUser())) {
            $this->signalProblem('Specify also the user associated to the report.');
        }
    }

    protected function checkParamFromDateExists()
    {
        $this->checkArReportExists();
        if (is_null($this->getArReport()->getFromDate())) {
            $this->signalProblem('Specify also the first date range, of the calls of the report.');
        }
    }

    protected function checkParamToDateExists()
    {
        $this->checkArReportExists();
        if (is_null($this->getArReport()->getToDate())) {
            $this->signalProblem('Specify also the last date range, of the calls of the report.');
        }
    }

    protected function checkVoIPVendorOrCommunicationChannelIsSpecified()
    {
        $this->checkArReportExists();

        $c = 0;

        if ($this->getArReport()->getParamShowCommunicationChannel()) {
            $c++;
        }

        if ($this->getArReport()->getParamShowVoipProvider()) {
            $c++;
        }

        if ($c != 1) {
            $this->signalProblem('This report type need to work on VoIP providers, or Communication Channel. Specify one of the two parameters.');
        }
    }

}
