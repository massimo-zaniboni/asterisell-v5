<?php

/* $LICENSE 2012, 2013:
 *
 * Copyright (C) 2012, 2013 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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
 * Generate different types of billing reports, but with a common structure,
 * according the params.
 *
 * CDR_system calls are in the report only if the report can display COSTS.
 */
abstract class BaseBillingReport extends ReportGenerator
{

    /**
     * @return BaseBillingReportCalcStore|null
     */
    public function getStore()
    {
        return parent::getStore();
    }

    public function getCharacterSet()
    {
        return "windows-1252";
    }

    public function calcStore($schedulerId)
    {

        $conn = Propel::getConnection();

        $report = $this->getArReport();

        $fromDate = fromMySQLTimestampToUnixTimestamp($report->getFromDate());
        if (!is_null($report->getToDate())) {
            $toDate = fromMySQLTimestampToUnixTimestamp($report->getToDate());
        } else {
            $toDate = null;
        }

        $store = new BaseBillingReportCalcStore();
        $store->setCharacterSet($this->getCharacterSet());
        $store->process($fromDate, $toDate, $conn);
        return $store;
    }

    /**
     * Sum all the values.
     * The params, if null, will always be matched with specific report filters,
     * if not null, it is assumed that they have a correct/compatible value.
     *
     * @param string|null $ids
     * @param int|null $destinationType null for summing all values
     * @param int|null $vendorId null for summing all values
     * @param int|null $communicationChannelId null for summing all values
     * @param int|null $geographicLocation
     * @param int|null $operatorType
     * @return array values: list(count of calls, duration, cost, income, cost savings)
     */
    public
    function getStoreValues($ids, $destinationType, $vendorId, $communicationChannelId, $geographicLocation, $operatorType)
    {
        assert((!is_null($ids)) || is_null($this->getOrganizationUnitIds()));

        if (is_null($vendorId)) {
            $vendorId = $this->getArReport()->getArVendorId();
        }

        if (is_null($destinationType)) {
            // sum all the allowed valuse, according the filter of the report

            $totCount = 0;
            $totDuration = 0;
            $totCost = 0;
            $totIncome = 0;
            $totSavings = 0;

            foreach ($this->getAllowedDestinationTypes() as $direction) {

                list($totCount1, $totDuration1, $totCost1, $totIncome1, $totSavings1) = $this->getStore()->getValues($ids, $direction, $vendorId, $communicationChannelId, $geographicLocation, $operatorType);

                $totCount += $totCount1;
                $totDuration += $totDuration1;
                $totCost += $totCost1;
                $totIncome += $totIncome1;
                $totSavings += $totSavings1;

            }

            return array($totCount, $totDuration, $totCost, $totIncome, $totSavings);

        } else {
            return $this->getStore()->getValues($ids, $destinationType, $vendorId, $communicationChannelId, $geographicLocation, $operatorType);
        }
    }

    ///////////////////////////////////////
    // ReportGenerator Interface Support //
    ///////////////////////////////////////

    public function getReportUserReadableName()
    {
        return mytr('Billing Report');
    }

    ///////////////////////////////////////////
    // Customizable Functions by Report Type //
    ///////////////////////////////////////////

    protected $cached_reportNamesOfCostColumn = null;

    /**
     * @return string[] the names to use for the cost column
     */
    protected function getReportNamesOfCostColumn()
    {

        if (is_null($this->cached_reportNamesOfCostColumn)) {

            $toShow = $this->getShowType();

            $isThereCost = in_array(self::SHOW_COST, $toShow);
            $isThereIncome = in_array(self::SHOW_INCOME, $toShow);

            $r = array();
            foreach ($toShow as $type) {
                if ($type == self::SHOW_COST_SAVING) {
                    $r[] = mytr("Cost Saving");
                } else if ($type == self::SHOW_EARN) {
                    $r[] = mytr("Earn");
                } else if ($type == self::SHOW_INCOME) {
                    if ($isThereCost && $isThereIncome) {
                        $r[] = mytr("Income");
                    } else {
                        $r[] = mytr("Cost");
                    }
                } else if ($type == self::SHOW_COST) {
                    $r[] = mytr("Cost");
                } else {
                    return '?';
                }
            }

            $this->cached_reportNamesOfCostColumn = $r;
        }

        return $this->cached_reportNamesOfCostColumn;
    }

    ///////////////////////
    // Report Generation //
    ///////////////////////

    protected function internalGenerateReport()
    {
        $this->makeCallsByOrganizationReport();
    }

    /**
     * A report showing info about the calls made from organizations,
     * and some totals on VoIP providers.
     *
     * Report layout for organization calls, is something like this:
     *
     * > Organization   | Calls |   Duration    | Cost
     * >
     * > A              |   10  |   100         | 100
     * > A.B            |   5   |   50          |  50
     * > A.C            |   5   |   50          |  50
     * >
     * > A.B            |   5   |   50          |  50
     * > A.B.R1         |   2   |   25          |  25
     * > A.B.R2         |   2   |   20          |  20
     * > A.B.R3         |   1   |   5           |  5
     *
     * This layout permits viewing for each organization, the ripartitation of calls with his children organizations.
     * It adopts a breath first approach.
     *
     * For each extensions, there is a list of call details, if requested.
     *
     * @return void
     * @throws ArProblemException
     *
     */
    protected function makeCallsByOrganizationReport()
    {
        $conn = Propel::getConnection();

        $report = $this->getArReport();
        $isDraftReport = $report->getProducedReportIsDraft();
        $startOrganizationId = $this->getArReport()->getArOrganizationUnitId();
        $maxChildrenLevel = $this->getArReport()->getParamExpandToLevel();
        if (is_null($maxChildrenLevel)) {
            $maxChildrenLevel = 0;
        }

        $orderCode = $this->getArReport()->getArReportOrderOfChildrenId();

        $fromDate = fromMySQLTimestampToUnixTimestamp($report->getFromDate());
        if (!is_null($report->getToDate())) {
            $toDate = fromMySQLTimestampToUnixTimestamp($report->getToDate());
        } else {
            $toDate = null;
        }

        /////////////////
        // Write Stats //
        /////////////////

        $title = $this->convertString($this->getReportDescription());

        $pdf = new BillingReportPDFTemplate();
        $pdf->setIsDraft($isDraftReport);
        $pdf->SetTitle($title);
        $pdf->setReportHeaderLine1($title);
        $pdf->AliasNbPages();

        // Configure the logo of the report owner

        $params = ArParamsPeer::getDefaultParams();
        $senderName = $params->getLegalName();
        $senderAddress = getCustomerAddressAccordingCulture(
            $params->getLegalCountry(),
            $params->getLegalStateProvince(),
            $params->getLegalCity(),
            $params->getLegalAddress(),
            $params->getLegalZipcode(),
            $params->getVat());

        $logoFileName = $params->getLogoImageInInvoicesWithCompletePath();
        $pdf->setCompanyInfo(
            $this->convertString($senderName),
            $this->convertString($senderAddress
                . maybeAddIfExistsCentral("\nemail: ", $params->getLegalEmail(), "")
                . maybeAddIfExistsCentral("\n" . mytr("tel:"), $params->getLegalPhone(), "")
                . maybeAddIfExistsCentral("\nfax: ", $params->getLegalFax(), "")
            ), $logoFileName);

        $pdf->AddPage();

        // Set Normal Info

        // Disable info about unrated calls
        // $errorInfo = $this->getInfoAboutUnratedCDRs();
        // if (!isEmptyOrNull($errorInfo)) {
        //     $pdf->addErrorInfo($this->convertString($errorInfo));
        // }
        $pdf->addInitialNotesInfo(mytr('Note: all costs are VAT excluded.'));

        $this->setTableHeaderForCallsSummary($pdf, false);

        /**
         * These are the userAndOrganizationKey to display in the report. It is a queue, used for implementing
         * a breadth first display of data.
         * @var int[] $idsToDisplayQueue
         */

        $idsToDisplayQueue = array();
        if (is_null($this->getOrganizationUnitIds())) {
            $idsToDisplayQueue[] = null;
            $startFromSuperRoot = true;
            // NOTE: do not change $maxChildrenLevel
        } else {
            $startFromSuperRoot = false;
            $idsToDisplayQueue[] = $this->getOrganizationUnitIds();
            if ($maxChildrenLevel > 0) {
                $maxChildrenLevel = OrganizationUnitInfo::getFullIdsNestingLevel($this->getOrganizationUnitIds()) + $maxChildrenLevel - 1;
            }
        }

        if ($startFromSuperRoot) {
            $startingLevel = 1;
        } else {
            $startingLevel = 0;
        }

        /**
         * @var bool[string] $organizationKeysAlreadyDisplayed true for organization that are already displayed in the report
         */
        $idsAlreadyDisplayed = array();

        foreach ($idsToDisplayQueue as $key) {
            if (!is_null($key)) {
                $idsAlreadyDisplayed[$key] = true;
            }
        }

        // Display each organization with his direct children organizations details.

        while (count($idsToDisplayQueue) > 0) {
            $currentIds = array_shift($idsToDisplayQueue);

            if (is_null($currentIds) || $maxChildrenLevel == 0 || (OrganizationUnitInfo::getFullIdsNestingLevel($currentIds) + $startingLevel) <= $maxChildrenLevel) {

                if ((!is_null($currentIds)) && isset($this->getStore()->idsWithDirectCalls[$currentIds])
                    && $this->getStore()->idsWithDirectCalls[$currentIds]
                ) {
                    $idsWithDirectCalls[$currentIds] = true;
                }

                // show the header with a complete name
                $this->reportOrganizationStats($pdf, $currentIds, null, false);

                foreach ($this->getOrderedDirectChildren($currentIds, $orderCode) as $childIds => $ignore) {
                    // NOTE: childIds is a unique key for an entire hierarchy

                    // show details, omitting the parent name
                    $this->reportOrganizationStats($pdf, $childIds, $currentIds, true);

                    if ($this->getStore()->hasChildren($childIds) && (!array_key_exists($childIds, $idsAlreadyDisplayed))) {
                        $idsAlreadyDisplayed[$childIds] = true;

                        $idsToDisplayQueue[] = $childIds;
                        // in next phases display also the details of the child with his children
                    }
                }
                $pdf->addTableRowSeparator();
            }
        }

        // Write call details

        $pdf->setDefaultBorderMargin();

        if ($this->getShowCallDetails()) {

            // Order ids with direct calls according their full name

            $rootLevel = $this->getRootOrganizationLevel();
            $rootIds = $this->getOrganizationUnitIds();

            $orderedIds = array();
            foreach ($this->getStore()->getChildrenIdsWithDirectCalls($rootIds) as $ids) {
                $orderedIds[$ids] = $this->getStore()->getCompleteNameInAFastWay($rootLevel, $ids, true);
            }

            asort($orderedIds, SORT_LOCALE_STRING);

            // Display info

            foreach ($orderedIds as $ids => $ignore) {
                $this->reportCallDetails($pdf, $ids);
            }
        }

        // Save the PDF document
        $pdfReportContent = $pdf->Output("", "S");
        $report->setProducedReportAlreadyReviewed(false);
        $report->setProducedReportIsDraft($isDraftReport);
        $report->setProducedReportMimeType('application/pdf');
        $report->setDocumentContent($pdfReportContent);
        $report->setProducedReportFileTypeSuffix('pdf');
        $report->save();

        // TODO: add links to the organization to the details of child organization and to the list of calls

    }

    /**
     * Display the global stats about a organization and his direct children.
     *
     * @param BillingReportPDFTemplate $pdf
     * @param string|null $currentIds organization stats to display, null for the root of all organizations
     * @param string|null $parentIds organization parent
     * @param bool $isDetail true for displaying detail info, true for displaying the main header with the totals
     */
    protected function reportOrganizationStats(BillingReportPDFTemplate $pdf, $currentIds, $parentIds, $isDetail)
    {
        // Write stats of the $currentIds

        if (is_null($currentIds)) {
            $currentName = self::getNameOfReportAssociatedToAllRootOrganizations();
        } else {
            $currentName = $this->getStore()->getCompleteNameInAFastWay($this->getRootOrganizationLevel() - 1, $currentIds, true);
        }

        if ((!is_null($parentIds)) && (!is_null($currentIds))) {
            $parentName = $this->getStore()->getCompleteNameInAFastWay($this->getRootOrganizationLevel() - 1, $parentIds, true);
            $relativeName = '..' . substr($currentName, strlen($parentName));
        } else {
            $relativeName = $currentName;
        }

        if (!$isDetail) {
            $pdf->addTableLongLineName($this->convertString($relativeName));
            $pdf->addTableHeader(false);
        }

        /**
         * @var string[] $row
         */
        $row = array();

        list($totCalls, $totDuration, $totCost, $totIncome, $totSavings) = $this->getStoreValues($currentIds, null, null, null, null, null);

        if (!$isDetail) {
            $row[] = $this->convertString(mytr('Totals:'));
        } else {
            $row[] = $this->convertString($relativeName);
        }
        $row[] = $totCalls;
        $row[] = $this->convertString(format_minute($totDuration));

        foreach ($this->getShowType() as $type) {
            if ($type == self::SHOW_COST_SAVING) {
                $row[] = $this->convertString(from_db_decimal_to_pdf_txt_decimal($totSavings));
            } else if ($type == self::SHOW_COST) {
                $row[] = $this->convertString(from_db_decimal_to_pdf_txt_decimal($totCost));
            } else if ($type == self::SHOW_INCOME) {
                $row[] = $this->convertString(from_db_decimal_to_pdf_txt_decimal($totIncome));
            } else if ($type == self::SHOW_EARN) {
                $row[] = $this->convertString(from_db_decimal_to_pdf_txt_decimal($totIncome - $totCost));
            } else {
                $row[] = '?';
            }
        }
        $pdf->addTableData($row, !$isDetail, false);

        // Write stats grouped by vendor and other params

        if (!$isDetail) {

            $infoToShow = array();
            if (count($this->getAllowedDestinationTypes()) > 1) {
                $infoToShow[] = self::SHOW_DIRECTION;
            }
            if ($this->getShowVoipProvider()) {
                $infoToShow[] = self::SHOW_VENDOR;
            }
            if ($this->getShowCommunicationChannel()) {
                $infoToShow[] = self::SHOW_CHANNEL;
            }
            if ($this->getArReport()->getParamShowGeographicLocation()) {
                $infoToShow[] = self::SHOW_GEOGRAPHIC_LOCATION;
            }
            if ($this->getArReport()->getParamShowConnectionType()) {
                // this info is not displayed
            }

            foreach ($infoToShow as $currentInfoToShow) {

                if ($currentInfoToShow == self::SHOW_DIRECTION) {
                    $groupTitle = mytr('Call directions');
                    $orderedGroups = $this->getOrderedDirections($currentIds);
                } else if ($currentInfoToShow == self::SHOW_CHANNEL) {
                    $groupTitle = mytr('Channels');
                    $orderedGroups = $this->getOrderedCommunicationChannels($currentIds);
                } else if ($currentInfoToShow == self::SHOW_VENDOR) {
                    $groupTitle = mytr('Vendors');
                    $orderedGroups = $this->getOrderedVendors($currentIds);
                } else if ($currentInfoToShow == self::SHOW_GEOGRAPHIC_LOCATION) {
                    $groupTitle = mytr('Locations');
                    $orderedGroups = $this->getOrderedGeographicLocations($currentIds);
                } else {
                    $this->signalProblem('Error in the code. Contact the assistance. Unrecognized info to display' . $currentInfoToShow);
                    break;
                }

                $row = array();
                $row[] = $this->convertString($groupTitle);
                $row[] = '';
                $row[] = '';
                foreach ($this->getShowType() as $type) {
                    $row[] = '';
                }
                $pdf->addTableData($row, false, true);

                foreach ($orderedGroups as $groupKey => $ignore) {

                    // NOTE: $vendorId can be a vendor or a channel according the type of report

                    $continue = true;
                    if ($currentInfoToShow == self::SHOW_DIRECTION) {
                        $groupName = DestinationType::getName($groupKey);
                        list($partialCalls, $partialDuration, $partialCost, $partialIncome, $partialSavings) = $this->getStoreValues($currentIds, $groupKey, null, null, null, null);
                    } else if ($currentInfoToShow == self::SHOW_CHANNEL) {
                        $groupObject = ArCommunicationChannelTypePeer::retrieveByPK($groupKey);
                        $groupName = $groupObject->getName();
                        list($partialCalls, $partialDuration, $partialCost, $partialIncome, $partialSavings) = $this->getStoreValues($currentIds, null, null, $groupKey, null, null);
                    } else if ($currentInfoToShow == self::SHOW_VENDOR) {
                        $groupObject = ArVendorPeer::retrieveByPK($groupKey);
                        if ($groupObject->getIsInternal()) {
                            $continue = false;
                        }
                        $groupName = $groupObject->getName();
                        list($partialCalls, $partialDuration, $partialCost, $partialIncome, $partialSavings) = $this->getStoreValues($currentIds, null, $groupKey, null, null, null);
                    } else if ($currentInfoToShow == self::SHOW_GEOGRAPHIC_LOCATION) {
                        $groupName = $groupKey;
                        list($partialCalls, $partialDuration, $partialCost, $partialIncome, $partialSavings) = $this->getStoreValues($currentIds, null, null, null, $groupKey, null);
                    } else {
                        $this->signalProblem('Error in the code. Contact the assistance. Unrecognized info to display' . $currentInfoToShow);
                        break;
                    }

                    if ($continue) {

                        $row = array();
                        $row[] = $this->convertString($groupName);
                        $row[] = $this->convertString($partialCalls);
                        $row[] = $this->convertString(format_minute($partialDuration));

                        foreach ($this->getShowType() as $type) {
                            if ($type == self::SHOW_COST_SAVING) {
                                $row[] = $this->convertString(from_db_decimal_to_pdf_txt_decimal($partialSavings));
                            } else if ($type == self::SHOW_COST) {
                                $row[] = $this->convertString(from_db_decimal_to_pdf_txt_decimal($partialCost));
                            } else if ($type == self::SHOW_INCOME) {
                                $row[] = $this->convertString(from_db_decimal_to_pdf_txt_decimal($partialIncome));
                            } else if ($type == self::SHOW_EARN) {
                                $row[] = $this->convertString(from_db_decimal_to_pdf_txt_decimal($partialIncome - $partialCost));
                            } else {
                                $row[] = '?';
                            }
                        }
                        $pdf->addTableData($row, false, false);
                    }
                }
            }

            $row = array();
            $row[] = $this->convertString(mytr('Child organizations'));
            $row[] = '';
            $row[] = '';
            foreach ($this->getShowType() as $type) {
                $row[] = '';

            }
            $pdf->addTableData($row, false, true);
        }
    }

    /**
     * @var PDOStatement|null
     */
    protected $callDetailsStmt = null;

    /**
     * @var int|null
     */
    protected $cachedMaxCallsInReport = null;

    /**
     * Display the calls details of the most specific user and organization.
     *
     * @param BillingReportPDFTemplate $pdf
     * @param string $ids full ids of the organization for wich display the calls
     * @return void
     */
    protected function reportCallDetails(BillingReportPDFTemplate $pdf, $ids)
    {
        $pageWidth = $pdf->getPageContentWidth();

        // title
        $currentName = $this->getStore()->getCompleteNameInAFastWay($this->getRootOrganizationLevel() - 1, $ids, true);
        $pdf->addTableLongLineName(mytr('Call details of ') . $this->convertString($currentName), true);

        // Call Details Header
        $tableHeader = array();
        $tableAlign = array();
        $displayCol = array();
        $tableWidth = array();
        $tableAlignVendor = array();

        $dataWidth = 14;
        $nameWidth = 30;

        $i = 0;
        $telNrIndex = $i;
        $tableHeader[$i] = mytr('Tel.Nr.');
        $tableAlign[$i] = 'L';
        $tableAlignVendor[$i] = 'L';
        $displayCol[$i] = true;
        $tableWidth[$i] = $nameWidth;

        $i++;
        $tableHeader[$i] = mytr('Dir.');
        $tableAlignVendor[$i] = 'C';
        $tableAlign[$i] = 'C';
        $displayCol[$i] = $this->getBothOutgoingAndIncoming();
        $tableWidth[$i] = 10;

        $i++;
        $locationIndex = $i;
        $tableHeader[$i] = mytr('Location');
        $tableAlign[$i] = 'L';
        $tableAlignVendor[$i] = 'L';
        $displayCol[$i] = true;
        $tableWidth[$i] = $nameWidth;

        $i++;
        $tableHeader[$i] = mytr('Connection');
        $tableAlign[$i] = 'L';
        $tableAlignVendor[$i] = 'L';
        $displayCol[$i] = true;
        $tableWidth[$i] = $nameWidth / 2;

        $i++;
        $tableHeader[$i] = mytr('Date');
        $tableAlign[$i] = 'L';
        $tableAlignVendor[$i] = 'L';
        $displayCol[$i] = true;
        $tableWidth[$i] = $dataWidth + $dataWidth;

        $i++;
        $tableHeader[$i] = mytr('Duration');
        $tableAlign[$i] = 'R';
        $tableAlignVendor[$i] = 'R';
        $displayCol[$i] = true;
        $tableWidth[$i] = $dataWidth;

        foreach ($this->getReportNamesOfCostColumn() as $typeName) {
            $i++;
            $tableHeader[$i] = $typeName;
            $tableAlign[$i] = 'R';
            $tableAlignVendor[$i] = 'R';
            $displayCol[$i] = true;
            $tableWidth[$i] = $dataWidth;
            $displayCol[$i] = true;
        }

        $i++;
        if ($this->getShowVoipProvider()) {
            $tableHeader[$i] = mytr('Vendor');
            $tableAlign[$i] = 'L';
            $tableAlignVendor[$i] = 'L';
            $displayCol[$i] = true;
            $tableWidth[$i] = $nameWidth;
            $i++;
        }

        if ($this->getShowCommunicationChannel()) {
            $tableHeader[$i] = mytr('Channel');
            $tableAlign[$i] = 'L';
            $tableAlignVendor[$i] = 'L';
            $displayCol[$i] = true;
            $tableWidth[$i] = $nameWidth;
            $i++;
        }

        // adjust column size for using all the space
        $usedWidth = 0;
        foreach ($tableWidth as $i => $w) {
            if ($displayCol[$i]) {
                $usedWidth += $w;
            }
        }
        $remainingWidth = $pageWidth - $usedWidth;
        $tableWidth[$telNrIndex] += $remainingWidth;
        $usedWidth = $pageWidth;

        $pdf->setTableAligns($tableAlign);
        $pdf->setTableAlignsVendor($tableAlignVendor);
        $pdf->setTableDisplay($displayCol);
        $pdf->setTableHeader($tableHeader);
        $pdf->setTableWidths($tableWidth);

        $pdf->addTableHeader(true);

        //
        // Retrieve calls
        //

        if (is_null($this->callDetailsStmt)) {
            $this->cachedMaxCallsInReport = trim(sfConfig::get('app_max_calls_in_pdf_report_section'));
            if (isEmptyOrNull($this->cachedMaxCallsInReport)) {
                $this->cachedMaxCallsInReport = 100;
            } else {
                $this->cachedMaxCallsInReport = intval($this->cachedMaxCallsInReport);
            }
            $additionalQueryCond = 'AND ar_cdr.cached_parent_id_hierarchy = ?';

            $conn = Propel::getConnection();
            $query = $this->getQueryOnCdrs($additionalQueryCond, $this->cachedMaxCallsInReport);
            $this->callDetailsStmt = $conn->prepare($query);
        }

        $this->callDetailsStmt->execute(array($ids));

        $count = 0;
        while (($rs = $this->callDetailsStmt->fetch(PDO::FETCH_ASSOC)) !== false) {

            $count++;

            $row = array();

            $callDate = fromMySQLTimestampToUnixTimestamp($rs['calldate']);

            if ($this->getShowMaskedTelephoneNumbers()) {
                $row[] = $this->convertString($rs['cached_masked_external_telephone_number']);
            } else {
                $row[] = $this->convertString($rs['cached_external_telephone_number']);
            }

            if ($rs['destination_type'] == DestinationType::incoming) {
                $row[] = 'in';
            } else if ($rs['destination_type'] == DestinationType::outgoing) {
                $row[] = 'out';
            } else if ($rs['destination_type'] == DestinationType::internal) {
                $row[] = 'int';
            } else {
                $row[] = ' ';
            }

            $row[] = $this->convertString($rs['geographic_location']);
            $row[] = $this->convertString($rs['operator_type']);
            $row[] = $this->convertString(format_unixtimestamp_according_config($callDate));
            $row[] = $this->convertString(format_minute($rs['duration']));

            foreach ($this->getShowType() as $type) {

                if ($type == self::SHOW_COST_SAVING) {
                    $v = $rs['cost_saving'];
                } else if ($type == self::SHOW_COST) {
                    $v = $rs['cost'];
                } else if ($type == self::SHOW_INCOME) {
                    $v = $rs['income'];
                } else if ($type == self::SHOW_EARN) {
                    $v = intval($rs['income']) - intval($rs['cost']);
                } else {
                    $v = '?';
                }

                $row[] = $this->convertString(from_db_decimal_to_pdf_txt_decimal($v));
            }

            if ($this->getShowVoipProvider()) {
                $row[] = $this->convertString($rs['vendor_name']);
            }

            if ($this->getShowCommunicationChannel()) {
                $row[] = $this->convertString($rs['channel_name']);
            }

            $pdf->addTableData($row, false, false, true);
        }

        $this->callDetailsStmt->closeCursor();

        if ($count == $this->cachedMaxCallsInReport) {
            $row = array();

            $row[] = mytr('and more calls') . '...';
            $row[] = '';
            $row[] = '';
            $row[] = '';
            $row[] = '';
            $row[] = '';
            $row[] = '';
            $row[] = '';
            $row[] = '';

            $pdf->addTableData($row, false, false, true);
        }

        $pdf->addTableRowSeparator();
    }

    /**
     * Set table headers.
     *
     * @param BillingReportPDFTemplate $pdf
     * @param bool $isUserAtForm true for using "User @ Organization" form.
     */
    public function setTableHeaderForCallsSummary(BillingReportPDFTemplate $pdf, $isUserAtForm)
    {
        $tableHeader = array();
        $tableAlign = array();
        $displayCol = array();
        $tableWidth = array();
        $tableAlignVendor = array();

        $callNrWidth = 12;
        $otherWidth = 22;

        // organization name
        $i = 0;
        $organizationIndex = $i;
        $tableHeader[$i] = '';
        $tableAlignVendor[$i] = 'R';
        $tableAlign[$i] = 'R';
        $displayCol[$i] = true;
        $tableWidth[$i] = 60;

        $i++;
        $tableHeader[$i] = mytr('Nr');
        $tableAlign[$i] = 'C';
        $tableAlignVendor[$i] = 'C';
        $displayCol[$i] = true;
        $tableWidth[$i] = $callNrWidth;

        $i++;
        $tableHeader[$i] = mytr('Duration');
        $tableAlign[$i] = 'R';
        $tableAlignVendor[$i] = 'R';
        $displayCol[$i] = true;
        $tableWidth[$i] = $otherWidth;

        foreach ($this->getReportNamesOfCostColumn() as $typeName) {
            $i++;
            $tableHeader[$i] = $typeName;
            $tableAlign[$i] = 'R';
            $tableAlignVendor[$i] = 'R';
            $displayCol[$i] = true;
            $tableWidth[$i] = $otherWidth;
        }

        $totCols = $i + 1;

        // use all the page size
        $usedWidth = 0;
        foreach ($tableWidth as $i => $w) {
            if ($displayCol[$i]) {
                $usedWidth += $w;
            }
        }

        if ($usedWidth < $pdf->getPageContentWidth()) {
            $increaseWidth = intval(($pdf->getPageContentWidth() - $usedWidth) / ($totCols * 2));

            for ($i = 0;
                 $i < $totCols;
                 $i++) {

                if ($i == $organizationIndex) {
                    // assign to first column (the more important) the more space
                    $tableWidth[$i] += ($increaseWidth * $totCols);
                } else {
                    $tableWidth[$i] += $increaseWidth;
                }
            }

            // center table again
            $usedWidth = 0;
            foreach ($tableWidth as $i => $w) {
                if ($displayCol[$i]) {
                    $usedWidth += $w;
                }
            }

            // assign the remaining space to the first column
            $tableWidth[$organizationIndex] += ($pdf->getPageContentWidth() - $usedWidth);
        }

        // set table

        $pdf->setTableAligns($tableAlign);
        $pdf->setTableAlignsVendor($tableAlignVendor);
        $pdf->setTableDisplay($displayCol);
        $pdf->setTableHeader($tableHeader);
        $pdf->setTableWidths($tableWidth);
    }

    /**
     * @param string $additionalConditions additional SQL conditions starting with "AND "
     * @param int $maxCallsInReport
     * @return string SQL query retrieving CDRs
     */
    protected function getQueryOnCdrs($additionalConditions, $maxCallsInReport = 0)
    {
        $conn = Propel::getConnection();

        // Create a filter on call directions

        $filterOnOutgoing = 'ar_cdr.destination_type = ' . DestinationType::outgoing;
        $filterOnIncoming = 'ar_cdr.destination_type = ' . DestinationType::incoming;
        $filterOnInternal = 'ar_cdr.destination_type = ' . DestinationType::internal;
        $filterOnSystem   = 'ar_cdr.destination_type = ' . DestinationType::system;

        $filterOnDirection = '(false ';
        if ($this->getShowAlsoOutgoingCalls()) {
            $filterOnDirection .= "OR $filterOnOutgoing ";
        }

        if ($this->getShowAlsoIncomingCalls()) {
            $filterOnDirection .= "OR $filterOnIncoming ";
        }

        if ($this->getShowAlsoInternalCalls()) {
            $filterOnDirection .= "OR $filterOnInternal ";
        }

        if ($this->getShowCallCost()) {
            $filterOnDirection .= "OR $filterOnSystem ";
        }

        $filterOnDirection .= ')';

        // TODO: add constraints in the proper place
        assert(!isEmptyOrNull($this->getArReport()->getFromDate()));

        // Create the main query

        $query = 'SELECT ar_cdr . calldate as calldate
                , ar_cdr . ar_organization_unit_id as ar_organization_unit_id
                , ar_cdr . count_of_calls as count_of_calls
                , ar_cdr . income as income
                , ar_cdr . cost as cost
                , ar_cdr . cost_saving as cost_saving
                , ar_cdr . billsec as duration
                , ar_cdr . destination_type as destination_type
                , ar_cdr . cached_masked_external_telephone_number as cached_masked_external_telephone_number
                , ar_cdr . cached_external_telephone_number as cached_external_telephone_number
                , ar_cdr . ar_vendor_id as vendor_id
                , ar_telephone_prefix . geographic_location as geographic_location
                , ar_telephone_prefix . operator_type as operator_type
                , ar_party . name as vendor_name
                , ar_cdr . cached_parent_id_hierarchy as cached_parent_id_hierarchy
                , chn . id as channel_id
                , chn . name as channel_name
                 FROM ar_cdr JOIN ar_telephone_prefix ON ar_cdr . ar_telephone_prefix_id = ar_telephone_prefix . id
                      , ar_party, ar_vendor
                      , ar_communication_channel_type AS chn
                 WHERE ar_cdr . calldate >= ' . $conn->quote($this->getArReport()->getFromDate()) . '
                 AND ar_cdr.ar_vendor_id = ar_vendor.id AND ar_vendor.ar_party_id = ar_party.id
                 AND chn.id = ar_cdr.ar_communication_channel_type_id
                 ';

        if (!isEmptyOrNull($this->getArReport()->getToDate())) {
            $query .= " AND ar_cdr.calldate < " . $conn->quote($this->getArReport()->getToDate());
        }

        if (!is_null($this->getArReport()->getArVendorId())) {

            $query .= " AND ar_cdr.ar_vendor_id = " . $this->getArReport()->getArVendorId();
        }

        $query .= " AND $filterOnDirection $additionalConditions ORDER BY ar_cdr.calldate ";

        if ($maxCallsInReport > 0) {
            $query .= " LIMIT $maxCallsInReport ";
        }

        sfContext::getInstance()->getLogger()->info('Report query: ' . $query);

        return $query;
    }

    /**
     * @param string|null $currentIds null for the root of all customers
     * @return array[string] an array with the key of direct children, ordered according the report needs.
     * @throw ArProblemException
     */
    protected function getOrderedDirectChildren($currentIds)
    {
        $order = $this->getArReport()->getArReportOrderOfChildrenId();

        // get the children
        $r = array();

        if (is_null($currentIds)) {
            $toInspect = $this->getStore()->getRootIds();
        } else {
            if (isset($this->getStore()->idsToChildren[$currentIds])) {
                $toInspect = $this->getStore()->idsToChildren[$currentIds];
            } else {
                $toInspect = array();
            }
        }

        foreach ($toInspect as $childIds => $ignore) {
            $orderValue = null;
            list($countOfCalls, $duration, $cost, $income, $costSavings) = $this->getStoreValues($childIds, null, null, null, null, null);
            switch ($order) {
                case ArReportOrderOfChildren::ORDER_BY_CALL_COST:
                    $orderValue = $cost;
                    break;
                case ArReportOrderOfChildren::ORDER_BY_CALL_DURATION:
                    $orderValue = $duration;
                    break;
                case ArReportOrderOfChildren::ORDER_BY_CALL_INCOME:
                    $orderValue = $income;
                    break;
                case ArReportOrderOfChildren::ORDER_BY_COUNT_OF_CALLS:
                    $orderValue = $countOfCalls;
                    break;
                case ArReportOrderOfChildren::ORDER_BY_NAME:
                    $orderValue = $this->getStore()->idsToLastName[$childIds];
                    break;
                default:
                    $this->signalProblem('Error in the code. Contact the assistance. Unrecognized ArReportOrderOfChildren with code ' . $order);
                    break;
            }

            $r[$childIds] = $orderValue;
        }


        // order the children
        switch ($order) {
            case ArReportOrderOfChildren::ORDER_BY_CALL_COST:
            case ArReportOrderOfChildren::ORDER_BY_CALL_DURATION:
            case ArReportOrderOfChildren::ORDER_BY_CALL_INCOME:
            case ArReportOrderOfChildren::ORDER_BY_COUNT_OF_CALLS:
                arsort($r, SORT_DESC);
                break;
            case ArReportOrderOfChildren::ORDER_BY_NAME:
                asort($r, SORT_LOCALE_STRING);
                break;
            default:
                $this->signalProblem('Error in the code. Contact the assistance. Unrecognized ArReportOrderOfChildren with code ' . $order);
                break;
        }

        return $r;
    }


    /**
     * @param string|null $ids
     * @return array direction => orderValue ordered according the order criteria
     */
    public function getOrderedDirections($ids)
    {
        $order = $this->getArReport()->getArReportOrderOfChildrenId();

        // retrieve all directions
        $directions = $this->getAllowedDestinationTypes();

        // associate the order value to directions

        $result = array();

        foreach ($directions as $directionId) {
            $orderValue = null;
            list($countOfCalls, $duration, $cost, $income, $costSavings) = $this->getStoreValues($ids, $directionId, null, null, null, null);
            switch ($order) {
                case ArReportOrderOfChildren::ORDER_BY_CALL_COST:
                    $orderValue = $cost;
                    break;
                case ArReportOrderOfChildren::ORDER_BY_CALL_DURATION:
                    $orderValue = $duration;
                    break;
                case ArReportOrderOfChildren::ORDER_BY_CALL_INCOME:
                    $orderValue = $income;
                    break;
                case ArReportOrderOfChildren::ORDER_BY_COUNT_OF_CALLS:
                    $orderValue = $countOfCalls;
                    break;
                case ArReportOrderOfChildren::ORDER_BY_NAME:
                    $orderValue = DestinationType::getName($directionId);
                    break;
                default:
                    $this->signalProblem('Error in the code. Contact the assistance. Unrecognized ArReportOrderOfChildren with code ' . $order);
                    break;
            }

            $result[$directionId] = $orderValue;
        }

        // order
        switch ($order) {
            case ArReportOrderOfChildren::ORDER_BY_CALL_COST:
            case ArReportOrderOfChildren::ORDER_BY_CALL_DURATION:
            case ArReportOrderOfChildren::ORDER_BY_CALL_INCOME:
            case ArReportOrderOfChildren::ORDER_BY_COUNT_OF_CALLS:
                arsort($result, SORT_DESC);
                break;
            case ArReportOrderOfChildren::ORDER_BY_NAME:
                asort($result, SORT_LOCALE_STRING);
                break;
            default:
                $this->signalProblem('Error in the code. Contact the assistance. Unrecognized ArReportOrderOfChildren with code ' . $order);
                break;
        }

        return $result;
    }

    /**
     * @param string|null $ids
     * @return array vendorId => orderValue ordered according the order criteria
     */
    public function getOrderedVendors($ids)
    {
        $order = $this->getArReport()->getArReportOrderOfChildrenId();

        // retrieve all vendors
        $vendors = array();

        if (!is_null($ids)) {
            foreach ($this->getAllowedDestinationTypes() as $direction) {
                if (array_key_exists($ids, $this->getStore()->values)
                    && array_key_exists($direction, $this->getStore()->values[$ids])
                ) {
                    $isSet = true;
                } else {
                    $isSet = false;
                }

                if ($isSet) {
                    foreach ($this->getStore()->values[$ids][$direction] as $vendorId => $ignore) {
                        $vendors[$vendorId] = true;
                    }
                }
            }
        } else {
            foreach ($this->getAllowedDestinationTypes() as $direction) {
                foreach ($this->getStore()->values as $currentIds => $ignore1) {
                    if (array_key_exists($currentIds, $this->getStore()->values)
                        && array_key_exists($direction, $this->getStore()->values[$currentIds])
                    ) {
                        $isSet = true;
                    } else {
                        $isSet = false;
                    }
                    if ($isSet) {
                        foreach ($this->getStore()->values[$currentIds][$direction] as $vendorId => $ignore2) {
                            $vendors[$vendorId] = true;
                        }
                    }
                }
            }
        }

        // associate the order value to vendors

        foreach ($vendors as $vendorId => $ignore) {
            $orderValue = null;
            list($countOfCalls, $duration, $cost, $income, $costSavings) = $this->getStoreValues($ids, null, $vendorId, null, null, null);
            switch ($order) {
                case ArReportOrderOfChildren::ORDER_BY_CALL_COST:
                    $orderValue = $cost;
                    break;
                case ArReportOrderOfChildren::ORDER_BY_CALL_DURATION:
                    $orderValue = $duration;
                    break;
                case ArReportOrderOfChildren::ORDER_BY_CALL_INCOME:
                    $orderValue = $income;
                    break;
                case ArReportOrderOfChildren::ORDER_BY_COUNT_OF_CALLS:
                    $orderValue = $countOfCalls;
                    break;
                case ArReportOrderOfChildren::ORDER_BY_NAME:
                    $vendor = ArVendorPeer::retrieveByPK($vendorId);
                    $orderValue = $vendor->getName();
                    break;
                default:
                    $this->signalProblem('Error in the code. Contact the assistance. Unrecognized ArReportOrderOfChildren with code ' . $order);
                    break;
            }

            $vendors[$vendorId] = $orderValue;
        }

        // order
        switch ($order) {
            case ArReportOrderOfChildren::ORDER_BY_CALL_COST:
            case ArReportOrderOfChildren::ORDER_BY_CALL_DURATION:
            case ArReportOrderOfChildren::ORDER_BY_CALL_INCOME:
            case ArReportOrderOfChildren::ORDER_BY_COUNT_OF_CALLS:
                arsort($vendors, SORT_DESC);
                break;
            case ArReportOrderOfChildren::ORDER_BY_NAME:
                asort($vendors, SORT_LOCALE_STRING);
                break;
            default:
                $this->signalProblem('Error in the code. Contact the assistance. Unrecognized ArReportOrderOfChildren with code ' . $order);
                break;
        }

        return $vendors;
    }


    /**
     * @param string|null $ids
     * @return array channelId => orderValue ordered according the order criteria
     */
    public function getOrderedCommunicationChannels($ids)
    {
        $order = $this->getArReport()->getArReportOrderOfChildrenId();

        // retrieve all channels
        $channels = array();

        if (!is_null($ids)) {
            foreach ($this->getAllowedDestinationTypes() as $direction) {
                if (isset($this->getStore()->values[$ids])) {
                    if (isset($this->getStore()->values[$ids][$direction])) {
                        foreach ($this->getStore()->values[$ids][$direction] as $vendorId => $rest) {
                            foreach ($rest as $channelId => $ignore) {
                                $channels[$channelId] = true;
                            }
                        }
                    }
                }
            }
        } else {
            foreach ($this->getAllowedDestinationTypes() as $direction) {
                foreach ($this->getStore()->values as $currentIds => $ignore) {
                    if (isset($this->getStore()->values[$currentIds][$direction])) {
                        foreach ($this->getStore()->values[$currentIds][$direction] as $vendorId => $rest) {
                            foreach ($rest as $channelId => $ignore) {
                                $channels[$channelId] = true;
                            }
                        }
                    }
                }
            }
        }

        // associate the order value to channels

        foreach ($channels as $channelId => $ignore) {
            $orderValue = null;
            list($countOfCalls, $duration, $cost, $income, $costSavings) = $this->getStoreValues($ids, null, null, $channelId, null, null);
            switch ($order) {
                case ArReportOrderOfChildren::ORDER_BY_CALL_COST:
                    $orderValue = $cost;
                    break;
                case ArReportOrderOfChildren::ORDER_BY_CALL_DURATION:
                    $orderValue = $duration;
                    break;
                case ArReportOrderOfChildren::ORDER_BY_CALL_INCOME:
                    $orderValue = $income;
                    break;
                case ArReportOrderOfChildren::ORDER_BY_COUNT_OF_CALLS:
                    $orderValue = $countOfCalls;
                    break;
                case ArReportOrderOfChildren::ORDER_BY_NAME:
                    $channel = ArCommunicationChannelTypePeer::retrieveByPK($channelId);
                    $orderValue = $channel->getName();
                    break;
                default:
                    $this->signalProblem('Error in the code. Contact the assistance. Unrecognized ArReportOrderOfChildren with code ' . $order);
                    break;
            }

            $channels[$channelId] = $orderValue;
        }

        // order
        switch ($order) {
            case ArReportOrderOfChildren::ORDER_BY_CALL_COST:
            case ArReportOrderOfChildren::ORDER_BY_CALL_DURATION:
            case ArReportOrderOfChildren::ORDER_BY_CALL_INCOME:
            case ArReportOrderOfChildren::ORDER_BY_COUNT_OF_CALLS:
                arsort($channels, SORT_DESC);
                break;
            case ArReportOrderOfChildren::ORDER_BY_NAME:
                arsort($channels, SORT_LOCALE_STRING);
                break;
            default:
                $this->signalProblem('Error in the code. Contact the assistance. Unrecognized ArReportOrderOfChildren with code ' . $order);
                break;
        }

        return $channels;
    }

    /**
     * @param string|null $ids
     * @return array vendorId => orderValue ordered according the order criteria
     */
    public
    function getOrderedGeographicLocations($ids)
    {
        $order = $this->getArReport()->getArReportOrderOfChildrenId();

        // retrieve all geographic locations
        $groups = array();

        if (!is_null($ids)) {
            foreach ($this->getAllowedDestinationTypes() as $direction) {
                if (isset($this->getStore()->values[$ids])) {
                    if (isset($this->getStore()->values[$ids][$direction])) {
                        foreach ($this->getStore()->values[$ids][$direction] as $vendorId => $rest) {
                            foreach ($rest as $channelId => $rest2) {
                                foreach ($rest2 as $geographicLocation => $rest3) {
                                    $values = $this->getStoreValues($ids, null, null, null, $geographicLocation, null);
                                    list($countOfCalls, $duration, $cost, $income, $costSavings) = $values;
                                    if ($countOfCalls > 0) {
                                        $groups[$geographicLocation] = $values;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        } else {
            foreach ($this->getAllowedDestinationTypes() as $direction) {
                foreach ($this->getStore()->values as $currentIds => $ignore) {
                    if (isset($this->getStore()->values[$currentIds][$direction])) {
                        foreach ($this->getStore()->values[$currentIds][$direction] as $vendorId => $rest) {
                            foreach ($rest as $channelId => $rest2) {
                                foreach ($rest2 as $geographicLocation => $rest3) {
                                    $values = $this->getStoreValues($ids, null, null, null, $geographicLocation, null);
                                    list($countOfCalls, $duration, $cost, $income, $costSavings) = $values;
                                    if ($countOfCalls > 0) {
                                        $groups[$geographicLocation] = $values;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        // associate the order value to groups

        foreach ($groups as $groupKey => $values) {
            $orderValue = null;
            list($countOfCalls, $duration, $cost, $income, $costSavings) = $values;
            switch ($order) {
                case ArReportOrderOfChildren::ORDER_BY_CALL_COST:
                    $orderValue = $cost;
                    break;
                case ArReportOrderOfChildren::ORDER_BY_CALL_DURATION:
                    $orderValue = $duration;
                    break;
                case ArReportOrderOfChildren::ORDER_BY_CALL_INCOME:
                    $orderValue = $income;
                    break;
                case ArReportOrderOfChildren::ORDER_BY_COUNT_OF_CALLS:
                    $orderValue = $countOfCalls;
                    break;
                case ArReportOrderOfChildren::ORDER_BY_NAME:
                    $orderValue = $groupKey;
                    break;
                default:
                    $this->signalProblem('Error in the code. Contact the assistance. Unrecognized ArReportOrderOfChildren with code ' . $order);
                    break;
            }

            $groups[$groupKey] = $orderValue;
        }

        // order
        switch ($order) {
            case ArReportOrderOfChildren::ORDER_BY_CALL_COST:
            case ArReportOrderOfChildren::ORDER_BY_CALL_DURATION:
            case ArReportOrderOfChildren::ORDER_BY_CALL_INCOME:
            case ArReportOrderOfChildren::ORDER_BY_COUNT_OF_CALLS:
                arsort($groups, SORT_DESC);
                break;
            case ArReportOrderOfChildren::ORDER_BY_NAME:
                asort($groups, SORT_LOCALE_STRING);
                break;
            default:
                $this->signalProblem('Error in the code. Contact the assistance. Unrecognized ArReportOrderOfChildren with code ' . $order);
                break;
        }

        return $groups;
    }

    ///////////////////////
    // Utility Functions //
    ///////////////////////

    /**
     * @param mixed $value a string in UTF-8 format
     * @return string in PDF format
     */
    public
    function convertString($value)
    {
        return $this->getStore()->convertString($value);
    }
}
