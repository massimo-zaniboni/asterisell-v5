<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Display the costs of each vendor or channel on a separate column,
 * in order to quickly compares these values.
 *
 * Something like
 *
 * > +-----------+-------+---------+----------+-----+
 * > | Institute | Total | PTPrime | Vodafone | ... |
 * > +-----------+-------+---------+----------+-----+
 * > | U. Aveiro | ...   |
 * > | FCCN      | ...   |
 *
 */
class Report_CompareVendorsOrChannels extends BaseBillingReport
{

    public function getReportUserReadableName()
    {
        if ($this->getShowVendorOrChannel() == self::SHOW_CHANNEL) {
            return mytr('Compare Channels');
        } else {
            return mytr('Compare Vendors');
        }
    }

    /**
     * @param string $ids
     * @param int $vendorOrChannelId vendor or channel id according report settings
     * @return array list(countOfCalls, duration, array(array(columnName, cost), array(columnName, income), array(columnName, earn)), asString)
     * according report settings
     */
    protected function getVendorOrChannelValues($ids, $vendorOrChannelId)
    {
        if ($this->getShowVendorOrChannel() == self::SHOW_CHANNEL) {
            list($countOfCalls, $duration, $cost, $income, $costSavings) = $this->getStoreValues($ids, null, null, $vendorOrChannelId, null, null);
        } else {
            list($countOfCalls, $duration, $cost, $income, $costSavings) = $this->getStoreValues($ids, null, $vendorOrChannelId, null, null, null);
        }

        return array($countOfCalls, $duration, $this->getColumnCostAndIncomeValues($cost, $income, $costSavings), $this->getColumnCostAndIncomeValuesAsString($cost, $income, $costSavings));
    }

    /**
     * @param int $totCost
     * @param int $totIncome
     * @param int $totCostSaving
     * @return array array(list(columnName, cost), list(columnName, income), list(columnName, earn))
     * according parameters of the report
     */
    protected function getColumnCostAndIncomeValues($totCost, $totIncome, $totCostSaving)
    {
        $row = array();
        foreach ($this->getShowType() as $type) {
            if ($type == self::SHOW_COST_SAVING) {
                $row[] = $totCostSaving;
            } else if ($type == self::SHOW_COST) {
                $row[] = $totCost;
            } else if ($type == self::SHOW_INCOME) {
                $row[] = $totIncome;
            } else if ($type == self::SHOW_EARN) {
                $row[] = $totIncome - $totCost;
            } else {
                $row[] = '?';
            }
        }

        $i = 0;
        $row2 = array();
        foreach ($this->getReportNamesOfCostColumn() as $name) {
            $row2[] = array($name, $row[$i]);
            $i++;
        }

        return $row2;
    }

    protected function getColumnCostAndIncomeValuesAsString($totCost, $totIncome, $totCostSaving)
    {
        $totValues = $this->getColumnCostAndIncomeValues($totCost, $totIncome, $totCostSaving);
        $showColumnName = (count($totValues) > 1);
        $values = array();
        foreach ($totValues as $costValue) {
            list ($columnName, $cost) = $costValue;

            if ($showColumnName) {
                $nameToUse = $columnName;
            } else {
                $nameToUse = '';
            }

            $values[] = $nameToUse . ' ' . $this->convertString(from_db_decimal_to_pdf_txt_decimal($cost));
        }

        return join("\n", $values);
    }

    ///////////////////////////////////////
    // ReportGenerator Interface Support //
    ///////////////////////////////////////

    public function deriveReportParams()
    {

        $this->checkParamArOrganizationUnitIdExists();
        $this->checkParamFromDateExists();
        $this->checkParamToDateExists();
        $this->checkVoIPVendorOrCommunicationChannelIsSpecified();

        $report = $this->getArReport();

        $report->setParamExpandToLevel(1);

        $report->setParamShowAlsoOutgoingCalls(true);
        $report->setParamShowAlsoIncomingCalls(true);

        $report->setParamShowMaskedTelephoneNumbers(true);
        $report->setParamShowCallDetails(false);
        $report->setArReportOrderOfChildrenId(ArReportOrderOfChildren::ORDER_BY_CALL_COST);
    }

    ///////////////////////
    // Report Generation //
    ///////////////////////

    protected function internalGenerateReport()
    {
        $this->makeCallsByOrganizationReport();
    }

    /**
     * @return void
     * @throws ArProblemException
     *
     */
    protected function makeCallsByOrganizationReport()
    {

        $report = $this->getArReport();

        $rootFullIds = $this->getOrganizationUnitIds();

        $title = $this->convertString($this->getReportDescription());

        $pdf = new BillingReportPDFTemplate();
        $pdf->SetTitle($title);
        $pdf->setReportHeaderLine1($title);
        $pdf->AliasNbPages();
        $pdf->AddPage();

        $errorInfo = $this->getInfoAboutUnratedCDRs();
        if (!isEmptyOrNull($errorInfo)) {
            $pdf->addErrorInfo($this->convertString($errorInfo));
        }

        $this->setTableHeaderForCallsSummary($pdf, true);

        $pdf->addTableHeader();
        $pdf->addTableRowSeparator();

        // Display totals
        $this->reportOrganizationStats($pdf, $rootFullIds, null, true);

        foreach ($this->getOrderedDirectChildren($rootFullIds) as $childIds => $ignore) {
            $this->reportOrganizationStats($pdf, $childIds, $rootFullIds, false);
        }

        // Save the PDF document
        $pdfReportContent = $pdf->Output("", "S");
        $report->setProducedReportMimeType('application/pdf');
        $report->setProducedReportFileTypeSuffix('pdf');
        $report->setProducedReportAlreadyReviewed(0);
        $report->setDocumentContent($pdfReportContent);
        $report->save();
    }

    /**
     * Display the global stats about a organization and his direct children.
     *
     * @param BillingReportPDFTemplate $pdf
     * @param string $currentIds organization stats to display
     * @param string|null $parentIds organization parent
     * @param bool $areTotals
     */
    protected function reportOrganizationStats(BillingReportPDFTemplate $pdf, $currentIds, $parentIds, $areTotals)
    {
        /**
         * @var string[] $row
         */
        $row = array();

        // Write stats of the $currentIds
        $currentName = $this->getStore()->getCompleteNameInAFastWay($this->getRootOrganizationLevel() - 1, $currentIds, true);

        if (!is_null($parentIds)) {
            $parentName = $this->getStore()->getCompleteNameInAFastWay($this->getRootOrganizationLevel() - 1, $parentIds, true);
            $relativeName = '..' . substr($currentName, strlen($parentName));
        } else {
            $relativeName = $currentName;
        }

        $row[] = $this->convertString($relativeName);

        // Cells for totals values
        list($totCalls, $totDuration, $totCost, $totIncome, $totSavings) = $this->getStoreValues($currentIds, null, null, null, null, null);
        $row[] = $this->getColumnCostAndIncomeValuesAsString($totCost, $totIncome, $totSavings);

        // Cells for each vendor
        if ($this->getShowVoipProvider()) {
            $toScan = $this->getOrderedVendors($this->getOrganizationUnitIds());
        } else {
            $toScan = $this->getOrderedCommunicationChannels($this->getOrganizationUnitIds());
        }

        $toScan = $this->filterAndNormalizeVendors($toScan);

        foreach ($toScan as $vendorOrChannelId => $ignore) {
            list($vendorCalls, $vendorDuration, $vendorValues, $vendorValuesAsString) = $this->getVendorOrChannelValues($currentIds, $vendorOrChannelId);
            $row[] = $vendorValuesAsString;
        }

        $pdf->addTableData($row, $areTotals, false, false);
    }

    /**
     * Set table headers.
     *
     * @param BillingReportPDFTemplate $pdf
     * @param bool $isUserAtForm not used
     */
    public function setTableHeaderForCallsSummary(BillingReportPDFTemplate $pdf, $isUserAtForm)
    {
        $tableHeader = array();
        $tableAlign = array();
        $displayCol = array();
        $tableWidth = array();
        $tableAlignVendor = array();

        $pageWidth = $pdf->getPageContentWidth();

        $fixedCols = 1;

        if ($this->getShowVoipProvider()) {
            $vendorsOrChannels = $this->getOrderedVendors($this->getOrganizationUnitIds());
        } else {
            $vendorsOrChannels = $this->getOrderedCommunicationChannels($this->getOrganizationUnitIds());
        }
        // use a fixed order for columns, for having coherent details and totals
        $vendorsOrChannels = $this->filterAndNormalizeVendors($vendorsOrChannels);

        $width = intval($pageWidth / (count($vendorsOrChannels) + $fixedCols + 2));
        $organizationNameWidth = $width * 2;

        $i = 0;
        $tableHeader[$i] = mytr('Organization');
        $tableAlignVendor[$i] = 'L';
        $tableAlign[$i] = 'L';
        $displayCol[$i] = true;
        $tableWidth[$i] = $organizationNameWidth;

        // Create a columns for each vendor and total

        $names = array();
        $names[] = mytr('Totals');
        if ($this->getArReport()->getParamShowCostSaving()) {
            $names[] = mytr('Cost Saving');
        }

        foreach ($vendorsOrChannels as $vendorId => $ignore) {
            if ($this->getShowVendorOrChannel() == self::SHOW_CHANNEL) {
                $vendor = ArCommunicationChannelTypePeer::retrieveByPK($vendorId);
                $vendorName = $vendor->getName();
            } else {
                $vendor = ArVendorPeer::retrieveByPK($vendorId);
                $vendorName = $vendor->getName();
            }

            $names[] = $this->convertString($vendorName);
        }


        foreach ($names as $name) {
            $i++;
            $tableHeader[$i] = $name;
            $tableAlign[$i] = 'R';
            $tableAlignVendor[$i] = 'R';

            $displayCol[$i] = true;

            $tableWidth[$i] = $width;
        }

        // set table

        $pdf->setTableAligns($tableAlign);
        $pdf->setTableAlignsVendor($tableAlignVendor);
        $pdf->setTableDisplay($displayCol);
        $pdf->setTableHeader($tableHeader);
        $pdf->setTableWidths($tableWidth);
    }

    /**
     * @param array $arr order and normalize the content of the array.
     * The key of the array must be the vedor or the communication channel id.
     * @return array the same array, with the vendors ordered in a constant way, and only if they have some value.
     */
    protected function filterAndNormalizeVendors($arr)
    {
        static $vendorIdsHavingTotals = null;

        if (is_null($vendorIdsHavingTotals)) {

            $vendorIdsHavingTotals = array();

            if ($this->getShowVoipProvider()) {
                $vendorsOrChannels = $this->getOrderedVendors($this->getOrganizationUnitIds());
            } else {
                $vendorsOrChannels = $this->getOrderedCommunicationChannels($this->getOrganizationUnitIds());
            }

            foreach ($vendorsOrChannels as $vendorId => $ignore) {

                if ($this->getShowVoipProvider()) {
                    list($countOfCalls, $duration, $cost, $income, $costSavings) = $this->getStoreValues($this->getOrganizationUnitIds(), null, $vendorId, null, null, null);
                } else {
                    list($countOfCalls, $duration, $cost, $income, $costSavings) = $this->getStoreValues($this->getOrganizationUnitIds(), null, null, $vendorId, null, null);
                }

                $includeVendorId = false;
                foreach ($this->getShowType() as $type) {
                    if ($type == self::SHOW_COST_SAVING) {
                        $includeVendorId = $includeVendorId || ($costSavings > 0);
                    } else if ($type == self::SHOW_COST) {
                        $includeVendorId = $includeVendorId || ($cost > 0);
                    } else if ($type == self::SHOW_INCOME) {
                        $includeVendorId = $includeVendorId || ($income > 0);
                    }
                }

                if ($includeVendorId) {
                    $vendorIdsHavingTotals[$vendorId] = true;
                }
            }

            ksort($vendorIdsHavingTotals);

        }

        $r = array();
        foreach ($vendorIdsHavingTotals as $vendorId => $ignore) {
            assert(isset($arr[$vendorId]));

            $r[$vendorId] = $arr[$vendorId];
        }

        ksort($r);

        return $r;
    }

}
