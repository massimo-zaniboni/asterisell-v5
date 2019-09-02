<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

class wholesale_number_transactionActions extends autoWholesale_number_transactionActions
{

    public function executeList($request)
    {
        UpdateWholesaleNumberStats::updateStats();
        parent::executeList($request);
    }

    public function executeExportToCsv($request)
    {
        if ($this->getRequest()->isMethod(sfRequest::POST)) {
            if ($this->getRequest()->getParameter('downloadFreeNumbers')) {
                return $this->produceCSVResult(time(), 2);
            } else if ($this->getRequest()->getParameter('downloadNewNumbers')) {
                return $this->produceCSVResult(time(), 5);
            } else if ($this->getRequest()->getParameter('downloadActiveNumbers')) {
                return $this->produceCSVResult(time(), 4);
            } else if ($this->getRequest()->getParameter('downloadResellerNumbers')) {
                $resellerId = $this->getRequest()->getParameter('select_reseller');
                return $this->produceCSVResult(time(), 3, $resellerId);
            } else if ($this->getRequest()->getParameter('importCsv')) {
                $fileName = $this->getRequest()->getFilePath('csvFile');
                if (is_null($fileName) || (strlen(trim($fileName)) == 0)) {
                    $this->getUser()->setFlash('error', "There is no CSV file.");
                    return $this->redirect('wholesale_number_transaction/list');
                }

                $handle = fopen($fileName, 'r');
                if ($handle == false) {
                    $this->getUser()->setFlash('error', "Error opening file \"$fileName\"");
                    return $this->redirect('wholesale_number_transaction/list');
                }

                $processor = new ChangeWholesaleInfo();
                $processOk = $processor->importFromCSVFile($handle);

                if (is_string($processOk)) {
                    $this->getUser()->setFlash('error', $processOk);
                } else {
                    $this->getUser()->setFlash('notice', "Inserted/changed $processOk wholesale numbers.");
                }
            }
        } else {
            $at = $this->getRequest()->getParameter('at');
            $status = $this->getRequest()->getParameter('status');
            return $this->produceCSVResult($at, intval($status));
        }
        return $this->redirect('wholesale_number_transaction/list');
    }

    /**
     * @param Criteria $c
     */
    protected
    function addFiltersCriteria($c)
    {
        if (isset($this->filters['filter_on_reseller']) && !isEmptyOrNull($this->filters['filter_on_reseller'])) {
            $s = $this->filters['filter_on_reseller'];
            $c->add(ArWholesaleNumberTransactionPeer::RESELLER_CODES, "%,$s,%", Criteria::LIKE);
        }

        if (isset($this->filters['filter_on_date'])) {
            $fromDate = fromSymfonyTimestampToUnixTimestamp($this->filters['filter_on_date']);
            if (!is_null($fromDate)) {
                $c->add(ArWholesaleNumberTransactionPeer::FROM_DATE, fromUnixTimestampToMySQLTimestamp($fromDate), Criteria::LESS_EQUAL);
            }
        }
    }

    /**
     * @param int|null $fromDate
     * @param int $status
     * 0 for showing transaction at $fromDate,
     * 1 for showing history until $fromDate,
     * 2 for showing free numbers
     * 3 for showing numbers assigned to a reseller
     * 4 for showing only active numbers
     * 5 for generating a template with new numbers
     * @param int|null $resellerId
     * @return string
     */
    protected function produceCSVResult($fromDate, $status, $resellerId = null)
    {
        $process = true;
        $p = array();
        $filter = '';
        $order = '';
        $fromDateS = fromUnixTimestampToMySQLTimestamp($fromDate);
        if ($status == 0) {
            VariableFrame::$wholesaleEffect = 'ALL';
            $filter = 'from_date = ?';
            $p[] = $fromDateS;
            $order = 'from_date DESC, ar_reseller_id, telephone_number';
        } else if ($status == 1) {
            VariableFrame::$wholesaleEffect = 'ALL';
            $filter = 'from_date <= ?';
            $p[] = $fromDateS;
            $order = 'from_date DESC, ar_reseller_id, telephone_number';
        } else if ($status == 4) {
            VariableFrame::$wholesaleEffect = 'ALL';
            $filter = 'csv_is_current = 1';
            $order = ' ar_reseller_id, telephone_number';
         } else if ($status == 5) {
            VariableFrame::$wholesaleEffect = 'ALL';
        } else if ($status == 2) {
            VariableFrame::$wholesaleEffect = 'ALL';
            $filter = 'w.csv_is_current = 1 AND w.ar_reseller_id IS NULL AND w.exists = 1 ';
            $order = 'ar_wholesale_carrier_id, telephone_number';
        } else if ($status == 3 && !isEmptyOrNull($resellerId)) {
            VariableFrame::$wholesaleEffect = 'RESELLER';
            $filter = ' w.ar_reseller_id = ? AND w.csv_is_current =  1 AND w.exists = 1 ';
            $p[] = intval($resellerId);
            $order = 'ar_wholesale_carrier_id, telephone_number';
        } else {
            $process = false;
        }

        if ($process) {
            UpdateWholesaleNumberStats::updateStats();

            $q = "SELECT telephone_number,
                       w.from_date AS from_date,
                       `exists`,
                       extension_codes,
                       use_default_extension_codes,
                       ar_reseller_id,
                       ar_wholesale_carrier_id,
                       income_price AS income,
                       cost_price AS cost,
                       csv_comment,
                       csv_last_date,
                       r.internal_name AS reseller_code,
                       c.internal_name AS carrier_code
                FROM ar_wholesale_number AS w
                LEFT JOIN ar_reseller AS r
                ON w.ar_reseller_id = r.id
                LEFT JOIN ar_wholesale_carrier AS c
                ON w.ar_wholesale_carrier_id = c.id
                WHERE $filter
                ORDER BY $order
           ";

            // the framework invoke templates/exportToCsvSuccess.php
            VariableFrame::$wholeQuery = $q;
            VariableFrame::$listParams = $p;
            VariableFrame::$wholesaleStatus = $status;
            VariableFrame::$wholeFromDate = $fromDate;
            sfConfig::set('sf_web_debug', false);
        } else {
            return $this->redirect('wholesale_number_transaction/list');
        }
    }
}
