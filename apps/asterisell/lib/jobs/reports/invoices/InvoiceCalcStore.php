<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Calculate the totals of all customers, because a scan of data must be done in any case,
 * and it is cheaper doing it only one time for all customers.
 *
 * Consider only billable parties.
 *
 * DEV-NOTE: in new code, use InvoiceCalcStore2 instead.
 *
 * Take in consideration the decimal to use in invoices in all phases of calculations,
 * so calculations are not always exact if considering all the DB precision digits,
 * but they are exact and coherent from a fiscal point of view.
 * This is fair because the differences in totals will be only in the last decimal part,
 * so without real implications for the customer, but the invoice is perfectly
 * legal from a fiscal point of view, because all fields are coherent together.
 * An annoying thing it is that customers can complain for small differences between the totals
 * seen on the call report and the billed cost.
 */
class InvoiceCalcStore extends ReportCalcStore
{

    protected $isEmptyVar = false;

    /**
     * @return bool true if the store contains no data in the specified time frame
     */
    public function isEmpty()
    {
        return $this->isEmptyVar;
    }

    /**
     * @var int
     */
    public $fromTime = 0;

    /**
     * @var int
     */
    public $toTime = 0;

    /**
     * Group data according the app.yml specifications.
     * Group to the most specific billable customer.
     *
     * @var array billable_ar_organization_id => group-key  => (group-human-description, count of calls, duration, cost, income)
     */
    protected $values;

    /**
     * @var array billable_ar_organization_id => int the from date that can be in the past
     * respect the report reference from_date in case of postponed organizations.
     */
    protected $organizationFromDate;

    /**
     * @param int $organizationId billable organization
     * @return int CDRs must be computed from this date. This take
     * in consideration also post-poned invoices.
     */
    public function getOrganizationFromDate($organizationId) {
        if (array_key_exists($organizationId, $this->organizationFromDate)) {
           return $this->organizationFromDate[$organizationId];
        } else {
            return $this->fromTime;
        }
    }

    /**
     * @param int $organizationId
     * @param ArReport $params
     * @return array list(group-human-description, count-of-calls, duration, cost, income),
     * ordered according the report settings
     * @throws ArProblemException
     */
    public function getDetails($organizationId, ArReport $params) {
        if (isset($this->values[$organizationId])) {

            $toOrder = array();

            foreach ($this->values[$organizationId] as $groupKey => $value) {

                $orderValue = null;
                list($groupHumanDescription, $countOfCalls, $duration, $cost, $income) = $value;

                switch ($params->getArReportOrderOfChildrenId()) {
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
                        $orderValue = $groupHumanDescription;
                        break;
                    default:
                        throw(ArProblemException::createWithGarbageCollection(
                            ArProblemType::TYPE_ERROR,
                            ArProblemDomain::APPLICATION,
                            null,
                            'unknown order type - ' . get_class($this),
                            'Unrecognized ArReportOrderOfChildren with code ' . $params->getArReportOrderOfChildrenId() . ' in class ' . get_class($this),
                            'Report is not generated.',
                            'This is a problem in the code. Contact the assistance.',
                            null));
                }

                $toOrder[$groupKey] = $orderValue;
            }

            switch ($params->getArReportOrderOfChildrenId()) {
                case ArReportOrderOfChildren::ORDER_BY_CALL_COST:
                case ArReportOrderOfChildren::ORDER_BY_CALL_DURATION:
                case ArReportOrderOfChildren::ORDER_BY_CALL_INCOME:
                case ArReportOrderOfChildren::ORDER_BY_COUNT_OF_CALLS:
                    arsort($toOrder, SORT_DESC);
                    break;
                case ArReportOrderOfChildren::ORDER_BY_NAME:
                    asort($toOrder, SORT_LOCALE_STRING);
                    break;
                default:
                    throw(ArProblemException::createWithGarbageCollection(
                        ArProblemType::TYPE_ERROR,
                        ArProblemDomain::APPLICATION,
                        null,
                            'unknown order type - ' . get_class($this),
                            'Unrecognized ArReportOrderOfChildren with code ' . $params->getArReportOrderOfChildrenId() . ' in class ' . get_class($this),
                        'Report is not generated.',
                        'This is a problem in the code. Contact the assistance.',
                        null));
            }

            $r = array();
            foreach($toOrder as $groupKey => $ignore) {
              $r[] = $this->values[$organizationId][$groupKey];
            }

            return $r;

        } else {
            return array();
        }
    }

    /**
     * @param int $organizationId
     * @param ArParams $params
     * @param ArReport $report
     * @return array list($setVatPerc, $setTotalCalls, $setTotalDuration, $setTotalCostsWithoutTax, $setTotalCostsTax, $setTotalCostsWithTax, $setTotalIncomesWithoutTax, $setTotalIncomesTax, $setTotalIncomesWithTax);
     * where all values are in db format
     *
     * NOTE: execute the operation on details, because there can be different currency rounds, respect
     * exact totals, and so the sum must be the sum of already rounded partial totals, and not the
     * global total.
     */
    public function getInvoiceTotals($organizationId, ArParams $params, ArReport $report) {

        $vatPerc = $params->getVatTaxPercAsPhpDecimal();
        $setVatPerc = convertToDbMoney($vatPerc);

        $setTotalIncomesWithoutTax = 0;
        $setTotalCostsWithoutTax = 0;
        $setTotalDuration = 0;
        $setTotalCalls = 0;

        foreach($this->getDetails($organizationId, $report) as $details) {
            list($ignore, $countCalls, $duration, $cost, $income) = $details;
            $setTotalCalls += $countCalls;
            $setTotalDuration += $duration;
            $setTotalCostsWithoutTax += $cost;
            $setTotalIncomesWithoutTax += $income;
        }

        list($setTotalIncomesTax, $setTotalIncomesWithTax) = invoice_amount_with_vat($setTotalIncomesWithoutTax, $vatPerc);
        list($setTotalCostsTax, $setTotalCostsWithTax) = invoice_amount_with_vat($setTotalCostsWithoutTax, $vatPerc);

        return array($setVatPerc, $setTotalCalls, $setTotalDuration, $setTotalCostsWithoutTax, $setTotalCostsTax, $setTotalCostsWithTax, $setTotalIncomesWithoutTax, $setTotalIncomesTax, $setTotalIncomesWithTax);
    }

    /**
     * Update the content of ar_postponed_report_tmp with the correct time-frame for each organization,
     * taking in account the postponed time frames.
     *
     * @param PropelPDO $conn
     * @param int|null $schedulerId the ID of the scheduler with the postponed invoices to include
     * @param int $fromDate the starting date of the new report-set to calculate
     * @return int the older calldate to use as worst case scenario index
     *
     * @require we are calculating the more recent report-set, and not report-set in the past
     */
    public function updatePostponedTmpTable(PropelPDO $conn, $schedulerId, $fromDate) {

        // Start with an empty situation
        $stmt = $conn->prepare('TRUNCATE ar_postponed_report_tmp');
        $stmt->execute();

        // Stop here if there is no scheduler, and so postponed invoices must be not taken in account
        if (is_null($schedulerId)) {
            return $fromDate;
        }

        //
        // Prepare Statements
        //

        // Discover the previous report-set.
        // NOTE: the more recent report-set is the newly created report-sets
        // with all the new invoices, but it does not contain (yet) postpone info,
        // and it can be discarded.
        $lastRS = $conn->prepare('
        SELECT id, from_date, to_date
        FROM ar_report_set
        WHERE from_date < ?
        AND ar_report_scheduler_id = ?
        ORDER BY from_date DESC
        LIMIT 1;
        ');

        $setAllOrganizationsAsUnprocessed = $conn->prepare('
        UPDATE ar_postponed_report_tmp
        SET is_processed = FALSE
        WHERE NOT is_billed');

        $processPostponedOrganizations = $conn->prepare('
        UPDATE ar_postponed_report_tmp AS t
        JOIN ar_postponed_report AS p
        ON t.ar_organization_unit_id = p.ar_organization_unit_id
        SET t.from_date = ?
        ,   t.is_processed = TRUE
        WHERE p.ar_report_set_id = ?
        AND   NOT t.is_billed
        ');

        $setAsBilledAllUnprocessedOrganizations = $conn->prepare('
        UPDATE ar_postponed_report_tmp
        SET is_billed = TRUE
        WHERE is_processed = FALSE');

        $thereAreOrganizationsToProcess = $conn->prepare('
        SELECT ar_organization_unit_id
        FROM ar_postponed_report_tmp
        WHERE NOT is_billed
        LIMIT 2');

        //
        // Scan all report sets from more recent to older,
        // and stop when there are no any more post poned organization.
        // The result will be put on ar_postponed_report_tmp table.
        //

        $minCalldate = $fromDate;
        $firstPassage = true;
        $again = true;
        $lastFromDate = fromUnixTimestampToMySQLTimestamp($fromDate);

        while($again) {
            $rsFromDate = null;
            $rsId = null;

            $lastRS->execute(array($lastFromDate, $schedulerId));
            $rs = $lastRS->fetch(PDO::FETCH_NUM);
            if ($rs !== false) {
              $rsId = $rs[0];
              $rsFromDate = $rs[1];
            }
            $lastRS->closeCursor();

            if (is_null($rsId)) {
                // there are no any more record-set, so all organizations in the pending table
                // are for sure not billed, and must be processed.
                $again = false;

            } else {
                $lastFromDate = $rsFromDate;
                $minCalldate = fromMySQLTimestampToUnixTimestamp($rsFromDate);

                if($firstPassage) {
                    $firstPassage = false;

                    // All the organizations postponed from current report-set are the starting point.
                    // They are not billed, so they must be billed now.
                    // But we don't know if a more older from_date must be used, so this date will be
                    // refined in next passages.
                    $stmt = $conn->prepare('
                      INSERT INTO ar_postponed_report_tmp(ar_organization_unit_id, from_date, is_billed, is_processed)
                      SELECT p.ar_organization_unit_id, ?, FALSE, FALSE
                      FROM ar_postponed_report AS p
                      WHERE p.ar_report_set_id = ?
                    ');
                    $stmt->execute(array($rsFromDate, $rsId));

                } else {

                    // There are not billed organizations. We are searching in past report-set,
                    // for seeing the last time they were billed, and using so the correct from_date.

                    $setAllOrganizationsAsUnprocessed->execute();
                    $processPostponedOrganizations->execute(array($rsFromDate, $rsId));
                    $setAsBilledAllUnprocessedOrganizations->execute();

                    $again = false;
                    $thereAreOrganizationsToProcess->execute();
                    while (($thereAreOrganizationsToProcess->fetch(PDO::FETCH_NUM)) !== false) {
                        $again = true;
                    }
                    $thereAreOrganizationsToProcess->closeCursor();
                }
            }
       }

       return $minCalldate;
   }

    public function debugShowTmpTable(PropelPDO $conn) {
        $query = '
        SELECT ar_organization_unit_id
        , from_date
        , is_billed
        , is_processed
        FROM ar_postponed_report_tmp
        ORDER BY ar_organization_unit_id
        ';

        $stmt = $conn->prepare($query);
        $stmt->execute();
        while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
            echo "\nOrganization " . $rs[0]
                . " from_date " . $rs[1]
                . " is_billed " . $rs[2]
                . " is_processed " . $rs[3];
        }
        $stmt->closeCursor();
    }

    /**
     * Process the store
     *
     * @param int $from
     * @param int|null $to
     * @param ArReport $reportParams
     * @param int|null $reportSchedulerId
     * @param PropelPDO $conn
     */
    public function process($from, $to, ArReport $reportParams, $reportSchedulerId, PropelPDO $conn)
    {
        $this->values = array();
        $this->organizationFromDate = array();

        $this->fromTime = $from;
        $this->toTime = $to;

        $minCallDate = $this->updatePostponedTmpTable($conn, $reportSchedulerId, $from);

        $conditionOnDirectionArr = array();
        if ($reportParams->getParamShowAlsoInternalCalls()) {
            $conditionOnDirectionArr[]=  'ar_cdr.destination_type = ' . DestinationType::internal;
        }
        if ($reportParams->getParamShowAlsoOutgoingCalls()) {
            $conditionOnDirectionArr[] = 'ar_cdr.destination_type = ' . DestinationType::outgoing;
        }
        if ($reportParams->getParamShowAlsoIncomingCalls()) {
            $conditionOnDirectionArr[] = 'ar_cdr.destination_type = ' . DestinationType::incoming;
        }
        $conditionOnDirection = '(' . join(' OR ', $conditionOnDirectionArr) . ')';

        $isThereOnlyOneDirection = (count($conditionOnDirectionArr) == 1);

        $groupFields = '';
        if ($reportParams->getParamShowCommunicationChannel()) {
            $fieldName = 'ar_cdr.ar_communication_channel_type_id';
            $groupFields .= ', ' . $fieldName;
        }
        if ($reportParams->getParamShowGeographicLocation()) {
            $fieldName = 'ar_telephone_prefix.geographic_location';
            $groupFields .= ', ' . $fieldName;
        }
        if ($reportParams->getParamShowConnectionType()) {
            $fieldName = 'ar_telephone_prefix.operator_type';
            $groupFields .= ', ' . $fieldName;
        }

        $query = '
SELECT
    ar_cdr.billable_ar_organization_unit_id
  , ar_cdr.destination_type '
. $groupFields . '
  , SUM(ar_cdr.count_of_calls)
  , SUM(ar_cdr.income)
  , SUM(ar_cdr.cost)
  , SUM(ar_cdr.cost_saving)
  , SUM(ar_cdr.billsec)
  , IFNULL(p.from_date, ?)
FROM (ar_cdr
     JOIN ar_telephone_prefix ON ar_cdr.ar_telephone_prefix_id = ar_telephone_prefix.id
     ) LEFT JOIN ar_postponed_report_tmp AS p
     ON p.ar_organization_unit_id = ar_cdr.billable_ar_organization_unit_id
WHERE '
. $conditionOnDirection . '
AND ar_cdr.calldate >= ?
AND ar_cdr.calldate >= IFNULL(p.from_date, ?)
AND ar_cdr.calldate < ?
GROUP BY
  ar_cdr.billable_ar_organization_unit_id
, ar_cdr.destination_type '
. $groupFields . '
';

        // sfContext::getInstance()->getLogger()->info('INVOICE: query is ' . $query);

        // Scan all CDRs in the date range, completing stats

        $params = array(
            fromUnixTimestampToMySQLDate($this->fromTime)      // report from this date (or postponed)
          , fromUnixTimestampToMySQLTimestamp($minCallDate)    // worst-case postponed date used for efficient CDR range
          , fromUnixTimestampToMySQLTimestamp($this->fromTime) // sum CDRS from this date (or postponed)
          , fromUnixTimestampToMySQLTimestamp($this->toTime)); // sum CDRS until this date

        $stm = $conn->prepare($query);
        $stm->execute($params);
        while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {

            $this->isEmptyVar = false;

            $organizationId = $rs[0];
            $direction = $rs[1];

            $descriptionArr = array();
            $keyArr = array();

            if (! $isThereOnlyOneDirection) {
                $keyArr[] = $direction;
                $descriptionArr[] = mytr(DestinationType::getUntraslatedName($direction) . ' calls');
            }

            $i = 1;
            if ($reportParams->getParamShowCommunicationChannel()) {
                $i++;
                $keyArr[] = $rs[$i];
                $descriptionArr[] = $rs[$i];
            }
            if ($reportParams->getParamShowGeographicLocation()) {
                $i++;
                $keyArr[] = $rs[$i];
                $descriptionArr[] = $rs[$i];
            }
            if ($reportParams->getParamShowConnectionType()) {
                $i++;
                $keyArr[] = $rs[$i];
                $descriptionArr[] = $rs[$i];
            }

            $i++;
            $countOfCalls = $rs[$i];

            $i++;
            $income = round_db_decimal_according_invoice_decimal($rs[$i]);

            $i++;
            $cost = round_db_decimal_according_invoice_decimal($rs[$i]);

            $i++;
            $saving = round_db_decimal_according_invoice_decimal($rs[$i]);

            $i++;
            $duration = $rs[$i];

            // Each report can use CDRs in the past if the organization billing was postponed
            $i++;
            $organizationFromDate = fromMySQLTimestampToUnixTimestamp($rs[$i]);
            $this->organizationFromDate[$organizationId] = $organizationFromDate;

           $groupKey = join(',', $keyArr);

            if (!isset($this->values[$organizationId])) {
                $this->values[$organizationId] = array();
            }

            if (!isset($this->values[$organizationId][$groupKey])) {
                if (count($descriptionArr) > 0) {
                  $groupDescription = join(', ', $descriptionArr);
                } else {
                    $groupDescription = mytr('Calls');
                }
                $this->values[$organizationId][$groupKey] = array($groupDescription, 0, 0, 0, 0, 0);
            }

            list($groupKeyName, $countOfCalls1, $duration1, $cost1, $income1, $saving1) = $this->values[$organizationId][$groupKey];
            $this->values[$organizationId][$groupKey] = array($groupKeyName, $countOfCalls1 + $countOfCalls, $duration1 + $duration, $cost1 + $cost, $income1 + $income, $saving1 + $saving);

            // sfContext::getInstance()->getLogger()->info('INVOICE CALC STORE LINE: ' . $groupKeyName);
        }

        $stm->closeCursor();
    }
}

