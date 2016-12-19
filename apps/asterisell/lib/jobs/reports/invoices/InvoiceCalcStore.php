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
 * Calculated the totals of all customers, because a scan of data must be done in any case,
 * and it is cheaper doing it only one time for all customers.
 *
 * Consider only billable parties.
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
     * @var array organizationId => true for organizationId that are billable
     */
    public $billableOrganizationIds;

    /**
     * Process the store
     *
     * @param int $from
     * @param int|null $to
     * @param ArReport $reportParams
     * @param PropelPDO $conn
     */
    public function process($from, $to, ArReport $reportParams, PropelPDO $conn)
    {
        $this->fromTime = $from;
        $this->toTime = $to;

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
FROM ar_cdr
JOIN ar_telephone_prefix ON ar_cdr.ar_telephone_prefix_id = ar_telephone_prefix.id
WHERE '
. $conditionOnDirection . '
AND ar_cdr.calldate >= ?
AND ar_cdr.calldate < ?
GROUP BY
  ar_cdr.billable_ar_organization_unit_id
, ar_cdr.destination_type '
. $groupFields . '
';

        // sfContext::getInstance()->getLogger()->info('INVOICE: query is ' . $query);

        // Scan all CDRs in the date range, completing stats

        $params = array(fromUnixTimestampToMySQLTimestamp($this->fromTime), fromUnixTimestampToMySQLTimestamp($this->toTime));

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
            $income = $rs[$i];

            $i++;
            $cost = $rs[$i];

            $i++;
            $saving = $rs[$i];

            $i++;
            $duration = $rs[$i];

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

