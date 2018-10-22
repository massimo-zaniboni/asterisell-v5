<?php
// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell', 'ChannelUsage'));

/**
 * Check if there are suspicious calls.
 *
 * This job must be scheduled after call-rating.
 *
 * It runs according `app_check_cost_limits_after_minutes` and `check_frauds` setting.
 *
 * It can not detected calls that are active and not already registered in the CDR table.
 *
 * Warnings are put in the error table.
 *
 * See the code for more details.
 */
class CheckFrauds extends FixedJobProcessor
{

    /**
     * This file contains the date of last check of call cost limits.
     */
    const FILE_WITH_LAST_CHECK_DATE = "last_check_CheckFrauds";

    /**
     * @return always TRUE. Errors are reported on the error table.
     */
    public function process()
    {
        // Profiling
        //
        $time1 = microtime_float();

        $timeFrameInMinutes = sfConfig::get('app_check_cost_limits_after_minutes');

        $checkFile = self::FILE_WITH_LAST_CHECK_DATE;
        $checkLimit = strtotime("-$timeFrameInMinutes minutes");
        $mutex = new Mutex($checkFile);
        if ($mutex->maybeTouch($checkLimit)) {
            $this->checkAll();

            // Profiling
            //
            $time2 = microtime_float();
            $totTime = $time2 - $time1;
            return "CheckFrauds executed in $totTime seconds.";
        } else {
            return "CheckFrauds will be checked later, according application settings.";
        }
    }

    public function  checkAll()
    {
        $tot = $this->nrOfUnratedCalls();
        $maxTot = sfConfig::get('app_check_frauds_max_unprocessed_calls');

        if (!is_null($maxTot)) {
            if ($maxTot > 0) {
                if ($tot > $maxTot) {
                    // NOTE: advise repeatedly if the number is increasing
                    $incr = floor($tot / $maxTot);
                    $nextIncr = $incr + $maxTot;



$problemDuplicationKey = "CheckFrauds - unprocessed calls " . $incr;
$problemDescription = "CheckFrauds warning: there are $tot unprocessed calls.";
$problemEffect = "The total cost of these calls is undefined, and in case of frauds it can be high. If there will be $nextIncr unprocessed calls you will be advised. Some false positive warning is possible, because new calls made from customers are not processed immediately.";
$problemProposedSolution = "Complete rate and accounts settings, in order to correctly processing these unprocessed calls. If there are too much false positive increase the limit in the configuration file.";
ArProblemException::createWithoutGarbageCollection(
    ArProblemType::TYPE_ERROR,
    ArProblemDomain::RATES,
    null,
    $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
                }
            }
        }

        $this->checkCallsCostLimits();
        $this->checkCallsCostRespectPastUsage();
    }

    /**
     * Warn according `check_frauds_compare_xx_last_days_respect_customer_daily_max_cost`.
     *
     * @return TRUE if there are no errors.
     */
    private function checkCallsCostLimits()
    {
        $lastDays = sfConfig::get('app_check_frauds_compare_xx_last_days_respect_customer_daily_max_cost');

        if ($lastDays == 0) {
            return TRUE;
        }

        $timeFrame = strtotime("-" . $lastDays . " day");

        $connection = Propel::getConnection();
        $query = 'SELECT ar_party.id as party_id, ar_party.max_limit_30 as max_limit_30, SUM(cdr.income) as total_cost FROM ar_party, ar_office, ar_asterisk_account, ar_cdr WHERE ar_cdr.ar_asterisk_account_id = ar_asterisk_account.id AND ar_asterisk_account.ar_office_id = ar_office.id AND ar_office.ar_party_id = ar_party.id AND ar_cdr.calldate >= ? and ar_cdr.income > 0 GROUP BY ar_party.id;';
        $statement = $connection->prepare($query);
        // TODO check for timestamp conversion
        $statement->bindValue(0, $timeFrame);
        $statement->execute();

        $allOk = TRUE;
        while ($rs = $statement->fetch(PDO::FETCH_ASSOC)) {
            $costLimit30 = $rs['max_limit_30'];
            $effectiveCost = $rs['total_cost'];

            if (!is_null($costLimit30)) {
                $costLimit1 = bcdiv($costLimit30, 30, 0);
                $costLimit = bcmul($costLimit1, $lastDays, 0);

                if ($effectiveCost > $costLimit) {
                    $allOk = FALSE;

                    $id = $rs['party_id'];

                    // note: integer division, the number of times the fixed quantity stays
                    // inside the current limit
                    $costLimitIncr = $costLimit;
                    $incr = floor($effectiveCost / $costLimitIncr);

                    // consider an increase for repeated warnings...
                    $nextIncr = bcadd($effectiveCost, $costLimitIncr, 0);

                    $party = ArPartyPeer::retrieveByPk($id);



$problemDuplicationKey = "CheckFrauds - $id - cost limits " . $incr;
$problemDescription = "CheckFrauds warning: " . "party with id \"$id\" and name \"" . $party->getFullName() . "\" has spent in last $lastDays days " . from_db_decimal_to_monetary_txt_according_locale($effectiveCost) . " instead of allowed " . from_db_decimal_to_monetary_txt_according_locale($costLimit) . " (according his cost limit) ";
$problemEffect = "There can be some fraud. Another warning of this type will be sent if the costs will be greather than " . from_db_decimal_to_monetary_txt_according_locale($nextIncr);
$problemProposedSolution = "Inspect the CDRs in order to see if there is something of anomalous/suspect. If it is all right, maybe increase the customer limits.";
ArProblemException::createWithoutGarbageCollection(
    ArProblemType::TYPE_ERROR,
    ArProblemDomain::SAFETY,
    null,
    $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
                }
            }
        }

        return $allOk;
    }

    /**
     * Warn according `check_frauds_compare_x_last_months_respect_current_costs`.
     *
     * @return TRUE if there are no errors.
     */
    private function checkCallsCostRespectPastUsage()
    {
        $lastMonths = sfConfig::get('app_check_frauds_compare_x_last_months_respect_current_costs');

        if ($lastMonths == 0) {
            return TRUE;
        }

        // search past months, but not today.
        $minTime = strtotime("-" . $lastMonths * 30 . " day");
        $maxTime = strtotime("yesterday");

        $connection = Propel::getConnection();
        $query = 'select ar_asterisk_account_id, sum(income) as  total_cost from ar_cdr WHERE not destination_type = 0 and not destination_type = 4 and ar_cdr.calldate >= ? and ar_cdr.calldate < ? and weekday(cdr.calldate) =  weekday(current_date) and income > 0 and (hour(cdr.calldate) div 6) = (hour(current_time) div 6) group by ar_asterisk_account_id;';

        // fill the array with maximum values
        $statement = $connection->prepare($query);
        // TODO check for timestamp conversion
        $statement->set(0, $minTime);
        $statement->set(1, $maxTime);
        $statement->execute();

        $maxCostByAccount = array();
        while ($rs = $statement->fetch(PDO::FETCH_ASSOC)) {
            $accountId = $rs['ar_asterisk_account_id'];
            $totalCost = $rs['total_cost'];

            $maxCost = 0;
            if (array_key_exists($accountId, $maxCostByAccount)) {
                $maxCost =  $maxCostByAccount[$accountId];
            }

            if ($maxCost < $totalCost) {
                $maxCostByAccount[$accountId] = $totalCost;
            }
        }

        // check for accounts with last calls over the limit
        $statement = $connection->prepare($query);
        $statement->bindValue(0, strtotime('today'));
        $statement->bindValue(1, strtotime('tomorrow'));
        $rs = $statement->executeQuery();

        $allOk = TRUE;
        while ($rs = $statement->fetch(PDO::FETCH_ASSOC)) {
            $accountId = $rs['ar_asterisk_account_id'];
            $totalCost = $rs['total_cost'];

            $maxCost = 0;
            if (array_key_exists($accountId, $maxCostByAccount)) {
                $maxCost =  $maxCostByAccount[$accountId];
            }
            $maxCostMiddle = bcdiv($maxCost, 4, 0);
            $maxCostLimit = bcadd($maxCost, $maxCostMiddle, 0);

            if ($maxCostLimit < $totalCost && $maxCostLimit > 0) {
                $allOk = FALSE;

                // note: integer division, the number of times the fixed quantity stays
                // inside the current limit
                $costLimitIncr = $maxCostMiddle;
                $incr = floor($totalCost / $maxCostLimit);

                // consider an increase for repeated warnings...
                $nextIncr = bcadd($totalCost, $costLimitIncr, 0);

                $account = ArAsteriskAccountPeer::retrieveByPk($accountId);
                $office = $account->getArOffice();
                $party = $office->getArParty();



$problemDuplicationKey = "CheckFrauds - $accountId - past cost limits " . $incr;
$problemDescription = "CheckFrauds warning: " . "account with id \"$accountId\" and name \"" . $account->getName() . "\", associated to customer \"" . $party->getFullName() . "\", has spent in last hours " . from_db_decimal_to_monetary_txt_according_locale($totalCost) . ", that is greater than previous " . from_db_decimal_to_monetary_txt_according_locale($maxCost) . " maximum cost, calculated inspecting last $lastMonths months, and considering the same day of week and comparable hours of the day.";
$problemEffect = "There can be some fraud. Another warning of this type will be sent if the costs will be greather than " . from_db_decimal_to_monetary_txt_according_locale($nextIncr);
$problemProposedSolution = "Inspect the CDRs in order to see if there is something of anomalous/suspect. Adjust customer monthly cost if it is the case. Note: delete this error only when you are sure that the calls are no frauds, because calls older than 6 hours will not be checked again, and you will be not notified again. ";
ArProblemException::createWithoutGarbageCollection(
    ArProblemType::TYPE_ERROR,
    ArProblemDomain::CONFIGURATIONS,
    null,
    $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
            }
        }

        return $allOk;
    }


    /**
     * Check if there are too much unrated calls.
     */
    public function  nrOfUnratedCalls()
    {
        $connection = Propel::getConnection();
        $query = 'SELECT SUM(count_of_calls) as tot FROM ar_cdr WHERE ar_cdr.destination_type = ' . DestinationType::error . ' AND ar_cdr.error_destination_type = ' . DestinationType::outgoing;
        $statement = $connection->prepare($query);
        $statement->execute();

        $lastTot = 0;

        while ($rs = $statement->fetch(PDO::FETCH_ASSOC)) {
            $lastTot = $rs['tot'];
        }
        $statement->closeCursor();

        return $lastTot;
    }
}

?>
