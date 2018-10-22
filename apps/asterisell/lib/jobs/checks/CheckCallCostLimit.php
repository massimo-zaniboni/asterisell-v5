<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Check Customers reaching their limits about cost of calls
 * and inform them.
 *
 * The check is performed according 'check_cost_limits_after_minutes' configuration parameter,
 * and emails are sent according 'repeat_advise_of_high_cost_limit_after_days' configuration parameter.
 *
 * Produce a "CustomerHasHighCallCostEvent" event.
 */
class CheckCallCostLimit extends FixedJobProcessor
{

    public function process()
    {

        $timeFrameInMinutes = sfConfig::get('app_check_cost_limits_after_minutes');

        $checkFile = get_class($this);
        $checkLimit = strtotime("-$timeFrameInMinutes minutes");
        $mutex = new Mutex($checkFile);
        if ($timeFrameInMinutes > 0 && $mutex->maybeTouch($checkLimit)) {
            ArProblemException::garbageCollect(get_class($this), null, null);
            $log = $this->checkLimits();
            return $log;
        } else {
            return "Call cost limits will be checked later, every $timeFrameInMinutes minutes, according application settings.";
        }
    }

    private function checkLimits()
    {
        $prof = new JobProfiler('accounts');

        $info = OrganizationUnitInfo::getInstance();

        //
        // Configure Params
        //

        $nowDate = date("c");
        $method = trim(sfConfig::get('app_max_cost_limit_timeframe'));

        if ($method === '30') {
            $timeframe = strtotime("-30 day");
            $timeframeDescription = "in the last 30 days";
        } else {
            $timeframe = strtotime(date('Y') . '-' . date('m') . '-' . '01');
            $timeframeDescription = "on current month";
        }

        $sendEmailAfterDays = sfConfig::get('app_repeat_advise_of_high_cost_limit_after_days');
        $isCustomerAdviseEnabled = TRUE;
        if ($sendEmailAfterDays == 0) {
            $isCustomerAdviseEnabled = FALSE;
            $adviseTimeFrame = time();
        } else {
            $adviseTimeFrame = strtotime("-" . "$sendEmailAfterDays day");
        }

        //
        // Retrieve info about the limits associated to parties.
        //

        $query = '
        SELECT
          s.ar_organization_unit_id
        , p.id
        , p.max_limit_30
        , p.last_email_advise_for_max_limit_30
        FROM ar_organization_unit_has_structure AS s
        INNER JOIN ar_party AS p
        ON s.ar_party_id = p.id
        WHERE max_limit_30 > 0
        ';

        $unitIdWithIncomeLimit = array();
        $partyIdLastSentEmail = array();
        $stm = Propel::getConnection()->prepare($query);
        $stm->execute();
        while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
            $id = $rs[0];
            $partyId = $rs[1];
            $limit = $rs[2];
            $lastAdvise = fromMySQLTimestampToUnixTimestamp($rs[3]);

            $unitIdWithIncomeLimit[$id] = $limit;
            $partyIdLastSentEmail[$partyId] = $lastAdvise;
        }
        $stm->closeCursor();

        //
        // Calculate sum of incomes
        //

        $query = '
        SELECT cached_parent_id_hierarchy, SUM(income)
        FROM ar_cdr
        WHERE calldate >= ?
        GROUP BY cached_parent_id_hierarchy
        ';

        /**
         * @var array $totIncomes from unitId to total incomes
         */
        $totIncomes = array();

        $stm = Propel::getConnection()->prepare($query);
        $stm->execute(array(fromUnixTimestampToMySQLTimestamp($timeframe)));
        while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
            $prof->incrementProcessedUnits();
            $organizationIds = $rs[0];
            $income = $rs[1];

            foreach (explode('/', $organizationIds) as $unitId) {
                // A cost is propagated to the entire hierarchy chain.
                if (array_key_exists($unitId, $unitIdWithIncomeLimit)) {
                    // Consider only units with a limit
                    addToStatsArray($totIncomes, $unitId, $income);
                }
            }
        }
        $stm->closeCursor();

        //
        // Compare Totals with Limits and Generate Warnings.
        // Generate a separate error message for each user, in order to use the automatic escalation process
        // of advise admin of errors.
        //

        foreach ($unitIdWithIncomeLimit as $unitId => $costLimit) {

            if (array_key_exists($unitId, $totIncomes)) {
                $effectiveCost = $totIncomes[$unitId];
            } else {
                $effectiveCost = 0;
            }

            $this->assertCondition(!is_null($effectiveCost));
            $this->assertCondition($costLimit > 0);

            if ($effectiveCost > $costLimit) {

                // Use a different type of error according the involved sums
                if ($effectiveCost > 4 * $costLimit) {
                    $errorType = ArProblemType::TYPE_CRITICAL;
                    $errorDomain = ArProblemDomain::SAFETY;
                } else if ($effectiveCost > 2 * $costLimit) {
                    $errorType = ArProblemType::TYPE_ERROR;
                    $errorDomain = ArProblemDomain::SAFETY;
                } else {
                    $errorType = ArProblemType::TYPE_WARNING;
                    $errorDomain = ArProblemDomain::CONFIGURATIONS;
                }

                if ($effectiveCost > 4 * $costLimit) {
                    $problemEffect = 'There can be a security breach, and the calls are placed from some hacker. ';
                    $problemSolution = 'Check if there is some security breach in you VoIP servers, or accounts. ';;
                } else if ($effectiveCost > 2 * $costLimit) {
                    $problemEffect = 'There can be a security breach, and the calls are placed from some hacker. ';
                    $problemSolution = 'Check if there is some security breach in you VoIP servers, or accounts/users.';
                } else {
                    $problemEffect = '';
                    $problemSolution = '';
                }

                $problemSolution .= "\nIf the calls are valid calls, then the user limits configured in the party section, must be increased.";
                $problemEffect .= "\nIf there are more problems of this type, or the problem became worse, the administrator will be advised again.";

                if ($isCustomerAdviseEnabled) {
                    $problemEffect .= "\nBecause the configuration option is enabled, the first user of the account hierarchy, with a valid email, will be informed of the problem. Also the first billable user of the account hierarchy will be notified (in case it is not the same).";
                } else {
                    $problemEffect .= "\nBecause the configuration option is not enabled, the user will be not notified of the problem.";
                }

                $partyId = $info->getArPartyId($unitId, $nowDate);
                $name = $info->getFullNameAtDate($unitId, $nowDate, false, false, null, false, false);

                ArProblemException::createWithGarbageCollection(
                    $errorType,
                    $errorDomain,
                    null,
                    get_class($this) . " - $unitId $errorType $errorDomain",
                    get_class($this),
                    null,
                    null,
                    'Account ' . $name . ' with identifier ' . $unitId . ' has spent ' . from_db_decimal_to_monetary_txt_according_locale($effectiveCost) . " instead of allowed " . from_db_decimal_to_monetary_txt_according_locale($costLimit) . ' (according his cost limit) ' . $timeframeDescription . '.',
                    $problemEffect,
                    $problemSolution,
                    null);

                // Advise the customer via mail only at a certain interval.
                if ($isCustomerAdviseEnabled) {
                    if (is_null($partyIdLastSentEmail[$partyId]) || ($partyIdLastSentEmail[$partyId] < $adviseTimeFrame)) {
                        $d = new CustomerHasHighCallCostEvent();
                        $d->unitId = $unitId;
                        $d->arPartyId = $partyId;
                        $d->effectiveCost = $effectiveCost;
                        $d->costLimit = $costLimit;
                        $d->fromDate = $timeframe;
                        $d->toDate = $nowDate;
                        $d->method = $method;

                        ArJobQueuePeer::addNew($d, null, null);

                        // NOTE: $party->getLastEmailAdviseForMaxLimit30 is updated
                        // ad the moment of email sending.
                    }
                }
            }
        }

        return $prof->stop();
    }
}
