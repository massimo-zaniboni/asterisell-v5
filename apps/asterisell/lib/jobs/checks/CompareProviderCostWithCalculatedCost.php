<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Compare costs calculated from Asterisell, with costs signaled from the VoIP provider.
 * Signal a unique error message with the totals, and only for the current unbilled time-frame.
 * The admin can review calls with unexpected costs in the call report, using filters.
 */
class CompareProviderCostWithCalculatedCost extends FixedJobProcessor
{

    public function process()
    {
        $conn = Propel::getConnection();
        $fromDate = self::getOfficialCallDate($conn);
        if (is_null($fromDate)) {
            $fromDate = self::getGlobalStartingDateForCDRProcessinng($conn);
        }

        $prof = new JobProfiler('days in the past');
        $daysInThePast = ceil(abs(time() - $fromDate) / (60 * 60 * 24));
        $prof->addToProcessedUnits($daysInThePast);

        ArProblemException::garbageCollect(get_class($this), $fromDate, null);

        $query = 'SELECT COUNT(id)
                  , SUM(cost)
                  , SUM(expected_cost)
                  , SUM(cost - expected_cost)
                  , SUM(ABS(cost - expected_cost))
                  FROM  ar_cdr
                  WHERE calldate >= ?
                  AND   (NOT ar_cdr.destination_type = ?)
                  AND   (NOT ar_cdr.destination_type = ?)
                  AND   expected_cost IS NOT NULL
                  AND   cost <> expected_cost
                  ';
        $stmt = $conn->prepare($query);
        $stmt->execute(array(fromUnixTimestampToMySQLTimestamp($fromDate)
        , DestinationType::ignored
        , DestinationType::known_error
        ));

        while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
            $totCDRS = $rs[0];
            $totCosts = $rs[1];
            $totExpectedCosts = $rs[2];
            $difference = $rs[3];
            $absoluteDifference = $rs[4];

            if ($totCDRS > 0) {
                $problemDuplicationKey = "Check expected costs from " . $fromDate;
                $problemDescription = "In the yet to bill time-frame, from " . fromUnixTimestampToSymfonyStrDate($fromDate)
                    . " there are " . $totCDRS . " CDRs with unexpected vendor calculated cost"
                    . ".\n\nThe total calculated cost from Asterisell is " . from_db_decimal_to_monetary_txt_according_locale($totCosts)
                    . ", and it is different from total expected cost indicated from the Vendor, that is " . from_db_decimal_to_monetary_txt_according_locale($totExpectedCosts)
                    . ".\n\nThe relative difference (compensating positive and negative differences) is " . from_db_decimal_to_monetary_txt_according_locale($difference)
                    . ".\n\nThe absolute difference (summing absolute differences) is " . from_db_decimal_to_monetary_txt_according_locale($absoluteDifference);
                $problemEffect = "There are differences between what you expect to pay for the calls, and what the vendor will bill to you.";
                $problemProposedSolution = "You can filter the calls with unexpected vendor costs, in the call report, using the \"vendor cost filter\".\n\nYou can click on the cost/income link, for inspecting the rating details, and comparing cost value with expected_cost value.\n\nYou should check if Asterisell is applying the correct vendor rate specifications.\n\nThe most common error is that you are applying wrong rates, and you had to align Asterisell rates, with vendor rates.\n\nOtherwise there are errors in vendor calcs, or Asterisell calcs.";
                ArProblemException::createWithGarbageCollection(
                    ArProblemType::TYPE_ERROR,
                    ArProblemDomain::RATES,
                    null,
                    $problemDuplicationKey,
                    get_class($this),
                    $fromDate,
                    null,
                    $problemDescription,
                    $problemEffect,
                    $problemProposedSolution);
            }

        }
        $stmt->closeCursor();

        return "Checked " . $prof->stop();
    }
}