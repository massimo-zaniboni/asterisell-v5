<?php

require 'generator_header.php';

// TODO disabled temporary budgetting support

echo '<?php';

?>

/**************************************************************
!!!                                                        !!!
!!! WARNING: This file is automatic generated.             !!!
!!!                                                        !!!
!!! In order to modify this file change the content of     !!!
!!!                                                        !!!
!!!    /module_template/call_report_template               !!!
!!!                                                        !!!
!!! and execute                                            !!!
!!!                                                        !!!
!!!    sh generate_modules.sh                              !!!
!!!                                                        !!!
**************************************************************/
use_helper('I18N', 'Number', 'Asterisell');

$moduleName = <?php echo "\"" . FieldsToShow::getModuleName($generateForAdmin) . "\""; ?>;

<?php if ($generateForAdmin) { ?>

    // Show if there are the problems to resolve in the header.
    //
    $conn = Propel::getConnection();
    $importantErrors = getNrOfRecordsInTable('ar_current_problem', $conn, "ar_problem_type_id = " . ArProblemType::TYPE_ERROR . ' OR ar_problem_type_id = ' . ArProblemType::TYPE_CRITICAL);
    $warningErrors = getNrOfRecordsInTable('ar_current_problem', $conn, "ar_problem_type_id = " . ArProblemType::TYPE_WARNING);
    $problemMsg = '';
    $thereIsError = false;

    if ($importantErrors > 0 || $warningErrors > 0) {
      $thereIsError = true;
      $alsoErrors = false;
      if ($importantErrors > 0) {
        $problemMsg = '<a href="' . url_for('problem/list') . '">' . $importantErrors .
          ' important</a> problems to solve';
        $alsoErrors = true;
      }

      if ($warningErrors > 0) {
        if ($alsoErrors) {
          $problemMsg .= ', and ';
        }
        $problemMsg .= '<a href="' . url_for('problem/list') . '">' . $warningErrors . ' warnings</a> to inspect';
      }

      $problemMsg = __('There are ') . $problemMsg . '. ';
    }

    $cdrErrors = CustomCDRServices::getInstance()->getCDRsWithErrorsByDestinationType(VariableFrame::$startFilterDate, VariableFrame::$endFilterDate, $conn);
    $problemType = CustomCDRServices::getInstance()->getProblemType($cdrErrors);

    if ($problemType !== ArProblemType::TYPE_INFO) {
      $thereIsError = true;
      $problemMsg .= 'In the selected time frame ' . CustomCDRServices::getInstance()->getErrorDescription($cdrErrors, null, 'cdrlist_unprocessed/list');
    }

    $reratingMsg = FixedJobProcessor::getScheduledRatingEventDescription();
    if (!is_null($reratingMsg)) {
       if ($thereIsError) {
         $problemMsg .= '<br/>';
       }
       $problemMsg .= $reratingMsg;
       $thereIsError = true;
    }

    if ($thereIsError) {
    echo '
    <div id="allertMessage">' . $problemMsg . '</div>';
    }

<?php } else { ?>

    // Generate for customer

    // Calc budget

    /* XXX
    $partyTotLimitStr = ' -- ';

    // Calc the last 30 days limits for customer
    //
    $partyId = $sf_user->getPartyId();
    $party = ArPartyPeer::retrieveByPK($partyId);
    $partyTotLimit = $party->getMaxLimit30();
    if ($partyTotLimit > 0) {
    $partyTotLimitStr = format_from_db_decimal_to_currency_locale($partyTotLimit);
    } else {
    $partyTotLimitStr = ' -- ';
    }
    $checker = new CheckCallCostLimit();
    $partyTotCost = $checker->checkPartyLimits($partyId);
    $partyTotCostStr = format_from_db_decimal_to_currency_locale($partyTotCost);
    */

<?php } ?>

$currency = sfConfig::get('app_currency');

///////////////////////
// Show Column Names //
///////////////////////

echo '<table cellspacing="0" class="sf_admin_list"><thead>';

// Count cols

<?php

// count, duration, cost
$countCols = 3;

// income, earn
if (FieldsToShow::isFieldToShow($generateForAdmin, FieldsToShow::EARN)) {
    $countCols += 2;
}

if (FieldsToShow::isFieldToShow($generateForAdmin, FieldsToShow::COST_SAVINGS)) {
    $countCols++;
}

// these cols are needed for the buttons
if ($generateForAdmin) {
    if ($countCols < 5) {
        $countCols = 5;
    }
} else {
    if ($countCols < 4) {
        $countCols = 4;
    }
}

if (!$generateForAdmin) {
    // add columns for budgetting
    $countCols += 2;
}
?>

// Filter description
echo '
<tr>
    <th colspan="' . <?php echo $countCols ?> . '">' . VariableFrame::$filterDescription . '</th>
</tr>
';
echo '
<tr>
    <th colspan="' . <?php echo $countCols ?> . '"></th>
</tr>
';

// Headers with totals

<?php
$usedCols = 2;
?>

echo '
<tr>' . '
    <th>' . __('Calls') . '</th>
    ' . '
    <th>' . __('Duration') . '</th>
    ';

    <?php

    if (FieldsToShow::isFieldToShow($generateForAdmin, FieldsToShow::EARN)) {
        $usedCols += 3;
        ?>
        echo '
        <th>' . __('Customer Income (VAT excluded)') . '</th>' . '
        <th>' . __('Vendor Cost (VAT excluded)') . '</th>' . '
        <th>' . __('Earn (VAT excluded)') . '</th>';
    <?php
    } else {
        $usedCols++;
        if ($generateForAdmin) {
            ?>
            echo '<th>' . __('Vendor Cost (VAT excluded)') . '</th>';
        <?php } else { ?>
            echo '<th>' . __('Cost (VAT excluded)') . '</th>';
        <?php
        }
    }

    if (FieldsToShow::isFieldToShow($generateForAdmin, FieldsToShow::COST_SAVINGS)) {
        $usedCols++;
        ?>
        echo '<th>' . __('Cost Savings (VAT excluded)') . '</th>';
    <?php
    }

    if (!$generateForAdmin) {
        ?>

        /* XXX
        // Customer specific header with budgetting

        $last30LimitsStr = "";
        if (isCostLimitTimeFrame30Days()) {
        $last30LimitsStr = __('Last 30 days Limit');
        } else {
        $last30LimitsStr = __('Month Limit');
        }

        $lastCostLimitsStr = "";
        if (isCostLimitTimeFrame30Days()) {
        $lastCostLimitsStr = __('Last 30 days Cost');
        } else {
        $lastCostLimitsStr = __('Current Month Cost');
        }

        echo '<th>' . $last30LimitsStr . '</th>' . '<th>' . $lastCostLimitsStr . '</th>';
        */

    <?php
    }

    for ($i = $usedCols; $i < $countCols; $i++) {
        ?>
        echo '<th></th>';
    <?php
    }

    ?>

    echo '</tr></thead>';

/////////////////
// Show totals //
/////////////////

echo '<tbody><tr class="sf_admin_row_1">' . '<td>' . VariableFrame::$countOfRecords
        . '</td>' . '<td>' . format_minute(VariableFrame::$totSeconds) . '</td>';
    <?php

    $usedCols = 2;

    if (FieldsToShow::isFieldToShow($generateForAdmin, FieldsToShow::EARN)) {
        $usedCols += 3;
        ?>
        echo '<td>' . format_from_db_decimal_to_currency_locale(VariableFrame::$totIncomes) . '</td>';
        echo '<td>' . format_from_db_decimal_to_currency_locale(VariableFrame::$totCosts) . '</td>';
        echo '<td>' . format_from_db_decimal_to_currency_locale(VariableFrame::$totEarn) . '</td>';
    <?php
    } else {
        if (FieldsToShow::isFieldToShow($generateForAdmin, FieldsToShow::COST)) {
            $usedCols += 1;
        ?>
          echo '<td>' . format_from_db_decimal_to_currency_locale(VariableFrame::$totCosts) . '</td>';
        <?php
        }

        if (FieldsToShow::isFieldToShow($generateForAdmin, FieldsToShow::INCOME)) {
          $usedCols += 1;
        ?>
          echo '<td>' . format_from_db_decimal_to_currency_locale(VariableFrame::$totIncomes) . '</td>';
        <?php
        }
        ?>

    <?php
    }

    if (FieldsToShow::isFieldToShow($generateForAdmin, FieldsToShow::COST_SAVINGS)) {
        $usedCols += 1;

        ?>

        echo '<td>' . format_from_db_decimal_to_currency_locale(VariableFrame::$totSavingCosts) . '</td>';
    <?php
    }

    if (!$generateForAdmin) {

        /* XXX
        // Customer budgetting columns

        $usedCols += 2;
        */
        ?>

        // XXX echo '<td>' . $partyTotLimitStr . '</td>' . '<td>' . $partyTotCostStr . '</td>';

    <?php
    }

    for ($i = $usedCols; $i < $countCols; $i++) {
        ?>
        echo '<td></td>';
    <?php
    }

    ?>

    echo '</tr>';

//////////////////
// Show Buttons //
//////////////////

echo '<tr class="sf_admin_row_0">';
        // Show documents to read

        list($allDocumentsToRead, $allDocuments) = $sf_user->countReports();

        if ($allDocuments == 0 && $allDocumentsToRead == 0) {
        echo '<td colspan="2">' . link_to(__('no documents to read'), 'viewdocument/list');
            } else if ($allDocumentsToRead == 0) {
            echo '<td  colspan="2">' . link_to($allDocumentsToRead . ' ' . __('new') . __(' of ') . $allDocuments . ' ' . __('documents'),
            'viewdocument/list');
            } else {
            echo '<td  colspan="2">' . link_to($allDocumentsToRead . ' ' . __('new') . __(' of ') . $allDocuments . ' ' . __('documents'),
            'viewdocument/list');
            }

            <?php
            if ($generateForAdmin) {
                ?>

                $conn = Propel::getConnection();

                // Report set

                $stmt = $conn->prepare('
                SELECT COUNT(*)
                FROM ar_report_set
                WHERE must_be_reviewed = ?
                ');
                $stmt->execute(array(true));

                $new = 0;
                while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
                $new = $rs[0];
                }
                $stmt->closeCursor();

                if ($new > 0) {
                echo '<br/>' . link_to($new . ' ' . __('generated documents sets to review and confirm, before sending to users'), 'report_set/list?' . ArReportSet::SHOW_ONLY_TO_REVIEW_PARAM_NAME . '=true');
                           }

                           // Single Documents

                           $stmt = $conn->prepare('
                           SELECT COUNT(*)
                           FROM ar_report
                           WHERE produced_report_already_reviewed = ?
                           AND ar_report_set_id IS NULL
                           AND is_template = FALSE
                           ');
                           $stmt->execute(array(false));

                           $new = 0;
                           while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
                           $new = $rs[0];
                           }
                           $stmt->closeCursor();

                           if ($new > 0) {
                           echo '<br/>' . link_to($new . ' ' . __('generated documents to review and confirm, before sending to users'), 'report/list?' . ArReportSet::SHOW_ONLY_TO_REVIEW_PARAM_NAME . '=true');
                }

            <?php
            }
            $remainingCols = $countCols - 2;
            ?>

            echo '</td><td colspan="' . <?php echo $remainingCols ?> . '">';
            echo form_tag("$moduleName/userActs");

        echo '<input type="submit" class="user_actions" name="exportToCsv" value="' . __('Export to CSV') . '"/>';
        <?php
        if ($generateForAdmin) {
        ?>
          echo '<input type="submit" class="user_actions" name="exportExtensions" value="' . __('Extensions/DIDS') . '"/>';
          echo '<input type="submit" class="user_actions" name="resetCallsCost" value="' . __('Re-rate Calls in Timeframe') .'"/>';
          echo '<input type="submit" class="user_actions" name="resetRecentCallsCost" value="' . __('Re-rate Not Yet Billed Calls') .'"/>';
        <?php
        }
        ?>

        echo '</form></td></tr>';
        echo '</tbody></table><br/>';


<?php if ($generateForAdmin) { ?>

    if (VariableFrame::$showChannelUsage) {
    // load this heavy helper only when needed
    //
    use_helper('Number', 'ChannelUsage');

    $stats = new StatsOnCalls(VariableFrame::$filterCondition, VariableFrame::$startFilterDate, VariableFrame::$endFilterDate);

    if (!$stats->isEmpty()) {
    echo '<h2>Total calls, respecting the filter/condition: ' . $stats->totCalls .' (' . $stats->getMeanCalls() . '
        calls per ' . $stats->numDays . ' days)</h2>';

    echo '<h2>Max number of concurrent calls: ' . $stats->maxNrOfConcurrentCalls . ' (safe limit is ' .
        $stats->concurrentCallsSafeLimit .')</h2>';

    echo '<h2>Number of Calls above the safe limit: ' . $stats->dangerousCalls . ' (' . $stats->getDangerousCallsPerc()
        . '% of ' . $stats->totCalls . ' total calls)</h2>';

    $graph1 = new CalculatedDistributionGraph(__('Concurrent Calls Grouped by Occurrence'), __('Show the number of calls, performed when there were already a certain number of active calls. This graph shows the typical bandwidth usage/requirements.'), $stats->concurrentCallsDistribution, $stats->concurrentCallsSafeLimit);
    echo $graph1->getGraphInsert();

    $graph2 = new CalculatedGraph(__('Concurrent Calls'), __('Show for each day, the max number of active concurrent calls. This allows to inspect the maximum bandwidth usage over time.'),  $stats->nrOfConcurrentCalls, $stats->concurrentCallsSafeLimit);
    echo $graph2->getGraphInsert();

    $graph3 = new CalculatedGraph(__('Total Calls'), __('Show for each day, the total number of processed calls.'), $stats->nrOfTotCalls);
    echo $graph3->getGraphInsert();
    }
    }
<?php } ?>
