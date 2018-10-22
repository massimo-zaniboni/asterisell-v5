<?php

require 'generator_header.php';

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

$conn = Propel::getConnection();
$currency = sfConfig::get('app_currency');
$params = ArParamsPeer::getDefaultParams();
$officialCallDate = fromMySQLTimestampToUnixTimestamp($params->getOfficialCalldate());

$detailsCols = count(VariableFrame::$headerColOrder);
$allCols = $detailsCols + 1;
$ln = 1;

/**
 * Add a data cell in the status bar.
 *
 * @param int $allCols
 * @param string $style
 * @param string $content
 * @param bool $completeRow
 */
function echoStatusTableDT($allCols, $style, $content, $completeRow) {
  static $countCells = 0;

  if ($completeRow) {
    if (fmod($countCells++, $allCols) == 0) {
      return;
    } else {
      echo '<td></td>';
      echoStatusTableDT($allCols, '', '', true);
    }
  } else {
    if (fmod($countCells++, $allCols) == 0) {
      echo '<tr>';
    }
    echo '<td' . $style . '>' . $content . '</td>';
    if (fmod($countCells, $allCols) == 0) {
      echo '</tr>';
    }
  }
}


// -------------------
// Show stats of calls

// Show col names:
//
// |filter descr.|  column    |...
// |type|subtype |value%|value|...

echo '<table cellspacing="0" class="sf_admin_list"><thead>';
echo '<tr><th style="text-align:right;">' . VariableFrame::$filterDescription . '</th>';
foreach(VariableFrame::$headerColOrder as $col) {
   echo '<th style="text-align:right;">' . VariableFrame::$headerColNames[$col] . '</th>';
}
echo '</tr></thead><tbody>';

// Show totals
echo '<tr class="sf_admin_row_0"><td style="text-align:right;">' . __('Totals') . '</td>';
foreach(VariableFrame::$headerColOrder as $col) {
  echo '<td style="text-align:right;">' . printValue(statsArrayValue(VariableFrame::$headerTotals, $col), VariableFrame::$headerColFormat[$col]) . '</td>';
}
echo '</tr>';

// Show single rows with details
$ln = 0;
foreach(VariableFrame::$headerRowOrder as $row) {
  if (array_key_exists($row, VariableFrame::$headerTable)) {
    $rowArr = VariableFrame::$headerTable[$row];
    $rowName = VariableFrame::$headerRowNames[$row];
    foreach($rowArr as $rowDetailName => $rowDetails) {
      // use a different color for odd rows
      $odd = fmod(++$ln, 2);
      echo '<tr class="sf_admin_row_' . $odd . '">';
      echo '<td style="text-align:right;">' . $rowName . ': ' . $rowDetailName . '</td>';
      foreach(VariableFrame::$headerColOrder as $col) {
        $v = $rowDetails[$col];
        $t = VariableFrame::$headerTotals[$col];
        if ($t == 0) {
          $perc = 100;
        } else {
          $perc = intval(ceil(($v * 100.0) / ($t * 1.0)));
        }
        echo '<td style="text-align:right;">' . $perc . '%  /  ' . printValue($v, VariableFrame::$headerColFormat[$col]) . '</td>';
      }
    }
  }
  echo '</tr>';
}

    // Show actions
    echo '<tr><th colspan="' . $allCols . '"></th></tr>';
    echo '<tr><th style="text-align:center;" colspan="' . $allCols . '">'
         . form_tag("$moduleName/userActs")
         . '<input type="submit" class="user_actions" name="exportToCsv" value="' . __('Export to CSV') . '"/>';

    <?php
    if ($generateForAdmin) {
    ?>
          echo '<input type="submit" class="user_actions" name="exportExtensions" value="' . __('Extensions/DIDS') . '"/>';
          echo '<input type="submit" class="user_actions" name="resetCallsCost" value="' . __('Re-rate Calls from Selected Calldate') .'"/>';
          echo '<input type="submit" class="user_actions" name="resetRecentCallsCost" value="' . __('Re-rate Not Yet Billed Calls') .'"/>';
    <?php
    }
    ?>
    echo '<tr><th colspan="' . $allCols . '"></th></tr>';
    echo '</form></th></tr>';


    // ---------------------------------
    // Show unbilled calls and status

    // Draw the status bar
    //
    // | Status title              |
    // |value1|value2|value3       |

 <?php if ($generateForAdmin) { ?>
    echoStatusTableDT(
             $allCols
           , ''
           , fromUnixTimestampToSymfonyStrDate($officialCallDate) . ' is last billing date'
           , false);

    // Show rated and unrated calls

    list($ratedCDRS, $unratedCDRS) = CustomCDRServices::getInstance()->getRatedCDRStats($officialCallDate, null, $conn);
    $totCallsByType = array();
    $totCalls = 0;
    $destTypes = array();
    foreach($ratedCDRS as $destType => $count) {
      $totCalls += $count;
      addToStatsArray($totCallsByType, $destType, $count);
      $destTypes[$destType] = true;
    }

    foreach($unratedCDRS as $destType => $count) {
      $totCalls += $count;
      addToStatsArray($totCallsByType, $destType, $count);
      $destTypes[$destType] = true;
    }

    foreach($destTypes as $destType => $ignore) {
      echoStatusTableDT(
             $allCols
           , ''
           , printValue(statsArrayValue($ratedCDRS, $destType), 0) . ' yet to bill '
           . DestinationType::getUntraslatedName($destType)
           . ' calls'
           , false);

      $t = $totCallsByType[$destType];
      $v = statsArrayValue($unratedCDRS, $destType);
      if ($t == 0) {
        $perc = 100;
      } else {
        $perc = intval(round(($v * 100.0) / ($t * 1.0)));
      }
      if ($perc > 1) {
        $style = ' style="background:orange;"';
      } else {
        $style = '';
      }

      echoStatusTableDT(
           $allCols
           , $style
           , $perc . '%  /  '
             . printValue(statsArrayValue($unratedCDRS, $destType), 0) . ' '
             . DestinationType::getUntraslatedName($destType)
             . ' calls with errors'
           , false);
    }

    // Show rating problems

    $importantErrors = getNrOfRecordsInTable('ar_current_problem', $conn, "ar_problem_type_id = " . ArProblemType::TYPE_ERROR . ' OR ar_problem_type_id = ' . ArProblemType::TYPE_CRITICAL);
    $warningErrors = getNrOfRecordsInTable('ar_current_problem', $conn, "ar_problem_type_id = " . ArProblemType::TYPE_WARNING);

    if ($importantErrors > 0) {
      echoStatusTableDT(
           $allCols
           ,' style="background:orange;"'
           , $importantErrors
             . ' <a href="' . url_for('problem/list') . '">important problems to solve</a>'
           , false);
    }

    if ($warningErrors > 0) {
      echoStatusTableDT(
           $allCols
           ,''
           , $warningErrors . ' <a href="' . url_for('problem/list') . '">warnings to inspects</a>'
           , false);
    }

<?php } ?>
    // Show documents to read

    list($allDocumentsToRead, $allDocuments) = $sf_user->countReports();

    $readDocuments = $allDocuments - $allDocumentsToRead;
    if ($readDocuments > 0) {
      echoStatusTableDT(
           $allCols
           ,''
           , $readDocuments . ' ' . link_to(__('already read documents'), 'viewdocument/list')
           , false);
    }

    if ($allDocumentsToRead > 0) {
      echoStatusTableDT(
           $allCols
           ,' style="background:orange;"'
           , $allDocumentsToRead . ' ' .link_to(__('new documents to read'), 'viewdocument/list')
           , false);
    }

 <?php if ($generateForAdmin) { ?>
     // Show report sets to send for the administrator
    $conn = Propel::getConnection();

    $stmt = $conn->prepare('
              SELECT COUNT(*)
              FROM ar_report_set
              WHERE must_be_reviewed = ?
              ');
    $stmt->execute(array(1));

    $new = 0;
    while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
             $new = $rs[0];
    }
    $stmt->closeCursor();

    if ($new > 0) {

      echoStatusTableDT(
           $allCols
           ,' style="background:orange;"'
           , $new . ' ' . link_to('generated report sets to review and confirm', 'report_set/list?' . ArReportSet::SHOW_ONLY_TO_REVIEW_PARAM_NAME . '=true')
           , false);
    }

    $stmt = $conn->prepare('
                   SELECT COUNT(*)
                   FROM ar_report
                   WHERE produced_report_already_reviewed = ?
                   AND ar_report_set_id IS NULL
                   AND is_template = FALSE
                   ');
    $stmt->execute(array(0));

    $new = 0;
    while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
      $new = $rs[0];
    }
    $stmt->closeCursor();

    if ($new > 0) {

      echoStatusTableDT(
           $allCols
           ,' style="background:orange;"'
           , $new . ' ' . link_to('generated reports to review and confirm', 'report/list?' . ArReportSet::SHOW_ONLY_TO_REVIEW_PARAM_NAME . '=true')
           , false);
    }

    // Show rerating events
    $reratingMsg = FixedJobProcessor::getScheduledRatingEventDescription();
    if (!is_null($reratingMsg)) {

      echoStatusTableDT(
            $allCols
           ,' style="background:orange;"'
           , $reratingMsg
           , false);
     }

<?php } ?>

 echoStatusTableDT($allCols,'','' ,true);

 echo '</tbody></table><br/>';

