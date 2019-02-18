<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

// see notes in the generator for more info...
require 'generator_header.php';

echo '<?php';
echo "\n";

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

use_helper('Markdown');

$nrOfCols = <?php echo FieldsToShow::getCountFieldsToShow($generateForAdmin); ?>;

// ----------------
// Table header

$moduleName = <?php echo "'". FieldsToShow::getModuleName($generateForAdmin) . "'"; ?>;

echo '<table cellspacing="0" class="sf_admin_list">';
echo '<thead>';
echo '<tr>';
    
include_partial('list_th_tabular');

echo '</tr>';
echo '</thead>';
echo '<tfoot>';
echo '<tr><th colspan="' . $nrOfCols . '"/th>';
echo '<div class="float-right">';

////////////////
// Pagination //
////////////////

  $currPage = $sf_request->getParameter('page', 1);
  displayCalls($moduleName, $currPage, VariableFrame::$startOrganizationId);

echo '</tbody>';
echo '</table>';

function get_debug_cdr_link($r) {
  return 'debug_cdr/edit?calldate=' . urlencode(strtotime($r['calldate'])) . '&is_service_cdr=' . urlencode($r['is_service_cdr']) . '&id=' . urlencode($r['id']);
}

function displayCalls($moduleName, $currPage, $rootFilterOrganizationId) {

  // NOTE: this is a template function, and the params are php template, and they are not part of the
  // function params.

  $nrOfRecords = VariableFrame::$countOfRecords;

  $recordsPerPage = sfConfig::get('app_how_many_calls_in_call_report');
  $nrOfPages = ceil($nrOfRecords / $recordsPerPage);
  $lastPage = $nrOfPages;
  $haveToPaginate = ($nrOfPages > 1);

  // How many indexed pages in the navigation bar.
  //
  // <prev - 1 - 2 - 3 - 4 - 5 - next>
  $pagesInTheBar = 5;

  $centerPagesinInTheBar = ceil($pagesInTheBar / 2);
  $leftmostPage = $currPage - $centerPagesinInTheBar;
  if ($leftmostPage < 1) {
    $leftmostPage = 1;
  }

  $prevPage = $currPage - 1;
  if ($prevPage < 1) {
    $prevPage = 1;
  }

  $nextPage = $currPage + 1;
  if ($nextPage > $nrOfPages) {
    $nextPage = $nrOfPages;
  }

  if ($haveToPaginate) {

    echo link_to(image_tag(sfConfig::get('sf_admin_web_dir').'/images/first.png', array('align' => 'absmiddle', 'alt' => __('First'), 'title' => __('First'))), $moduleName . '/list?page=1');

    echo link_to(image_tag(sfConfig::get('sf_admin_web_dir').'/images/previous.png', array('align' => 'absmiddle', 'alt' => __('Previous'), 'title' => __('Previous'))), $moduleName . '/list?page='.$prevPage);

    for($i = $leftmostPage, $checkRecords = ($leftmostPage - 1) * $recordsPerPage; $i <= $leftmostPage + $pagesInTheBar; $i++, $checkRecords += $recordsPerPage) {
      if ($checkRecords < $nrOfRecords) {
        echo link_to_unless($i == $currPage, '  ' . $i . '  ', $moduleName . '/list?page='.$i);
      }
    }

    echo link_to(image_tag(sfConfig::get('sf_admin_web_dir').'/images/next.png', array('align' => 'absmiddle', 'alt' => __('Next'), 'title' => __('Next'))), $moduleName . '/list?page='.$nextPage);

    echo link_to(image_tag(sfConfig::get('sf_admin_web_dir').'/images/last.png', array('align' => 'absmiddle', 'alt' => __('Last'), 'title' => __('Last'))), $moduleName . '/list?page='.$lastPage);
  }

  echo '</div>';

  echo format_number_choice('[0] no result|[1] 1 result|(1,+Inf] %1% results', array('%1%' => $nrOfRecords), $nrOfRecords);

  echo '</th></tr></tfoot><tbody>';

  $useLinkToCDR = (VariableFrame::$groupOn == 0 || VariableFrame::$groupOn == 4);

  $currency = sfConfig::get('app_currency');

  // Show all records
  $info = OrganizationUnitInfo::getInstance();
  $stm = Propel::getConnection()->prepare(getQueryFromParts(
                          VariableFrame::$listFrom,
                          VariableFrame::$listCondition,
                          VariableFrame::$listGroupBy,
                          VariableFrame::$listSort,
                          VariableFrame::$listSelect,
                          $recordsPerPage,
                          ($currPage - 1) * $recordsPerPage));

  $stm->execute(VariableFrame::$listParams);
  $ln = 1;
  while ($r = $stm->fetch(PDO::FETCH_ASSOC)) {
    // use a different color for odd rows
    $odd = fmod(++$ln, 2);

    echo '<tr class="sf_admin_row_' . $odd . '">';

    <?php
    // Produce static code for showing fields according static parameters

    $fieldsToShow = FieldsToShow::getFieldsToShowInCallReport($generateForAdmin);

    // the order of fields, is the same showing order
    foreach($fieldsToShow as $fieldToShow) {
        switch($fieldToShow) {
            case FieldsToShow::ORGANIZATION_ID:
            ?>

            $organizationId = null;
            if (VariableFrame::$groupOn == 0) {
              $organizationId = $r['ar_organization_unit_id'];
            } else if (VariableFrame::$groupOn == 2) {
              $organizationId = $r['billable_ar_organization_unit_id'];
            } else if (VariableFrame::$groupOn == 1 || VariableFrame::$groupOn == 3) {
              $organizationId = OrganizationUnitInfo::getLastId($r['cached_parent_id_hierarchy']);
            }

              <?php if ($generateForAdmin) { ?>

                echo '<td>';
                // DEV-NOTE: UTF-8 and specialchars are already applied because in this case an HTML link is generated
                echo $info->getFullNameAtDate($organizationId, fromMySQLTimestampToUnixTimestamp($r['calldate']), true, false, $rootFilterOrganizationId, false, true);
                echo '</td>';

              <?php } else { ?>

                echo '<td>';
                echo htmlspecialchars(
                       $info->getFullNameAtDate($organizationId, fromMySQLTimestampToUnixTimestamp($r['calldate']), false, false, $rootFilterOrganizationId, true, true)
                , ENT_QUOTES
                , 'UTF-8');
                echo '</td>';
              <?php } ?>
            <?php
            break;

            case FieldsToShow::CALL_DIRECTION:
            ?>
            if (VariableFrame::$showCallDetails) {
              echo '<td>' . DestinationType::getSymbol($r['destination_type'], $r['is_redirect']) . '</td>';
            } else {
              echo '<td></td>';
            }
            <?php

            break;

            case FieldsToShow::EXTERNAL_TELEPHONE_NUMBER:
            ?>
            if (VariableFrame::$groupOn == 0) {
              if (VariableFrame::$showMaskedTelephoneNumbers) {
                $n = $r['cached_masked_external_telephone_number'];
              } else {
                $n = $r['cached_external_telephone_number'];
              }
              echo '<td>' . htmlspecialchars($n, ENT_QUOTES, 'UTF-8') . '</td>';
            } else {
              echo '<td></td>';
            }
            <?php
            break;

            case FieldsToShow::GEOGRAPHIC_LOCATION:
            ?>
            if (VariableFrame::$showCallDetails) {
              echo '<td>' . htmlspecialchars($r['geographic_location'], ENT_QUOTES, 'UTF-8') . '</td>';
            } else {
               echo '<td></td>';
            }

            <?php
            break;

            case FieldsToShow::OPERATOR_TYPE:
            ?>
            if (VariableFrame::$showCallDetails) {
              echo '<td>' . htmlspecialchars($r['operator_type'], ENT_QUOTES, 'UTF-8') . '</td>';
            } else {
               echo '<td></td>';
            }

            <?php
            break;

            case FieldsToShow::CALL_DATE:
            ?>
            if (VariableFrame::$groupOn == 0) {
              echo '<td>' . htmlspecialchars(format_date_according_config($r['calldate']), ENT_QUOTES, 'UTF-8') . '</td>';
            } else {
              echo '<td>' . $r['count_of_calls'] . '</td>';
            }
            <?php
            break;

            case FieldsToShow::BILL_SEC:
            ?>
            echo '<td>' . htmlspecialchars(format_minute($r['billsec']), ENT_QUOTES, 'UTF-8') . '</td>';
            <?php
            break;

            case FieldsToShow::COST:
            ?>
                <?php if ($generateForAdmin) { ?>
                 if ($useLinkToCDR) {
                    echo '<td>' . link_to(format_from_db_decimal_to_call_report_currency($r['cost']), get_debug_cdr_link($r)) . '</td>';
                  } else {
                    echo '<td>' . format_from_db_decimal_to_call_report_currency($r['cost']) . '</td>';
                  }
                <?php } else { ?>
                  echo '<td>' . format_from_db_decimal_to_call_report_currency($r['cost']) . '</td>';
                <?php } ?>
            <?php
            break;

            case FieldsToShow::INCOME:
            ?>
                <?php if ($generateForAdmin) { ?>
                 if ($useLinkToCDR) {
                    echo '<td>' . link_to(format_from_db_decimal_to_call_report_currency($r['income']), get_debug_cdr_link($r)) . '</td>';
                  } else {
                    echo '<td>' . format_from_db_decimal_to_call_report_currency($r['income']) . '</td>';
                  }
                <?php } else { ?>
                  echo '<td>' . format_from_db_decimal_to_call_report_currency($r['income']) . '</td>';
                <?php } ?>
            <?php
            break;

            case FieldsToShow::EARN:
            ?>
            $earn = $r['income'] - $r['cost'];
            echo '<td>' . format_from_db_decimal_to_call_report_currency($earn) . '</td>';
            <?php
            break;

            case FieldsToShow::COST_SAVINGS:
            ?>
            echo '<td>' . format_from_db_decimal_to_call_report_currency($r['cost_saving']) . '</td>';
            <?php
            break;

            case FieldsToShow::VENDOR:
            ?>
            if (VariableFrame::$showCallDetails) {
              $vendor = ArVendorPeer::retrieveByPK($r['ar_vendor_id']);

              if (!is_null($vendor)) {
                echo '<td>' . htmlspecialchars($vendor->getName(), ENT_QUOTES, 'UTF-8') . '</td>';
              } else {
                echo '<td></td>';
              }
            } else {
               echo '<td></td>';
            }

            <?php
            break;

            case FieldsToShow::COMMUNICATION_CHANNEL:
            ?>
            if (VariableFrame::$showCallDetails) {
              echo '<td>' . htmlspecialchars(ArCommunicationChannelTypePeer::getName($r['ar_communication_channel_type_id']), ENT_QUOTES, 'UTF-8') . '</td>';
            } else {
              echo '<td></td>';
            }

            <?php
            break;

            case FieldsToShow::COUNT_OF_CALLS:
            ?>
            echo '<td>' . $r['count_of_calls'] . '</td>';
            <?php
            break;

        }
    }
  ?>

    echo '</tr>';
  }
  $stm->closeCursor();
}
