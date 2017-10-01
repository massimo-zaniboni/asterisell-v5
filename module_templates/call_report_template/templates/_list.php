<?php

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

//////////////////
// Table header //
//////////////////

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
//
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

$c = clone VariableFrame::$filterConditionWithOrder;
$c->addJoin(ArCdrPeer::AR_TELEPHONE_PREFIX_ID, ArTelephonePrefixPeer::ID);

// Values to retrieve
//
$i = 0;
$c->clearSelectColumns();

$organizationUnitIdIndex = $i++;
if (VariableFrame::$groupOn == 0) {
    // group on calls
    $c->addSelectColumn(ArCdrPeer::AR_ORGANIZATION_UNIT_ID);
} else if (VariableFrame::$groupOn == 1) {
    // group on extensions
    $c->addSelectColumn(ArCdrPeer::AR_ORGANIZATION_UNIT_ID);
} else if (VariableFrame::$groupOn == 2) {
    // group on billable organization
    $c->addSelectColumn(ArCdrPeer::BILLABLE_AR_ORGANIZATION_UNIT_ID);
}

if (VariableFrame::$groupOn == 0) {
    // group on calls
    $externalNumberIndex = $i++;
    if (VariableFrame::$showMaskedTelephoneNumbers) {
      $c->addSelectColumn(ArCdrPeer::CACHED_MASKED_EXTERNAL_TELEPHONE_NUMBER);
    } else {
     $c->addSelectColumn(ArCdrPeer::CACHED_EXTERNAL_TELEPHONE_NUMBER);
    }
} else {
    $externalNumberIndex = -1;
}

$calldateIndex = $i++;
if (VariableFrame::$groupOn == 0) {
    // group on calls
    $c->addSelectColumn(ArCdrPeer::CALLDATE);
} else {
    $c->addSelectColumn('MIN(' . ArCdrPeer::CALLDATE . ')');
}

if (VariableFrame::$groupOn == 0) {
    // group on calls
    $typeIndex = $i++;
    $c->addSelectColumn(ArCdrPeer::DESTINATION_TYPE);
} else {
    $typeIndex = -1;
}

$billsecIndex = $i++;
if (VariableFrame::$groupOn == 0) {
    // group on calls
    $c->addSelectColumn(ArCdrPeer::BILLSEC);
} else if (VariableFrame::$groupOn == 1) {
    // group on extensions
    $c->addSelectColumn('SUM(' . ArCdrPeer::BILLSEC . ')');
} else if (VariableFrame::$groupOn == 2) {
    // group on billable organization
    $c->addSelectColumn('SUM(' . ArCdrPeer::BILLSEC . ')');
}

$incomeIndex = $i++;
if (VariableFrame::$groupOn == 0) {
    // group on calls
    $c->addSelectColumn(ArCdrPeer::INCOME);
} else if (VariableFrame::$groupOn == 1) {
    // group on extensions
    $c->addSelectColumn('SUM(' . ArCdrPeer::INCOME . ')');
} else if (VariableFrame::$groupOn == 2) {
    // group on billable organization
    $c->addSelectColumn('SUM(' . ArCdrPeer::INCOME . ')');
}

$costIndex = $i++;
if (VariableFrame::$groupOn == 0) {
    // group on calls
    $c->addSelectColumn(ArCdrPeer::COST);
} else if (VariableFrame::$groupOn == 1) {
    // group on extensions
    $c->addSelectColumn('SUM(' . ArCdrPeer::COST . ')');
} else if (VariableFrame::$groupOn == 2) {
    // group on billable organization
    $c->addSelectColumn('SUM(' . ArCdrPeer::COST . ')');
}

if (VariableFrame::$groupOn == 0) {
    // group on calls
    $vendorIdIndex = $i++;
    $c->addSelectColumn(ArCdrPeer::AR_VENDOR_ID);
} else {
    $vendorIdIndex = -1;
}

if (VariableFrame::$groupOn == 0) {
    // group on calls
    $geographicLocationIndex = $i++;
    $c->addSelectColumn(ArTelephonePrefixPeer::GEOGRAPHIC_LOCATION);
} else {
    $geographicLocationIndex = -1;
}

if (VariableFrame::$groupOn == 0) {
    // group on calls
    $operatorTypeIndex = $i++;
    $c->addSelectColumn(ArTelephonePrefixPeer::OPERATOR_TYPE);
} else {
    $operatorTypeIndex = -1;
}

$countOfCallsIndex = $i++;
if (VariableFrame::$groupOn == 0) {
    // group on calls
    $c->addSelectColumn(ArCdrPeer::COUNT_OF_CALLS);
} else if (VariableFrame::$groupOn == 1) {
    // group on extensions
    $c->addSelectColumn('SUM(' . ArCdrPeer::COUNT_OF_CALLS . ')');
} else if (VariableFrame::$groupOn == 2) {
    // group on billable organizations
    $c->addSelectColumn('SUM(' . ArCdrPeer::COUNT_OF_CALLS . ')');
}

if (VariableFrame::$groupOn == 0) {
    // group on calls
    $communicationChannelIndex = $i++;
    $c->addSelectColumn(ArCdrPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID);
} else {
    $communicationChannelIndex = -1;
}

$costSavingIndex = $i++;
if (VariableFrame::$groupOn == 0) {
    // group on calls
    $c->addSelectColumn(ArCdrPeer::COST_SAVING);
} else if (VariableFrame::$groupOn == 1) {
    // group on extensions
    $c->addSelectColumn('SUM(' . ArCdrPeer::COST_SAVING . ')');
} else if (VariableFrame::$groupOn == 2) {
    // group on billable organization
    $c->addSelectColumn('SUM(' . ArCdrPeer::COST_SAVING . ')');
}

if (VariableFrame::$groupOn == 0) {
    // group on calls
    $isRedirectIndex = $i++;
    $c->addSelectColumn(ArCdrPeer::IS_REDIRECT);
} else {
   $isRedirectIndex = -1;
}

if (VariableFrame::$groupOn == 0) {
    // group on calls
    $cdrIdIndex = $i++;
    $c->addSelectColumn(ArCdrPeer::ID);
} else {
    $cdrIdIndex = -1;
}

$c->setOffset(($currPage - 1) * $recordsPerPage);
$c->setLimit($recordsPerPage);

$currency = sfConfig::get('app_currency');

$rs = BasePeer::doSelect($c);

//
// All the results will be read and put in an array: they are not many values
// due to pagination.
//

// Process every record
//
$ln = 1;
foreach($rs as $r) {

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

            <?php if ($generateForAdmin) { ?>

                echo '<td>';
                // DEV-NOTE: UTF-8 and specialchars are already applied because in this case an HTML link is generated
                echo OrganizationUnitInfo::getInstance()->getFullNameAtDate($r[$organizationUnitIdIndex], fromMySQLTimestampToUnixTimestamp($r[$calldateIndex]), true, false, $rootFilterOrganizationId, false, true);
                echo '</td>';

            <?php } else { ?>

                echo '<td>';
                echo htmlspecialchars(
                    OrganizationUnitInfo::getInstance()->getFullNameAtDate($r[$organizationUnitIdIndex], fromMySQLTimestampToUnixTimestamp($r[$calldateIndex]), false, false, $rootFilterOrganizationId, true, true)
                , ENT_QUOTES
                , 'UTF-8');
                echo '</td>';
            <?php } ?>

            <?php
            break;

            case FieldsToShow::CALL_DIRECTION:
            ?>
            if ($isRedirectIndex != -1) {
                echo '<td>' . DestinationType::getSymbol($r[$typeIndex], $r[$isRedirectIndex]) . '</td>';
            } else {
                echo '<td></td>';
            }
            <?php

            break;

            case FieldsToShow::EXTERNAL_TELEPHONE_NUMBER:
            ?>
            if ($externalNumberIndex != -1) {
              echo '<td>' . htmlspecialchars($r[$externalNumberIndex], ENT_QUOTES, 'UTF-8') . '</td>';
             } else {
                    echo '<td></td>';
            }
            <?php
            break;

            case FieldsToShow::GEOGRAPHIC_LOCATION:
            ?>
            if ($geographicLocationIndex != -1) {
             echo '<td>' . htmlspecialchars($r[$geographicLocationIndex], ENT_QUOTES, 'UTF-8') . '</td>';
            } else {
                    echo '<td></td>';

            }
            <?php
            break;

            case FieldsToShow::OPERATOR_TYPE:
            ?>
            if ($operatorTypeIndex != -1) {
              echo '<td>' . htmlspecialchars($r[$operatorTypeIndex], ENT_QUOTES, 'UTF-8') . '</td>';
            } else {
              echo '<td></td>';
            }
            <?php
            break;

            case FieldsToShow::CALL_DATE:
            ?>
            if (VariableFrame::$groupOn == 0) {
                echo '<td>' . htmlspecialchars(format_date_according_config($r[$calldateIndex]), ENT_QUOTES, 'UTF-8') . '</td>';
            } else if ($countOfCallsIndex != -1) {
                echo '<td>' . $r[$countOfCallsIndex] . '</td>';
            } else {
                echo '<td></td>';
            }
            <?php
            break;

            case FieldsToShow::BILL_SEC:
            ?>
            echo '<td>' . htmlspecialchars(format_minute($r[$billsecIndex]), ENT_QUOTES, 'UTF-8') . '</td>';
            <?php
            break;

            case FieldsToShow::COST:
            ?>
                <?php if ($generateForAdmin) { ?>
                  if ($cdrIdIndex != -1) {
                    $t = $r[$cdrIdIndex];
                  } else {
                    $t = -1;
                  }
                  if (is_null($t) || $t == -1 || strlen(trim($t)) == 0) {
                    echo '<td>' . format_from_db_decimal_to_call_report_currency($r[$costIndex]) . '</td>';
                  } else {
                    echo '<td>' . link_to(format_from_db_decimal_to_call_report_currency($r[$costIndex]), 'debug_cdr/edit?id=' . $t) . '</td>';
                  }
                <?php } else { ?>
                  echo '<td>' . format_from_db_decimal_to_call_report_currency($r[$costIndex]) . '</td>';
                <?php } ?>
            <?php
            break;

            case FieldsToShow::INCOME:
            ?>
                <?php if ($generateForAdmin) { ?>
                 if ($cdrIdIndex != -1) {
                   $t = $r[$cdrIdIndex];
                 } else {
                   $t = -1;
                 }
                 if (is_null($t) || $t == -1 || strlen(trim($t)) == 0) {
                  echo '<td>' . format_from_db_decimal_to_call_report_currency($r[$incomeIndex]) . '</td>';
                 } else {
                   echo '<td>' . link_to(format_from_db_decimal_to_call_report_currency($r[$incomeIndex]), 'debug_cdr/edit?id=' . $t) . '</td>';
                 }
                <?php } else { ?>
                  echo '<td>' . format_from_db_decimal_to_call_report_currency($r[$incomeIndex]) . '</td>';
                <?php } ?>
            <?php
            break;

            case FieldsToShow::EARN:
            ?>
            $earn = $r[$incomeIndex] - $r[$costIndex];
            echo '<td>' . format_from_db_decimal_to_call_report_currency($earn) . '</td>';
            <?php
            break;

            case FieldsToShow::COST_SAVINGS:
            ?>
            echo '<td>' . format_from_db_decimal_to_call_report_currency($r[$costSavingIndex]) . '</td>';
            <?php
            break;

            case FieldsToShow::VENDOR:
            ?>
            if ($vendorIdIndex != -1) {
                $vendor = ArVendorPeer::retrieveByPK($r[$vendorIdIndex]);

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
            if ($communicationChannelIndex != -1) {
                echo '<td>' . htmlspecialchars(VariableFrame::getCommunicationChannelName($r[$communicationChannelIndex]), ENT_QUOTES, 'UTF-8') . '</td>';
            } else {
                echo '<td></td>';
            }
            <?php
            break;

            case FieldsToShow::COUNT_OF_CALLS:
            ?>
            echo '<td>' . $r[$countOfCallsIndex] . '</td>';
            <?php
            break;

            case FieldsToShow::DEBUG_COST_RATE:
                ?>
                echo '<td>' . htmlspecialchars($r[$debugCostRateIndex], ENT_QUOTES, 'UTF-8') . '</td>';
                <?php
                break;

            case FieldsToShow::DEBUG_INCOME_RATE:
                ?>
                echo '<td>' . htmlspecialchars($r[$debugIncomeRateIndex], ENT_QUOTES, 'UTF-8') . '</td>';
                <?php
                break;

            case FieldsToShow::DEBUG_RESIDUAL_INCOME_RATE:
                ?>
                echo '<td>' . htmlspecialchars($r[$debugResidualIncomeRateIndex], ENT_QUOTES, 'UTF-8') . '</td>';
                <?php
                break;

            case FieldsToShow::DEBUG_RESIDUAL_CALL_DURATION:
                ?>
                echo '<td>' . $r[$debugResidualCallDurationIndex] . '</td>';
                <?php
                break;

            case FieldsToShow::DEBUG_BUNDLE_ORGANIZATION_ID:
                ?>
                echo '<td>' . $r[$debugBundleOrganizationIdIndex] . '</td>';
                <?php
                break;

            case FieldsToShow::DEBUG_BUNDLE_LEFT_CALLS:
                ?>
                echo '<td>' . $r[$debugBundleLeftCallsIndex] . '</td>';
                <?php
                break;

            case FieldsToShow::DEBUG_BUNDLE_LEFT_DURATION:
                ?>
                echo '<td>' . $r[$debugBundleLeftDurationIndex] . '</td>';
                <?php
                break;

            case FieldsToShow::DEBUG_BUNDLE_LEFT_COST:
                ?>
                echo '<td>' . format_from_db_decimal_to_call_report_currency($r[$debugBundleLeftCostIndex]) . '</td>';
                <?php
                break;

            case FieldsToShow::ORGANIZATION_LEVEL:
            break;

            case FieldsToShow::CURRENCY:
            break;

            default:
            $fileName = basename($_SERVER['PHP_SELF']);
            die('Not supported FieldShow case in file ' . $fileName);
            break;
        }
    }
  ?>

  echo '</tr>';
}
}
