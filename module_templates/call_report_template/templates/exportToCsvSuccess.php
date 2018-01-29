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
use_helper('I18N', 'Debug', 'Date', 'Asterisell');

/**
 * @var AsterisellUser $sf_user
 */
$const_maxNestedAccount = 15;

// Set UTF-8 encoding
echo "\xEF\xBB\xBF";


// Header
//

<?php
// Produce static code for showing fields according static parameters

$fieldsToShow = FieldsToShow::getFieldsToShowInCSVReport($generateForAdmin);

// the order of fields, is the same showing order
foreach($fieldsToShow as $fieldToShow) {
    switch($fieldToShow) {
        case FieldsToShow::ORGANIZATION_ID:
        if ($generateForAdmin) {
            ?>
          echo csv_field(__('Billable CRM'), true);
          echo csv_field(__('Account'), false);
        <?php
        } else {
        ?>
          echo csv_field(__('Account'), true);
        <?php
        }
        ?>

        <?php
            break;

        case FieldsToShow::CALL_DIRECTION:
            ?>
        if (VariableFrame::$groupOn == 0) {
          echo csv_field(__('Direction'), false);
        }
        <?php

            break;

        case FieldsToShow::EXTERNAL_TELEPHONE_NUMBER:
            ?>
        if (VariableFrame::$groupOn == 0) {
         echo csv_field(__('Telephone Number'), false);
        }
        <?php
            break;

        case FieldsToShow::GEOGRAPHIC_LOCATION:
            ?>
        if (VariableFrame::$groupOn == 0) {
         echo csv_field(__('Location'), false);
        }
        <?php
            break;

        case FieldsToShow::OPERATOR_TYPE:
            ?>
        if (VariableFrame::$groupOn == 0) {
          echo csv_field(__('Connection type'), false);
        }
        <?php
            break;

        case FieldsToShow::CALL_DATE:
            ?>
        if (VariableFrame::$groupOn == 0) {
         echo csv_field(__('Date'), false);
        } else {
         echo csv_field(__('Calls'), false);
        }
        <?php
            break;

        case FieldsToShow::BILL_SEC:
            ?>
        echo csv_field(__('Duration in seconds'), false);
        <?php
            break;

        case FieldsToShow::COST:
            ?>
        echo csv_field(__('Cost (VAT excluded)'), false);
        <?php
            break;

        case FieldsToShow::INCOME:
            if ($generateForAdmin) {
                ?> echo csv_field(__('Income (VAT excluded)'), false); <?php
            } else {
                ?> echo csv_field(__('Cost (VAT excluded)'), false); <?php
            }
            break;

        case FieldsToShow::EARN:
            ?>
        echo csv_field(__('Earn (VAT excluded)'), false);
        <?php
            break;

        case FieldsToShow::COST_SAVINGS:
            ?>
        echo csv_field(__('Cost Savings (VAT excluded)'), false);
        <?php
            break;

        case FieldsToShow::VENDOR:
            ?>
         if (VariableFrame::$groupOn == 0) {
           echo csv_field(__('Vendor'), false);
         }
        <?php
            break;

        case FieldsToShow::COMMUNICATION_CHANNEL:
            ?>
         if (VariableFrame::$groupOn == 0) {
           echo csv_field(__('Communication Channel'), false);
         }
        <?php
            break;

        case FieldsToShow::COUNT_OF_CALLS:
            ?>
        echo csv_field(__('Nr.'), false);

        <?php
            break;

        case FieldsToShow::ORGANIZATION_LEVEL:
            ?>
            for($i = 1; $i <= $const_maxNestedAccount; $i++) {
                echo csv_field(__("Organization Level") . " $i", false);
            }
            <?php
            break;

        case FieldsToShow::CURRENCY:
            ?>
            echo csv_field(__('Currency'), false);
            <?php
            break;

        case FieldsToShow::DEBUG_COST_RATE:
            ?>
        if (VariableFrame::$groupOn == 0) {
             echo csv_field(__('Applied Cost Rate'));
        }
            <?php
            break;

        case FieldsToShow::DEBUG_INCOME_RATE:
            ?>
        if (VariableFrame::$groupOn == 0) {
             echo csv_field(__('Applied Income Rate'));
        }
            <?php
            break;

        case FieldsToShow::DEBUG_RESIDUAL_INCOME_RATE:
            ?>
        if (VariableFrame::$groupOn == 0) {
            echo csv_field(__('Applied Residual Income Rate'));
        }
            <?php
            break;

        case FieldsToShow::DEBUG_RESIDUAL_CALL_DURATION:
            ?>
        if (VariableFrame::$groupOn == 0) {
            echo csv_field(__('Residual Call Duration'));
        }
            <?php
            break;

        case FieldsToShow::DEBUG_BUNDLE_ORGANIZATION_ID:
            ?>
        if (VariableFrame::$groupOn == 0) {
            echo csv_field(__('Bundle Organization Id'));
        }
            <?php
            break;

        case FieldsToShow::DEBUG_BUNDLE_LEFT_CALLS:
            ?>
        if (VariableFrame::$groupOn == 0) {
             echo csv_field(__('Left Calls in Bundle'));
        }
            <?php
            break;

        case FieldsToShow::DEBUG_BUNDLE_LEFT_DURATION:
            ?>
        if (VariableFrame::$groupOn == 0) {
             echo csv_field(__('Left Duration in Bundle'));
        }
            <?php
            break;

        case FieldsToShow::DEBUG_BUNDLE_LEFT_COST:
            ?>
        if (VariableFrame::$groupOn == 0) {
             echo csv_field(__('Left Cost in Bundle'));
        }
            <?php
            break;

        default:
            $fileName = basename($_SERVER['PHP_SELF']);
            die('Not supported FieldShow case in file ' . $fileName);
            break;
    }
}
?>

echo "\n";

//
// Show Fields
//


    $c = clone VariableFrame::$filterConditionWithOrder;
    $c->addJoin(ArCdrPeer::AR_TELEPHONE_PREFIX_ID, ArTelephonePrefixPeer::ID);

    // Values to retrieve
    //
    $i = 0;
    $c->clearSelectColumns();

    $billableOrganitazionUnitIdIndex = $i++;
    $c->addSelectColumn(ArCdrPeer::BILLABLE_AR_ORGANIZATION_UNIT_ID);

$organizationUnitIdIndex = 0;
if (VariableFrame::$groupOn == 0) {
    // group on calls
    $organizationUnitIdIndex = $i++;
    $c->addSelectColumn(ArCdrPeer::AR_ORGANIZATION_UNIT_ID);
} else if (VariableFrame::$groupOn == 1) {
    // group on extensions
    $organizationUnitIdIndex = $i++;
    $c->addSelectColumn(ArCdrPeer::AR_ORGANIZATION_UNIT_ID);
} else if (VariableFrame::$groupOn == 2) {
    // group on billable organization
    $organizationUnitIdIndex = $billableOrganitazionUnitIdIndex;
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

if (VariableFrame::$groupOn == 0) {
  $cachedParentIdHierarchyIndex = $i++;
  $c->addSelectColumn(ArCdrPeer::CACHED_PARENT_ID_HIERARCHY);

  $debugCostRateIndex = $i++;
  $c->addSelectColumn(ArCdrPeer::DEBUG_COST_RATE);

  $debugIncomeRateIndex = $i++;
  $c->addSelectColumn(ArCdrPeer::DEBUG_INCOME_RATE);

  $debugResidualIncomeRateIndex = $i++;
  $c->addSelectColumn(ArCdrPeer::DEBUG_RESIDUAL_INCOME_RATE);

  $debugResidualCallDurationIndex = $i++;
  $c->addSelectColumn(ArCdrPeer::DEBUG_RESIDUAL_CALL_DURATION);

  $debugBundleLeftCallsIndex = $i++;
  $c->addSelectColumn(ArCdrPeer::DEBUG_BUNDLE_LEFT_CALLS);

  $debugBundleLeftDurationIndex = $i++;
  $c->addSelectColumn(ArCdrPeer::DEBUG_BUNDLE_LEFT_DURATION);

  $debugBundleLeftCostIndex = $i++;
  $c->addSelectColumn(ArCdrPeer::DEBUG_BUNDLE_LEFT_COST);

  $debugBundleOrganizationUnitIdIndex = $i++;
  $c->addSelectColumn(ArCdrPeer::BUNDLE_AR_ORGANIZATION_UNIT_ID);
}
    $currency = sfConfig::get('app_currency');

    // Process every $cdr using doSelectRS that fetch only one object at once from DB
    // TODO force usage of specific index on calldate...
    $rs = ArCdrPeer::doSelectStmt($c);

    // Process every record
    //
    while ($r = $rs->fetch(PDO::FETCH_NUM)) {

        $cdrDate = fromMySQLTimestampToUnixTimestamp($r[$calldateIndex]);
        $unitId = $r[$organizationUnitIdIndex];

<?php

// Produce static code for showing fields according static parameters
// the order of fields, is the same showing order
foreach($fieldsToShow as $fieldToShow) {
    switch($fieldToShow) {
        case FieldsToShow::ORGANIZATION_ID:
        if ($generateForAdmin) {
            ?>
           $crm = OrganizationUnitInfo::getInstance()->getPartyCRM($r[$billableOrganitazionUnitIdIndex], $cdrDate);
           if (is_null($crm)) {
              $crm = '';
           }
           echo csv_field($crm, true);
           echo csv_field(OrganizationUnitInfo::getInstance()->getFullNameAtDate($unitId, $cdrDate, false, false, null, false), false);
        <?php
        } else {
        ?>
            echo csv_field(OrganizationUnitInfo::getInstance()->getFullNameAtDate($unitId, $cdrDate, false, false, null, false), true);
        <?php
        }
        ?>

        <?php
            break;

        case FieldsToShow::CALL_DIRECTION:
            ?>
         if (VariableFrame::$groupOn == 0) {
            echo csv_field(DestinationType::getName($r[$typeIndex], $r[$isRedirectIndex]), false);
         }
        <?php

            break;

        case FieldsToShow::EXTERNAL_TELEPHONE_NUMBER:
            ?>
        if (VariableFrame::$groupOn == 0) {
          echo csv_field($r[$externalNumberIndex], false);
        }
        <?php
            break;

        case FieldsToShow::GEOGRAPHIC_LOCATION:
            ?>
        if (VariableFrame::$groupOn == 0) {
          echo csv_field($r[$geographicLocationIndex], false);
        }
        <?php
            break;

        case FieldsToShow::OPERATOR_TYPE:
            ?>
        if (VariableFrame::$groupOn == 0) {
          echo csv_field($r[$operatorTypeIndex], false);
        }
        <?php
            break;

        case FieldsToShow::CALL_DATE:
            ?>
        if (VariableFrame::$groupOn == 0) {
          echo csv_field($r[$calldateIndex], false);
        } else {
          echo csv_field($r[$countOfCallsIndex], false);
        }
        <?php
            break;

        case FieldsToShow::BILL_SEC:
            ?>
        echo csv_field($r[$billsecIndex], false);
        <?php
            break;

        case FieldsToShow::COST:
            ?>
        echo csv_numeric_field(from_db_decimal_to_php_decimal($r[$costIndex]), false);
        <?php
            break;

        case FieldsToShow::INCOME:
            ?>
        echo csv_numeric_field(from_db_decimal_to_php_decimal($r[$incomeIndex]), false);
        <?php
            break;

        case FieldsToShow::EARN:
            ?>
        $earn = $r[$incomeIndex] - $r[$costIndex];
        echo csv_numeric_field(from_db_decimal_to_php_decimal($earn), false);
        <?php
            break;

        case FieldsToShow::COST_SAVINGS:
            ?>
        echo csv_numeric_field(from_db_decimal_to_php_decimal($r[$costSavingIndex]), false);
        <?php
            break;

        case FieldsToShow::VENDOR:
            ?>
        if (VariableFrame::$groupOn == 0) {
            $vendor = ArVendorPeer::retrieveByPK($r[$vendorIdIndex]);
            $vendorName = $vendor->getName();
            echo csv_field($vendorName, false);
        }
        <?php
            break;

        case FieldsToShow::COMMUNICATION_CHANNEL:
            ?>
        if (VariableFrame::$groupOn == 0) {
            echo csv_field(VariableFrame::getCommunicationChannelName($r[$communicationChannelIndex]), false);
        }
        <?php
            break;

        case FieldsToShow::COUNT_OF_CALLS:
            ?>
        echo csv_field($r[$countOfCallsIndex], false);
        <?php
            break;

        case FieldsToShow::DEBUG_COST_RATE:
            ?>
        if (VariableFrame::$groupOn == 0) {
             echo csv_field($r[$debugCostRateIndex]);
        }

            <?php
            break;

        case FieldsToShow::DEBUG_INCOME_RATE:
            ?>
        if (VariableFrame::$groupOn == 0) {
             echo csv_field($r[$debugIncomeRateIndex]);
        }
            <?php
            break;

        case FieldsToShow::DEBUG_RESIDUAL_INCOME_RATE:
            ?>
        if (VariableFrame::$groupOn == 0) {
             echo csv_field($r[$debugResidualIncomeRateIndex]);
        }
            <?php
            break;

        case FieldsToShow::DEBUG_RESIDUAL_CALL_DURATION:
            ?>
        if (VariableFrame::$groupOn == 0) {
             echo csv_numeric_field($r[$debugResidualCallDurationIndex], false);
        }
            <?php
            break;

        case FieldsToShow::DEBUG_BUNDLE_ORGANIZATION_ID:
            ?>
        if (VariableFrame::$groupOn == 0) {
             echo csv_numeric_field($r[$debugBundleOrganizationUnitIdIndex], false);
        }
            <?php
            break;

        case FieldsToShow::DEBUG_BUNDLE_LEFT_CALLS:
            ?>
        if (VariableFrame::$groupOn == 0) {
             echo csv_numeric_field($r[$debugBundleLeftCallsIndex], false);
        }
            <?php
            break;

        case FieldsToShow::DEBUG_BUNDLE_LEFT_DURATION:
            ?>
        if (VariableFrame::$groupOn == 0) {
             echo csv_numeric_field($r[$debugBundleLeftDurationIndex], false);
        }
            <?php
            break;

        case FieldsToShow::DEBUG_BUNDLE_LEFT_COST:
            ?>
        if (VariableFrame::$groupOn == 0) {
             echo csv_numeric_field(from_db_decimal_to_php_decimal($r[$debugBundleLeftCostIndex]), false);
        }
            <?php
            break;

        case FieldsToShow::ORGANIZATION_LEVEL:
            ?>
        // Put each part of the organization level on a separate column, in order to enable filtering and grouping
        // using Excel
        if (VariableFrame::$groupOn == 0) {
            $cachedIDS =  $r[$cachedParentIdHierarchyIndex];
        } else {
            $info = OrganizationUnitInfo::getInstance();
            $cachedIDS = $info->getFullIds($r[$organizationUnitIdIndex], $cdrDate);
        }
        $parents = OrganizationUnitInfo::getParentIdsFromCachedParentIdHierarchy($cachedIDS);

        $countParents = 0;
        foreach($parents as $parentId) {
        /**
        * @var ArOrganizationUnit $parent
        */
        $countParents++;
        echo csv_field(OrganizationUnitInfo::getInstance()->getHumanReadableName($parentId), false);
        }

        for(; $countParents < $const_maxNestedAccount; $countParents++) {
        echo csv_field('', false);
        }
        <?php
            break;

        case FieldsToShow::CURRENCY:
            ?>
        echo csv_field($currency, false);
        <?php
            break;

        default:
            $fileName = basename($_SERVER['PHP_SELF']);
            die('Not supported FieldShow case in file ' . $fileName);
            break;
    }
}
?>

        echo "\n";
}
