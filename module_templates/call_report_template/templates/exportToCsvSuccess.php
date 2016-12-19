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
            ?>
        echo csv_field(__('Account'), false);
        <?php
            break;

        case FieldsToShow::CALL_DIRECTION:
            ?>
        echo csv_field(__('Direction'), false);

        <?php

            break;

        case FieldsToShow::EXTERNAL_TELEPHONE_NUMBER:
            ?>
        echo csv_field(__('Telephone Number'), false);
        <?php
            break;

        case FieldsToShow::GEOGRAPHIC_LOCATION:
            ?>
        echo csv_field(__('Location'), false);
        <?php
            break;

        case FieldsToShow::OPERATOR_TYPE:
            ?>
        echo csv_field(__('Connection type'), false);
        <?php
            break;

        case FieldsToShow::CALL_DATE:
            ?>
        echo csv_field(__('Date'), false);
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
        echo csv_field(__('Vendor'), false);
        <?php
            break;

        case FieldsToShow::COMMUNICATION_CHANNEL:
            ?>
        echo csv_field(__('Communication Channel'), false);
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
            echo csv_field(__('Applied Cost Rate'));
            <?php
            break;

        case FieldsToShow::DEBUG_INCOME_RATE:
            ?>
            echo csv_field(__('Applied Income Rate'));
            <?php
            break;

        case FieldsToShow::DEBUG_RESIDUAL_INCOME_RATE:
            ?>
            echo csv_field(__('Applied Residual Income Rate'));
            <?php
            break;

        case FieldsToShow::DEBUG_RESIDUAL_CALL_DURATION:
            ?>
            echo csv_field(__('Residual Call Duration'));
            <?php
            break;

        case FieldsToShow::DEBUG_BUNDLE_ORGANIZATION_ID:
            ?>
            echo csv_field(__('Bundle Organization Id'));
            <?php
            break;

        case FieldsToShow::DEBUG_BUNDLE_LEFT_CALLS:
            ?>
            echo csv_field(__('Left Calls in Bundle'));
            <?php
            break;

        case FieldsToShow::DEBUG_BUNDLE_LEFT_DURATION:
            ?>
            echo csv_field(__('Left Duration in Bundle'));
            <?php
            break;

        case FieldsToShow::DEBUG_BUNDLE_LEFT_COST:
            ?>
            echo csv_field(__('Left Cost in Bundle'));
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

    $organizationUnitIdIndex = $i++;
    $c->addSelectColumn(ArCdrPeer::AR_ORGANIZATION_UNIT_ID);

    $externalNumberIndex = $i++;
    if (VariableFrame::$showMaskedTelephoneNumbers) {
      $c->addSelectColumn(ArCdrPeer::CACHED_MASKED_EXTERNAL_TELEPHONE_NUMBER);
    } else {
      $c->addSelectColumn(ArCdrPeer::CACHED_EXTERNAL_TELEPHONE_NUMBER);
    }

    $calldateIndex = $i++;
    $c->addSelectColumn(ArCdrPeer::CALLDATE);

    $typeIndex = $i++;
    $c->addSelectColumn(ArCdrPeer::DESTINATION_TYPE);

    $billsecIndex = $i++;
    $c->addSelectColumn(ArCdrPeer::BILLSEC);

    $incomeIndex = $i++;
    $c->addSelectColumn(ArCdrPeer::INCOME);

    $costIndex = $i++;
    $c->addSelectColumn(ArCdrPeer::COST);

    $vendorIdIndex = $i++;
    $c->addSelectColumn(ArCdrPeer::AR_VENDOR_ID);

    $geographicLocationIndex = $i++;
    $c->addSelectColumn(ArTelephonePrefixPeer::GEOGRAPHIC_LOCATION);

    $operatorTypeIndex = $i++;
    $c->addSelectColumn(ArTelephonePrefixPeer::OPERATOR_TYPE);

    $countOfCallsIndex = $i++;
    $c->addSelectColumn(ArCdrPeer::COUNT_OF_CALLS);

    $communicationChannelIndex = $i++;
    $c->addSelectColumn(ArCdrPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID);

    $costSavingIndex = $i++;
    $c->addSelectColumn(ArCdrPeer::COST_SAVING);

    $isRedirectIndex = $i++;
    $c->addSelectColumn(ArCdrPeer::IS_REDIRECT);

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
            ?>
        echo csv_field(OrganizationUnitInfo::getInstance()->getFullNameAtDate($unitId, $cdrDate, false, false, null, false), false);
        <?php
            break;

        case FieldsToShow::CALL_DIRECTION:
            ?>
        echo csv_field(DestinationType::getName($r[$typeIndex], $r[$isRedirectIndex]), false);
        <?php

            break;

        case FieldsToShow::EXTERNAL_TELEPHONE_NUMBER:
            ?>
        echo csv_field($r[$externalNumberIndex], false);
        <?php
            break;

        case FieldsToShow::GEOGRAPHIC_LOCATION:
            ?>
        echo csv_field($r[$geographicLocationIndex], false);
        <?php
            break;

        case FieldsToShow::OPERATOR_TYPE:
            ?>
        echo csv_field($r[$operatorTypeIndex], false);
        <?php
            break;

        case FieldsToShow::CALL_DATE:
            ?>
        echo csv_field($r[$calldateIndex], false);
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
        $vendor = ArVendorPeer::retrieveByPK($r[$vendorIdIndex]);
        $vendorName = $vendor->getName();
        echo csv_field($vendorName, false);
        <?php
            break;

        case FieldsToShow::COMMUNICATION_CHANNEL:
            ?>
        echo csv_field(VariableFrame::getCommunicationChannelName($r[$communicationChannelIndex]), false);
        <?php
            break;

        case FieldsToShow::COUNT_OF_CALLS:
            ?>
        echo csv_field($r[$countOfCallsIndex], false);
        <?php
            break;

        case FieldsToShow::DEBUG_COST_RATE:
            ?>
            echo csv_field($r[$debugCostRateIndex]);
            <?php
            break;

        case FieldsToShow::DEBUG_INCOME_RATE:
            ?>
            echo csv_field($r[$debugIncomeRateIndex]);
            <?php
            break;

        case FieldsToShow::DEBUG_RESIDUAL_INCOME_RATE:
            ?>
            echo csv_field($r[$debugResidualIncomeRateIndex]);
            <?php
            break;

        case FieldsToShow::DEBUG_RESIDUAL_CALL_DURATION:
            ?>
            echo csv_numeric_field($r[$debugResidualCallDurationIndex], false);
            <?php
            break;

        case FieldsToShow::DEBUG_BUNDLE_ORGANIZATION_ID:
            ?>
            echo csv_numeric_field($r[$debugBundleOrganizationUnitIdIndex], false);
            <?php
            break;

        case FieldsToShow::DEBUG_BUNDLE_LEFT_CALLS:
            ?>
            echo csv_numeric_field($r[$debugBundleLeftCallsIndex], false);
            <?php
            break;

        case FieldsToShow::DEBUG_BUNDLE_LEFT_DURATION:
            ?>
            echo csv_numeric_field($r[$debugBundleLeftDurationIndex], false);
            <?php
            break;

        case FieldsToShow::DEBUG_BUNDLE_LEFT_COST:
            ?>
            echo csv_numeric_field(from_db_decimal_to_php_decimal($r[$debugBundleLeftCostIndex]), false);
            <?php
            break;

        case FieldsToShow::ORGANIZATION_LEVEL:
            ?>
        // Put each part of the organization level on a separate column, in order to enable filtering and grouping
        // using Excel
        $parents = OrganizationUnitInfo::getParentIdsFromCachedParentIdHierarchy($r[$cachedParentIdHierarchyIndex]);
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
