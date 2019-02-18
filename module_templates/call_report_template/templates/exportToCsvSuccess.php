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

// ---------------------------------
// Headers

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
        if (VariableFrame::$showCallDetails) {
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
        if (VariableFrame::$showCallDetails) {
         echo csv_field(__('Location'), false);
        }
        <?php
            break;

        case FieldsToShow::OPERATOR_TYPE:
            ?>
        if (VariableFrame::$showCallDetails) {
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
         if (VariableFrame::$showCallDetails) {
           echo csv_field(__('Vendor'), false);
         }
        <?php
            break;

        case FieldsToShow::COMMUNICATION_CHANNEL:
            ?>
         if (VariableFrame::$showCallDetails) {
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

        case FieldsToShow::DEBUG_BUNDLE_ORGANIZATION_ID:
            ?>
        if (VariableFrame::$groupOn == 0) {
            echo csv_field(__('Bundle Organization Id'));
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

// ------------------------------
// Show data

  $currency = sfConfig::get('app_currency');
  $info = OrganizationUnitInfo::getInstance();
  $stm = Propel::getConnection()->prepare(getQueryFromParts(
                          VariableFrame::$listFrom,
                          VariableFrame::$listCondition,
                          VariableFrame::$listGroupBy,
                          VariableFrame::$listSort,
                          VariableFrame::$exportToCSVSelect));

  $stm->execute(VariableFrame::$listParams);
  while ($r = $stm->fetch(PDO::FETCH_ASSOC)) {

        $cdrDate = fromMySQLTimestampToUnixTimestamp($r['calldate']);

        $billableUnitId = null;
        $unitId = null;
        $ids = null;
        if (VariableFrame::$groupOn == 0) {
          $unitId = $r['ar_organization_unit_id'];
          $billableUnitId = $r['billable_ar_organization_unit_id'];
        } else if (VariableFrame::$groupOn == 1 || VariableFrame::$groupOn == 3) {
          $ids = $r['cached_parent_id_hierarchy'];
          $unitId = OrganizationUnitInfo::getLastId($ids);
          $billableUnitId = $info->getBillableArOrganizationId($ids, $cdrDate);
        } else if (VariableFrame::$groupOn == 2) {
          $unitId = $r['billable_ar_organization_unit_id'];
          $billableUnitId = $unitId;
        }

<?php

// Produce static code for showing fields according static parameters
// the order of fields, is the same showing order
foreach($fieldsToShow as $fieldToShow) {
    switch($fieldToShow) {
        case FieldsToShow::ORGANIZATION_ID:
        if ($generateForAdmin) {
            ?>
           $crm = OrganizationUnitInfo::getInstance()->getPartyCRM($billableUnitId, $cdrDate);
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
            if (VariableFrame::$showCallDetails) {
              echo csv_field(DestinationType::getName($r['destination_type'], $r['is_redirect']), false);
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
              echo csv_field($n, false);
            }
        <?php
            break;

        case FieldsToShow::GEOGRAPHIC_LOCATION:
            ?>
        if (VariableFrame::$showCallDetails) {
          echo csv_field($r['geographic_location'], false);
        }
        <?php
            break;

        case FieldsToShow::OPERATOR_TYPE:
            ?>
        if (VariableFrame::$showCallDetails) {
          echo csv_field($r['operator_type'], false);
        }
        <?php
            break;

        case FieldsToShow::CALL_DATE:
            ?>
        if (VariableFrame::$groupOn == 0) {
          echo csv_field($r['calldate'], false);
        } else {
          echo csv_field($r['count_of_calls'], false);
        }
        <?php
            break;

        case FieldsToShow::BILL_SEC:
            ?>
        echo csv_field($r['billsec'], false);
        <?php
            break;

        case FieldsToShow::COST:
            ?>
        echo csv_numeric_field(from_db_decimal_to_php_decimal($r['cost']), false);
        <?php
            break;

        case FieldsToShow::INCOME:
            ?>
        echo csv_numeric_field(from_db_decimal_to_php_decimal($r['income']), false);
        <?php
            break;

        case FieldsToShow::EARN:
            ?>
        $earn = $r['income'] - $r['cost'];
        echo csv_numeric_field(from_db_decimal_to_php_decimal($earn), false);
        <?php
            break;

        case FieldsToShow::COST_SAVINGS:
            ?>
        echo csv_numeric_field(from_db_decimal_to_php_decimal($r['cost_saving']), false);
        <?php
            break;

        case FieldsToShow::VENDOR:
            ?>
        if (VariableFrame::$showCallDetails) {
            $vendor = ArVendorPeer::retrieveByPK($r['ar_vendor_id']);
            $vendorName = $vendor->getName();
            echo csv_field($vendorName, false);
        }
        <?php
            break;

        case FieldsToShow::COMMUNICATION_CHANNEL:
            ?>
        if (VariableFrame::$showCallDetails) {
            echo csv_field(ArCommunicationChannelTypePeer::getName($r['ar_communication_channel_type_id']), false);
        }
        <?php
            break;

        case FieldsToShow::COUNT_OF_CALLS:
            ?>
        echo csv_field($r['count_of_calls'], false);
        <?php
            break;

        case FieldsToShow::DEBUG_COST_RATE:
            ?>
        if (VariableFrame::$groupOn == 0) {
             echo csv_field($r['debug_cost_rate']);
        }

            <?php
            break;

        case FieldsToShow::DEBUG_INCOME_RATE:
            ?>
        if (VariableFrame::$groupOn == 0) {
             echo csv_field($r['debug_income_rate']);
        }
            <?php
            break;

        case FieldsToShow::DEBUG_BUNDLE_ORGANIZATION_ID:
            ?>
        if (VariableFrame::$groupOn == 0) {
             $id = $r['bundle_ar_organization_unit_id'];
             if (is_null($id)) {
               $n = '';
             } else {
               $n = OrganizationUnitInfo::getInstance()->getFullNameAtDate($id, $cdrDate, false, false, null, false);
             }
             echo csv_field($n, false);
        }
            <?php
            break;

        case FieldsToShow::ORGANIZATION_LEVEL:
            ?>
        // Put each part of the organization level on a separate column, in order to enable filtering and grouping
        // using Excel
        $cachedIDS = $ids;
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
