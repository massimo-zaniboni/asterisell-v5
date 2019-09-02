<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

// see notes in the generator for more info...
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
!!!    php asterisell.php activate                         !!!
!!!                                                        !!!
**************************************************************/

sfLoader::loadHelpers(array('I18N', 'Debug', 'Asterisell', 'CustomLocaleConversions'));

/**
 * Actions and controls for the customer/admin call report.
 * It contains a header/dashboard with summary of calls,
 * and a detailed list of calls.
 * The list of calls can contain single calls, or calls grouped organization by organization.
 *
 * General design guidelines
 * =========================
 *
 * A custom version is generated for customer and admin, also according the static configurations of the application.
 *
 * Whenever possible ar_cached_grouped_cdr is used because it contains daily sum of values.
 *
 * Customer have no access to slow features requiring entire scanning of ar_cdr, except the case
 * of CSV download.
 *
 * The values that must be read from the templates code, are put in `VariableFrame` static fields.
 *
 */
class <?php echo FieldsToShow::getClassName($generateForAdmin); ?> extends <?php echo FieldsToShow::getParentClassName($generateForAdmin); ?> {

// ----------------------------------------
// Internal functions

/**
 * Init info needed by any view/method, but that is not heavy to calculate.
 * Put results on these global variables:
 * - VariableFrame::$srcCanBeGrouped
 * - VariableFrame::$fromDate
 * - VariableFrame::$toDate
 * - VariableFrame::$isWholeDay
 * - VariableFrame::$filterDescription
 * - VariableFrame::$groupOn
 * - VariableFrame::$filterOnOrganization
 * - VariableFrame::$showCallDetails
 * - VariableFrame::$startOrganizationId
 * - VariableFrame::$showMaskedTelephoneNumbers
 * - VariableFrame::$showMoreDirections
 * - VariableFrame::$showCommunicationChannel
 */
protected function initInfo() {

  <?php if ($generateForAdmin) { ?>
      $isAdmin = true;
      VariableFrame::$showMoreDirections = true;
  <?php } else { ?>
      $isAdmin = false;
      VariableFrame::$showMoreDirections = (count(DestinationType::getAllowedDestinations(false)) > 1);
  <?php } ?>

  // Load data from controllers (required from the Symphony framework)
  $this->processFilters();
  $this->processSort();
  $this->filters = $this->getUser()->getAttributeHolder()->getAll('sf_admin/ar_cdr/filters');

  // Add custom info, processing the data loaded from controllers
  VariableFrame::$srcCanBeGrouped = $this->canUseCachedGroupedCDRSForTotals();

  $c = array();
  $params = array();
  list($descr1, $descr3) = $this->addAllFiltersAndGroupsButNoTimeFrame(true, $c, $params, true);

  $descr2 = $this->initTimeFrameFilters();

  VariableFrame::$filterDescription = $descr1 . ' ' . $descr2 . ' ' . $descr3;

  VariableFrame::$showCommunicationChannel = false;
  <?php
  $fieldsToShow = FieldsToShow::getFieldsToShowInCallReport($generateForAdmin);

  foreach ($fieldsToShow as $fieldToShow) {
      switch ($fieldToShow) {
        case FieldsToShow::COMMUNICATION_CHANNEL:
          ?>
          VariableFrame::$showCommunicationChannel = true;
          <?php
          break;
      }
  }
  ?>
}

/**
 * Compute (potentially heavy) info for the dashboard, and the list of calls,
 * and put on `VariableFrame` vars, so they can be accessed by template methods.
 * Variables set:
 * - all the vars of `initInfo`
 * - VariableFrame::$listViewName
 * - VariableFrame::$listCondition
 * - VariableFrame::$listParams
 * - VariableFrame::$listSort
 * - VariableFrame::$listUseGroupdCdrs
 * - VariableFrame::$listSelect
 * - VariableFrame::$listHeaderCondition
 * - VariableFrame::$listHeaderParams
 * - VariableFrame::$exportToCSVSelect
 * - VariableFrame::$countOfRecords
 * - VariableFrame::$filterOnOperatorType
 * - VariableFrame::$filterOnVendor
 * - VarbialeFrame::$filterOnGeographicLocation
 */
protected function initListInfo() {

   $conn = Propel::getConnection();

   <?php if ($generateForAdmin) { ?>
     $isAdmin = true;
  <?php } else { ?>
     $isAdmin = false;
  <?php }
     $fieldsToShow = FieldsToShow::getFieldsToShowInCallReport($generateForAdmin);
   ?>

  $this->initInfo();

  // Add in the filter only the values presents in current time-frame,
  // in order to reduce the selections.
  VariableFrame::$filterOnOperatorType = array("" => "");
  VariableFrame::$filterOnCommunicationChannel = array("" => "");
  VariableFrame::$filterOnVendor = array("" => "");
  VariableFrame::$countOfRecords = 0;

  // Init the header formats

  VariableFrame::$headerRowOrder = array();
  VariableFrame::$headerColOrder = array('calls', 'seconds');

  VariableFrame::$headerColNames = array();
  VariableFrame::$headerColFormat = array();
  VariableFrame::$headerRowNames = array();

  VariableFrame::$headerColNames['calls'] = __('Calls');
  VariableFrame::$headerColFormat['calls'] = 0;

  <?php
      foreach ($fieldsToShow as $fieldToShow) {
        switch ($fieldToShow) {
          case FieldsToShow::BILL_SEC:
            ?>
            VariableFrame::$headerColNames['seconds'] = __('Duration');
            VariableFrame::$headerColFormat['seconds'] = 2;
            <?php
            break;

          case FieldsToShow::INCOME:
            ?>
            VariableFrame::$headerColOrder[] = 'income';
            if ($isAdmin) {
              VariableFrame::$headerColNames['income'] = __('Income');
            } else {
              VariableFrame::$headerColNames['income'] = __('Cost');
            }
            VariableFrame::$headerColFormat['income'] = 1;
            <?php
            break;

          case FieldsToShow::COST:
            ?>
            VariableFrame::$headerColOrder[] = 'cost';
            VariableFrame::$headerColNames['cost'] = __('Cost');
            VariableFrame::$headerColFormat['cost'] = 1;
            <?php
            break;

          case FieldsToShow::EARN:
            ?>
            VariableFrame::$headerColOrder[] = 'earn';
            VariableFrame::$headerColNames['earn'] = __('Earn');
            VariableFrame::$headerColFormat['earn'] = 1;
            <?php
            break;

          case FieldsToShow::COST_SAVINGS:
            ?>
            VariableFrame::$headerColOrder[] = 'cost_savings';
            VariableFrame::$headerColNames['cost_savings'] = __('Cost savings');
            VariableFrame::$headerColFormat['cost_savings'] = 1;
            <?php
            break;

          case FieldsToShow::CALL_DIRECTION:
            ?>
            VariableFrame::$headerRowOrder[] = 'direction';
             <?php
            break;

          case FieldsToShow::OPERATOR_TYPE:
            ?>
            VariableFrame::$headerRowOrder[] = 'operator_type';
             <?php
            break;
          case FieldsToShow::COMMUNICATION_CHANNEL:
            ?>
            VariableFrame::$headerRowOrder[] = 'channel';
             <?php
            break;

          case FieldsToShow::VENDOR:
            ?>
            VariableFrame::$headerRowOrder[] = 'vendor';
            <?php
            break;
          }
      }
  ?>
  VariableFrame::$headerRowNames['channel'] = __('Communication channel');
  VariableFrame::$headerRowNames['vendor'] = __('Vendor');
  VariableFrame::$headerRowNames['operator_type'] = __('Operator type');
  VariableFrame::$headerRowNames['direction'] = __('Call direction');

  // Put here all aggregate values
  $table = array();
  $totals = array();
  VariableFrame::$filterOnGeographicLocation = array("" => "");

  $startConditions = array();
  $startParams = array();
  $this->addAllFiltersAndGroupsButNoTimeFrame(true, $startConditions, $startParams, VariableFrame::$srcCanBeGrouped);

  // Aggregate all possible different intervals of daily aggregate data,
  // for obtaining a unique view.
  // $interval :
  // - 0 the entire non whole interval on ar_cdr,
  // - 1 the starting non-whole part on ar_cdr,
  // - 2 the entire whole part on ar_cached_grouped_cdr,
  // - 3 the ending non whole-part on ar_cdr
  for ($interval = 0; $interval < 4; $interval++) {
    $isWholeDay = VariableFrame::$isWholeDay;

    if ($isWholeDay && VariableFrame::$srcCanBeGrouped && $interval !== 2) {
      continue;
      // NOTE: skip because in case of whole intervals, suffice to process only the whole interval
    }

    if (   ($interval == 0 && !VariableFrame::$srcCanBeGrouped)
        || ($interval !== 0 && VariableFrame::$srcCanBeGrouped)) {

      $canGroupCDRS = VariableFrame::$srcCanBeGrouped && ($interval == 2);

      $select = array();
      $groupByPart = array();
      $c = $startConditions;
      $params = $startParams;
      $fromPart = getCdrListView($isAdmin, 100, VariableFrame::$filterOnOrganization, $canGroupCDRS, $c, $groupByPart, $select);
      $isIntervalGenerated = addTimeFrameFilters($c, $params, VariableFrame::$fromDate, VariableFrame::$toDate, $interval);

      if (!$isIntervalGenerated) {
        continue;
        // NOTE: skip because there is no data to process in this interval
      }

      $stm = $conn->prepare(getQueryFromParts($fromPart, $c, $groupByPart, null, $select));
      $stm->execute($params);
      while ($rec = $stm->fetch(PDO::FETCH_ASSOC)) {
        VariableFrame::$countOfRecords += $rec['count_of_records'];

        $values = array();
        $values['calls'] = $rec['count_of_calls'];

        // add only columns that can be displayed
        // NOTE: this code is generated at "compile-time" so the run-time code
        // contains only direct instructions.
        <?php
        foreach ($fieldsToShow as $fieldToShow) {
          switch ($fieldToShow) {
            case FieldsToShow::BILL_SEC:
              ?>
              $values['seconds'] = $rec['billsec'];
              <?php
              break;

            case FieldsToShow::INCOME:
              ?>
              $values['income'] = $rec['income'];
              <?php
              break;

            case FieldsToShow::COST:
              ?>
              $values['cost'] = $rec['cost'];
              <?php
              break;

            case FieldsToShow::EARN:
              ?>
              $values['earn'] = $rec['income'] - $rec['cost'];
              <?php
              break;

            case FieldsToShow::COST_SAVINGS:
              ?>
              $values['cost savings'] = $rec['cost_saving'];
              <?php
              break;
            }
        }
        ?>

        addValuesToStatsArray($totals, $values);

        // Add only rows that can be displayed
        <?php
         foreach ($fieldsToShow as $fieldToShow) {
          switch ($fieldToShow) {
           case FieldsToShow::COMMUNICATION_CHANNEL:
              ?>
              addToTableArray($table, 'channel', ArCommunicationChannelTypePeer::getName($rec['ar_communication_channel_type_id']), $values);
              <?php
              break;
            case FieldsToShow::VENDOR:
              ?>
              addToTableArray($table, 'vendor', ArVendorPeer::getName($rec['ar_vendor_id']), $values);
              <?php
              break;
            case FieldsToShow::OPERATOR_TYPE:
              ?>
              addToTableArray($table, 'operator_type', $rec['operator_type'], $values);
              <?php
              break;
            }
         }
        ?>

        // NOTE: in case of customers it is applied automatically a filter excluding error calls,
        // and other not allowed directions, so only proper values are sent.
        addToTableArray($table, 'direction', DestinationType::getName($rec['destination_type']), $values);

        VariableFrame::$filterOnCommunicationChannel[$rec['ar_communication_channel_type_id']] = ArCommunicationChannelTypePeer::getName($rec['ar_communication_channel_type_id']);
        VariableFrame::$filterOnOperatorType[$rec['operator_type']] = $rec['operator_type'];
        VariableFrame::$filterOnVendor[$rec['ar_vendor_id']] = ArVendorPeer::getName($rec['ar_vendor_id']);
        VariableFrame::$filterOnGeographicLocation[$rec['geographic_location']] = $rec['geographic_location'];
      }
      $stm->closeCursor();
    }
  }

  VariableFrame::$headerTotals = $totals;
  VariableFrame::$headerTable = $table;

  asort(VariableFrame::$filterOnCommunicationChannel);
  asort(VariableFrame::$filterOnOperatorType);
  asort(VariableFrame::$filterOnVendor);
  asort(VariableFrame::$filterOnGeographicLocation);

  // ----------------------------------------------------
  // Prepare query for viewing the list/details of calls
  //
  // The query is not too much heavy because:
  // - in case of grouped ar_cached_grouped_cdr few records must be retrieved
  // - in case of ar_cdr few calls are displayed because there is a LIMIT condition on the view
  // - no sums must be done, because it is already pre-computed
  // - only in case of strange filters (on telephone number and so on) SUMS must be done, but the user is warned about this

  if (VariableFrame::$groupOn == 0) {
    $interval = 0;
  } else {
    $interval = 2;
  }

  VariableFrame::$listCondition = array();
  VariableFrame::$listGroupBy = array();
  VariableFrame::$listSort = array();
  VariableFrame::$listParams = array();
  VariableFrame::$listSelect = array();
  VariableFrame::$listHeaderCondition = array();
  VariableFrame::$listHeaderParams = array();

  VariableFrame::$listFrom = getCdrListView($isAdmin, VariableFrame::$groupOn, VariableFrame::$filterOnOrganization,
                                            VariableFrame::$srcCanBeGrouped,
                                            VariableFrame::$listCondition,
                                            VariableFrame::$listGroupBy,
                                            VariableFrame::$listSelect);


  if (VariableFrame::$groupOn == 0) {
    VariableFrame::$exportToCSVSelect = array();
    $ignore1 = array();
    $ignore2 = array();
    getCdrListView($isAdmin, 4, VariableFrame::$filterOnOrganization, VariableFrame::$srcCanBeGrouped,
                   $ignore1, $ignore2, VariableFrame::$exportToCSVSelect);
  } else {
    VariableFrame::$exportToCSVSelect = VariableFrame::$listSelect;
  }

  $this->addAllFiltersAndGroupsButNoTimeFrame(
    false,
    VariableFrame::$listCondition,
    VariableFrame::$listParams,
    VariableFrame::$srcCanBeGrouped);

  $this->addAllFiltersAndGroupsButNoTimeFrame(
    true,
    VariableFrame::$listHeaderCondition,
    VariableFrame::$listHeaderParams,
    VariableFrame::$srcCanBeGrouped);

  addTimeFrameFilters(
    VariableFrame::$listCondition,
    VariableFrame::$listParams,
    VariableFrame::$fromDate,
    VariableFrame::$toDate,
    $interval);

  addTimeFrameFilters(
    VariableFrame::$listHeaderCondition,
    VariableFrame::$listHeaderParams,
    VariableFrame::$fromDate,
    VariableFrame::$toDate,
    $interval);

  $this->addCustomSortCriteria(VariableFrame::$listSort, VariableFrame::$groupOn);
}

/**
 * @return bool true if ar_cached_grouped_cdr can be used.
 * The result is cached, and the next call is faster.
 */
protected function canUseCachedGroupedCDRSForTotals() {
  if (! is_null(filterValue($this->filters, 'is_redirect'))) {
    return FALSE;
  }

  if (! is_null(filterValue($this->filters, 'filter_on_external_telephone_number'))) {
    return FALSE;
  }

  $vendorCostF = filterValue($this->filters, 'filter_on_vendor_cost');
  if (!is_null($vendorCostF) && $vendorCostF === DestinationType::VENDOR_COST_DIFFERENT_FROM_EXPECTED) {
    return FALSE;
  }

  return TRUE;
}

/**
 * Add all filters criteria, excluded the criteria based on time-frame.
 *
 * Store some data (indipendent from the params) on VariableFrame variables:
 * - `VariableFrame::$groupOn`
 * - `VariableFrame::$startOrganizationId`
 * - `VariableFrame::$filterOnOrganization`
 * - `VariableFrame::$showMaskedTelephoneNumbers`
 *
 * PRECONDITION: called after `initInfo()` because it uses many contexn values inside VariableFrame
 * POSTCONDITION: filters are checked for sanity/security constraints according
 * the privileges of the logged user.
 *
 * NOTE: the enabled/disabled filters must the same configured in
 * generator.yml, filters section.
 *
 * @param bool $isHeader true for generating filters for the header, false for the list of calls
 * @param array $c SQL conditions
 * @param array $params SQL params
 * @param bool $canUseGroupedCDRS true if criteria are compatible with ar_cached_grouped_cdr
 * @return array list(string, string) a human readable description of the applied filter, without the part on the timeframes
 * that must be inserted between the two returned strings.
 */
protected function addAllFiltersAndGroupsButNoTimeFrame($isHeader, & $c, & $params, $canUseGroupedCDRS)
{

  // ----------------------------------------------------------
  // Select the correct view,
  // and doing so implicitely select the correct grouping policy.

  $filterDescr_clauses = array();
  $filterOnDestinationTypeApplied = false;
  $filterOnRedirectedCalls = null;
  $destinationType = null;

  if (isset($this->filters['grouping'])) {
    VariableFrame::$groupOn = intval($this->filters['grouping']);
  } else {
    VariableFrame::$groupOn = 0;
  }

  $groupOn = VariableFrame::$groupOn;
  VariableFrame::$showCallDetails = (VariableFrame::$groupOn == 0 || VariableFrame::$groupOn == 3);
  VariableFrame::$filterOnOrganization = false;

  <?php if ($generateForAdmin) { ?>
    $isAdmin = true;
  <?php } else { ?>
    $isAdmin = false;
  <?php } ?>

  // -----------------------------
  // Add normal filters

  <?php
  // Produce static code for showing fields according static parameters

  $fieldsToShow = FieldsToShow::getFieldsToShowInCallReport($generateForAdmin);

  // the order of fields, is the same showing order
  foreach ($fieldsToShow as $fieldToShow) {
    switch ($fieldToShow) {
        case FieldsToShow::ORGANIZATION_ID:
            ?>

            if (isset($this->filters['filter_on_voip_account_short_code']) && ! isEmptyOrNull($this->filters['filter_on_voip_account_short_code'])) {
              $organizationId = $this->filters['filter_on_voip_account_short_code'];
            } else if (isset($this->filters['filter_on_voip_account']) && ! isEmptyOrNull($this->filters['filter_on_voip_account'])) {
              $organizationId = $this->filters['filter_on_voip_account'];
            } else if (isset($this->filters['filter_on_proper_organization']) && ! isEmptyOrNull($this->filters['filter_on_proper_organization'])) {
              $organizationId = $this->filters['filter_on_proper_organization'];
            } else {
              $organizationId = null;
            }

            <?php if ($generateForAdmin) { ?>
              if (is_null($organizationId)) {
                // try to apply an implicit filter, in case there is only one organization
                $organizationId = OrganizationUnitInfo::getInstance()->getUniqueRootOrganizationIdIfExists();
              }

              if (!is_null($organizationId)) {
                VariableFrame::$filterOnOrganization = true;
              }
        <?php } else { ?>
              // normal customer

              VariableFrame::$filterOnOrganization = true;

              if (! is_null($organizationId)) {
                if (! OrganizationUnitInfo::getInstance()->canViewCallsOfOrganization($this->getContext()->getUser()->getOrganizationId(), $organizationId)) {
                  $organizationId = null;
                }
              }

              if (is_null($organizationId)) {
                $organizationId = $this->getContext()->getUser()->getOrganizationId();
              }
        <?php } ?>

            // A user with permission on 10, can view all the calls having 10 on the hierarchy path.
            VariableFrame::$startOrganizationId = $organizationId;
            if (! is_null($organizationId)) {
              $cond = '%' . OrganizationUnitInfo::getSingleIds($organizationId) . '%';
              $c[] = "cached_parent_id_hierarchy LIKE ?";
              $params[] = $cond;
            }

            <?php
            break;

        case FieldsToShow::CALL_DIRECTION:
            ?>
            if (isset($this->filters['is_redirect']) && $this->filters['is_redirect'] !== '')
            {
              $c[] = "is_redirect = ?";
              $params[] = $this->filters['is_redirect'];
              if ($this->filters['is_redirect']) {
                $filterOnRedirectedCalls = true;
              } else {
                $filterOnRedirectedCalls = false;
              }

            } else {
              $filterOnRedirectedCalls = null;
            }

            $destinationType = filterValue($this->filters, 'filter_on_destination_type');
            $filterOnDestinationTypeApplied = false;
            if (!is_null($destinationType)) {
              $c[] = "destination_type = ?";
              $params[] = $destinationType;
              $filterOnDestinationTypeApplied = true;
            }
            <?php
            break;

        case FieldsToShow::EXTERNAL_TELEPHONE_NUMBER:
            ?>
          if ($isAdmin) {
            // NOTE: expensive filter to calculate
            $loc = filterValue($this->filters, 'filter_on_external_telephone_number');
            if (! isEmptyOrNull($loc)) {
              $loc = trim($loc);
              if (substr($loc, -1, 1) == '*') {
                $loc = substr($loc, 0, -1);
              }
              if ($loc !== FALSE && !isEmptyOrNull($loc)) {
                $c[] = "cached_masked_external_telephone_number LIKE ?";
                $params[] = $loc .'%';
                $filterDescr_clauses[] = __('external telephone number starting with ') . '"' . __($loc) . '"';
              }
            }
          }
            <?php
            break;

        case FieldsToShow::GEOGRAPHIC_LOCATION:
            ?>
            $loc = filterValue($this->filters, 'filter_on_dst_geographic_location');
            if (!is_null($loc)) {
              $c[] = "geographic_location = ?";
              $params[] = $loc;
              $filterDescr_clauses[] = __('telephone number location ') . __($loc);
            }

            <?php
            break;

        case FieldsToShow::OPERATOR_TYPE:
            ?>
            // Filter on type of destination number according prefix table
            //
            $loc = filterValue($this->filters, 'filter_on_dst_operator_type');
            if (!is_null($loc)) {
              $c[] = "operator_type = ?";
              $params[] = $loc;
              $filterDescr_clauses[] = __('connection type ') . __($loc);
            }

            <?php
            break;

        case FieldsToShow::CALL_DATE:
            ?>
            <?php
            break;

        case FieldsToShow::BILL_SEC:
            ?>
            <?php
            break;

        case FieldsToShow::COST:
            ?>
            <?php
            break;

        case FieldsToShow::INCOME:
            ?>
            <?php
            break;

        case FieldsToShow::EARN:
            ?>
            <?php
            break;

        case FieldsToShow::COST_SAVINGS:
            ?>
            <?php
            break;

        case FieldsToShow::VENDOR:
            ?>
            $id = filterValue($this->filters, 'filter_on_vendor');
            if (!is_null($id)) {
              $c[] = "ar_vendor_id = ?";
              $params[] = $id;
              $filterDescr_clauses[] = __('vendor ') . ArVendorPeer::getName($id);
            }

            $vendorCostF = filterValue($this->filters, 'filter_on_vendor_cost');
            if ($vendorCostF === DestinationType::VENDOR_COST_DIFFERENT_FROM_EXPECTED) {
              $c[] = "expected_cost <> cost";
              $filterDescr_clauses[] = __('unexpected vendor cost');
             }
            <?php
            break;

        case FieldsToShow::COMMUNICATION_CHANNEL:
            ?>
            $id = filterValue($this->filters, 'filter_on_communication_channel');
            if (!is_null($id)) {
              $c[] = "ar_communication_channel_type_id = ?";
              $params[] = $id;
              $filterDescr_clauses[] = __('communication channel ') . ArCommunicationChannelTypePeer::getName($id);
            }

            <?php
            break;

        case FieldsToShow::COUNT_OF_CALLS:
            ?>
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

 <?php if ($generateForAdmin) { ?>
    // Admin can view all types of destination types except
    // unprocessed and ignored calls, that are displayed on
    // separate reports.
    //
    if (!$filterOnDestinationTypeApplied) {
      DestinationType::addFiltersAccordingConfiguration(true, $c, $params);
    }

<?php } else { ?>
    // Normal users do not see error/unprocessed/ignored calls
    //
    if (!$filterOnDestinationTypeApplied) {
      DestinationType::addFiltersAccordingConfiguration(false, $c, $params);
    }
<?php } ?>

  VariableFrame::$showMaskedTelephoneNumbers = true;
  if (ArViewAllUserPermissionsPeer::haveUserPermission($this->getUser()->getUserId(), ArPermission::CAN_VIEW_COMPLETE_TELEPHONE_NUMBERS)) {
    if (isset($this->filters['show_masked_telephone_numbers']) && $this->filters['show_masked_telephone_numbers'] == '1') {
      VariableFrame::$showMaskedTelephoneNumbers = false;
    }
  }

  // ---------------------------------------------------------
  // Create the human readable description of the filter

  $d1 = translateFilterOnCalls($filterOnRedirectedCalls, $destinationType);

<?php if ($generateForAdmin) { ?>

    if (! is_null(VariableFrame::$startOrganizationId)) {
      $d1 .= __('of ') . OrganizationUnitInfo::getInstance()->getFullNameAtDate(VariableFrame::$startOrganizationId, null, true, true) . ' ';
    }

<?php } else { ?>

    if (! is_null(VariableFrame::$startOrganizationId)) {
      $d1 .= htmlspecialchars(
          __('of ') . OrganizationUnitInfo::getInstance()->getFullNameAtDate(VariableFrame::$startOrganizationId, null, false, false) . ' '
        , ENT_QUOTES
        , 'UTF-8');
    }

<?php } ?>

    $d2 = '';
    if (count($filterDescr_clauses) > 0) {
      $d2 .= __(' with ') . implode(', and ', $filterDescr_clauses);
    }

    return array($d1, $d2);
}

/**
 * Read filter criterias, and complete:
 * - VarialeFrame::$fromDate
 * - VariableFrame::$toDate
 * - VariableFrame::$isWholeDay
 *
 * @require VariableFrame::$groupOn setted
 * @return string the human readable description of the filter
 */
protected function initTimeFrameFilters() {

  <?php if ($generateForAdmin) { ?>
    $isAdmin = true;
  <?php } else { ?>
    $isAdmin = false;
  <?php } ?>

  // ------------------------------
  // Read the params of the filter

  $fromDate = null;
  $toDate = null;

  $fromDate1 = filterValue($this->filters, 'filter_on_calldate_from');
  if (!is_null($fromDate1)) {
    $fromDate = fromSymfonyTimestampToUnixTimestamp($fromDate1);
    $this->filters['filter_on_calldate_from'] = $fromDate;
  }

  $toDate1 = filterValue($this->filters, 'filter_on_calldate_to');
  if (!is_null($toDate1)) {
    $toDate = fromSymfonyTimestampToUnixTimestamp($toDate1);
    $this->filters['filter_on_calldate_to'] = $toDate;
  }

  $frame = filterValue($this->filters, 'filter_on_timeframe');
  if (is_null($frame)) {
    $frame = 'recent';
  }

  // start from today, removing the time...
  $baseDate = time();

  switch ($frame) {
    case 'recent':
      $appParams = ArParamsPeer::getDefaultParams();
      if (is_null($appParams)) {
        $frame = '1';
      } else {
        $d1 = fromMySQLTimestampToUnixTimestamp($appParams->getOfficialCalldate());
        if (is_null($d1)) {
          $frame = '1';
          // NOTE: do not execute break, so calls '1'
          // MAYBE improve the design of this code
        } else {
          $fromDate = $d1;
          $toDate = null;
          break;
        }
      }
    case 'specific':
      if (is_null($fromDate)) {
        // set a default timeframe in order to avoid the processing
        // of all made calls.
        $frame = '1';
      } else {
        // already setup
        break;
      }
    case '1':
      $fromDate = startWith00Timestamp(strtotime("today"));
      $toDate = null;
      break;
    case '2':
      $fromDate = startWith00Timestamp(strtotime("-1 day", $baseDate));
      $toDate = null;
      break;
    case '3':
      $fromDate = startWith00Timestamp(strtotime("-6 days", $baseDate));
      $toDate = null;
      break;
    case '4':
      $fromDate = startWith00Timestamp(strtotime("-13 days", $baseDate));
      $toDate = null;
      break;
    case '5':
      $fromDate = startWith00Timestamp(strtotime("-29 days", $baseDate));
      $toDate = null;
      break;
    case '20':
      // this month
      //
      $now = time();
      $mm = date('m', $now);
      $yy = date('Y', $now);
      $dd = date('d', $now);
      $fromDate = strtotime("$yy-$mm-01");
      $toDate = strtotime("$yy-$mm-$dd");
      $toDate = strtotime("+1 day", $toDate);
      break;
  case '21':
      // last month
      //
      $now = time();
      $mm = date('m', $now);
      $yy = date('Y', $now);

      $mm--;
      if ($mm < 1) {
        $mm = 12;
        $yy--;
      }
      $fromDate = strtotime("$yy-$mm-01");
      $dd = date('t', $fromDate);
      $toDate = strtotime("$yy-$mm-$dd");
      $toDate = strtotime("+1 day", $toDate);
      break;
  case 'this-year':
      $now = time();
      $yy = date('Y', $now);

      $fromDate = strtotime("$yy-01-01");
      $toDate = null;
      break;

  case 'last-year':
      $now = time();
      $yy = date('Y', $now);
      $yyy = $yy - 1;

      $fromDate = strtotime("$yyy-01-01");
      $toDate = strtotime("$yy-01-01");
      break;

  default:
      trigger_error(__("timestamp filter not recognized") . ":" . $frame);
      $this->forward404();
  }

  if (VariableFrame::$groupOn != 0) {
    // NOTE: grouped calls are displayed using whole days time frames
    $fromDate = fromUnixTimestampToWholeDayStart($fromDate, true);
    $toDate = fromUnixTimestampToWholeDayEnd($toDate, true);
  }

  VariableFrame::$isWholeDay = true;
  if ($fromDate != fromUnixTimestampToWholeDayStart($fromDate, true)
      || (!is_null($toDate) && $toDate != fromUnixTimestampToWholeDayEnd($toDate, true))) {
    VariableFrame::$isWholeDay = false;
  }

  // ---------------------------------------------------------
  // Create the human readable description of the filter

<?php if ($generateForAdmin) { ?>
    $filterDescr = getUserReadableTimeFrame($fromDate, $toDate, false, false);
<?php } else { ?>
    $filterDescr = getUserReadableTimeFrame($fromDate, $toDate, false, true);
<?php } ?>

  VariableFrame::$fromDate = $fromDate;
  VariableFrame::$toDate = $toDate;

  return $filterDescr;
}

/**
 * @param array $c a list of SQL statements with SORT criteria
 * @param int $groupOn 0 for a list of single calls, another value for grouping the calls
 */
protected function addCustomSortCriteria(& $c, $groupOn)
{

  // MAYBE signal if some sort criteria is expensive or not,
  // and enable/disable them in an ad-hoc fashion

<?php if ($generateForAdmin) { ?>
    // Administrators can select expensive sort criteria
    if ($sort_column = $this->getUser()->getAttribute('sort', null, 'sf_admin/ar_cdr/sort')) {
      if (strtoupper($sort_column) == 'BILLSEC') {
        $sort_column = "billsec";
      } else if (strtoupper($sort_column) == 'INCOME') {
        $sort_column = "income";
      } else if (strtoupper($sort_column) == 'COST') {
        $sort_column = "cost";
      } else if (strtoupper($sort_column) == 'CALLDATE') {
        if ($groupOn == 0) {
          $sort_column = "calldate";
        } else {
          // NOTE: in case of grouped list view, the CALLDATE column contains instead the number of calls.
          $sort_column = "count_of_calls";
        }
      } else if (strtoupper($sort_column) == 'COST_SAVING') {
        $sort_column = "cost_saving";
      } else {
        // default sort-criteria
        if ($groupOn == 0) {
          $sort_column = "calldate";
        } else {
          $sort_column = "income";
        }
      }
    } else {
      // default sort-criteria
      if ($groupOn == 0) {
        $sort_column = "calldate";
      } else {
        $sort_column = "income";
      }
    }

    // Apply the filter
    if ($this->getUser()->getAttribute('type', null, 'sf_admin/ar_cdr/sort') == 'asc') {
      $ascDesc = 'ASC';
    } else {
      $ascDesc = 'DESC';
    }

<?php } else { ?>
    // FOR NORMAL USERS: ordering can be an expensive operation,
    // so it is disabled for normal users.
    // They can in case export calls to CSV, and order them.
    // So use the sort on CALLDATE that is efficient thanks to the filter, and SQL LIMIT behavior.
    $sort_column = "calldate";
    $ascDesc = 'DESC';
    // NOTE: force this, otherwise if the user change for other cols, it is not clear
    // and the UI is confusing

    // reset attributes on the Web UI
    $this->getUser()->setAttribute('type', null, 'sf_admin/ar_cdr/sort');
    $this->getUser()->setAttribute('sort', null, 'sf_admin/ar_cdr/sort');
<?php } ?>

   $c[] = $sort_column . ' ' . $ascDesc;

    // force a sort on ID for viewing all the calls in the LIMIT pagination,
    // without unintendeed skips.
    $c[] = "id ASC";
}

// -------------------------------------
// Overridden functions of the framework

/**
 * Override the framework method.
 * This is the entry point.
 */
public function executeList($request)
{
  $this->initListInfo();
}

public function executeCreate($request)
{
  return $this->manageSecurityError();
}

public function executeSave($request)
{
  return $this->manageSecurityError();
}

public function executeEdit($request)
{
  return $this->manageSecurityError();
}

public function executeDelete($request)
{
  return $this->manageSecurityError();
}

public function handleErrorEdit()
{
  return $this->manageSecurityError();
}

/**
* Do nothing
* @param $request
*/
public function executeDeleteSelected($request) {

}


/**
* Make explicitely nothing
*/
protected function saveArCdr($cdr)
{

}

/**
* Make explicitely nothing
*/
protected function deleteArCdr($cdr)
{

}

/**
* Make explicitely nothing
*/
protected function updateArCdrFromRequest()
{

}

protected function addSortCriteria($c)
{
    // do nothing because `addCustomSortCriteria` is explicitely called
}

// ----------------------------------------
// New custom actions

public function executeExportToCsv($request) {
    // execute list operation and then invoke templates/exportToCsvSuccess.php
    return $this->executeList($request);
}

public function executeExportToExcel($request) {
    // execute list operation and then invoke templates/exportToExcelSuccess.php
    return $this->executeList($request);
}

public function executeGetSvg($request) {
  // execute templates/getSvgSuccess.php
  //
  // NOTE: I'm using this method for retrieving files
  // in order to set the http header
  // "content-type" to "image/svg+xml" as required
  // from the browser for SVG files.
  //
  return sfView::SUCCESS;
}

public function executeUserActs($request) {
    $moduleName = <?php echo '"' . FieldsToShow::getModuleName($generateForAdmin) . '"'; ?> ;

    if ($request->isMethod(sfRequest::POST))
    {

<?php if ($generateForAdmin) { ?>

        if ($this->getRequestParameter('resetCallsCost')) {
          $this->initInfo();
          FixedJobProcessor::rerateCalls(VariableFrame::$fromDate);
          return $this->redirect("$moduleName/list");
        } else if ($this->getRequestParameter('resetRecentCallsCost')) {
          FixedJobProcessor::rerateCallsFromOfficialCalldate(false);
          return $this->redirect("$moduleName/list");
        }

<?php } ?>

        if ($this->getRequestParameter('exportToCsv')) {
          return $this->forward($moduleName, 'exportToCsv');
        } else if ($this->getRequestParameter('exportToExcel')) {
          return $this->forward($moduleName, 'exportToExcel');
        } else if ($this->getRequestParameter('exportExtensions')) {
          $this->initInfo();
          return $this->forward('get_type_of_customers', 'exportToCsv');
        }
    } // end sfRequest::POST
    return $this->forward($moduleName, 'list');
} // end function

protected function manageSecurityError() {
  $module = getSuggestedCallReportModule(sfContext::getInstance()->getUser());
  return $this->redirect($module);
}

}

<?php
echo '?>' . "\n";
?>
