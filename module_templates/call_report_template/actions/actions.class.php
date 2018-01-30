<?php

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

class <?php echo FieldsToShow::getClassName($generateForAdmin); ?> extends <?php echo FieldsToShow::getParentClassName($generateForAdmin); ?> {

protected $cachedStartDate = NULL;
protected $cachedEndDate = NULL;
protected $cachedFilterDescr = NULL;
protected $areDateCached = FALSE;

public function executeUserActs($request) {
$moduleName = <?php echo '"' . FieldsToShow::getModuleName($generateForAdmin) . '"'; ?> ;

if ($request->isMethod(sfRequest::POST))
{

<?php if ($generateForAdmin) { ?>

    if ($this->getRequestParameter('resetCallsCost')) {
      $this->initBeforeCalcCondition();
      list($fromDate, $toDate, $descr) = $this->getAndUpdateTimeFrame();
      FixedJobProcessor::rerateCalls($fromDate, $toDate);
      return $this->redirect("$moduleName/list");

    } else if ($this->getRequestParameter('resetRecentCallsCost')) {
      FixedJobProcessor::rerateCallsFromOfficialCalldate(false);
      return $this->redirect("$moduleName/list");

    } else if ($this->getRequestParameter('showChannelUsage')) {
      $this->setFlash('show_channel_usage', TRUE);
      return $this->forward($moduleName, 'list');
    } else if ($this->getRequestParameter('hideChannelUsage')) {
      $this->getUser()->setFlash('show_channel_usage', FALSE);
      return $this->forward($moduleName, 'list');
    }

<?php } ?>

if ($this->getRequestParameter('exportToCsv')) {
  return $this->forward($moduleName, 'exportToCsv');
} else if ($this->getRequestParameter('exportToExcel')) {
  return $this->forward($moduleName, 'exportToExcel');
} else if ($this->getRequestParameter('exportExtensions')) {
  $this->initBeforeCalcCondition();
  list($startDate, $endDate, $descr) = $this->getAndUpdateTimeFrame();
  VariableFrame::$startFilterDate = $startDate;
  VariableFrame::$endFilterDate = $endDate;
  return $this->forward('get_type_of_customers', 'exportToCsv');
}
} // end sfRequest::POST
return $this->forward($moduleName, 'list');
} // end function

public function executeExportToCsv($request) {
// execute list operation and then invoke templates/exportToCsvSuccess.php
//
return $this->executeList($request);
}

public function executeExportToExcel($request) {
// execute list operation and then invoke templates/exportToExcelSuccess.php
//
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

/**
* Call this function before call other functions on filters.
*/
protected function initBeforeCalcCondition() {
$this->processFilters();
$this->processSort();
$this->filters = $this->getUser()->getAttributeHolder()->getAll('sf_admin/ar_cdr/filters');
}

/**
* @pre call first self::initBeforeCalcCondition()
* @return a Condition
*/
protected function calcConditionWithoutJoins() {
  $fullCondition = new Criteria();
  $this->addFiltersCriteria($fullCondition);
  return $fullCondition;
}

/**
* @pre call first self::initBeforeCalcCondition()
*/
protected function addJoinsToCondition($c) {
  ArCdrPeer::addAllJoinsExceptVendorCondition($c);
}

  /**
  * @param Criteria $c
  */
  protected function addSortCriteria($c)
  {
    if ($sort_column = $this->getUser()->getAttribute('sort', null, 'sf_admin/ar_cdr/sort')) {
      if (VariableFrame::$groupOn != 0 && strtoupper($sort_column) == 'BILLSEC') {
        $sort_column = 'SUM(ar_cdr.billsec)';
      } else if (VariableFrame::$groupOn != 0 && strtoupper($sort_column) == 'INCOME') {
        $sort_column = 'SUM(ar_cdr.income)';
       } else if (VariableFrame::$groupOn != 0 && strtoupper($sort_column) == 'COST') {
        $sort_column = 'SUM(ar_cdr.cost)';
       } else if (VariableFrame::$groupOn != 0 && strtoupper($sort_column) == 'CALLDATE') {
        $sort_column = 'SUM(ar_cdr.count_of_calls)';
       } else if (VariableFrame::$groupOn != 0 && strtoupper($sort_column) == 'COST_SAVING') {
        $sort_column = 'SUM(ar_cdr.cost_saving)';
       } else {
        // camelize lower case to be able to compare with BasePeer::TYPE_PHPNAME translate field name
        $sort_column = ArCdrPeer::translateFieldName(sfInflector::camelize(strtolower($sort_column)), BasePeer::TYPE_PHPNAME, BasePeer::TYPE_COLNAME);
      }

      if ($this->getUser()->getAttribute('type', null, 'sf_admin/ar_cdr/sort') == 'asc') {
        $c->addAscendingOrderByColumn($sort_column);
      } else {
        $c->addDescendingOrderByColumn($sort_column);
      }
    }

    // force a sort on ID for viewing all the calls in the LIMIT pagination
    $c->addAscendingOrderByColumn(ArCdrPeer::ID);
  }

/**
* @pre call first self::initBeforeCalcCondition()
*/
protected function addOrder($c) {
  $this->addSortCriteria($c);
}

public function executeList($request) {
  $this->initBeforeCalcCondition();
  $filterWithJoins = $this->calcConditionWithoutJoins();
  $this->addJoinsToCondition($filterWithJoins);
  $this->updateVariableFrameWithHeaderInfo($filterWithJoins);
}

/**
* Put in VariableFrame information that will be used both
* from _list_header and _list module.
* This allows to compute some values only once.
*
* @param $c the filter condition used
*/
protected function updateVariableFrameWithHeaderInfo($c) {

  VariableFrame::$filterCondition = clone($c);

  $filterWithOrder = clone($c);
  $this->addOrder($filterWithOrder);

  VariableFrame::$filterConditionWithOrder = $filterWithOrder;

  list($startDate, $endDate, $descr) = $this->getAndUpdateTimeFrame();
  VariableFrame::$startFilterDate = $startDate;
  VariableFrame::$endFilterDate = $endDate;

  VariableFrame::$showChannelUsage = $this->getUser()->getFlash('show_channel_usage');
  if (is_null(VariableFrame::$showChannelUsage)) {
    VariableFrame::$showChannelUsage = FALSE;
  }

  // Calculate totals
  // NOTE: in order to reduce to 1/2 the heavy queries,
  // perform a group on available geographic locations in time range
  // in this query. It allows performing a unique table scan on the database.
  $c2 = clone($c);
  $c2->clearSelectColumns();
  $c2->addSelectColumn('SUM(' . ArCdrPeer::COUNT_OF_CALLS . ')');     // field 0
  $c2->addSelectColumn('SUM(' . ArCdrPeer::BILLSEC . ')');  // field 1
  $c2->addSelectColumn('SUM(' . ArCdrPeer::INCOME . ')');   // field 2
  $c2->addSelectColumn('SUM(' . ArCdrPeer::COST . ')');     // field 3
  $c2->addSelectColumn(ArTelephonePrefixPeer::GEOGRAPHIC_LOCATION); // field 4
  $c2->addSelectColumn('SUM(' . ArCdrPeer::COST_SAVING . ')');     // field 5
  $c2->addGroupByColumn(ArTelephonePrefixPeer::GEOGRAPHIC_LOCATION);
  $rs = BasePeer::doSelect($c2);

  $totCalls = 0;
  $totSeconds = 0;
  $totIncomes = 0;
  $totCosts = 0;
  $totSavingCosts = 0;
  $geoLoc = array();

  foreach($rs as $rec) {
    $totCalls += $rec[0];
    $totSeconds += $rec[1];
    $totIncomes += $rec[2];
    $totCosts += $rec[3];
    $totSavingCosts += $rec[5];
    $geoLoc[$rec[4]] = 0;
  }

  VariableFrame::$geographicLocationsInTimeRange = $geoLoc;
  VariableFrame::$countOfRecords = $totCalls;
  VariableFrame::$totSeconds = $totSeconds;
  VariableFrame::$totIncomes = $totIncomes;
  VariableFrame::$totCosts = $totCosts;
  VariableFrame::$totSavingCosts = $totSavingCosts;
  VariableFrame::$totEarn = $totIncomes - $totCosts;
}

/**
* Override addFiltersCriteria in order to add a more strict filter.
*
* POSTCONDITION: the resulting $c does not contain any select field
* (required from the pager that adds its fields)
*
* POSTCONDITION: filters are checked for sanity/security constraints according
* the privileges of the logged user.
*
* NOTE: the enabled/disabled filters must the same configured in
* generator.yml, filters section.
*
*/
protected function addFiltersCriteria($c) {

// TODO check if this is required
// parent::addFiltersCriteria($c);

$filterDescr_clauses = array();
$filterOnDestinationTypeApplied = false;
$filterOnRedirectedCalls = null;
$destinationType = null;

if (isset($this->filters['grouping'])) {
  VariableFrame::$groupOn = $this->filters['grouping'];
} else {
  VariableFrame::$groupOn = 0;
}

if (VariableFrame::$groupOn == 0) {
    // group on calls
} else if (VariableFrame::$groupOn == 1) {
    // group on extensions
    $c->addGroupByColumn(ArCdrPeer::CACHED_PARENT_ID_HIERARCHY);
} else if (VariableFrame::$groupOn == 2) {
    // group on billable organization
    $c->addGroupByColumn(ArCdrPeer::BILLABLE_AR_ORGANIZATION_UNIT_ID);
}

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
            $organizationId = OrganizationUnitInfo::getInstance()->getUniqueRootOrganizationIdIfExists();
            }
        <?php } else { ?>
            // normal customer

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
            $c->add(ArCdrPeer::CACHED_PARENT_ID_HIERARCHY,
            '%' . OrganizationUnitInfo::getSingleIds($organizationId) . '%',
            Criteria::LIKE);
            }

            <?php
            break;

        case FieldsToShow::CALL_DIRECTION:
            ?>
            if (isset($this->filters['is_redirect']) && $this->filters['is_redirect'] !== '')
            {
              $c->add(ArCdrPeer::IS_REDIRECT, $this->filters['is_redirect']);

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
              $c->add(ArCdrPeer::DESTINATION_TYPE, $destinationType);
              $filterOnDestinationTypeApplied = true;
            }
            <?php
            break;

        case FieldsToShow::EXTERNAL_TELEPHONE_NUMBER:
            ?>
            $loc = filterValue($this->filters, 'filter_on_external_telephone_number');
            if (! isEmptyOrNull($loc)) {
            $c->add(ArCdrPeer::CACHED_MASKED_EXTERNAL_TELEPHONE_NUMBER, $loc .'%', Criteria::LIKE);
            $filterDescr_clauses[] = __('external telephone number starting with ') . '"' . __($loc) . '"';
            }

            <?php
            break;

        case FieldsToShow::GEOGRAPHIC_LOCATION:
            ?>
            $loc = filterValue($this->filters, 'filter_on_dst_geographic_location');
            if (!is_null($loc)) {
            $c->add(ArTelephonePrefixPeer::GEOGRAPHIC_LOCATION, $loc);

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
            $c->add(ArTelephonePrefixPeer::OPERATOR_TYPE, $loc);

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
            if (isset($this->filters['ar_vendor_id_is_empty']))
            {
            // nothing to do
            }
            else if (isset($this->filters['ar_vendor_id']) && $this->filters['ar_vendor_id'] !== '')
            {
            $c->add(ArCdrPeer::AR_VENDOR_ID, $this->filters['ar_vendor_id']);
            $vendor = ArVendorPeer::retrieveByPk($this->filters['ar_vendor_id']);

            if (! is_null($vendor) && ! isEmptyOrNull($vendor->getName())) {
            $filterDescr_clauses[] = __('vendor ') . $vendor->getName();
            } else {
            $filterDescr_clauses[] = __('unknown vendor');
            }
            }

            $vendorCostF = filterValue($this->filters, 'filter_on_vendor_cost');
            if ($vendorCostF === DestinationType::VENDOR_COST_DIFFERENT_FROM_EXPECTED) {
              $c->add(ArCdrPeer::EXPECTED_COST, ArCdrPeer::COST, Criteria::NOT_EQUAL);
            }
            <?php
            break;

        case FieldsToShow::COMMUNICATION_CHANNEL:
            ?>

            if (isset($this->filters['ar_communication_channel_type_id_is_empty']))
            {
            // nothing to do
            } else if (isset($this->filters['ar_communication_channel_type_id']) && $this->filters['ar_communication_channel_type_id'] !== '') {
            $c->add(ArCdrPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID, $this->filters['ar_communication_channel_type_id']);
            $channel = ArCommunicationChannelTypePeer::retrieveByPk($this->filters['ar_communication_channel_type_id']);
            if (! is_null($channel)) {
            $filterDescr_clauses[] = __('communication channel ') . $channel->getName();
            } else {
            $filterDescr_clauses[] = __('unknown channel');
            }
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
    DestinationType::addAdminFiltersAccordingConfiguration($c);
    }

<?php } else { ?>
    // Normal users do not see unprocessed/ignored calls
    //
    if (!$filterOnDestinationTypeApplied) {
    DestinationType::addCustomerFiltersAccordingConfiguration($c);
    }
<?php } ?>

$this->addFilterOnTimeFrame($c);

VariableFrame::$showMaskedTelephoneNumbers = true;
if (ArViewAllUserPermissionsPeer::haveUserPermission($this->getUser()->getUserId(), ArPermission::CAN_VIEW_COMPLETE_TELEPHONE_NUMBERS)) {
if (isset($this->filters['show_masked_telephone_numbers']) && $this->filters['show_masked_telephone_numbers'] == '1') {
VariableFrame::$showMaskedTelephoneNumbers = false;
}
}

//
// Create the human readable description of the filter
//

list($ignore1, $ignore2, $filterDescr_time) = $this->getAndUpdateTimeFrame();
VariableFrame::$filterDescription = translateFilterOnCalls($filterOnRedirectedCalls, $destinationType);

<?php if ($generateForAdmin) { ?>

    if (! is_null(VariableFrame::$startOrganizationId)) {
    VariableFrame::$filterDescription .= __('of ') . OrganizationUnitInfo::getInstance()->getFullNameAtDate(VariableFrame::$startOrganizationId, null, true, true) . ' ';
    }

<?php } else { ?>

    if (! is_null(VariableFrame::$startOrganizationId)) {
    VariableFrame::$filterDescription .= htmlspecialchars(
    __('of ') . OrganizationUnitInfo::getInstance()->getFullNameAtDate(VariableFrame::$startOrganizationId, null, false, false) . ' '
    , ENT_QUOTES
    , 'UTF-8');
    }

<?php } ?>

if (! is_null($filterDescr_time)) {
VariableFrame::$filterDescription .=  ' ' . $filterDescr_time;
}

if (count($filterDescr_clauses) > 0) {
VariableFrame::$filterDescription .= __(' with ') . implode(', and ', $filterDescr_clauses);
}

}

/**
* @return list($startDate, $endDate, $filterDescr) in unix timestamp format.
* @pre call first $this->initBeforeCalcCondition();
*/
protected function getAndUpdateTimeFrame() {
if ($this->areDateCached == TRUE) {
return array($this->cachedStartDate, $this->cachedEndDate, $this->cachedFilterDescr);
}

$fromDate = null;
$toDate = null;

$fromDate1 = filterValue($this->filters, 'filter_on_calldate_from');
if (!is_null($fromDate1)) {
$fromDate = fromSymfonyTimestampToUnixTimestamp($fromDate1);
}

$toDate1 = filterValue($this->filters, 'filter_on_calldate_to');
if (!is_null($toDate1)) {
$toDate = fromSymfonyTimestampToUnixTimestamp($toDate1);
}

$frame = filterValue($this->filters, 'filter_on_timeframe');
if (is_null($frame)) {
  $frame = 'recent';
}

// start from today, removing the time...
//
$baseDate = time();

switch ($frame) {
case 'recent':
  $params = ArParamsPeer::getDefaultParams();
  if (is_null($params)) {
    $frame = '1';
  } else {
    $d1 = fromMySQLTimestampToUnixTimestamp($params->getOfficialCalldate());
    if (is_null($d1)) {
      $frame = '1';
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

<?php if ($generateForAdmin) { ?>
    $filterDescr = getUserReadableTimeFrame($fromDate, $toDate, false, false);
<?php } else { ?>
    $filterDescr = getUserReadableTimeFrame($fromDate, $toDate, false, true);
<?php } ?>

$this->cachedStartDate = $fromDate;
$this->cachedEndDate = $toDate;
$this->cachedFilterDescr = $filterDescr;
$this->areDateCached = TRUE;

return array($fromDate, $toDate, $filterDescr);
}

/**
* Apply a filter on time frame.
*
* @param $c the initial condition
* @return $c the final condition with the new constraints
*/
protected function addFilterOnTimeFrame($c) {
list($fromDate, $toDate, $descr) = $this->getAndUpdateTimeFrame();

if (is_null($toDate)) {
$filterFromDate = fromUnixTimestampToMySQLTimestamp($fromDate);
$c->add(ArCdrPeer::CALLDATE, $filterFromDate, Criteria::GREATER_EQUAL);
} else {
$filterFromDate = fromUnixTimestampToMySQLTimestamp($fromDate);
$filterToDate = fromUnixTimestampToMySQLTimestamp($toDate);

$c2  = $c->getNewCriterion(ArCdrPeer::CALLDATE, $filterFromDate, Criteria::GREATER_EQUAL);
$c2->addAnd($c->getNewCriterion(ArCdrPeer::CALLDATE, $filterToDate, Criteria::LESS_THAN));
$c->add($c2);
}

$this->filters['filter_on_calldate_to'] =  fromUnixTimestampToSymfonyStrDate($toDate);
$this->filters['filter_on_calldate_from'] = fromUnixTimestampToSymfonyStrDate($fromDate);
}

protected function manageSecurityError() {
$module = getSuggestedCallReportModule(sfContext::getInstance()->getUser());
return $this->redirect($module);
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

}

<?php
echo '?>' . "\n";
?>
