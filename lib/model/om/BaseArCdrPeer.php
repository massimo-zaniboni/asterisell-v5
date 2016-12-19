<?php

/**
 * Base static class for performing query and update operations on the 'ar_cdr' table.
 *
 * 
 *
 * @package    lib.model.om
 */
abstract class BaseArCdrPeer {

	/** the default database name for this class */
	const DATABASE_NAME = 'propel';

	/** the table name for this class */
	const TABLE_NAME = 'ar_cdr';

	/** the related Propel class for this table */
	const OM_CLASS = 'ArCdr';

	/** A class that can be returned by this peer. */
	const CLASS_DEFAULT = 'lib.model.ArCdr';

	/** the related TableMap class for this table */
	const TM_CLASS = 'ArCdrTableMap';
	
	/** The total number of columns. */
	const NUM_COLUMNS = 33;

	/** The number of lazy-loaded columns. */
	const NUM_LAZY_LOAD_COLUMNS = 0;

	/** the column name for the ID field */
	const ID = 'ar_cdr.ID';

	/** the column name for the CALLDATE field */
	const CALLDATE = 'ar_cdr.CALLDATE';

	/** the column name for the TO_CALLDATE field */
	const TO_CALLDATE = 'ar_cdr.TO_CALLDATE';

	/** the column name for the IS_IMPORTED_SERVICE_CDR field */
	const IS_IMPORTED_SERVICE_CDR = 'ar_cdr.IS_IMPORTED_SERVICE_CDR';

	/** the column name for the COUNT_OF_CALLS field */
	const COUNT_OF_CALLS = 'ar_cdr.COUNT_OF_CALLS';

	/** the column name for the DESTINATION_TYPE field */
	const DESTINATION_TYPE = 'ar_cdr.DESTINATION_TYPE';

	/** the column name for the IS_REDIRECT field */
	const IS_REDIRECT = 'ar_cdr.IS_REDIRECT';

	/** the column name for the DURATION field */
	const DURATION = 'ar_cdr.DURATION';

	/** the column name for the BILLSEC field */
	const BILLSEC = 'ar_cdr.BILLSEC';

	/** the column name for the AR_ORGANIZATION_UNIT_ID field */
	const AR_ORGANIZATION_UNIT_ID = 'ar_cdr.AR_ORGANIZATION_UNIT_ID';

	/** the column name for the CACHED_PARENT_ID_HIERARCHY field */
	const CACHED_PARENT_ID_HIERARCHY = 'ar_cdr.CACHED_PARENT_ID_HIERARCHY';

	/** the column name for the BILLABLE_AR_ORGANIZATION_UNIT_ID field */
	const BILLABLE_AR_ORGANIZATION_UNIT_ID = 'ar_cdr.BILLABLE_AR_ORGANIZATION_UNIT_ID';

	/** the column name for the BUNDLE_AR_ORGANIZATION_UNIT_ID field */
	const BUNDLE_AR_ORGANIZATION_UNIT_ID = 'ar_cdr.BUNDLE_AR_ORGANIZATION_UNIT_ID';

	/** the column name for the INCOME field */
	const INCOME = 'ar_cdr.INCOME';

	/** the column name for the COST_SAVING field */
	const COST_SAVING = 'ar_cdr.COST_SAVING';

	/** the column name for the AR_VENDOR_ID field */
	const AR_VENDOR_ID = 'ar_cdr.AR_VENDOR_ID';

	/** the column name for the AR_COMMUNICATION_CHANNEL_TYPE_ID field */
	const AR_COMMUNICATION_CHANNEL_TYPE_ID = 'ar_cdr.AR_COMMUNICATION_CHANNEL_TYPE_ID';

	/** the column name for the COST field */
	const COST = 'ar_cdr.COST';

	/** the column name for the EXPECTED_COST field */
	const EXPECTED_COST = 'ar_cdr.EXPECTED_COST';

	/** the column name for the AR_TELEPHONE_PREFIX_ID field */
	const AR_TELEPHONE_PREFIX_ID = 'ar_cdr.AR_TELEPHONE_PREFIX_ID';

	/** the column name for the CACHED_EXTERNAL_TELEPHONE_NUMBER field */
	const CACHED_EXTERNAL_TELEPHONE_NUMBER = 'ar_cdr.CACHED_EXTERNAL_TELEPHONE_NUMBER';

	/** the column name for the EXTERNAL_TELEPHONE_NUMBER_WITH_APPLIED_PORTABILITY field */
	const EXTERNAL_TELEPHONE_NUMBER_WITH_APPLIED_PORTABILITY = 'ar_cdr.EXTERNAL_TELEPHONE_NUMBER_WITH_APPLIED_PORTABILITY';

	/** the column name for the CACHED_MASKED_EXTERNAL_TELEPHONE_NUMBER field */
	const CACHED_MASKED_EXTERNAL_TELEPHONE_NUMBER = 'ar_cdr.CACHED_MASKED_EXTERNAL_TELEPHONE_NUMBER';

	/** the column name for the ERROR_DESTINATION_TYPE field */
	const ERROR_DESTINATION_TYPE = 'ar_cdr.ERROR_DESTINATION_TYPE';

	/** the column name for the AR_PROBLEM_DUPLICATION_KEY field */
	const AR_PROBLEM_DUPLICATION_KEY = 'ar_cdr.AR_PROBLEM_DUPLICATION_KEY';

	/** the column name for the DEBUG_COST_RATE field */
	const DEBUG_COST_RATE = 'ar_cdr.DEBUG_COST_RATE';

	/** the column name for the DEBUG_INCOME_RATE field */
	const DEBUG_INCOME_RATE = 'ar_cdr.DEBUG_INCOME_RATE';

	/** the column name for the DEBUG_RESIDUAL_INCOME_RATE field */
	const DEBUG_RESIDUAL_INCOME_RATE = 'ar_cdr.DEBUG_RESIDUAL_INCOME_RATE';

	/** the column name for the DEBUG_RESIDUAL_CALL_DURATION field */
	const DEBUG_RESIDUAL_CALL_DURATION = 'ar_cdr.DEBUG_RESIDUAL_CALL_DURATION';

	/** the column name for the DEBUG_BUNDLE_LEFT_CALLS field */
	const DEBUG_BUNDLE_LEFT_CALLS = 'ar_cdr.DEBUG_BUNDLE_LEFT_CALLS';

	/** the column name for the DEBUG_BUNDLE_LEFT_DURATION field */
	const DEBUG_BUNDLE_LEFT_DURATION = 'ar_cdr.DEBUG_BUNDLE_LEFT_DURATION';

	/** the column name for the DEBUG_BUNDLE_LEFT_COST field */
	const DEBUG_BUNDLE_LEFT_COST = 'ar_cdr.DEBUG_BUNDLE_LEFT_COST';

	/** the column name for the DEBUG_RATING_DETAILS field */
	const DEBUG_RATING_DETAILS = 'ar_cdr.DEBUG_RATING_DETAILS';

	/**
	 * An identiy map to hold any loaded instances of ArCdr objects.
	 * This must be public so that other peer classes can access this when hydrating from JOIN
	 * queries.
	 * @var        array ArCdr[]
	 */
	public static $instances = array();


	// symfony behavior
	
	/**
	 * Indicates whether the current model includes I18N.
	 */
	const IS_I18N = false;

	/**
	 * holds an array of fieldnames
	 *
	 * first dimension keys are the type constants
	 * e.g. self::$fieldNames[self::TYPE_PHPNAME][0] = 'Id'
	 */
	private static $fieldNames = array (
		BasePeer::TYPE_PHPNAME => array ('Id', 'Calldate', 'ToCalldate', 'IsImportedServiceCdr', 'CountOfCalls', 'DestinationType', 'IsRedirect', 'Duration', 'Billsec', 'ArOrganizationUnitId', 'CachedParentIdHierarchy', 'BillableArOrganizationUnitId', 'BundleArOrganizationUnitId', 'Income', 'CostSaving', 'ArVendorId', 'ArCommunicationChannelTypeId', 'Cost', 'ExpectedCost', 'ArTelephonePrefixId', 'CachedExternalTelephoneNumber', 'ExternalTelephoneNumberWithAppliedPortability', 'CachedMaskedExternalTelephoneNumber', 'ErrorDestinationType', 'ArProblemDuplicationKey', 'DebugCostRate', 'DebugIncomeRate', 'DebugResidualIncomeRate', 'DebugResidualCallDuration', 'DebugBundleLeftCalls', 'DebugBundleLeftDuration', 'DebugBundleLeftCost', 'DebugRatingDetails', ),
		BasePeer::TYPE_STUDLYPHPNAME => array ('id', 'calldate', 'toCalldate', 'isImportedServiceCdr', 'countOfCalls', 'destinationType', 'isRedirect', 'duration', 'billsec', 'arOrganizationUnitId', 'cachedParentIdHierarchy', 'billableArOrganizationUnitId', 'bundleArOrganizationUnitId', 'income', 'costSaving', 'arVendorId', 'arCommunicationChannelTypeId', 'cost', 'expectedCost', 'arTelephonePrefixId', 'cachedExternalTelephoneNumber', 'externalTelephoneNumberWithAppliedPortability', 'cachedMaskedExternalTelephoneNumber', 'errorDestinationType', 'arProblemDuplicationKey', 'debugCostRate', 'debugIncomeRate', 'debugResidualIncomeRate', 'debugResidualCallDuration', 'debugBundleLeftCalls', 'debugBundleLeftDuration', 'debugBundleLeftCost', 'debugRatingDetails', ),
		BasePeer::TYPE_COLNAME => array (self::ID, self::CALLDATE, self::TO_CALLDATE, self::IS_IMPORTED_SERVICE_CDR, self::COUNT_OF_CALLS, self::DESTINATION_TYPE, self::IS_REDIRECT, self::DURATION, self::BILLSEC, self::AR_ORGANIZATION_UNIT_ID, self::CACHED_PARENT_ID_HIERARCHY, self::BILLABLE_AR_ORGANIZATION_UNIT_ID, self::BUNDLE_AR_ORGANIZATION_UNIT_ID, self::INCOME, self::COST_SAVING, self::AR_VENDOR_ID, self::AR_COMMUNICATION_CHANNEL_TYPE_ID, self::COST, self::EXPECTED_COST, self::AR_TELEPHONE_PREFIX_ID, self::CACHED_EXTERNAL_TELEPHONE_NUMBER, self::EXTERNAL_TELEPHONE_NUMBER_WITH_APPLIED_PORTABILITY, self::CACHED_MASKED_EXTERNAL_TELEPHONE_NUMBER, self::ERROR_DESTINATION_TYPE, self::AR_PROBLEM_DUPLICATION_KEY, self::DEBUG_COST_RATE, self::DEBUG_INCOME_RATE, self::DEBUG_RESIDUAL_INCOME_RATE, self::DEBUG_RESIDUAL_CALL_DURATION, self::DEBUG_BUNDLE_LEFT_CALLS, self::DEBUG_BUNDLE_LEFT_DURATION, self::DEBUG_BUNDLE_LEFT_COST, self::DEBUG_RATING_DETAILS, ),
		BasePeer::TYPE_FIELDNAME => array ('id', 'calldate', 'to_calldate', 'is_imported_service_cdr', 'count_of_calls', 'destination_type', 'is_redirect', 'duration', 'billsec', 'ar_organization_unit_id', 'cached_parent_id_hierarchy', 'billable_ar_organization_unit_id', 'bundle_ar_organization_unit_id', 'income', 'cost_saving', 'ar_vendor_id', 'ar_communication_channel_type_id', 'cost', 'expected_cost', 'ar_telephone_prefix_id', 'cached_external_telephone_number', 'external_telephone_number_with_applied_portability', 'cached_masked_external_telephone_number', 'error_destination_type', 'ar_problem_duplication_key', 'debug_cost_rate', 'debug_income_rate', 'debug_residual_income_rate', 'debug_residual_call_duration', 'debug_bundle_left_calls', 'debug_bundle_left_duration', 'debug_bundle_left_cost', 'debug_rating_details', ),
		BasePeer::TYPE_NUM => array (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, )
	);

	/**
	 * holds an array of keys for quick access to the fieldnames array
	 *
	 * first dimension keys are the type constants
	 * e.g. self::$fieldNames[BasePeer::TYPE_PHPNAME]['Id'] = 0
	 */
	private static $fieldKeys = array (
		BasePeer::TYPE_PHPNAME => array ('Id' => 0, 'Calldate' => 1, 'ToCalldate' => 2, 'IsImportedServiceCdr' => 3, 'CountOfCalls' => 4, 'DestinationType' => 5, 'IsRedirect' => 6, 'Duration' => 7, 'Billsec' => 8, 'ArOrganizationUnitId' => 9, 'CachedParentIdHierarchy' => 10, 'BillableArOrganizationUnitId' => 11, 'BundleArOrganizationUnitId' => 12, 'Income' => 13, 'CostSaving' => 14, 'ArVendorId' => 15, 'ArCommunicationChannelTypeId' => 16, 'Cost' => 17, 'ExpectedCost' => 18, 'ArTelephonePrefixId' => 19, 'CachedExternalTelephoneNumber' => 20, 'ExternalTelephoneNumberWithAppliedPortability' => 21, 'CachedMaskedExternalTelephoneNumber' => 22, 'ErrorDestinationType' => 23, 'ArProblemDuplicationKey' => 24, 'DebugCostRate' => 25, 'DebugIncomeRate' => 26, 'DebugResidualIncomeRate' => 27, 'DebugResidualCallDuration' => 28, 'DebugBundleLeftCalls' => 29, 'DebugBundleLeftDuration' => 30, 'DebugBundleLeftCost' => 31, 'DebugRatingDetails' => 32, ),
		BasePeer::TYPE_STUDLYPHPNAME => array ('id' => 0, 'calldate' => 1, 'toCalldate' => 2, 'isImportedServiceCdr' => 3, 'countOfCalls' => 4, 'destinationType' => 5, 'isRedirect' => 6, 'duration' => 7, 'billsec' => 8, 'arOrganizationUnitId' => 9, 'cachedParentIdHierarchy' => 10, 'billableArOrganizationUnitId' => 11, 'bundleArOrganizationUnitId' => 12, 'income' => 13, 'costSaving' => 14, 'arVendorId' => 15, 'arCommunicationChannelTypeId' => 16, 'cost' => 17, 'expectedCost' => 18, 'arTelephonePrefixId' => 19, 'cachedExternalTelephoneNumber' => 20, 'externalTelephoneNumberWithAppliedPortability' => 21, 'cachedMaskedExternalTelephoneNumber' => 22, 'errorDestinationType' => 23, 'arProblemDuplicationKey' => 24, 'debugCostRate' => 25, 'debugIncomeRate' => 26, 'debugResidualIncomeRate' => 27, 'debugResidualCallDuration' => 28, 'debugBundleLeftCalls' => 29, 'debugBundleLeftDuration' => 30, 'debugBundleLeftCost' => 31, 'debugRatingDetails' => 32, ),
		BasePeer::TYPE_COLNAME => array (self::ID => 0, self::CALLDATE => 1, self::TO_CALLDATE => 2, self::IS_IMPORTED_SERVICE_CDR => 3, self::COUNT_OF_CALLS => 4, self::DESTINATION_TYPE => 5, self::IS_REDIRECT => 6, self::DURATION => 7, self::BILLSEC => 8, self::AR_ORGANIZATION_UNIT_ID => 9, self::CACHED_PARENT_ID_HIERARCHY => 10, self::BILLABLE_AR_ORGANIZATION_UNIT_ID => 11, self::BUNDLE_AR_ORGANIZATION_UNIT_ID => 12, self::INCOME => 13, self::COST_SAVING => 14, self::AR_VENDOR_ID => 15, self::AR_COMMUNICATION_CHANNEL_TYPE_ID => 16, self::COST => 17, self::EXPECTED_COST => 18, self::AR_TELEPHONE_PREFIX_ID => 19, self::CACHED_EXTERNAL_TELEPHONE_NUMBER => 20, self::EXTERNAL_TELEPHONE_NUMBER_WITH_APPLIED_PORTABILITY => 21, self::CACHED_MASKED_EXTERNAL_TELEPHONE_NUMBER => 22, self::ERROR_DESTINATION_TYPE => 23, self::AR_PROBLEM_DUPLICATION_KEY => 24, self::DEBUG_COST_RATE => 25, self::DEBUG_INCOME_RATE => 26, self::DEBUG_RESIDUAL_INCOME_RATE => 27, self::DEBUG_RESIDUAL_CALL_DURATION => 28, self::DEBUG_BUNDLE_LEFT_CALLS => 29, self::DEBUG_BUNDLE_LEFT_DURATION => 30, self::DEBUG_BUNDLE_LEFT_COST => 31, self::DEBUG_RATING_DETAILS => 32, ),
		BasePeer::TYPE_FIELDNAME => array ('id' => 0, 'calldate' => 1, 'to_calldate' => 2, 'is_imported_service_cdr' => 3, 'count_of_calls' => 4, 'destination_type' => 5, 'is_redirect' => 6, 'duration' => 7, 'billsec' => 8, 'ar_organization_unit_id' => 9, 'cached_parent_id_hierarchy' => 10, 'billable_ar_organization_unit_id' => 11, 'bundle_ar_organization_unit_id' => 12, 'income' => 13, 'cost_saving' => 14, 'ar_vendor_id' => 15, 'ar_communication_channel_type_id' => 16, 'cost' => 17, 'expected_cost' => 18, 'ar_telephone_prefix_id' => 19, 'cached_external_telephone_number' => 20, 'external_telephone_number_with_applied_portability' => 21, 'cached_masked_external_telephone_number' => 22, 'error_destination_type' => 23, 'ar_problem_duplication_key' => 24, 'debug_cost_rate' => 25, 'debug_income_rate' => 26, 'debug_residual_income_rate' => 27, 'debug_residual_call_duration' => 28, 'debug_bundle_left_calls' => 29, 'debug_bundle_left_duration' => 30, 'debug_bundle_left_cost' => 31, 'debug_rating_details' => 32, ),
		BasePeer::TYPE_NUM => array (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, )
	);

	/**
	 * Translates a fieldname to another type
	 *
	 * @param      string $name field name
	 * @param      string $fromType One of the class type constants BasePeer::TYPE_PHPNAME, BasePeer::TYPE_STUDLYPHPNAME
	 *                         BasePeer::TYPE_COLNAME, BasePeer::TYPE_FIELDNAME, BasePeer::TYPE_NUM
	 * @param      string $toType   One of the class type constants
	 * @return     string translated name of the field.
	 * @throws     PropelException - if the specified name could not be found in the fieldname mappings.
	 */
	static public function translateFieldName($name, $fromType, $toType)
	{
		$toNames = self::getFieldNames($toType);
		$key = isset(self::$fieldKeys[$fromType][$name]) ? self::$fieldKeys[$fromType][$name] : null;
		if ($key === null) {
			throw new PropelException("'$name' could not be found in the field names of type '$fromType'. These are: " . print_r(self::$fieldKeys[$fromType], true));
		}
		return $toNames[$key];
	}

	/**
	 * Returns an array of field names.
	 *
	 * @param      string $type The type of fieldnames to return:
	 *                      One of the class type constants BasePeer::TYPE_PHPNAME, BasePeer::TYPE_STUDLYPHPNAME
	 *                      BasePeer::TYPE_COLNAME, BasePeer::TYPE_FIELDNAME, BasePeer::TYPE_NUM
	 * @return     array A list of field names
	 */

	static public function getFieldNames($type = BasePeer::TYPE_PHPNAME)
	{
		if (!array_key_exists($type, self::$fieldNames)) {
			throw new PropelException('Method getFieldNames() expects the parameter $type to be one of the class constants BasePeer::TYPE_PHPNAME, BasePeer::TYPE_STUDLYPHPNAME, BasePeer::TYPE_COLNAME, BasePeer::TYPE_FIELDNAME, BasePeer::TYPE_NUM. ' . $type . ' was given.');
		}
		return self::$fieldNames[$type];
	}

	/**
	 * Convenience method which changes table.column to alias.column.
	 *
	 * Using this method you can maintain SQL abstraction while using column aliases.
	 * <code>
	 *		$c->addAlias("alias1", TablePeer::TABLE_NAME);
	 *		$c->addJoin(TablePeer::alias("alias1", TablePeer::PRIMARY_KEY_COLUMN), TablePeer::PRIMARY_KEY_COLUMN);
	 * </code>
	 * @param      string $alias The alias for the current table.
	 * @param      string $column The column name for current table. (i.e. ArCdrPeer::COLUMN_NAME).
	 * @return     string
	 */
	public static function alias($alias, $column)
	{
		return str_replace(ArCdrPeer::TABLE_NAME.'.', $alias.'.', $column);
	}

	/**
	 * Add all the columns needed to create a new object.
	 *
	 * Note: any columns that were marked with lazyLoad="true" in the
	 * XML schema will not be added to the select list and only loaded
	 * on demand.
	 *
	 * @param      criteria object containing the columns to add.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function addSelectColumns(Criteria $criteria)
	{
		$criteria->addSelectColumn(ArCdrPeer::ID);
		$criteria->addSelectColumn(ArCdrPeer::CALLDATE);
		$criteria->addSelectColumn(ArCdrPeer::TO_CALLDATE);
		$criteria->addSelectColumn(ArCdrPeer::IS_IMPORTED_SERVICE_CDR);
		$criteria->addSelectColumn(ArCdrPeer::COUNT_OF_CALLS);
		$criteria->addSelectColumn(ArCdrPeer::DESTINATION_TYPE);
		$criteria->addSelectColumn(ArCdrPeer::IS_REDIRECT);
		$criteria->addSelectColumn(ArCdrPeer::DURATION);
		$criteria->addSelectColumn(ArCdrPeer::BILLSEC);
		$criteria->addSelectColumn(ArCdrPeer::AR_ORGANIZATION_UNIT_ID);
		$criteria->addSelectColumn(ArCdrPeer::CACHED_PARENT_ID_HIERARCHY);
		$criteria->addSelectColumn(ArCdrPeer::BILLABLE_AR_ORGANIZATION_UNIT_ID);
		$criteria->addSelectColumn(ArCdrPeer::BUNDLE_AR_ORGANIZATION_UNIT_ID);
		$criteria->addSelectColumn(ArCdrPeer::INCOME);
		$criteria->addSelectColumn(ArCdrPeer::COST_SAVING);
		$criteria->addSelectColumn(ArCdrPeer::AR_VENDOR_ID);
		$criteria->addSelectColumn(ArCdrPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID);
		$criteria->addSelectColumn(ArCdrPeer::COST);
		$criteria->addSelectColumn(ArCdrPeer::EXPECTED_COST);
		$criteria->addSelectColumn(ArCdrPeer::AR_TELEPHONE_PREFIX_ID);
		$criteria->addSelectColumn(ArCdrPeer::CACHED_EXTERNAL_TELEPHONE_NUMBER);
		$criteria->addSelectColumn(ArCdrPeer::EXTERNAL_TELEPHONE_NUMBER_WITH_APPLIED_PORTABILITY);
		$criteria->addSelectColumn(ArCdrPeer::CACHED_MASKED_EXTERNAL_TELEPHONE_NUMBER);
		$criteria->addSelectColumn(ArCdrPeer::ERROR_DESTINATION_TYPE);
		$criteria->addSelectColumn(ArCdrPeer::AR_PROBLEM_DUPLICATION_KEY);
		$criteria->addSelectColumn(ArCdrPeer::DEBUG_COST_RATE);
		$criteria->addSelectColumn(ArCdrPeer::DEBUG_INCOME_RATE);
		$criteria->addSelectColumn(ArCdrPeer::DEBUG_RESIDUAL_INCOME_RATE);
		$criteria->addSelectColumn(ArCdrPeer::DEBUG_RESIDUAL_CALL_DURATION);
		$criteria->addSelectColumn(ArCdrPeer::DEBUG_BUNDLE_LEFT_CALLS);
		$criteria->addSelectColumn(ArCdrPeer::DEBUG_BUNDLE_LEFT_DURATION);
		$criteria->addSelectColumn(ArCdrPeer::DEBUG_BUNDLE_LEFT_COST);
		$criteria->addSelectColumn(ArCdrPeer::DEBUG_RATING_DETAILS);
	}

	/**
	 * Returns the number of rows matching criteria.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @return     int Number of matching rows.
	 */
	public static function doCount(Criteria $criteria, $distinct = false, PropelPDO $con = null)
	{
		// we may modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArCdrPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArCdrPeer::addSelectColumns($criteria);
		}

		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		$criteria->setDbName(self::DATABASE_NAME); // Set the correct dbName

		if ($con === null) {
			$con = Propel::getConnection(ArCdrPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}
		// BasePeer returns a PDOStatement
		$stmt = BasePeer::doCount($criteria, $con);

		if ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$count = (int) $row[0];
		} else {
			$count = 0; // no rows returned; we infer that means 0 matches.
		}
		$stmt->closeCursor();
		return $count;
	}
	/**
	 * Method to select one object from the DB.
	 *
	 * @param      Criteria $criteria object used to create the SELECT statement.
	 * @param      PropelPDO $con
	 * @return     ArCdr
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectOne(Criteria $criteria, PropelPDO $con = null)
	{
		$critcopy = clone $criteria;
		$critcopy->setLimit(1);
		$objects = ArCdrPeer::doSelect($critcopy, $con);
		if ($objects) {
			return $objects[0];
		}
		return null;
	}
	/**
	 * Method to do selects.
	 *
	 * @param      Criteria $criteria The Criteria object used to build the SELECT statement.
	 * @param      PropelPDO $con
	 * @return     array Array of selected Objects
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelect(Criteria $criteria, PropelPDO $con = null)
	{
		return ArCdrPeer::populateObjects(ArCdrPeer::doSelectStmt($criteria, $con));
	}
	/**
	 * Prepares the Criteria object and uses the parent doSelect() method to execute a PDOStatement.
	 *
	 * Use this method directly if you want to work with an executed statement durirectly (for example
	 * to perform your own object hydration).
	 *
	 * @param      Criteria $criteria The Criteria object used to build the SELECT statement.
	 * @param      PropelPDO $con The connection to use
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 * @return     PDOStatement The executed PDOStatement object.
	 * @see        BasePeer::doSelect()
	 */
	public static function doSelectStmt(Criteria $criteria, PropelPDO $con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArCdrPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		if (!$criteria->hasSelectClause()) {
			$criteria = clone $criteria;
			ArCdrPeer::addSelectColumns($criteria);
		}

		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		// BasePeer returns a PDOStatement
		return BasePeer::doSelect($criteria, $con);
	}
	/**
	 * Adds an object to the instance pool.
	 *
	 * Propel keeps cached copies of objects in an instance pool when they are retrieved
	 * from the database.  In some cases -- especially when you override doSelect*()
	 * methods in your stub classes -- you may need to explicitly add objects
	 * to the cache in order to ensure that the same objects are always returned by doSelect*()
	 * and retrieveByPK*() calls.
	 *
	 * @param      ArCdr $value A ArCdr object.
	 * @param      string $key (optional) key to use for instance map (for performance boost if key was already calculated externally).
	 */
	public static function addInstanceToPool(ArCdr $obj, $key = null)
	{
		if (Propel::isInstancePoolingEnabled()) {
			if ($key === null) {
				$key = (string) $obj->getId();
			} // if key === null
			self::$instances[$key] = $obj;
		}
	}

	/**
	 * Removes an object from the instance pool.
	 *
	 * Propel keeps cached copies of objects in an instance pool when they are retrieved
	 * from the database.  In some cases -- especially when you override doDelete
	 * methods in your stub classes -- you may need to explicitly remove objects
	 * from the cache in order to prevent returning objects that no longer exist.
	 *
	 * @param      mixed $value A ArCdr object or a primary key value.
	 */
	public static function removeInstanceFromPool($value)
	{
		if (Propel::isInstancePoolingEnabled() && $value !== null) {
			if (is_object($value) && $value instanceof ArCdr) {
				$key = (string) $value->getId();
			} elseif (is_scalar($value)) {
				// assume we've been passed a primary key
				$key = (string) $value;
			} else {
				$e = new PropelException("Invalid value passed to removeInstanceFromPool().  Expected primary key or ArCdr object; got " . (is_object($value) ? get_class($value) . ' object.' : var_export($value,true)));
				throw $e;
			}

			unset(self::$instances[$key]);
		}
	} // removeInstanceFromPool()

	/**
	 * Retrieves a string version of the primary key from the DB resultset row that can be used to uniquely identify a row in this table.
	 *
	 * For tables with a single-column primary key, that simple pkey value will be returned.  For tables with
	 * a multi-column primary key, a serialize()d version of the primary key will be returned.
	 *
	 * @param      string $key The key (@see getPrimaryKeyHash()) for this instance.
	 * @return     ArCdr Found object or NULL if 1) no instance exists for specified key or 2) instance pooling has been disabled.
	 * @see        getPrimaryKeyHash()
	 */
	public static function getInstanceFromPool($key)
	{
		if (Propel::isInstancePoolingEnabled()) {
			if (isset(self::$instances[$key])) {
				return self::$instances[$key];
			}
		}
		return null; // just to be explicit
	}
	
	/**
	 * Clear the instance pool.
	 *
	 * @return     void
	 */
	public static function clearInstancePool()
	{
		self::$instances = array();
	}
	
	/**
	 * Method to invalidate the instance pool of all tables related to ar_cdr
	 * by a foreign key with ON DELETE CASCADE
	 */
	public static function clearRelatedInstancePool()
	{
	}

	/**
	 * Retrieves a string version of the primary key from the DB resultset row that can be used to uniquely identify a row in this table.
	 *
	 * For tables with a single-column primary key, that simple pkey value will be returned.  For tables with
	 * a multi-column primary key, a serialize()d version of the primary key will be returned.
	 *
	 * @param      array $row PropelPDO resultset row.
	 * @param      int $startcol The 0-based offset for reading from the resultset row.
	 * @return     string A string version of PK or NULL if the components of primary key in result array are all null.
	 */
	public static function getPrimaryKeyHashFromRow($row, $startcol = 0)
	{
		// If the PK cannot be derived from the row, return NULL.
		if ($row[$startcol] === null) {
			return null;
		}
		return (string) $row[$startcol];
	}

	/**
	 * The returned array will contain objects of the default type or
	 * objects that inherit from the default.
	 *
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function populateObjects(PDOStatement $stmt)
	{
		$results = array();
	
		// set the class once to avoid overhead in the loop
		$cls = ArCdrPeer::getOMClass(false);
		// populate the object(s)
		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key = ArCdrPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj = ArCdrPeer::getInstanceFromPool($key))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj->hydrate($row, 0, true); // rehydrate
				$results[] = $obj;
			} else {
				$obj = new $cls();
				$obj->hydrate($row);
				$results[] = $obj;
				ArCdrPeer::addInstanceToPool($obj, $key);
			} // if key exists
		}
		$stmt->closeCursor();
		return $results;
	}

	/**
	 * Returns the number of rows matching criteria, joining the related ArOrganizationUnit table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinArOrganizationUnit(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArCdrPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArCdrPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArCdrPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArCdrPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$stmt = BasePeer::doCount($criteria, $con);

		if ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$count = (int) $row[0];
		} else {
			$count = 0; // no rows returned; we infer that means 0 matches.
		}
		$stmt->closeCursor();
		return $count;
	}


	/**
	 * Returns the number of rows matching criteria, joining the related ArVendor table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinArVendor(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArCdrPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArCdrPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArCdrPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArCdrPeer::AR_VENDOR_ID, ArVendorPeer::ID, $join_behavior);

		$stmt = BasePeer::doCount($criteria, $con);

		if ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$count = (int) $row[0];
		} else {
			$count = 0; // no rows returned; we infer that means 0 matches.
		}
		$stmt->closeCursor();
		return $count;
	}


	/**
	 * Returns the number of rows matching criteria, joining the related ArCommunicationChannelType table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinArCommunicationChannelType(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArCdrPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArCdrPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArCdrPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArCdrPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID, ArCommunicationChannelTypePeer::ID, $join_behavior);

		$stmt = BasePeer::doCount($criteria, $con);

		if ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$count = (int) $row[0];
		} else {
			$count = 0; // no rows returned; we infer that means 0 matches.
		}
		$stmt->closeCursor();
		return $count;
	}


	/**
	 * Returns the number of rows matching criteria, joining the related ArTelephonePrefix table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinArTelephonePrefix(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArCdrPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArCdrPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArCdrPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArCdrPeer::AR_TELEPHONE_PREFIX_ID, ArTelephonePrefixPeer::ID, $join_behavior);

		$stmt = BasePeer::doCount($criteria, $con);

		if ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$count = (int) $row[0];
		} else {
			$count = 0; // no rows returned; we infer that means 0 matches.
		}
		$stmt->closeCursor();
		return $count;
	}


	/**
	 * Selects a collection of ArCdr objects pre-filled with their ArOrganizationUnit objects.
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArCdr objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinArOrganizationUnit(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArCdrPeer::addSelectColumns($criteria);
		$startcol = (ArCdrPeer::NUM_COLUMNS - ArCdrPeer::NUM_LAZY_LOAD_COLUMNS);
		ArOrganizationUnitPeer::addSelectColumns($criteria);

		$criteria->addJoin(ArCdrPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArCdrPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArCdrPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {

				$cls = ArCdrPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArCdrPeer::addInstanceToPool($obj1, $key1);
			} // if $obj1 already loaded

			$key2 = ArOrganizationUnitPeer::getPrimaryKeyHashFromRow($row, $startcol);
			if ($key2 !== null) {
				$obj2 = ArOrganizationUnitPeer::getInstanceFromPool($key2);
				if (!$obj2) {

					$cls = ArOrganizationUnitPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol);
					ArOrganizationUnitPeer::addInstanceToPool($obj2, $key2);
				} // if obj2 already loaded
				
				// Add the $obj1 (ArCdr) to $obj2 (ArOrganizationUnit)
				$obj2->addArCdr($obj1);

			} // if joined row was not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArCdr objects pre-filled with their ArVendor objects.
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArCdr objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinArVendor(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArCdrPeer::addSelectColumns($criteria);
		$startcol = (ArCdrPeer::NUM_COLUMNS - ArCdrPeer::NUM_LAZY_LOAD_COLUMNS);
		ArVendorPeer::addSelectColumns($criteria);

		$criteria->addJoin(ArCdrPeer::AR_VENDOR_ID, ArVendorPeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArCdrPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArCdrPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {

				$cls = ArCdrPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArCdrPeer::addInstanceToPool($obj1, $key1);
			} // if $obj1 already loaded

			$key2 = ArVendorPeer::getPrimaryKeyHashFromRow($row, $startcol);
			if ($key2 !== null) {
				$obj2 = ArVendorPeer::getInstanceFromPool($key2);
				if (!$obj2) {

					$cls = ArVendorPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol);
					ArVendorPeer::addInstanceToPool($obj2, $key2);
				} // if obj2 already loaded
				
				// Add the $obj1 (ArCdr) to $obj2 (ArVendor)
				$obj2->addArCdr($obj1);

			} // if joined row was not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArCdr objects pre-filled with their ArCommunicationChannelType objects.
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArCdr objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinArCommunicationChannelType(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArCdrPeer::addSelectColumns($criteria);
		$startcol = (ArCdrPeer::NUM_COLUMNS - ArCdrPeer::NUM_LAZY_LOAD_COLUMNS);
		ArCommunicationChannelTypePeer::addSelectColumns($criteria);

		$criteria->addJoin(ArCdrPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID, ArCommunicationChannelTypePeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArCdrPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArCdrPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {

				$cls = ArCdrPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArCdrPeer::addInstanceToPool($obj1, $key1);
			} // if $obj1 already loaded

			$key2 = ArCommunicationChannelTypePeer::getPrimaryKeyHashFromRow($row, $startcol);
			if ($key2 !== null) {
				$obj2 = ArCommunicationChannelTypePeer::getInstanceFromPool($key2);
				if (!$obj2) {

					$cls = ArCommunicationChannelTypePeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol);
					ArCommunicationChannelTypePeer::addInstanceToPool($obj2, $key2);
				} // if obj2 already loaded
				
				// Add the $obj1 (ArCdr) to $obj2 (ArCommunicationChannelType)
				$obj2->addArCdr($obj1);

			} // if joined row was not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArCdr objects pre-filled with their ArTelephonePrefix objects.
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArCdr objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinArTelephonePrefix(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArCdrPeer::addSelectColumns($criteria);
		$startcol = (ArCdrPeer::NUM_COLUMNS - ArCdrPeer::NUM_LAZY_LOAD_COLUMNS);
		ArTelephonePrefixPeer::addSelectColumns($criteria);

		$criteria->addJoin(ArCdrPeer::AR_TELEPHONE_PREFIX_ID, ArTelephonePrefixPeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArCdrPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArCdrPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {

				$cls = ArCdrPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArCdrPeer::addInstanceToPool($obj1, $key1);
			} // if $obj1 already loaded

			$key2 = ArTelephonePrefixPeer::getPrimaryKeyHashFromRow($row, $startcol);
			if ($key2 !== null) {
				$obj2 = ArTelephonePrefixPeer::getInstanceFromPool($key2);
				if (!$obj2) {

					$cls = ArTelephonePrefixPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol);
					ArTelephonePrefixPeer::addInstanceToPool($obj2, $key2);
				} // if obj2 already loaded
				
				// Add the $obj1 (ArCdr) to $obj2 (ArTelephonePrefix)
				$obj2->addArCdr($obj1);

			} // if joined row was not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Returns the number of rows matching criteria, joining all related tables
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinAll(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArCdrPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArCdrPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArCdrPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArCdrPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArCdrPeer::AR_VENDOR_ID, ArVendorPeer::ID, $join_behavior);

		$criteria->addJoin(ArCdrPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID, ArCommunicationChannelTypePeer::ID, $join_behavior);

		$criteria->addJoin(ArCdrPeer::AR_TELEPHONE_PREFIX_ID, ArTelephonePrefixPeer::ID, $join_behavior);

		$stmt = BasePeer::doCount($criteria, $con);

		if ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$count = (int) $row[0];
		} else {
			$count = 0; // no rows returned; we infer that means 0 matches.
		}
		$stmt->closeCursor();
		return $count;
	}

	/**
	 * Selects a collection of ArCdr objects pre-filled with all related objects.
	 *
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArCdr objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinAll(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArCdrPeer::addSelectColumns($criteria);
		$startcol2 = (ArCdrPeer::NUM_COLUMNS - ArCdrPeer::NUM_LAZY_LOAD_COLUMNS);

		ArOrganizationUnitPeer::addSelectColumns($criteria);
		$startcol3 = $startcol2 + (ArOrganizationUnitPeer::NUM_COLUMNS - ArOrganizationUnitPeer::NUM_LAZY_LOAD_COLUMNS);

		ArVendorPeer::addSelectColumns($criteria);
		$startcol4 = $startcol3 + (ArVendorPeer::NUM_COLUMNS - ArVendorPeer::NUM_LAZY_LOAD_COLUMNS);

		ArCommunicationChannelTypePeer::addSelectColumns($criteria);
		$startcol5 = $startcol4 + (ArCommunicationChannelTypePeer::NUM_COLUMNS - ArCommunicationChannelTypePeer::NUM_LAZY_LOAD_COLUMNS);

		ArTelephonePrefixPeer::addSelectColumns($criteria);
		$startcol6 = $startcol5 + (ArTelephonePrefixPeer::NUM_COLUMNS - ArTelephonePrefixPeer::NUM_LAZY_LOAD_COLUMNS);

		$criteria->addJoin(ArCdrPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArCdrPeer::AR_VENDOR_ID, ArVendorPeer::ID, $join_behavior);

		$criteria->addJoin(ArCdrPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID, ArCommunicationChannelTypePeer::ID, $join_behavior);

		$criteria->addJoin(ArCdrPeer::AR_TELEPHONE_PREFIX_ID, ArTelephonePrefixPeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArCdrPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArCdrPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {
				$cls = ArCdrPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArCdrPeer::addInstanceToPool($obj1, $key1);
			} // if obj1 already loaded

			// Add objects for joined ArOrganizationUnit rows

			$key2 = ArOrganizationUnitPeer::getPrimaryKeyHashFromRow($row, $startcol2);
			if ($key2 !== null) {
				$obj2 = ArOrganizationUnitPeer::getInstanceFromPool($key2);
				if (!$obj2) {

					$cls = ArOrganizationUnitPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol2);
					ArOrganizationUnitPeer::addInstanceToPool($obj2, $key2);
				} // if obj2 loaded

				// Add the $obj1 (ArCdr) to the collection in $obj2 (ArOrganizationUnit)
				$obj2->addArCdr($obj1);
			} // if joined row not null

			// Add objects for joined ArVendor rows

			$key3 = ArVendorPeer::getPrimaryKeyHashFromRow($row, $startcol3);
			if ($key3 !== null) {
				$obj3 = ArVendorPeer::getInstanceFromPool($key3);
				if (!$obj3) {

					$cls = ArVendorPeer::getOMClass(false);

					$obj3 = new $cls();
					$obj3->hydrate($row, $startcol3);
					ArVendorPeer::addInstanceToPool($obj3, $key3);
				} // if obj3 loaded

				// Add the $obj1 (ArCdr) to the collection in $obj3 (ArVendor)
				$obj3->addArCdr($obj1);
			} // if joined row not null

			// Add objects for joined ArCommunicationChannelType rows

			$key4 = ArCommunicationChannelTypePeer::getPrimaryKeyHashFromRow($row, $startcol4);
			if ($key4 !== null) {
				$obj4 = ArCommunicationChannelTypePeer::getInstanceFromPool($key4);
				if (!$obj4) {

					$cls = ArCommunicationChannelTypePeer::getOMClass(false);

					$obj4 = new $cls();
					$obj4->hydrate($row, $startcol4);
					ArCommunicationChannelTypePeer::addInstanceToPool($obj4, $key4);
				} // if obj4 loaded

				// Add the $obj1 (ArCdr) to the collection in $obj4 (ArCommunicationChannelType)
				$obj4->addArCdr($obj1);
			} // if joined row not null

			// Add objects for joined ArTelephonePrefix rows

			$key5 = ArTelephonePrefixPeer::getPrimaryKeyHashFromRow($row, $startcol5);
			if ($key5 !== null) {
				$obj5 = ArTelephonePrefixPeer::getInstanceFromPool($key5);
				if (!$obj5) {

					$cls = ArTelephonePrefixPeer::getOMClass(false);

					$obj5 = new $cls();
					$obj5->hydrate($row, $startcol5);
					ArTelephonePrefixPeer::addInstanceToPool($obj5, $key5);
				} // if obj5 loaded

				// Add the $obj1 (ArCdr) to the collection in $obj5 (ArTelephonePrefix)
				$obj5->addArCdr($obj1);
			} // if joined row not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Returns the number of rows matching criteria, joining the related ArOrganizationUnit table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinAllExceptArOrganizationUnit(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArCdrPeer::TABLE_NAME);
		
		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArCdrPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY should not affect count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArCdrPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}
	
		$criteria->addJoin(ArCdrPeer::AR_VENDOR_ID, ArVendorPeer::ID, $join_behavior);

		$criteria->addJoin(ArCdrPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID, ArCommunicationChannelTypePeer::ID, $join_behavior);

		$criteria->addJoin(ArCdrPeer::AR_TELEPHONE_PREFIX_ID, ArTelephonePrefixPeer::ID, $join_behavior);

		$stmt = BasePeer::doCount($criteria, $con);

		if ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$count = (int) $row[0];
		} else {
			$count = 0; // no rows returned; we infer that means 0 matches.
		}
		$stmt->closeCursor();
		return $count;
	}


	/**
	 * Returns the number of rows matching criteria, joining the related ArVendor table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinAllExceptArVendor(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArCdrPeer::TABLE_NAME);
		
		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArCdrPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY should not affect count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArCdrPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}
	
		$criteria->addJoin(ArCdrPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArCdrPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID, ArCommunicationChannelTypePeer::ID, $join_behavior);

		$criteria->addJoin(ArCdrPeer::AR_TELEPHONE_PREFIX_ID, ArTelephonePrefixPeer::ID, $join_behavior);

		$stmt = BasePeer::doCount($criteria, $con);

		if ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$count = (int) $row[0];
		} else {
			$count = 0; // no rows returned; we infer that means 0 matches.
		}
		$stmt->closeCursor();
		return $count;
	}


	/**
	 * Returns the number of rows matching criteria, joining the related ArCommunicationChannelType table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinAllExceptArCommunicationChannelType(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArCdrPeer::TABLE_NAME);
		
		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArCdrPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY should not affect count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArCdrPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}
	
		$criteria->addJoin(ArCdrPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArCdrPeer::AR_VENDOR_ID, ArVendorPeer::ID, $join_behavior);

		$criteria->addJoin(ArCdrPeer::AR_TELEPHONE_PREFIX_ID, ArTelephonePrefixPeer::ID, $join_behavior);

		$stmt = BasePeer::doCount($criteria, $con);

		if ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$count = (int) $row[0];
		} else {
			$count = 0; // no rows returned; we infer that means 0 matches.
		}
		$stmt->closeCursor();
		return $count;
	}


	/**
	 * Returns the number of rows matching criteria, joining the related ArTelephonePrefix table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinAllExceptArTelephonePrefix(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArCdrPeer::TABLE_NAME);
		
		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArCdrPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY should not affect count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArCdrPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}
	
		$criteria->addJoin(ArCdrPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArCdrPeer::AR_VENDOR_ID, ArVendorPeer::ID, $join_behavior);

		$criteria->addJoin(ArCdrPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID, ArCommunicationChannelTypePeer::ID, $join_behavior);

		$stmt = BasePeer::doCount($criteria, $con);

		if ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$count = (int) $row[0];
		} else {
			$count = 0; // no rows returned; we infer that means 0 matches.
		}
		$stmt->closeCursor();
		return $count;
	}


	/**
	 * Selects a collection of ArCdr objects pre-filled with all related objects except ArOrganizationUnit.
	 *
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArCdr objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinAllExceptArOrganizationUnit(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		// $criteria->getDbName() will return the same object if not set to another value
		// so == check is okay and faster
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArCdrPeer::addSelectColumns($criteria);
		$startcol2 = (ArCdrPeer::NUM_COLUMNS - ArCdrPeer::NUM_LAZY_LOAD_COLUMNS);

		ArVendorPeer::addSelectColumns($criteria);
		$startcol3 = $startcol2 + (ArVendorPeer::NUM_COLUMNS - ArVendorPeer::NUM_LAZY_LOAD_COLUMNS);

		ArCommunicationChannelTypePeer::addSelectColumns($criteria);
		$startcol4 = $startcol3 + (ArCommunicationChannelTypePeer::NUM_COLUMNS - ArCommunicationChannelTypePeer::NUM_LAZY_LOAD_COLUMNS);

		ArTelephonePrefixPeer::addSelectColumns($criteria);
		$startcol5 = $startcol4 + (ArTelephonePrefixPeer::NUM_COLUMNS - ArTelephonePrefixPeer::NUM_LAZY_LOAD_COLUMNS);

		$criteria->addJoin(ArCdrPeer::AR_VENDOR_ID, ArVendorPeer::ID, $join_behavior);

		$criteria->addJoin(ArCdrPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID, ArCommunicationChannelTypePeer::ID, $join_behavior);

		$criteria->addJoin(ArCdrPeer::AR_TELEPHONE_PREFIX_ID, ArTelephonePrefixPeer::ID, $join_behavior);


		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArCdrPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArCdrPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {
				$cls = ArCdrPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArCdrPeer::addInstanceToPool($obj1, $key1);
			} // if obj1 already loaded

				// Add objects for joined ArVendor rows

				$key2 = ArVendorPeer::getPrimaryKeyHashFromRow($row, $startcol2);
				if ($key2 !== null) {
					$obj2 = ArVendorPeer::getInstanceFromPool($key2);
					if (!$obj2) {
	
						$cls = ArVendorPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol2);
					ArVendorPeer::addInstanceToPool($obj2, $key2);
				} // if $obj2 already loaded

				// Add the $obj1 (ArCdr) to the collection in $obj2 (ArVendor)
				$obj2->addArCdr($obj1);

			} // if joined row is not null

				// Add objects for joined ArCommunicationChannelType rows

				$key3 = ArCommunicationChannelTypePeer::getPrimaryKeyHashFromRow($row, $startcol3);
				if ($key3 !== null) {
					$obj3 = ArCommunicationChannelTypePeer::getInstanceFromPool($key3);
					if (!$obj3) {
	
						$cls = ArCommunicationChannelTypePeer::getOMClass(false);

					$obj3 = new $cls();
					$obj3->hydrate($row, $startcol3);
					ArCommunicationChannelTypePeer::addInstanceToPool($obj3, $key3);
				} // if $obj3 already loaded

				// Add the $obj1 (ArCdr) to the collection in $obj3 (ArCommunicationChannelType)
				$obj3->addArCdr($obj1);

			} // if joined row is not null

				// Add objects for joined ArTelephonePrefix rows

				$key4 = ArTelephonePrefixPeer::getPrimaryKeyHashFromRow($row, $startcol4);
				if ($key4 !== null) {
					$obj4 = ArTelephonePrefixPeer::getInstanceFromPool($key4);
					if (!$obj4) {
	
						$cls = ArTelephonePrefixPeer::getOMClass(false);

					$obj4 = new $cls();
					$obj4->hydrate($row, $startcol4);
					ArTelephonePrefixPeer::addInstanceToPool($obj4, $key4);
				} // if $obj4 already loaded

				// Add the $obj1 (ArCdr) to the collection in $obj4 (ArTelephonePrefix)
				$obj4->addArCdr($obj1);

			} // if joined row is not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArCdr objects pre-filled with all related objects except ArVendor.
	 *
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArCdr objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinAllExceptArVendor(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		// $criteria->getDbName() will return the same object if not set to another value
		// so == check is okay and faster
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArCdrPeer::addSelectColumns($criteria);
		$startcol2 = (ArCdrPeer::NUM_COLUMNS - ArCdrPeer::NUM_LAZY_LOAD_COLUMNS);

		ArOrganizationUnitPeer::addSelectColumns($criteria);
		$startcol3 = $startcol2 + (ArOrganizationUnitPeer::NUM_COLUMNS - ArOrganizationUnitPeer::NUM_LAZY_LOAD_COLUMNS);

		ArCommunicationChannelTypePeer::addSelectColumns($criteria);
		$startcol4 = $startcol3 + (ArCommunicationChannelTypePeer::NUM_COLUMNS - ArCommunicationChannelTypePeer::NUM_LAZY_LOAD_COLUMNS);

		ArTelephonePrefixPeer::addSelectColumns($criteria);
		$startcol5 = $startcol4 + (ArTelephonePrefixPeer::NUM_COLUMNS - ArTelephonePrefixPeer::NUM_LAZY_LOAD_COLUMNS);

		$criteria->addJoin(ArCdrPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArCdrPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID, ArCommunicationChannelTypePeer::ID, $join_behavior);

		$criteria->addJoin(ArCdrPeer::AR_TELEPHONE_PREFIX_ID, ArTelephonePrefixPeer::ID, $join_behavior);


		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArCdrPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArCdrPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {
				$cls = ArCdrPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArCdrPeer::addInstanceToPool($obj1, $key1);
			} // if obj1 already loaded

				// Add objects for joined ArOrganizationUnit rows

				$key2 = ArOrganizationUnitPeer::getPrimaryKeyHashFromRow($row, $startcol2);
				if ($key2 !== null) {
					$obj2 = ArOrganizationUnitPeer::getInstanceFromPool($key2);
					if (!$obj2) {
	
						$cls = ArOrganizationUnitPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol2);
					ArOrganizationUnitPeer::addInstanceToPool($obj2, $key2);
				} // if $obj2 already loaded

				// Add the $obj1 (ArCdr) to the collection in $obj2 (ArOrganizationUnit)
				$obj2->addArCdr($obj1);

			} // if joined row is not null

				// Add objects for joined ArCommunicationChannelType rows

				$key3 = ArCommunicationChannelTypePeer::getPrimaryKeyHashFromRow($row, $startcol3);
				if ($key3 !== null) {
					$obj3 = ArCommunicationChannelTypePeer::getInstanceFromPool($key3);
					if (!$obj3) {
	
						$cls = ArCommunicationChannelTypePeer::getOMClass(false);

					$obj3 = new $cls();
					$obj3->hydrate($row, $startcol3);
					ArCommunicationChannelTypePeer::addInstanceToPool($obj3, $key3);
				} // if $obj3 already loaded

				// Add the $obj1 (ArCdr) to the collection in $obj3 (ArCommunicationChannelType)
				$obj3->addArCdr($obj1);

			} // if joined row is not null

				// Add objects for joined ArTelephonePrefix rows

				$key4 = ArTelephonePrefixPeer::getPrimaryKeyHashFromRow($row, $startcol4);
				if ($key4 !== null) {
					$obj4 = ArTelephonePrefixPeer::getInstanceFromPool($key4);
					if (!$obj4) {
	
						$cls = ArTelephonePrefixPeer::getOMClass(false);

					$obj4 = new $cls();
					$obj4->hydrate($row, $startcol4);
					ArTelephonePrefixPeer::addInstanceToPool($obj4, $key4);
				} // if $obj4 already loaded

				// Add the $obj1 (ArCdr) to the collection in $obj4 (ArTelephonePrefix)
				$obj4->addArCdr($obj1);

			} // if joined row is not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArCdr objects pre-filled with all related objects except ArCommunicationChannelType.
	 *
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArCdr objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinAllExceptArCommunicationChannelType(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		// $criteria->getDbName() will return the same object if not set to another value
		// so == check is okay and faster
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArCdrPeer::addSelectColumns($criteria);
		$startcol2 = (ArCdrPeer::NUM_COLUMNS - ArCdrPeer::NUM_LAZY_LOAD_COLUMNS);

		ArOrganizationUnitPeer::addSelectColumns($criteria);
		$startcol3 = $startcol2 + (ArOrganizationUnitPeer::NUM_COLUMNS - ArOrganizationUnitPeer::NUM_LAZY_LOAD_COLUMNS);

		ArVendorPeer::addSelectColumns($criteria);
		$startcol4 = $startcol3 + (ArVendorPeer::NUM_COLUMNS - ArVendorPeer::NUM_LAZY_LOAD_COLUMNS);

		ArTelephonePrefixPeer::addSelectColumns($criteria);
		$startcol5 = $startcol4 + (ArTelephonePrefixPeer::NUM_COLUMNS - ArTelephonePrefixPeer::NUM_LAZY_LOAD_COLUMNS);

		$criteria->addJoin(ArCdrPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArCdrPeer::AR_VENDOR_ID, ArVendorPeer::ID, $join_behavior);

		$criteria->addJoin(ArCdrPeer::AR_TELEPHONE_PREFIX_ID, ArTelephonePrefixPeer::ID, $join_behavior);


		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArCdrPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArCdrPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {
				$cls = ArCdrPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArCdrPeer::addInstanceToPool($obj1, $key1);
			} // if obj1 already loaded

				// Add objects for joined ArOrganizationUnit rows

				$key2 = ArOrganizationUnitPeer::getPrimaryKeyHashFromRow($row, $startcol2);
				if ($key2 !== null) {
					$obj2 = ArOrganizationUnitPeer::getInstanceFromPool($key2);
					if (!$obj2) {
	
						$cls = ArOrganizationUnitPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol2);
					ArOrganizationUnitPeer::addInstanceToPool($obj2, $key2);
				} // if $obj2 already loaded

				// Add the $obj1 (ArCdr) to the collection in $obj2 (ArOrganizationUnit)
				$obj2->addArCdr($obj1);

			} // if joined row is not null

				// Add objects for joined ArVendor rows

				$key3 = ArVendorPeer::getPrimaryKeyHashFromRow($row, $startcol3);
				if ($key3 !== null) {
					$obj3 = ArVendorPeer::getInstanceFromPool($key3);
					if (!$obj3) {
	
						$cls = ArVendorPeer::getOMClass(false);

					$obj3 = new $cls();
					$obj3->hydrate($row, $startcol3);
					ArVendorPeer::addInstanceToPool($obj3, $key3);
				} // if $obj3 already loaded

				// Add the $obj1 (ArCdr) to the collection in $obj3 (ArVendor)
				$obj3->addArCdr($obj1);

			} // if joined row is not null

				// Add objects for joined ArTelephonePrefix rows

				$key4 = ArTelephonePrefixPeer::getPrimaryKeyHashFromRow($row, $startcol4);
				if ($key4 !== null) {
					$obj4 = ArTelephonePrefixPeer::getInstanceFromPool($key4);
					if (!$obj4) {
	
						$cls = ArTelephonePrefixPeer::getOMClass(false);

					$obj4 = new $cls();
					$obj4->hydrate($row, $startcol4);
					ArTelephonePrefixPeer::addInstanceToPool($obj4, $key4);
				} // if $obj4 already loaded

				// Add the $obj1 (ArCdr) to the collection in $obj4 (ArTelephonePrefix)
				$obj4->addArCdr($obj1);

			} // if joined row is not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArCdr objects pre-filled with all related objects except ArTelephonePrefix.
	 *
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArCdr objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinAllExceptArTelephonePrefix(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		// $criteria->getDbName() will return the same object if not set to another value
		// so == check is okay and faster
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArCdrPeer::addSelectColumns($criteria);
		$startcol2 = (ArCdrPeer::NUM_COLUMNS - ArCdrPeer::NUM_LAZY_LOAD_COLUMNS);

		ArOrganizationUnitPeer::addSelectColumns($criteria);
		$startcol3 = $startcol2 + (ArOrganizationUnitPeer::NUM_COLUMNS - ArOrganizationUnitPeer::NUM_LAZY_LOAD_COLUMNS);

		ArVendorPeer::addSelectColumns($criteria);
		$startcol4 = $startcol3 + (ArVendorPeer::NUM_COLUMNS - ArVendorPeer::NUM_LAZY_LOAD_COLUMNS);

		ArCommunicationChannelTypePeer::addSelectColumns($criteria);
		$startcol5 = $startcol4 + (ArCommunicationChannelTypePeer::NUM_COLUMNS - ArCommunicationChannelTypePeer::NUM_LAZY_LOAD_COLUMNS);

		$criteria->addJoin(ArCdrPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArCdrPeer::AR_VENDOR_ID, ArVendorPeer::ID, $join_behavior);

		$criteria->addJoin(ArCdrPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID, ArCommunicationChannelTypePeer::ID, $join_behavior);


		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArCdrPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArCdrPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {
				$cls = ArCdrPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArCdrPeer::addInstanceToPool($obj1, $key1);
			} // if obj1 already loaded

				// Add objects for joined ArOrganizationUnit rows

				$key2 = ArOrganizationUnitPeer::getPrimaryKeyHashFromRow($row, $startcol2);
				if ($key2 !== null) {
					$obj2 = ArOrganizationUnitPeer::getInstanceFromPool($key2);
					if (!$obj2) {
	
						$cls = ArOrganizationUnitPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol2);
					ArOrganizationUnitPeer::addInstanceToPool($obj2, $key2);
				} // if $obj2 already loaded

				// Add the $obj1 (ArCdr) to the collection in $obj2 (ArOrganizationUnit)
				$obj2->addArCdr($obj1);

			} // if joined row is not null

				// Add objects for joined ArVendor rows

				$key3 = ArVendorPeer::getPrimaryKeyHashFromRow($row, $startcol3);
				if ($key3 !== null) {
					$obj3 = ArVendorPeer::getInstanceFromPool($key3);
					if (!$obj3) {
	
						$cls = ArVendorPeer::getOMClass(false);

					$obj3 = new $cls();
					$obj3->hydrate($row, $startcol3);
					ArVendorPeer::addInstanceToPool($obj3, $key3);
				} // if $obj3 already loaded

				// Add the $obj1 (ArCdr) to the collection in $obj3 (ArVendor)
				$obj3->addArCdr($obj1);

			} // if joined row is not null

				// Add objects for joined ArCommunicationChannelType rows

				$key4 = ArCommunicationChannelTypePeer::getPrimaryKeyHashFromRow($row, $startcol4);
				if ($key4 !== null) {
					$obj4 = ArCommunicationChannelTypePeer::getInstanceFromPool($key4);
					if (!$obj4) {
	
						$cls = ArCommunicationChannelTypePeer::getOMClass(false);

					$obj4 = new $cls();
					$obj4->hydrate($row, $startcol4);
					ArCommunicationChannelTypePeer::addInstanceToPool($obj4, $key4);
				} // if $obj4 already loaded

				// Add the $obj1 (ArCdr) to the collection in $obj4 (ArCommunicationChannelType)
				$obj4->addArCdr($obj1);

			} // if joined row is not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}

	/**
	 * Returns the TableMap related to this peer.
	 * This method is not needed for general use but a specific application could have a need.
	 * @return     TableMap
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function getTableMap()
	{
		return Propel::getDatabaseMap(self::DATABASE_NAME)->getTable(self::TABLE_NAME);
	}

	/**
	 * Add a TableMap instance to the database for this peer class.
	 */
	public static function buildTableMap()
	{
	  $dbMap = Propel::getDatabaseMap(BaseArCdrPeer::DATABASE_NAME);
	  if (!$dbMap->hasTable(BaseArCdrPeer::TABLE_NAME))
	  {
	    $dbMap->addTableObject(new ArCdrTableMap());
	  }
	}

	/**
	 * The class that the Peer will make instances of.
	 *
	 * If $withPrefix is true, the returned path
	 * uses a dot-path notation which is tranalted into a path
	 * relative to a location on the PHP include_path.
	 * (e.g. path.to.MyClass -> 'path/to/MyClass.php')
	 *
	 * @param      boolean  Whether or not to return the path wit hthe class name 
	 * @return     string path.to.ClassName
	 */
	public static function getOMClass($withPrefix = true)
	{
		return $withPrefix ? ArCdrPeer::CLASS_DEFAULT : ArCdrPeer::OM_CLASS;
	}

	/**
	 * Method perform an INSERT on the database, given a ArCdr or Criteria object.
	 *
	 * @param      mixed $values Criteria or ArCdr object containing data that is used to create the INSERT statement.
	 * @param      PropelPDO $con the PropelPDO connection to use
	 * @return     mixed The new primary key.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doInsert($values, PropelPDO $con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArCdrPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		if ($values instanceof Criteria) {
			$criteria = clone $values; // rename for clarity
		} else {
			$criteria = $values->buildCriteria(); // build Criteria from ArCdr object
		}

		if ($criteria->containsKey(ArCdrPeer::ID) && $criteria->keyContainsValue(ArCdrPeer::ID) ) {
			throw new PropelException('Cannot insert a value for auto-increment primary key ('.ArCdrPeer::ID.')');
		}


		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		try {
			// use transaction because $criteria could contain info
			// for more than one table (I guess, conceivably)
			$con->beginTransaction();
			$pk = BasePeer::doInsert($criteria, $con);
			$con->commit();
		} catch(PropelException $e) {
			$con->rollBack();
			throw $e;
		}

		return $pk;
	}

	/**
	 * Method perform an UPDATE on the database, given a ArCdr or Criteria object.
	 *
	 * @param      mixed $values Criteria or ArCdr object containing data that is used to create the UPDATE statement.
	 * @param      PropelPDO $con The connection to use (specify PropelPDO connection object to exert more control over transactions).
	 * @return     int The number of affected rows (if supported by underlying database driver).
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doUpdate($values, PropelPDO $con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArCdrPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		$selectCriteria = new Criteria(self::DATABASE_NAME);

		if ($values instanceof Criteria) {
			$criteria = clone $values; // rename for clarity

			$comparison = $criteria->getComparison(ArCdrPeer::ID);
			$selectCriteria->add(ArCdrPeer::ID, $criteria->remove(ArCdrPeer::ID), $comparison);

		} else { // $values is ArCdr object
			$criteria = $values->buildCriteria(); // gets full criteria
			$selectCriteria = $values->buildPkeyCriteria(); // gets criteria w/ primary key(s)
		}

		// set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		return BasePeer::doUpdate($selectCriteria, $criteria, $con);
	}

	/**
	 * Method to DELETE all rows from the ar_cdr table.
	 *
	 * @return     int The number of affected rows (if supported by underlying database driver).
	 */
	public static function doDeleteAll($con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArCdrPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}
		$affectedRows = 0; // initialize var to track total num of affected rows
		try {
			// use transaction because $criteria could contain info
			// for more than one table or we could emulating ON DELETE CASCADE, etc.
			$con->beginTransaction();
			$affectedRows += BasePeer::doDeleteAll(ArCdrPeer::TABLE_NAME, $con);
			// Because this db requires some delete cascade/set null emulation, we have to
			// clear the cached instance *after* the emulation has happened (since
			// instances get re-added by the select statement contained therein).
			ArCdrPeer::clearInstancePool();
			ArCdrPeer::clearRelatedInstancePool();
			$con->commit();
			return $affectedRows;
		} catch (PropelException $e) {
			$con->rollBack();
			throw $e;
		}
	}

	/**
	 * Method perform a DELETE on the database, given a ArCdr or Criteria object OR a primary key value.
	 *
	 * @param      mixed $values Criteria or ArCdr object or primary key or array of primary keys
	 *              which is used to create the DELETE statement
	 * @param      PropelPDO $con the connection to use
	 * @return     int 	The number of affected rows (if supported by underlying database driver).  This includes CASCADE-related rows
	 *				if supported by native driver or if emulated using Propel.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	 public static function doDelete($values, PropelPDO $con = null)
	 {
		if ($con === null) {
			$con = Propel::getConnection(ArCdrPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		if ($values instanceof Criteria) {
			// invalidate the cache for all objects of this type, since we have no
			// way of knowing (without running a query) what objects should be invalidated
			// from the cache based on this Criteria.
			ArCdrPeer::clearInstancePool();
			// rename for clarity
			$criteria = clone $values;
		} elseif ($values instanceof ArCdr) { // it's a model object
			// invalidate the cache for this single object
			ArCdrPeer::removeInstanceFromPool($values);
			// create criteria based on pk values
			$criteria = $values->buildPkeyCriteria();
		} else { // it's a primary key, or an array of pks
			$criteria = new Criteria(self::DATABASE_NAME);
			$criteria->add(ArCdrPeer::ID, (array) $values, Criteria::IN);
			// invalidate the cache for this object(s)
			foreach ((array) $values as $singleval) {
				ArCdrPeer::removeInstanceFromPool($singleval);
			}
		}

		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		$affectedRows = 0; // initialize var to track total num of affected rows

		try {
			// use transaction because $criteria could contain info
			// for more than one table or we could emulating ON DELETE CASCADE, etc.
			$con->beginTransaction();
			
			$affectedRows += BasePeer::doDelete($criteria, $con);
			ArCdrPeer::clearRelatedInstancePool();
			$con->commit();
			return $affectedRows;
		} catch (PropelException $e) {
			$con->rollBack();
			throw $e;
		}
	}

	/**
	 * Validates all modified columns of given ArCdr object.
	 * If parameter $columns is either a single column name or an array of column names
	 * than only those columns are validated.
	 *
	 * NOTICE: This does not apply to primary or foreign keys for now.
	 *
	 * @param      ArCdr $obj The object to validate.
	 * @param      mixed $cols Column name or array of column names.
	 *
	 * @return     mixed TRUE if all columns are valid or the error message of the first invalid column.
	 */
	public static function doValidate(ArCdr $obj, $cols = null)
	{
		$columns = array();

		if ($cols) {
			$dbMap = Propel::getDatabaseMap(ArCdrPeer::DATABASE_NAME);
			$tableMap = $dbMap->getTable(ArCdrPeer::TABLE_NAME);

			if (! is_array($cols)) {
				$cols = array($cols);
			}

			foreach ($cols as $colName) {
				if ($tableMap->containsColumn($colName)) {
					$get = 'get' . $tableMap->getColumn($colName)->getPhpName();
					$columns[$colName] = $obj->$get();
				}
			}
		} else {

		}

		return BasePeer::doValidate(ArCdrPeer::DATABASE_NAME, ArCdrPeer::TABLE_NAME, $columns);
	}

	/**
	 * Retrieve a single object by pkey.
	 *
	 * @param      int $pk the primary key.
	 * @param      PropelPDO $con the connection to use
	 * @return     ArCdr
	 */
	public static function retrieveByPK($pk, PropelPDO $con = null)
	{

		if (null !== ($obj = ArCdrPeer::getInstanceFromPool((string) $pk))) {
			return $obj;
		}

		if ($con === null) {
			$con = Propel::getConnection(ArCdrPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria = new Criteria(ArCdrPeer::DATABASE_NAME);
		$criteria->add(ArCdrPeer::ID, $pk);

		$v = ArCdrPeer::doSelect($criteria, $con);

		return !empty($v) > 0 ? $v[0] : null;
	}

	/**
	 * Retrieve multiple objects by pkey.
	 *
	 * @param      array $pks List of primary keys
	 * @param      PropelPDO $con the connection to use
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function retrieveByPKs($pks, PropelPDO $con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArCdrPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$objs = null;
		if (empty($pks)) {
			$objs = array();
		} else {
			$criteria = new Criteria(ArCdrPeer::DATABASE_NAME);
			$criteria->add(ArCdrPeer::ID, $pks, Criteria::IN);
			$objs = ArCdrPeer::doSelect($criteria, $con);
		}
		return $objs;
	}

	// symfony behavior
	
	/**
	 * Returns an array of arrays that contain columns in each unique index.
	 *
	 * @return array
	 */
	static public function getUniqueColumnNames()
	{
	  return array();
	}

} // BaseArCdrPeer

// This is the static code needed to register the TableMap for this table with the main Propel class.
//
BaseArCdrPeer::buildTableMap();

