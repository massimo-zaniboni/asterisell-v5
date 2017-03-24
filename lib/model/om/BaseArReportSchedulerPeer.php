<?php

/**
 * Base static class for performing query and update operations on the 'ar_report_scheduler' table.
 *
 * 
 *
 * @package    lib.model.om
 */
abstract class BaseArReportSchedulerPeer {

	/** the default database name for this class */
	const DATABASE_NAME = 'propel';

	/** the table name for this class */
	const TABLE_NAME = 'ar_report_scheduler';

	/** the related Propel class for this table */
	const OM_CLASS = 'ArReportScheduler';

	/** A class that can be returned by this peer. */
	const CLASS_DEFAULT = 'lib.model.ArReportScheduler';

	/** the related TableMap class for this table */
	const TM_CLASS = 'ArReportSchedulerTableMap';
	
	/** The total number of columns. */
	const NUM_COLUMNS = 22;

	/** The number of lazy-loaded columns. */
	const NUM_LAZY_LOAD_COLUMNS = 0;

	/** the column name for the ID field */
	const ID = 'ar_report_scheduler.ID';

	/** the column name for the IS_ACTIVE field */
	const IS_ACTIVE = 'ar_report_scheduler.IS_ACTIVE';

	/** the column name for the LAST_EXECUTION_DATE field */
	const LAST_EXECUTION_DATE = 'ar_report_scheduler.LAST_EXECUTION_DATE';

	/** the column name for the LAST_FROM_DATE field */
	const LAST_FROM_DATE = 'ar_report_scheduler.LAST_FROM_DATE';

	/** the column name for the LAST_TO_DATE field */
	const LAST_TO_DATE = 'ar_report_scheduler.LAST_TO_DATE';

	/** the column name for the AR_REPORT_ID field */
	const AR_REPORT_ID = 'ar_report_scheduler.AR_REPORT_ID';

	/** the column name for the AR_ORGANIZATION_UNIT_ID field */
	const AR_ORGANIZATION_UNIT_ID = 'ar_report_scheduler.AR_ORGANIZATION_UNIT_ID';

	/** the column name for the SHORT_DESCRIPTION field */
	const SHORT_DESCRIPTION = 'ar_report_scheduler.SHORT_DESCRIPTION';

	/** the column name for the ADDITIONAL_DESCRIPTION field */
	const ADDITIONAL_DESCRIPTION = 'ar_report_scheduler.ADDITIONAL_DESCRIPTION';

	/** the column name for the NOTE field */
	const NOTE = 'ar_report_scheduler.NOTE';

	/** the column name for the PRODUCED_REPORT_MUST_BE_REVIEWED field */
	const PRODUCED_REPORT_MUST_BE_REVIEWED = 'ar_report_scheduler.PRODUCED_REPORT_MUST_BE_REVIEWED';

	/** the column name for the AR_REPORT_GENERATION_ID field */
	const AR_REPORT_GENERATION_ID = 'ar_report_scheduler.AR_REPORT_GENERATION_ID';

	/** the column name for the SCHEDULE_EVERY_X_DAYS field */
	const SCHEDULE_EVERY_X_DAYS = 'ar_report_scheduler.SCHEDULE_EVERY_X_DAYS';

	/** the column name for the SCHEDULE_EVERY_X_MONTHS field */
	const SCHEDULE_EVERY_X_MONTHS = 'ar_report_scheduler.SCHEDULE_EVERY_X_MONTHS';

	/** the column name for the START_GENERATION_AFTER_X_HOURS field */
	const START_GENERATION_AFTER_X_HOURS = 'ar_report_scheduler.START_GENERATION_AFTER_X_HOURS';

	/** the column name for the INTERNAL_NAME field */
	const INTERNAL_NAME = 'ar_report_scheduler.INTERNAL_NAME';

	/** the column name for the AR_LEGAL_DATE_GENERATION_METHOD_ID field */
	const AR_LEGAL_DATE_GENERATION_METHOD_ID = 'ar_report_scheduler.AR_LEGAL_DATE_GENERATION_METHOD_ID';

	/** the column name for the DAYS_TO_ADD_TO_LEGAL_DATE_GENERATION_METHOD field */
	const DAYS_TO_ADD_TO_LEGAL_DATE_GENERATION_METHOD = 'ar_report_scheduler.DAYS_TO_ADD_TO_LEGAL_DATE_GENERATION_METHOD';

	/** the column name for the IS_YEARLY_LEGAL_NUMERATION field */
	const IS_YEARLY_LEGAL_NUMERATION = 'ar_report_scheduler.IS_YEARLY_LEGAL_NUMERATION';

	/** the column name for the GENERATE_ONLY_IF_THERE_IS_COST field */
	const GENERATE_ONLY_IF_THERE_IS_COST = 'ar_report_scheduler.GENERATE_ONLY_IF_THERE_IS_COST';

	/** the column name for the MINIMUM_COST field */
	const MINIMUM_COST = 'ar_report_scheduler.MINIMUM_COST';

	/** the column name for the SEND_COMPACT_REPORT_LIST_TO_ACCOUNTANT field */
	const SEND_COMPACT_REPORT_LIST_TO_ACCOUNTANT = 'ar_report_scheduler.SEND_COMPACT_REPORT_LIST_TO_ACCOUNTANT';

	/**
	 * An identiy map to hold any loaded instances of ArReportScheduler objects.
	 * This must be public so that other peer classes can access this when hydrating from JOIN
	 * queries.
	 * @var        array ArReportScheduler[]
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
		BasePeer::TYPE_PHPNAME => array ('Id', 'IsActive', 'LastExecutionDate', 'LastFromDate', 'LastToDate', 'ArReportId', 'ArOrganizationUnitId', 'ShortDescription', 'AdditionalDescription', 'Note', 'ProducedReportMustBeReviewed', 'ArReportGenerationId', 'ScheduleEveryXDays', 'ScheduleEveryXMonths', 'StartGenerationAfterXHours', 'InternalName', 'ArLegalDateGenerationMethodId', 'DaysToAddToLegalDateGenerationMethod', 'IsYearlyLegalNumeration', 'GenerateOnlyIfThereIsCost', 'MinimumCost', 'SendCompactReportListToAccountant', ),
		BasePeer::TYPE_STUDLYPHPNAME => array ('id', 'isActive', 'lastExecutionDate', 'lastFromDate', 'lastToDate', 'arReportId', 'arOrganizationUnitId', 'shortDescription', 'additionalDescription', 'note', 'producedReportMustBeReviewed', 'arReportGenerationId', 'scheduleEveryXDays', 'scheduleEveryXMonths', 'startGenerationAfterXHours', 'internalName', 'arLegalDateGenerationMethodId', 'daysToAddToLegalDateGenerationMethod', 'isYearlyLegalNumeration', 'generateOnlyIfThereIsCost', 'minimumCost', 'sendCompactReportListToAccountant', ),
		BasePeer::TYPE_COLNAME => array (self::ID, self::IS_ACTIVE, self::LAST_EXECUTION_DATE, self::LAST_FROM_DATE, self::LAST_TO_DATE, self::AR_REPORT_ID, self::AR_ORGANIZATION_UNIT_ID, self::SHORT_DESCRIPTION, self::ADDITIONAL_DESCRIPTION, self::NOTE, self::PRODUCED_REPORT_MUST_BE_REVIEWED, self::AR_REPORT_GENERATION_ID, self::SCHEDULE_EVERY_X_DAYS, self::SCHEDULE_EVERY_X_MONTHS, self::START_GENERATION_AFTER_X_HOURS, self::INTERNAL_NAME, self::AR_LEGAL_DATE_GENERATION_METHOD_ID, self::DAYS_TO_ADD_TO_LEGAL_DATE_GENERATION_METHOD, self::IS_YEARLY_LEGAL_NUMERATION, self::GENERATE_ONLY_IF_THERE_IS_COST, self::MINIMUM_COST, self::SEND_COMPACT_REPORT_LIST_TO_ACCOUNTANT, ),
		BasePeer::TYPE_FIELDNAME => array ('id', 'is_active', 'last_execution_date', 'last_from_date', 'last_to_date', 'ar_report_id', 'ar_organization_unit_id', 'short_description', 'additional_description', 'note', 'produced_report_must_be_reviewed', 'ar_report_generation_id', 'schedule_every_x_days', 'schedule_every_x_months', 'start_generation_after_x_hours', 'internal_name', 'ar_legal_date_generation_method_id', 'days_to_add_to_legal_date_generation_method', 'is_yearly_legal_numeration', 'generate_only_if_there_is_cost', 'minimum_cost', 'send_compact_report_list_to_accountant', ),
		BasePeer::TYPE_NUM => array (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, )
	);

	/**
	 * holds an array of keys for quick access to the fieldnames array
	 *
	 * first dimension keys are the type constants
	 * e.g. self::$fieldNames[BasePeer::TYPE_PHPNAME]['Id'] = 0
	 */
	private static $fieldKeys = array (
		BasePeer::TYPE_PHPNAME => array ('Id' => 0, 'IsActive' => 1, 'LastExecutionDate' => 2, 'LastFromDate' => 3, 'LastToDate' => 4, 'ArReportId' => 5, 'ArOrganizationUnitId' => 6, 'ShortDescription' => 7, 'AdditionalDescription' => 8, 'Note' => 9, 'ProducedReportMustBeReviewed' => 10, 'ArReportGenerationId' => 11, 'ScheduleEveryXDays' => 12, 'ScheduleEveryXMonths' => 13, 'StartGenerationAfterXHours' => 14, 'InternalName' => 15, 'ArLegalDateGenerationMethodId' => 16, 'DaysToAddToLegalDateGenerationMethod' => 17, 'IsYearlyLegalNumeration' => 18, 'GenerateOnlyIfThereIsCost' => 19, 'MinimumCost' => 20, 'SendCompactReportListToAccountant' => 21, ),
		BasePeer::TYPE_STUDLYPHPNAME => array ('id' => 0, 'isActive' => 1, 'lastExecutionDate' => 2, 'lastFromDate' => 3, 'lastToDate' => 4, 'arReportId' => 5, 'arOrganizationUnitId' => 6, 'shortDescription' => 7, 'additionalDescription' => 8, 'note' => 9, 'producedReportMustBeReviewed' => 10, 'arReportGenerationId' => 11, 'scheduleEveryXDays' => 12, 'scheduleEveryXMonths' => 13, 'startGenerationAfterXHours' => 14, 'internalName' => 15, 'arLegalDateGenerationMethodId' => 16, 'daysToAddToLegalDateGenerationMethod' => 17, 'isYearlyLegalNumeration' => 18, 'generateOnlyIfThereIsCost' => 19, 'minimumCost' => 20, 'sendCompactReportListToAccountant' => 21, ),
		BasePeer::TYPE_COLNAME => array (self::ID => 0, self::IS_ACTIVE => 1, self::LAST_EXECUTION_DATE => 2, self::LAST_FROM_DATE => 3, self::LAST_TO_DATE => 4, self::AR_REPORT_ID => 5, self::AR_ORGANIZATION_UNIT_ID => 6, self::SHORT_DESCRIPTION => 7, self::ADDITIONAL_DESCRIPTION => 8, self::NOTE => 9, self::PRODUCED_REPORT_MUST_BE_REVIEWED => 10, self::AR_REPORT_GENERATION_ID => 11, self::SCHEDULE_EVERY_X_DAYS => 12, self::SCHEDULE_EVERY_X_MONTHS => 13, self::START_GENERATION_AFTER_X_HOURS => 14, self::INTERNAL_NAME => 15, self::AR_LEGAL_DATE_GENERATION_METHOD_ID => 16, self::DAYS_TO_ADD_TO_LEGAL_DATE_GENERATION_METHOD => 17, self::IS_YEARLY_LEGAL_NUMERATION => 18, self::GENERATE_ONLY_IF_THERE_IS_COST => 19, self::MINIMUM_COST => 20, self::SEND_COMPACT_REPORT_LIST_TO_ACCOUNTANT => 21, ),
		BasePeer::TYPE_FIELDNAME => array ('id' => 0, 'is_active' => 1, 'last_execution_date' => 2, 'last_from_date' => 3, 'last_to_date' => 4, 'ar_report_id' => 5, 'ar_organization_unit_id' => 6, 'short_description' => 7, 'additional_description' => 8, 'note' => 9, 'produced_report_must_be_reviewed' => 10, 'ar_report_generation_id' => 11, 'schedule_every_x_days' => 12, 'schedule_every_x_months' => 13, 'start_generation_after_x_hours' => 14, 'internal_name' => 15, 'ar_legal_date_generation_method_id' => 16, 'days_to_add_to_legal_date_generation_method' => 17, 'is_yearly_legal_numeration' => 18, 'generate_only_if_there_is_cost' => 19, 'minimum_cost' => 20, 'send_compact_report_list_to_accountant' => 21, ),
		BasePeer::TYPE_NUM => array (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, )
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
	 * @param      string $column The column name for current table. (i.e. ArReportSchedulerPeer::COLUMN_NAME).
	 * @return     string
	 */
	public static function alias($alias, $column)
	{
		return str_replace(ArReportSchedulerPeer::TABLE_NAME.'.', $alias.'.', $column);
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
		$criteria->addSelectColumn(ArReportSchedulerPeer::ID);
		$criteria->addSelectColumn(ArReportSchedulerPeer::IS_ACTIVE);
		$criteria->addSelectColumn(ArReportSchedulerPeer::LAST_EXECUTION_DATE);
		$criteria->addSelectColumn(ArReportSchedulerPeer::LAST_FROM_DATE);
		$criteria->addSelectColumn(ArReportSchedulerPeer::LAST_TO_DATE);
		$criteria->addSelectColumn(ArReportSchedulerPeer::AR_REPORT_ID);
		$criteria->addSelectColumn(ArReportSchedulerPeer::AR_ORGANIZATION_UNIT_ID);
		$criteria->addSelectColumn(ArReportSchedulerPeer::SHORT_DESCRIPTION);
		$criteria->addSelectColumn(ArReportSchedulerPeer::ADDITIONAL_DESCRIPTION);
		$criteria->addSelectColumn(ArReportSchedulerPeer::NOTE);
		$criteria->addSelectColumn(ArReportSchedulerPeer::PRODUCED_REPORT_MUST_BE_REVIEWED);
		$criteria->addSelectColumn(ArReportSchedulerPeer::AR_REPORT_GENERATION_ID);
		$criteria->addSelectColumn(ArReportSchedulerPeer::SCHEDULE_EVERY_X_DAYS);
		$criteria->addSelectColumn(ArReportSchedulerPeer::SCHEDULE_EVERY_X_MONTHS);
		$criteria->addSelectColumn(ArReportSchedulerPeer::START_GENERATION_AFTER_X_HOURS);
		$criteria->addSelectColumn(ArReportSchedulerPeer::INTERNAL_NAME);
		$criteria->addSelectColumn(ArReportSchedulerPeer::AR_LEGAL_DATE_GENERATION_METHOD_ID);
		$criteria->addSelectColumn(ArReportSchedulerPeer::DAYS_TO_ADD_TO_LEGAL_DATE_GENERATION_METHOD);
		$criteria->addSelectColumn(ArReportSchedulerPeer::IS_YEARLY_LEGAL_NUMERATION);
		$criteria->addSelectColumn(ArReportSchedulerPeer::GENERATE_ONLY_IF_THERE_IS_COST);
		$criteria->addSelectColumn(ArReportSchedulerPeer::MINIMUM_COST);
		$criteria->addSelectColumn(ArReportSchedulerPeer::SEND_COMPACT_REPORT_LIST_TO_ACCOUNTANT);
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
		$criteria->setPrimaryTableName(ArReportSchedulerPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArReportSchedulerPeer::addSelectColumns($criteria);
		}

		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		$criteria->setDbName(self::DATABASE_NAME); // Set the correct dbName

		if ($con === null) {
			$con = Propel::getConnection(ArReportSchedulerPeer::DATABASE_NAME, Propel::CONNECTION_READ);
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
	 * @return     ArReportScheduler
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectOne(Criteria $criteria, PropelPDO $con = null)
	{
		$critcopy = clone $criteria;
		$critcopy->setLimit(1);
		$objects = ArReportSchedulerPeer::doSelect($critcopy, $con);
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
		return ArReportSchedulerPeer::populateObjects(ArReportSchedulerPeer::doSelectStmt($criteria, $con));
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
			$con = Propel::getConnection(ArReportSchedulerPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		if (!$criteria->hasSelectClause()) {
			$criteria = clone $criteria;
			ArReportSchedulerPeer::addSelectColumns($criteria);
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
	 * @param      ArReportScheduler $value A ArReportScheduler object.
	 * @param      string $key (optional) key to use for instance map (for performance boost if key was already calculated externally).
	 */
	public static function addInstanceToPool(ArReportScheduler $obj, $key = null)
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
	 * @param      mixed $value A ArReportScheduler object or a primary key value.
	 */
	public static function removeInstanceFromPool($value)
	{
		if (Propel::isInstancePoolingEnabled() && $value !== null) {
			if (is_object($value) && $value instanceof ArReportScheduler) {
				$key = (string) $value->getId();
			} elseif (is_scalar($value)) {
				// assume we've been passed a primary key
				$key = (string) $value;
			} else {
				$e = new PropelException("Invalid value passed to removeInstanceFromPool().  Expected primary key or ArReportScheduler object; got " . (is_object($value) ? get_class($value) . ' object.' : var_export($value,true)));
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
	 * @return     ArReportScheduler Found object or NULL if 1) no instance exists for specified key or 2) instance pooling has been disabled.
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
	 * Method to invalidate the instance pool of all tables related to ar_report_scheduler
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
		$cls = ArReportSchedulerPeer::getOMClass(false);
		// populate the object(s)
		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key = ArReportSchedulerPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj = ArReportSchedulerPeer::getInstanceFromPool($key))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj->hydrate($row, 0, true); // rehydrate
				$results[] = $obj;
			} else {
				$obj = new $cls();
				$obj->hydrate($row);
				$results[] = $obj;
				ArReportSchedulerPeer::addInstanceToPool($obj, $key);
			} // if key exists
		}
		$stmt->closeCursor();
		return $results;
	}

	/**
	 * Returns the number of rows matching criteria, joining the related ArReport table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinArReport(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArReportSchedulerPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArReportSchedulerPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArReportSchedulerPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArReportSchedulerPeer::AR_REPORT_ID, ArReportPeer::ID, $join_behavior);

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
		$criteria->setPrimaryTableName(ArReportSchedulerPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArReportSchedulerPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArReportSchedulerPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArReportSchedulerPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

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
	 * Returns the number of rows matching criteria, joining the related ArReportGeneration table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinArReportGeneration(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArReportSchedulerPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArReportSchedulerPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArReportSchedulerPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArReportSchedulerPeer::AR_REPORT_GENERATION_ID, ArReportGenerationPeer::ID, $join_behavior);

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
	 * Selects a collection of ArReportScheduler objects pre-filled with their ArReport objects.
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArReportScheduler objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinArReport(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArReportSchedulerPeer::addSelectColumns($criteria);
		$startcol = (ArReportSchedulerPeer::NUM_COLUMNS - ArReportSchedulerPeer::NUM_LAZY_LOAD_COLUMNS);
		ArReportPeer::addSelectColumns($criteria);

		$criteria->addJoin(ArReportSchedulerPeer::AR_REPORT_ID, ArReportPeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArReportSchedulerPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArReportSchedulerPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {

				$cls = ArReportSchedulerPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArReportSchedulerPeer::addInstanceToPool($obj1, $key1);
			} // if $obj1 already loaded

			$key2 = ArReportPeer::getPrimaryKeyHashFromRow($row, $startcol);
			if ($key2 !== null) {
				$obj2 = ArReportPeer::getInstanceFromPool($key2);
				if (!$obj2) {

					$cls = ArReportPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol);
					ArReportPeer::addInstanceToPool($obj2, $key2);
				} // if obj2 already loaded
				
				// Add the $obj1 (ArReportScheduler) to $obj2 (ArReport)
				$obj2->addArReportScheduler($obj1);

			} // if joined row was not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArReportScheduler objects pre-filled with their ArOrganizationUnit objects.
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArReportScheduler objects.
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

		ArReportSchedulerPeer::addSelectColumns($criteria);
		$startcol = (ArReportSchedulerPeer::NUM_COLUMNS - ArReportSchedulerPeer::NUM_LAZY_LOAD_COLUMNS);
		ArOrganizationUnitPeer::addSelectColumns($criteria);

		$criteria->addJoin(ArReportSchedulerPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArReportSchedulerPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArReportSchedulerPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {

				$cls = ArReportSchedulerPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArReportSchedulerPeer::addInstanceToPool($obj1, $key1);
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
				
				// Add the $obj1 (ArReportScheduler) to $obj2 (ArOrganizationUnit)
				$obj2->addArReportScheduler($obj1);

			} // if joined row was not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArReportScheduler objects pre-filled with their ArReportGeneration objects.
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArReportScheduler objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinArReportGeneration(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArReportSchedulerPeer::addSelectColumns($criteria);
		$startcol = (ArReportSchedulerPeer::NUM_COLUMNS - ArReportSchedulerPeer::NUM_LAZY_LOAD_COLUMNS);
		ArReportGenerationPeer::addSelectColumns($criteria);

		$criteria->addJoin(ArReportSchedulerPeer::AR_REPORT_GENERATION_ID, ArReportGenerationPeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArReportSchedulerPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArReportSchedulerPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {

				$cls = ArReportSchedulerPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArReportSchedulerPeer::addInstanceToPool($obj1, $key1);
			} // if $obj1 already loaded

			$key2 = ArReportGenerationPeer::getPrimaryKeyHashFromRow($row, $startcol);
			if ($key2 !== null) {
				$obj2 = ArReportGenerationPeer::getInstanceFromPool($key2);
				if (!$obj2) {

					$cls = ArReportGenerationPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol);
					ArReportGenerationPeer::addInstanceToPool($obj2, $key2);
				} // if obj2 already loaded
				
				// Add the $obj1 (ArReportScheduler) to $obj2 (ArReportGeneration)
				$obj2->addArReportScheduler($obj1);

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
		$criteria->setPrimaryTableName(ArReportSchedulerPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArReportSchedulerPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArReportSchedulerPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArReportSchedulerPeer::AR_REPORT_ID, ArReportPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportSchedulerPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportSchedulerPeer::AR_REPORT_GENERATION_ID, ArReportGenerationPeer::ID, $join_behavior);

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
	 * Selects a collection of ArReportScheduler objects pre-filled with all related objects.
	 *
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArReportScheduler objects.
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

		ArReportSchedulerPeer::addSelectColumns($criteria);
		$startcol2 = (ArReportSchedulerPeer::NUM_COLUMNS - ArReportSchedulerPeer::NUM_LAZY_LOAD_COLUMNS);

		ArReportPeer::addSelectColumns($criteria);
		$startcol3 = $startcol2 + (ArReportPeer::NUM_COLUMNS - ArReportPeer::NUM_LAZY_LOAD_COLUMNS);

		ArOrganizationUnitPeer::addSelectColumns($criteria);
		$startcol4 = $startcol3 + (ArOrganizationUnitPeer::NUM_COLUMNS - ArOrganizationUnitPeer::NUM_LAZY_LOAD_COLUMNS);

		ArReportGenerationPeer::addSelectColumns($criteria);
		$startcol5 = $startcol4 + (ArReportGenerationPeer::NUM_COLUMNS - ArReportGenerationPeer::NUM_LAZY_LOAD_COLUMNS);

		$criteria->addJoin(ArReportSchedulerPeer::AR_REPORT_ID, ArReportPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportSchedulerPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportSchedulerPeer::AR_REPORT_GENERATION_ID, ArReportGenerationPeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArReportSchedulerPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArReportSchedulerPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {
				$cls = ArReportSchedulerPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArReportSchedulerPeer::addInstanceToPool($obj1, $key1);
			} // if obj1 already loaded

			// Add objects for joined ArReport rows

			$key2 = ArReportPeer::getPrimaryKeyHashFromRow($row, $startcol2);
			if ($key2 !== null) {
				$obj2 = ArReportPeer::getInstanceFromPool($key2);
				if (!$obj2) {

					$cls = ArReportPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol2);
					ArReportPeer::addInstanceToPool($obj2, $key2);
				} // if obj2 loaded

				// Add the $obj1 (ArReportScheduler) to the collection in $obj2 (ArReport)
				$obj2->addArReportScheduler($obj1);
			} // if joined row not null

			// Add objects for joined ArOrganizationUnit rows

			$key3 = ArOrganizationUnitPeer::getPrimaryKeyHashFromRow($row, $startcol3);
			if ($key3 !== null) {
				$obj3 = ArOrganizationUnitPeer::getInstanceFromPool($key3);
				if (!$obj3) {

					$cls = ArOrganizationUnitPeer::getOMClass(false);

					$obj3 = new $cls();
					$obj3->hydrate($row, $startcol3);
					ArOrganizationUnitPeer::addInstanceToPool($obj3, $key3);
				} // if obj3 loaded

				// Add the $obj1 (ArReportScheduler) to the collection in $obj3 (ArOrganizationUnit)
				$obj3->addArReportScheduler($obj1);
			} // if joined row not null

			// Add objects for joined ArReportGeneration rows

			$key4 = ArReportGenerationPeer::getPrimaryKeyHashFromRow($row, $startcol4);
			if ($key4 !== null) {
				$obj4 = ArReportGenerationPeer::getInstanceFromPool($key4);
				if (!$obj4) {

					$cls = ArReportGenerationPeer::getOMClass(false);

					$obj4 = new $cls();
					$obj4->hydrate($row, $startcol4);
					ArReportGenerationPeer::addInstanceToPool($obj4, $key4);
				} // if obj4 loaded

				// Add the $obj1 (ArReportScheduler) to the collection in $obj4 (ArReportGeneration)
				$obj4->addArReportScheduler($obj1);
			} // if joined row not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Returns the number of rows matching criteria, joining the related ArReport table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinAllExceptArReport(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArReportSchedulerPeer::TABLE_NAME);
		
		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArReportSchedulerPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY should not affect count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArReportSchedulerPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}
	
		$criteria->addJoin(ArReportSchedulerPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportSchedulerPeer::AR_REPORT_GENERATION_ID, ArReportGenerationPeer::ID, $join_behavior);

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
		$criteria->setPrimaryTableName(ArReportSchedulerPeer::TABLE_NAME);
		
		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArReportSchedulerPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY should not affect count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArReportSchedulerPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}
	
		$criteria->addJoin(ArReportSchedulerPeer::AR_REPORT_ID, ArReportPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportSchedulerPeer::AR_REPORT_GENERATION_ID, ArReportGenerationPeer::ID, $join_behavior);

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
	 * Returns the number of rows matching criteria, joining the related ArReportGeneration table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinAllExceptArReportGeneration(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArReportSchedulerPeer::TABLE_NAME);
		
		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArReportSchedulerPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY should not affect count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArReportSchedulerPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}
	
		$criteria->addJoin(ArReportSchedulerPeer::AR_REPORT_ID, ArReportPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportSchedulerPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

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
	 * Selects a collection of ArReportScheduler objects pre-filled with all related objects except ArReport.
	 *
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArReportScheduler objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinAllExceptArReport(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		// $criteria->getDbName() will return the same object if not set to another value
		// so == check is okay and faster
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArReportSchedulerPeer::addSelectColumns($criteria);
		$startcol2 = (ArReportSchedulerPeer::NUM_COLUMNS - ArReportSchedulerPeer::NUM_LAZY_LOAD_COLUMNS);

		ArOrganizationUnitPeer::addSelectColumns($criteria);
		$startcol3 = $startcol2 + (ArOrganizationUnitPeer::NUM_COLUMNS - ArOrganizationUnitPeer::NUM_LAZY_LOAD_COLUMNS);

		ArReportGenerationPeer::addSelectColumns($criteria);
		$startcol4 = $startcol3 + (ArReportGenerationPeer::NUM_COLUMNS - ArReportGenerationPeer::NUM_LAZY_LOAD_COLUMNS);

		$criteria->addJoin(ArReportSchedulerPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportSchedulerPeer::AR_REPORT_GENERATION_ID, ArReportGenerationPeer::ID, $join_behavior);


		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArReportSchedulerPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArReportSchedulerPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {
				$cls = ArReportSchedulerPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArReportSchedulerPeer::addInstanceToPool($obj1, $key1);
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

				// Add the $obj1 (ArReportScheduler) to the collection in $obj2 (ArOrganizationUnit)
				$obj2->addArReportScheduler($obj1);

			} // if joined row is not null

				// Add objects for joined ArReportGeneration rows

				$key3 = ArReportGenerationPeer::getPrimaryKeyHashFromRow($row, $startcol3);
				if ($key3 !== null) {
					$obj3 = ArReportGenerationPeer::getInstanceFromPool($key3);
					if (!$obj3) {
	
						$cls = ArReportGenerationPeer::getOMClass(false);

					$obj3 = new $cls();
					$obj3->hydrate($row, $startcol3);
					ArReportGenerationPeer::addInstanceToPool($obj3, $key3);
				} // if $obj3 already loaded

				// Add the $obj1 (ArReportScheduler) to the collection in $obj3 (ArReportGeneration)
				$obj3->addArReportScheduler($obj1);

			} // if joined row is not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArReportScheduler objects pre-filled with all related objects except ArOrganizationUnit.
	 *
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArReportScheduler objects.
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

		ArReportSchedulerPeer::addSelectColumns($criteria);
		$startcol2 = (ArReportSchedulerPeer::NUM_COLUMNS - ArReportSchedulerPeer::NUM_LAZY_LOAD_COLUMNS);

		ArReportPeer::addSelectColumns($criteria);
		$startcol3 = $startcol2 + (ArReportPeer::NUM_COLUMNS - ArReportPeer::NUM_LAZY_LOAD_COLUMNS);

		ArReportGenerationPeer::addSelectColumns($criteria);
		$startcol4 = $startcol3 + (ArReportGenerationPeer::NUM_COLUMNS - ArReportGenerationPeer::NUM_LAZY_LOAD_COLUMNS);

		$criteria->addJoin(ArReportSchedulerPeer::AR_REPORT_ID, ArReportPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportSchedulerPeer::AR_REPORT_GENERATION_ID, ArReportGenerationPeer::ID, $join_behavior);


		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArReportSchedulerPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArReportSchedulerPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {
				$cls = ArReportSchedulerPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArReportSchedulerPeer::addInstanceToPool($obj1, $key1);
			} // if obj1 already loaded

				// Add objects for joined ArReport rows

				$key2 = ArReportPeer::getPrimaryKeyHashFromRow($row, $startcol2);
				if ($key2 !== null) {
					$obj2 = ArReportPeer::getInstanceFromPool($key2);
					if (!$obj2) {
	
						$cls = ArReportPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol2);
					ArReportPeer::addInstanceToPool($obj2, $key2);
				} // if $obj2 already loaded

				// Add the $obj1 (ArReportScheduler) to the collection in $obj2 (ArReport)
				$obj2->addArReportScheduler($obj1);

			} // if joined row is not null

				// Add objects for joined ArReportGeneration rows

				$key3 = ArReportGenerationPeer::getPrimaryKeyHashFromRow($row, $startcol3);
				if ($key3 !== null) {
					$obj3 = ArReportGenerationPeer::getInstanceFromPool($key3);
					if (!$obj3) {
	
						$cls = ArReportGenerationPeer::getOMClass(false);

					$obj3 = new $cls();
					$obj3->hydrate($row, $startcol3);
					ArReportGenerationPeer::addInstanceToPool($obj3, $key3);
				} // if $obj3 already loaded

				// Add the $obj1 (ArReportScheduler) to the collection in $obj3 (ArReportGeneration)
				$obj3->addArReportScheduler($obj1);

			} // if joined row is not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArReportScheduler objects pre-filled with all related objects except ArReportGeneration.
	 *
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArReportScheduler objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinAllExceptArReportGeneration(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		// $criteria->getDbName() will return the same object if not set to another value
		// so == check is okay and faster
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArReportSchedulerPeer::addSelectColumns($criteria);
		$startcol2 = (ArReportSchedulerPeer::NUM_COLUMNS - ArReportSchedulerPeer::NUM_LAZY_LOAD_COLUMNS);

		ArReportPeer::addSelectColumns($criteria);
		$startcol3 = $startcol2 + (ArReportPeer::NUM_COLUMNS - ArReportPeer::NUM_LAZY_LOAD_COLUMNS);

		ArOrganizationUnitPeer::addSelectColumns($criteria);
		$startcol4 = $startcol3 + (ArOrganizationUnitPeer::NUM_COLUMNS - ArOrganizationUnitPeer::NUM_LAZY_LOAD_COLUMNS);

		$criteria->addJoin(ArReportSchedulerPeer::AR_REPORT_ID, ArReportPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportSchedulerPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);


		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArReportSchedulerPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArReportSchedulerPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {
				$cls = ArReportSchedulerPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArReportSchedulerPeer::addInstanceToPool($obj1, $key1);
			} // if obj1 already loaded

				// Add objects for joined ArReport rows

				$key2 = ArReportPeer::getPrimaryKeyHashFromRow($row, $startcol2);
				if ($key2 !== null) {
					$obj2 = ArReportPeer::getInstanceFromPool($key2);
					if (!$obj2) {
	
						$cls = ArReportPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol2);
					ArReportPeer::addInstanceToPool($obj2, $key2);
				} // if $obj2 already loaded

				// Add the $obj1 (ArReportScheduler) to the collection in $obj2 (ArReport)
				$obj2->addArReportScheduler($obj1);

			} // if joined row is not null

				// Add objects for joined ArOrganizationUnit rows

				$key3 = ArOrganizationUnitPeer::getPrimaryKeyHashFromRow($row, $startcol3);
				if ($key3 !== null) {
					$obj3 = ArOrganizationUnitPeer::getInstanceFromPool($key3);
					if (!$obj3) {
	
						$cls = ArOrganizationUnitPeer::getOMClass(false);

					$obj3 = new $cls();
					$obj3->hydrate($row, $startcol3);
					ArOrganizationUnitPeer::addInstanceToPool($obj3, $key3);
				} // if $obj3 already loaded

				// Add the $obj1 (ArReportScheduler) to the collection in $obj3 (ArOrganizationUnit)
				$obj3->addArReportScheduler($obj1);

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
	  $dbMap = Propel::getDatabaseMap(BaseArReportSchedulerPeer::DATABASE_NAME);
	  if (!$dbMap->hasTable(BaseArReportSchedulerPeer::TABLE_NAME))
	  {
	    $dbMap->addTableObject(new ArReportSchedulerTableMap());
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
		return $withPrefix ? ArReportSchedulerPeer::CLASS_DEFAULT : ArReportSchedulerPeer::OM_CLASS;
	}

	/**
	 * Method perform an INSERT on the database, given a ArReportScheduler or Criteria object.
	 *
	 * @param      mixed $values Criteria or ArReportScheduler object containing data that is used to create the INSERT statement.
	 * @param      PropelPDO $con the PropelPDO connection to use
	 * @return     mixed The new primary key.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doInsert($values, PropelPDO $con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArReportSchedulerPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		if ($values instanceof Criteria) {
			$criteria = clone $values; // rename for clarity
		} else {
			$criteria = $values->buildCriteria(); // build Criteria from ArReportScheduler object
		}

		if ($criteria->containsKey(ArReportSchedulerPeer::ID) && $criteria->keyContainsValue(ArReportSchedulerPeer::ID) ) {
			throw new PropelException('Cannot insert a value for auto-increment primary key ('.ArReportSchedulerPeer::ID.')');
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
	 * Method perform an UPDATE on the database, given a ArReportScheduler or Criteria object.
	 *
	 * @param      mixed $values Criteria or ArReportScheduler object containing data that is used to create the UPDATE statement.
	 * @param      PropelPDO $con The connection to use (specify PropelPDO connection object to exert more control over transactions).
	 * @return     int The number of affected rows (if supported by underlying database driver).
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doUpdate($values, PropelPDO $con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArReportSchedulerPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		$selectCriteria = new Criteria(self::DATABASE_NAME);

		if ($values instanceof Criteria) {
			$criteria = clone $values; // rename for clarity

			$comparison = $criteria->getComparison(ArReportSchedulerPeer::ID);
			$selectCriteria->add(ArReportSchedulerPeer::ID, $criteria->remove(ArReportSchedulerPeer::ID), $comparison);

		} else { // $values is ArReportScheduler object
			$criteria = $values->buildCriteria(); // gets full criteria
			$selectCriteria = $values->buildPkeyCriteria(); // gets criteria w/ primary key(s)
		}

		// set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		return BasePeer::doUpdate($selectCriteria, $criteria, $con);
	}

	/**
	 * Method to DELETE all rows from the ar_report_scheduler table.
	 *
	 * @return     int The number of affected rows (if supported by underlying database driver).
	 */
	public static function doDeleteAll($con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArReportSchedulerPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}
		$affectedRows = 0; // initialize var to track total num of affected rows
		try {
			// use transaction because $criteria could contain info
			// for more than one table or we could emulating ON DELETE CASCADE, etc.
			$con->beginTransaction();
			$affectedRows += BasePeer::doDeleteAll(ArReportSchedulerPeer::TABLE_NAME, $con);
			// Because this db requires some delete cascade/set null emulation, we have to
			// clear the cached instance *after* the emulation has happened (since
			// instances get re-added by the select statement contained therein).
			ArReportSchedulerPeer::clearInstancePool();
			ArReportSchedulerPeer::clearRelatedInstancePool();
			$con->commit();
			return $affectedRows;
		} catch (PropelException $e) {
			$con->rollBack();
			throw $e;
		}
	}

	/**
	 * Method perform a DELETE on the database, given a ArReportScheduler or Criteria object OR a primary key value.
	 *
	 * @param      mixed $values Criteria or ArReportScheduler object or primary key or array of primary keys
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
			$con = Propel::getConnection(ArReportSchedulerPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		if ($values instanceof Criteria) {
			// invalidate the cache for all objects of this type, since we have no
			// way of knowing (without running a query) what objects should be invalidated
			// from the cache based on this Criteria.
			ArReportSchedulerPeer::clearInstancePool();
			// rename for clarity
			$criteria = clone $values;
		} elseif ($values instanceof ArReportScheduler) { // it's a model object
			// invalidate the cache for this single object
			ArReportSchedulerPeer::removeInstanceFromPool($values);
			// create criteria based on pk values
			$criteria = $values->buildPkeyCriteria();
		} else { // it's a primary key, or an array of pks
			$criteria = new Criteria(self::DATABASE_NAME);
			$criteria->add(ArReportSchedulerPeer::ID, (array) $values, Criteria::IN);
			// invalidate the cache for this object(s)
			foreach ((array) $values as $singleval) {
				ArReportSchedulerPeer::removeInstanceFromPool($singleval);
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
			ArReportSchedulerPeer::clearRelatedInstancePool();
			$con->commit();
			return $affectedRows;
		} catch (PropelException $e) {
			$con->rollBack();
			throw $e;
		}
	}

	/**
	 * Validates all modified columns of given ArReportScheduler object.
	 * If parameter $columns is either a single column name or an array of column names
	 * than only those columns are validated.
	 *
	 * NOTICE: This does not apply to primary or foreign keys for now.
	 *
	 * @param      ArReportScheduler $obj The object to validate.
	 * @param      mixed $cols Column name or array of column names.
	 *
	 * @return     mixed TRUE if all columns are valid or the error message of the first invalid column.
	 */
	public static function doValidate(ArReportScheduler $obj, $cols = null)
	{
		$columns = array();

		if ($cols) {
			$dbMap = Propel::getDatabaseMap(ArReportSchedulerPeer::DATABASE_NAME);
			$tableMap = $dbMap->getTable(ArReportSchedulerPeer::TABLE_NAME);

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

		return BasePeer::doValidate(ArReportSchedulerPeer::DATABASE_NAME, ArReportSchedulerPeer::TABLE_NAME, $columns);
	}

	/**
	 * Retrieve a single object by pkey.
	 *
	 * @param      int $pk the primary key.
	 * @param      PropelPDO $con the connection to use
	 * @return     ArReportScheduler
	 */
	public static function retrieveByPK($pk, PropelPDO $con = null)
	{

		if (null !== ($obj = ArReportSchedulerPeer::getInstanceFromPool((string) $pk))) {
			return $obj;
		}

		if ($con === null) {
			$con = Propel::getConnection(ArReportSchedulerPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria = new Criteria(ArReportSchedulerPeer::DATABASE_NAME);
		$criteria->add(ArReportSchedulerPeer::ID, $pk);

		$v = ArReportSchedulerPeer::doSelect($criteria, $con);

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
			$con = Propel::getConnection(ArReportSchedulerPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$objs = null;
		if (empty($pks)) {
			$objs = array();
		} else {
			$criteria = new Criteria(ArReportSchedulerPeer::DATABASE_NAME);
			$criteria->add(ArReportSchedulerPeer::ID, $pks, Criteria::IN);
			$objs = ArReportSchedulerPeer::doSelect($criteria, $con);
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

} // BaseArReportSchedulerPeer

// This is the static code needed to register the TableMap for this table with the main Propel class.
//
BaseArReportSchedulerPeer::buildTableMap();

