<?php

/**
 * Base static class for performing query and update operations on the 'ar_cached_grouped_cdr' table.
 *
 * 
 *
 * @package    lib.model.om
 */
abstract class BaseArCachedGroupedCdrPeer {

	/** the default database name for this class */
	const DATABASE_NAME = 'propel';

	/** the table name for this class */
	const TABLE_NAME = 'ar_cached_grouped_cdr';

	/** the related Propel class for this table */
	const OM_CLASS = 'ArCachedGroupedCdr';

	/** A class that can be returned by this peer. */
	const CLASS_DEFAULT = 'lib.model.ArCachedGroupedCdr';

	/** the related TableMap class for this table */
	const TM_CLASS = 'ArCachedGroupedCdrTableMap';
	
	/** The total number of columns. */
	const NUM_COLUMNS = 14;

	/** The number of lazy-loaded columns. */
	const NUM_LAZY_LOAD_COLUMNS = 0;

	/** the column name for the CACHED_PARENT_ID_HIERARCHY field */
	const CACHED_PARENT_ID_HIERARCHY = 'ar_cached_grouped_cdr.CACHED_PARENT_ID_HIERARCHY';

	/** the column name for the BILLABLE_AR_ORGANIZATION_UNIT_ID field */
	const BILLABLE_AR_ORGANIZATION_UNIT_ID = 'ar_cached_grouped_cdr.BILLABLE_AR_ORGANIZATION_UNIT_ID';

	/** the column name for the CALLDATE field */
	const CALLDATE = 'ar_cached_grouped_cdr.CALLDATE';

	/** the column name for the DESTINATION_TYPE field */
	const DESTINATION_TYPE = 'ar_cached_grouped_cdr.DESTINATION_TYPE';

	/** the column name for the AR_COMMUNICATION_CHANNEL_TYPE_ID field */
	const AR_COMMUNICATION_CHANNEL_TYPE_ID = 'ar_cached_grouped_cdr.AR_COMMUNICATION_CHANNEL_TYPE_ID';

	/** the column name for the OPERATOR_TYPE field */
	const OPERATOR_TYPE = 'ar_cached_grouped_cdr.OPERATOR_TYPE';

	/** the column name for the AR_VENDOR_ID field */
	const AR_VENDOR_ID = 'ar_cached_grouped_cdr.AR_VENDOR_ID';

	/** the column name for the GEOGRAPHIC_LOCATION field */
	const GEOGRAPHIC_LOCATION = 'ar_cached_grouped_cdr.GEOGRAPHIC_LOCATION';

	/** the column name for the COUNT_OF_CALLS field */
	const COUNT_OF_CALLS = 'ar_cached_grouped_cdr.COUNT_OF_CALLS';

	/** the column name for the BILLSEC field */
	const BILLSEC = 'ar_cached_grouped_cdr.BILLSEC';

	/** the column name for the INCOME field */
	const INCOME = 'ar_cached_grouped_cdr.INCOME';

	/** the column name for the COST_SAVING field */
	const COST_SAVING = 'ar_cached_grouped_cdr.COST_SAVING';

	/** the column name for the COST field */
	const COST = 'ar_cached_grouped_cdr.COST';

	/** the column name for the ID field */
	const ID = 'ar_cached_grouped_cdr.ID';

	/**
	 * An identiy map to hold any loaded instances of ArCachedGroupedCdr objects.
	 * This must be public so that other peer classes can access this when hydrating from JOIN
	 * queries.
	 * @var        array ArCachedGroupedCdr[]
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
		BasePeer::TYPE_PHPNAME => array ('CachedParentIdHierarchy', 'BillableArOrganizationUnitId', 'Calldate', 'DestinationType', 'ArCommunicationChannelTypeId', 'OperatorType', 'ArVendorId', 'GeographicLocation', 'CountOfCalls', 'Billsec', 'Income', 'CostSaving', 'Cost', 'Id', ),
		BasePeer::TYPE_STUDLYPHPNAME => array ('cachedParentIdHierarchy', 'billableArOrganizationUnitId', 'calldate', 'destinationType', 'arCommunicationChannelTypeId', 'operatorType', 'arVendorId', 'geographicLocation', 'countOfCalls', 'billsec', 'income', 'costSaving', 'cost', 'id', ),
		BasePeer::TYPE_COLNAME => array (self::CACHED_PARENT_ID_HIERARCHY, self::BILLABLE_AR_ORGANIZATION_UNIT_ID, self::CALLDATE, self::DESTINATION_TYPE, self::AR_COMMUNICATION_CHANNEL_TYPE_ID, self::OPERATOR_TYPE, self::AR_VENDOR_ID, self::GEOGRAPHIC_LOCATION, self::COUNT_OF_CALLS, self::BILLSEC, self::INCOME, self::COST_SAVING, self::COST, self::ID, ),
		BasePeer::TYPE_FIELDNAME => array ('cached_parent_id_hierarchy', 'billable_ar_organization_unit_id', 'calldate', 'destination_type', 'ar_communication_channel_type_id', 'operator_type', 'ar_vendor_id', 'geographic_location', 'count_of_calls', 'billsec', 'income', 'cost_saving', 'cost', 'id', ),
		BasePeer::TYPE_NUM => array (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, )
	);

	/**
	 * holds an array of keys for quick access to the fieldnames array
	 *
	 * first dimension keys are the type constants
	 * e.g. self::$fieldNames[BasePeer::TYPE_PHPNAME]['Id'] = 0
	 */
	private static $fieldKeys = array (
		BasePeer::TYPE_PHPNAME => array ('CachedParentIdHierarchy' => 0, 'BillableArOrganizationUnitId' => 1, 'Calldate' => 2, 'DestinationType' => 3, 'ArCommunicationChannelTypeId' => 4, 'OperatorType' => 5, 'ArVendorId' => 6, 'GeographicLocation' => 7, 'CountOfCalls' => 8, 'Billsec' => 9, 'Income' => 10, 'CostSaving' => 11, 'Cost' => 12, 'Id' => 13, ),
		BasePeer::TYPE_STUDLYPHPNAME => array ('cachedParentIdHierarchy' => 0, 'billableArOrganizationUnitId' => 1, 'calldate' => 2, 'destinationType' => 3, 'arCommunicationChannelTypeId' => 4, 'operatorType' => 5, 'arVendorId' => 6, 'geographicLocation' => 7, 'countOfCalls' => 8, 'billsec' => 9, 'income' => 10, 'costSaving' => 11, 'cost' => 12, 'id' => 13, ),
		BasePeer::TYPE_COLNAME => array (self::CACHED_PARENT_ID_HIERARCHY => 0, self::BILLABLE_AR_ORGANIZATION_UNIT_ID => 1, self::CALLDATE => 2, self::DESTINATION_TYPE => 3, self::AR_COMMUNICATION_CHANNEL_TYPE_ID => 4, self::OPERATOR_TYPE => 5, self::AR_VENDOR_ID => 6, self::GEOGRAPHIC_LOCATION => 7, self::COUNT_OF_CALLS => 8, self::BILLSEC => 9, self::INCOME => 10, self::COST_SAVING => 11, self::COST => 12, self::ID => 13, ),
		BasePeer::TYPE_FIELDNAME => array ('cached_parent_id_hierarchy' => 0, 'billable_ar_organization_unit_id' => 1, 'calldate' => 2, 'destination_type' => 3, 'ar_communication_channel_type_id' => 4, 'operator_type' => 5, 'ar_vendor_id' => 6, 'geographic_location' => 7, 'count_of_calls' => 8, 'billsec' => 9, 'income' => 10, 'cost_saving' => 11, 'cost' => 12, 'id' => 13, ),
		BasePeer::TYPE_NUM => array (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, )
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
	 * @param      string $column The column name for current table. (i.e. ArCachedGroupedCdrPeer::COLUMN_NAME).
	 * @return     string
	 */
	public static function alias($alias, $column)
	{
		return str_replace(ArCachedGroupedCdrPeer::TABLE_NAME.'.', $alias.'.', $column);
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
		$criteria->addSelectColumn(ArCachedGroupedCdrPeer::CACHED_PARENT_ID_HIERARCHY);
		$criteria->addSelectColumn(ArCachedGroupedCdrPeer::BILLABLE_AR_ORGANIZATION_UNIT_ID);
		$criteria->addSelectColumn(ArCachedGroupedCdrPeer::CALLDATE);
		$criteria->addSelectColumn(ArCachedGroupedCdrPeer::DESTINATION_TYPE);
		$criteria->addSelectColumn(ArCachedGroupedCdrPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID);
		$criteria->addSelectColumn(ArCachedGroupedCdrPeer::OPERATOR_TYPE);
		$criteria->addSelectColumn(ArCachedGroupedCdrPeer::AR_VENDOR_ID);
		$criteria->addSelectColumn(ArCachedGroupedCdrPeer::GEOGRAPHIC_LOCATION);
		$criteria->addSelectColumn(ArCachedGroupedCdrPeer::COUNT_OF_CALLS);
		$criteria->addSelectColumn(ArCachedGroupedCdrPeer::BILLSEC);
		$criteria->addSelectColumn(ArCachedGroupedCdrPeer::INCOME);
		$criteria->addSelectColumn(ArCachedGroupedCdrPeer::COST_SAVING);
		$criteria->addSelectColumn(ArCachedGroupedCdrPeer::COST);
		$criteria->addSelectColumn(ArCachedGroupedCdrPeer::ID);
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
		$criteria->setPrimaryTableName(ArCachedGroupedCdrPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArCachedGroupedCdrPeer::addSelectColumns($criteria);
		}

		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		$criteria->setDbName(self::DATABASE_NAME); // Set the correct dbName

		if ($con === null) {
			$con = Propel::getConnection(ArCachedGroupedCdrPeer::DATABASE_NAME, Propel::CONNECTION_READ);
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
	 * @return     ArCachedGroupedCdr
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectOne(Criteria $criteria, PropelPDO $con = null)
	{
		$critcopy = clone $criteria;
		$critcopy->setLimit(1);
		$objects = ArCachedGroupedCdrPeer::doSelect($critcopy, $con);
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
		return ArCachedGroupedCdrPeer::populateObjects(ArCachedGroupedCdrPeer::doSelectStmt($criteria, $con));
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
			$con = Propel::getConnection(ArCachedGroupedCdrPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		if (!$criteria->hasSelectClause()) {
			$criteria = clone $criteria;
			ArCachedGroupedCdrPeer::addSelectColumns($criteria);
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
	 * @param      ArCachedGroupedCdr $value A ArCachedGroupedCdr object.
	 * @param      string $key (optional) key to use for instance map (for performance boost if key was already calculated externally).
	 */
	public static function addInstanceToPool(ArCachedGroupedCdr $obj, $key = null)
	{
		if (Propel::isInstancePoolingEnabled()) {
			if ($key === null) {
				$key = serialize(array((string) $obj->getCachedParentIdHierarchy(), (string) $obj->getBillableArOrganizationUnitId(), (string) $obj->getCalldate(), (string) $obj->getDestinationType(), (string) $obj->getArCommunicationChannelTypeId(), (string) $obj->getOperatorType(), (string) $obj->getArVendorId(), (string) $obj->getGeographicLocation()));
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
	 * @param      mixed $value A ArCachedGroupedCdr object or a primary key value.
	 */
	public static function removeInstanceFromPool($value)
	{
		if (Propel::isInstancePoolingEnabled() && $value !== null) {
			if (is_object($value) && $value instanceof ArCachedGroupedCdr) {
				$key = serialize(array((string) $value->getCachedParentIdHierarchy(), (string) $value->getBillableArOrganizationUnitId(), (string) $value->getCalldate(), (string) $value->getDestinationType(), (string) $value->getArCommunicationChannelTypeId(), (string) $value->getOperatorType(), (string) $value->getArVendorId(), (string) $value->getGeographicLocation()));
			} elseif (is_array($value) && count($value) === 8) {
				// assume we've been passed a primary key
				$key = serialize(array((string) $value[0], (string) $value[1], (string) $value[2], (string) $value[3], (string) $value[4], (string) $value[5], (string) $value[6], (string) $value[7]));
			} else {
				$e = new PropelException("Invalid value passed to removeInstanceFromPool().  Expected primary key or ArCachedGroupedCdr object; got " . (is_object($value) ? get_class($value) . ' object.' : var_export($value,true)));
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
	 * @return     ArCachedGroupedCdr Found object or NULL if 1) no instance exists for specified key or 2) instance pooling has been disabled.
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
	 * Method to invalidate the instance pool of all tables related to ar_cached_grouped_cdr
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
		if ($row[$startcol] === null && $row[$startcol + 1] === null && $row[$startcol + 2] === null && $row[$startcol + 3] === null && $row[$startcol + 4] === null && $row[$startcol + 5] === null && $row[$startcol + 6] === null && $row[$startcol + 7] === null) {
			return null;
		}
		return serialize(array((string) $row[$startcol], (string) $row[$startcol + 1], (string) $row[$startcol + 2], (string) $row[$startcol + 3], (string) $row[$startcol + 4], (string) $row[$startcol + 5], (string) $row[$startcol + 6], (string) $row[$startcol + 7]));
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
		$cls = ArCachedGroupedCdrPeer::getOMClass(false);
		// populate the object(s)
		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key = ArCachedGroupedCdrPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj = ArCachedGroupedCdrPeer::getInstanceFromPool($key))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj->hydrate($row, 0, true); // rehydrate
				$results[] = $obj;
			} else {
				$obj = new $cls();
				$obj->hydrate($row);
				$results[] = $obj;
				ArCachedGroupedCdrPeer::addInstanceToPool($obj, $key);
			} // if key exists
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
	  $dbMap = Propel::getDatabaseMap(BaseArCachedGroupedCdrPeer::DATABASE_NAME);
	  if (!$dbMap->hasTable(BaseArCachedGroupedCdrPeer::TABLE_NAME))
	  {
	    $dbMap->addTableObject(new ArCachedGroupedCdrTableMap());
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
		return $withPrefix ? ArCachedGroupedCdrPeer::CLASS_DEFAULT : ArCachedGroupedCdrPeer::OM_CLASS;
	}

	/**
	 * Method perform an INSERT on the database, given a ArCachedGroupedCdr or Criteria object.
	 *
	 * @param      mixed $values Criteria or ArCachedGroupedCdr object containing data that is used to create the INSERT statement.
	 * @param      PropelPDO $con the PropelPDO connection to use
	 * @return     mixed The new primary key.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doInsert($values, PropelPDO $con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArCachedGroupedCdrPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		if ($values instanceof Criteria) {
			$criteria = clone $values; // rename for clarity
		} else {
			$criteria = $values->buildCriteria(); // build Criteria from ArCachedGroupedCdr object
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
	 * Method perform an UPDATE on the database, given a ArCachedGroupedCdr or Criteria object.
	 *
	 * @param      mixed $values Criteria or ArCachedGroupedCdr object containing data that is used to create the UPDATE statement.
	 * @param      PropelPDO $con The connection to use (specify PropelPDO connection object to exert more control over transactions).
	 * @return     int The number of affected rows (if supported by underlying database driver).
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doUpdate($values, PropelPDO $con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArCachedGroupedCdrPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		$selectCriteria = new Criteria(self::DATABASE_NAME);

		if ($values instanceof Criteria) {
			$criteria = clone $values; // rename for clarity

			$comparison = $criteria->getComparison(ArCachedGroupedCdrPeer::CACHED_PARENT_ID_HIERARCHY);
			$selectCriteria->add(ArCachedGroupedCdrPeer::CACHED_PARENT_ID_HIERARCHY, $criteria->remove(ArCachedGroupedCdrPeer::CACHED_PARENT_ID_HIERARCHY), $comparison);

			$comparison = $criteria->getComparison(ArCachedGroupedCdrPeer::BILLABLE_AR_ORGANIZATION_UNIT_ID);
			$selectCriteria->add(ArCachedGroupedCdrPeer::BILLABLE_AR_ORGANIZATION_UNIT_ID, $criteria->remove(ArCachedGroupedCdrPeer::BILLABLE_AR_ORGANIZATION_UNIT_ID), $comparison);

			$comparison = $criteria->getComparison(ArCachedGroupedCdrPeer::CALLDATE);
			$selectCriteria->add(ArCachedGroupedCdrPeer::CALLDATE, $criteria->remove(ArCachedGroupedCdrPeer::CALLDATE), $comparison);

			$comparison = $criteria->getComparison(ArCachedGroupedCdrPeer::DESTINATION_TYPE);
			$selectCriteria->add(ArCachedGroupedCdrPeer::DESTINATION_TYPE, $criteria->remove(ArCachedGroupedCdrPeer::DESTINATION_TYPE), $comparison);

			$comparison = $criteria->getComparison(ArCachedGroupedCdrPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID);
			$selectCriteria->add(ArCachedGroupedCdrPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID, $criteria->remove(ArCachedGroupedCdrPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID), $comparison);

			$comparison = $criteria->getComparison(ArCachedGroupedCdrPeer::OPERATOR_TYPE);
			$selectCriteria->add(ArCachedGroupedCdrPeer::OPERATOR_TYPE, $criteria->remove(ArCachedGroupedCdrPeer::OPERATOR_TYPE), $comparison);

			$comparison = $criteria->getComparison(ArCachedGroupedCdrPeer::AR_VENDOR_ID);
			$selectCriteria->add(ArCachedGroupedCdrPeer::AR_VENDOR_ID, $criteria->remove(ArCachedGroupedCdrPeer::AR_VENDOR_ID), $comparison);

			$comparison = $criteria->getComparison(ArCachedGroupedCdrPeer::GEOGRAPHIC_LOCATION);
			$selectCriteria->add(ArCachedGroupedCdrPeer::GEOGRAPHIC_LOCATION, $criteria->remove(ArCachedGroupedCdrPeer::GEOGRAPHIC_LOCATION), $comparison);

		} else { // $values is ArCachedGroupedCdr object
			$criteria = $values->buildCriteria(); // gets full criteria
			$selectCriteria = $values->buildPkeyCriteria(); // gets criteria w/ primary key(s)
		}

		// set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		return BasePeer::doUpdate($selectCriteria, $criteria, $con);
	}

	/**
	 * Method to DELETE all rows from the ar_cached_grouped_cdr table.
	 *
	 * @return     int The number of affected rows (if supported by underlying database driver).
	 */
	public static function doDeleteAll($con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArCachedGroupedCdrPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}
		$affectedRows = 0; // initialize var to track total num of affected rows
		try {
			// use transaction because $criteria could contain info
			// for more than one table or we could emulating ON DELETE CASCADE, etc.
			$con->beginTransaction();
			$affectedRows += BasePeer::doDeleteAll(ArCachedGroupedCdrPeer::TABLE_NAME, $con);
			// Because this db requires some delete cascade/set null emulation, we have to
			// clear the cached instance *after* the emulation has happened (since
			// instances get re-added by the select statement contained therein).
			ArCachedGroupedCdrPeer::clearInstancePool();
			ArCachedGroupedCdrPeer::clearRelatedInstancePool();
			$con->commit();
			return $affectedRows;
		} catch (PropelException $e) {
			$con->rollBack();
			throw $e;
		}
	}

	/**
	 * Method perform a DELETE on the database, given a ArCachedGroupedCdr or Criteria object OR a primary key value.
	 *
	 * @param      mixed $values Criteria or ArCachedGroupedCdr object or primary key or array of primary keys
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
			$con = Propel::getConnection(ArCachedGroupedCdrPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		if ($values instanceof Criteria) {
			// invalidate the cache for all objects of this type, since we have no
			// way of knowing (without running a query) what objects should be invalidated
			// from the cache based on this Criteria.
			ArCachedGroupedCdrPeer::clearInstancePool();
			// rename for clarity
			$criteria = clone $values;
		} elseif ($values instanceof ArCachedGroupedCdr) { // it's a model object
			// invalidate the cache for this single object
			ArCachedGroupedCdrPeer::removeInstanceFromPool($values);
			// create criteria based on pk values
			$criteria = $values->buildPkeyCriteria();
		} else { // it's a primary key, or an array of pks
			$criteria = new Criteria(self::DATABASE_NAME);
			// primary key is composite; we therefore, expect
			// the primary key passed to be an array of pkey values
			if (count($values) == count($values, COUNT_RECURSIVE)) {
				// array is not multi-dimensional
				$values = array($values);
			}
			foreach ($values as $value) {
				$criterion = $criteria->getNewCriterion(ArCachedGroupedCdrPeer::CACHED_PARENT_ID_HIERARCHY, $value[0]);
				$criterion->addAnd($criteria->getNewCriterion(ArCachedGroupedCdrPeer::BILLABLE_AR_ORGANIZATION_UNIT_ID, $value[1]));
				$criterion->addAnd($criteria->getNewCriterion(ArCachedGroupedCdrPeer::CALLDATE, $value[2]));
				$criterion->addAnd($criteria->getNewCriterion(ArCachedGroupedCdrPeer::DESTINATION_TYPE, $value[3]));
				$criterion->addAnd($criteria->getNewCriterion(ArCachedGroupedCdrPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID, $value[4]));
				$criterion->addAnd($criteria->getNewCriterion(ArCachedGroupedCdrPeer::OPERATOR_TYPE, $value[5]));
				$criterion->addAnd($criteria->getNewCriterion(ArCachedGroupedCdrPeer::AR_VENDOR_ID, $value[6]));
				$criterion->addAnd($criteria->getNewCriterion(ArCachedGroupedCdrPeer::GEOGRAPHIC_LOCATION, $value[7]));
				$criteria->addOr($criterion);
				// we can invalidate the cache for this single PK
				ArCachedGroupedCdrPeer::removeInstanceFromPool($value);
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
			ArCachedGroupedCdrPeer::clearRelatedInstancePool();
			$con->commit();
			return $affectedRows;
		} catch (PropelException $e) {
			$con->rollBack();
			throw $e;
		}
	}

	/**
	 * Validates all modified columns of given ArCachedGroupedCdr object.
	 * If parameter $columns is either a single column name or an array of column names
	 * than only those columns are validated.
	 *
	 * NOTICE: This does not apply to primary or foreign keys for now.
	 *
	 * @param      ArCachedGroupedCdr $obj The object to validate.
	 * @param      mixed $cols Column name or array of column names.
	 *
	 * @return     mixed TRUE if all columns are valid or the error message of the first invalid column.
	 */
	public static function doValidate(ArCachedGroupedCdr $obj, $cols = null)
	{
		$columns = array();

		if ($cols) {
			$dbMap = Propel::getDatabaseMap(ArCachedGroupedCdrPeer::DATABASE_NAME);
			$tableMap = $dbMap->getTable(ArCachedGroupedCdrPeer::TABLE_NAME);

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

		return BasePeer::doValidate(ArCachedGroupedCdrPeer::DATABASE_NAME, ArCachedGroupedCdrPeer::TABLE_NAME, $columns);
	}

	/**
	 * Retrieve object using using composite pkey values.
	 * @param      string $cached_parent_id_hierarchy
	 * @param      int $billable_ar_organization_unit_id
	 * @param      string $calldate
	 * @param      int $destination_type
	 * @param      int $ar_communication_channel_type_id
	 * @param      string $operator_type
	 * @param      int $ar_vendor_id
	 * @param      string $geographic_location
	 * @param      PropelPDO $con
	 * @return     ArCachedGroupedCdr
	 */
	public static function retrieveByPK($cached_parent_id_hierarchy, $billable_ar_organization_unit_id, $calldate, $destination_type, $ar_communication_channel_type_id, $operator_type, $ar_vendor_id, $geographic_location, PropelPDO $con = null) {
		$key = serialize(array((string) $cached_parent_id_hierarchy, (string) $billable_ar_organization_unit_id, (string) $calldate, (string) $destination_type, (string) $ar_communication_channel_type_id, (string) $operator_type, (string) $ar_vendor_id, (string) $geographic_location));
 		if (null !== ($obj = ArCachedGroupedCdrPeer::getInstanceFromPool($key))) {
 			return $obj;
		}

		if ($con === null) {
			$con = Propel::getConnection(ArCachedGroupedCdrPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}
		$criteria = new Criteria(ArCachedGroupedCdrPeer::DATABASE_NAME);
		$criteria->add(ArCachedGroupedCdrPeer::CACHED_PARENT_ID_HIERARCHY, $cached_parent_id_hierarchy);
		$criteria->add(ArCachedGroupedCdrPeer::BILLABLE_AR_ORGANIZATION_UNIT_ID, $billable_ar_organization_unit_id);
		$criteria->add(ArCachedGroupedCdrPeer::CALLDATE, $calldate);
		$criteria->add(ArCachedGroupedCdrPeer::DESTINATION_TYPE, $destination_type);
		$criteria->add(ArCachedGroupedCdrPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID, $ar_communication_channel_type_id);
		$criteria->add(ArCachedGroupedCdrPeer::OPERATOR_TYPE, $operator_type);
		$criteria->add(ArCachedGroupedCdrPeer::AR_VENDOR_ID, $ar_vendor_id);
		$criteria->add(ArCachedGroupedCdrPeer::GEOGRAPHIC_LOCATION, $geographic_location);
		$v = ArCachedGroupedCdrPeer::doSelect($criteria, $con);

		return !empty($v) ? $v[0] : null;
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

} // BaseArCachedGroupedCdrPeer

// This is the static code needed to register the TableMap for this table with the main Propel class.
//
BaseArCachedGroupedCdrPeer::buildTableMap();

