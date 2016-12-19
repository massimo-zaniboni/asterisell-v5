<?php

/**
 * Base static class for performing query and update operations on the 'ar_service' table.
 *
 * 
 *
 * @package    lib.model.om
 */
abstract class BaseArServicePeer {

	/** the default database name for this class */
	const DATABASE_NAME = 'propel';

	/** the table name for this class */
	const TABLE_NAME = 'ar_service';

	/** the related Propel class for this table */
	const OM_CLASS = 'ArService';

	/** A class that can be returned by this peer. */
	const CLASS_DEFAULT = 'lib.model.ArService';

	/** the related TableMap class for this table */
	const TM_CLASS = 'ArServiceTableMap';
	
	/** The total number of columns. */
	const NUM_COLUMNS = 14;

	/** The number of lazy-loaded columns. */
	const NUM_LAZY_LOAD_COLUMNS = 0;

	/** the column name for the ID field */
	const ID = 'ar_service.ID';

	/** the column name for the INTERNAL_NAME field */
	const INTERNAL_NAME = 'ar_service.INTERNAL_NAME';

	/** the column name for the CUSTOMER_NAME field */
	const CUSTOMER_NAME = 'ar_service.CUSTOMER_NAME';

	/** the column name for the CUSTOMER_DESCRIPTION field */
	const CUSTOMER_DESCRIPTION = 'ar_service.CUSTOMER_DESCRIPTION';

	/** the column name for the VENDOR_NAME field */
	const VENDOR_NAME = 'ar_service.VENDOR_NAME';

	/** the column name for the VENDOR_DESCRIPTION field */
	const VENDOR_DESCRIPTION = 'ar_service.VENDOR_DESCRIPTION';

	/** the column name for the EXTERNAL_CRM_CODE field */
	const EXTERNAL_CRM_CODE = 'ar_service.EXTERNAL_CRM_CODE';

	/** the column name for the CUSTOMER_PRICE_DEPEND_FROM_ACTIVATION_DATE field */
	const CUSTOMER_PRICE_DEPEND_FROM_ACTIVATION_DATE = 'ar_service.CUSTOMER_PRICE_DEPEND_FROM_ACTIVATION_DATE';

	/** the column name for the CUSTOMER_PRICE_CHANGE_WITH_PRICE_LIST field */
	const CUSTOMER_PRICE_CHANGE_WITH_PRICE_LIST = 'ar_service.CUSTOMER_PRICE_CHANGE_WITH_PRICE_LIST';

	/** the column name for the IS_ENABLED field */
	const IS_ENABLED = 'ar_service.IS_ENABLED';

	/** the column name for the IS_APPLIED_ONLY_ONE_TIME field */
	const IS_APPLIED_ONLY_ONE_TIME = 'ar_service.IS_APPLIED_ONLY_ONE_TIME';

	/** the column name for the SCHEDULE_TIMEFRAME field */
	const SCHEDULE_TIMEFRAME = 'ar_service.SCHEDULE_TIMEFRAME';

	/** the column name for the WAS_COMPILED field */
	const WAS_COMPILED = 'ar_service.WAS_COMPILED';

	/** the column name for the SCHEDULE_FROM field */
	const SCHEDULE_FROM = 'ar_service.SCHEDULE_FROM';

	/**
	 * An identiy map to hold any loaded instances of ArService objects.
	 * This must be public so that other peer classes can access this when hydrating from JOIN
	 * queries.
	 * @var        array ArService[]
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
		BasePeer::TYPE_PHPNAME => array ('Id', 'InternalName', 'CustomerName', 'CustomerDescription', 'VendorName', 'VendorDescription', 'ExternalCrmCode', 'CustomerPriceDependFromActivationDate', 'CustomerPriceChangeWithPriceList', 'IsEnabled', 'IsAppliedOnlyOneTime', 'ScheduleTimeframe', 'WasCompiled', 'ScheduleFrom', ),
		BasePeer::TYPE_STUDLYPHPNAME => array ('id', 'internalName', 'customerName', 'customerDescription', 'vendorName', 'vendorDescription', 'externalCrmCode', 'customerPriceDependFromActivationDate', 'customerPriceChangeWithPriceList', 'isEnabled', 'isAppliedOnlyOneTime', 'scheduleTimeframe', 'wasCompiled', 'scheduleFrom', ),
		BasePeer::TYPE_COLNAME => array (self::ID, self::INTERNAL_NAME, self::CUSTOMER_NAME, self::CUSTOMER_DESCRIPTION, self::VENDOR_NAME, self::VENDOR_DESCRIPTION, self::EXTERNAL_CRM_CODE, self::CUSTOMER_PRICE_DEPEND_FROM_ACTIVATION_DATE, self::CUSTOMER_PRICE_CHANGE_WITH_PRICE_LIST, self::IS_ENABLED, self::IS_APPLIED_ONLY_ONE_TIME, self::SCHEDULE_TIMEFRAME, self::WAS_COMPILED, self::SCHEDULE_FROM, ),
		BasePeer::TYPE_FIELDNAME => array ('id', 'internal_name', 'customer_name', 'customer_description', 'vendor_name', 'vendor_description', 'external_crm_code', 'customer_price_depend_from_activation_date', 'customer_price_change_with_price_list', 'is_enabled', 'is_applied_only_one_time', 'schedule_timeframe', 'was_compiled', 'schedule_from', ),
		BasePeer::TYPE_NUM => array (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, )
	);

	/**
	 * holds an array of keys for quick access to the fieldnames array
	 *
	 * first dimension keys are the type constants
	 * e.g. self::$fieldNames[BasePeer::TYPE_PHPNAME]['Id'] = 0
	 */
	private static $fieldKeys = array (
		BasePeer::TYPE_PHPNAME => array ('Id' => 0, 'InternalName' => 1, 'CustomerName' => 2, 'CustomerDescription' => 3, 'VendorName' => 4, 'VendorDescription' => 5, 'ExternalCrmCode' => 6, 'CustomerPriceDependFromActivationDate' => 7, 'CustomerPriceChangeWithPriceList' => 8, 'IsEnabled' => 9, 'IsAppliedOnlyOneTime' => 10, 'ScheduleTimeframe' => 11, 'WasCompiled' => 12, 'ScheduleFrom' => 13, ),
		BasePeer::TYPE_STUDLYPHPNAME => array ('id' => 0, 'internalName' => 1, 'customerName' => 2, 'customerDescription' => 3, 'vendorName' => 4, 'vendorDescription' => 5, 'externalCrmCode' => 6, 'customerPriceDependFromActivationDate' => 7, 'customerPriceChangeWithPriceList' => 8, 'isEnabled' => 9, 'isAppliedOnlyOneTime' => 10, 'scheduleTimeframe' => 11, 'wasCompiled' => 12, 'scheduleFrom' => 13, ),
		BasePeer::TYPE_COLNAME => array (self::ID => 0, self::INTERNAL_NAME => 1, self::CUSTOMER_NAME => 2, self::CUSTOMER_DESCRIPTION => 3, self::VENDOR_NAME => 4, self::VENDOR_DESCRIPTION => 5, self::EXTERNAL_CRM_CODE => 6, self::CUSTOMER_PRICE_DEPEND_FROM_ACTIVATION_DATE => 7, self::CUSTOMER_PRICE_CHANGE_WITH_PRICE_LIST => 8, self::IS_ENABLED => 9, self::IS_APPLIED_ONLY_ONE_TIME => 10, self::SCHEDULE_TIMEFRAME => 11, self::WAS_COMPILED => 12, self::SCHEDULE_FROM => 13, ),
		BasePeer::TYPE_FIELDNAME => array ('id' => 0, 'internal_name' => 1, 'customer_name' => 2, 'customer_description' => 3, 'vendor_name' => 4, 'vendor_description' => 5, 'external_crm_code' => 6, 'customer_price_depend_from_activation_date' => 7, 'customer_price_change_with_price_list' => 8, 'is_enabled' => 9, 'is_applied_only_one_time' => 10, 'schedule_timeframe' => 11, 'was_compiled' => 12, 'schedule_from' => 13, ),
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
	 * @param      string $column The column name for current table. (i.e. ArServicePeer::COLUMN_NAME).
	 * @return     string
	 */
	public static function alias($alias, $column)
	{
		return str_replace(ArServicePeer::TABLE_NAME.'.', $alias.'.', $column);
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
		$criteria->addSelectColumn(ArServicePeer::ID);
		$criteria->addSelectColumn(ArServicePeer::INTERNAL_NAME);
		$criteria->addSelectColumn(ArServicePeer::CUSTOMER_NAME);
		$criteria->addSelectColumn(ArServicePeer::CUSTOMER_DESCRIPTION);
		$criteria->addSelectColumn(ArServicePeer::VENDOR_NAME);
		$criteria->addSelectColumn(ArServicePeer::VENDOR_DESCRIPTION);
		$criteria->addSelectColumn(ArServicePeer::EXTERNAL_CRM_CODE);
		$criteria->addSelectColumn(ArServicePeer::CUSTOMER_PRICE_DEPEND_FROM_ACTIVATION_DATE);
		$criteria->addSelectColumn(ArServicePeer::CUSTOMER_PRICE_CHANGE_WITH_PRICE_LIST);
		$criteria->addSelectColumn(ArServicePeer::IS_ENABLED);
		$criteria->addSelectColumn(ArServicePeer::IS_APPLIED_ONLY_ONE_TIME);
		$criteria->addSelectColumn(ArServicePeer::SCHEDULE_TIMEFRAME);
		$criteria->addSelectColumn(ArServicePeer::WAS_COMPILED);
		$criteria->addSelectColumn(ArServicePeer::SCHEDULE_FROM);
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
		$criteria->setPrimaryTableName(ArServicePeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArServicePeer::addSelectColumns($criteria);
		}

		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		$criteria->setDbName(self::DATABASE_NAME); // Set the correct dbName

		if ($con === null) {
			$con = Propel::getConnection(ArServicePeer::DATABASE_NAME, Propel::CONNECTION_READ);
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
	 * @return     ArService
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectOne(Criteria $criteria, PropelPDO $con = null)
	{
		$critcopy = clone $criteria;
		$critcopy->setLimit(1);
		$objects = ArServicePeer::doSelect($critcopy, $con);
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
		return ArServicePeer::populateObjects(ArServicePeer::doSelectStmt($criteria, $con));
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
			$con = Propel::getConnection(ArServicePeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		if (!$criteria->hasSelectClause()) {
			$criteria = clone $criteria;
			ArServicePeer::addSelectColumns($criteria);
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
	 * @param      ArService $value A ArService object.
	 * @param      string $key (optional) key to use for instance map (for performance boost if key was already calculated externally).
	 */
	public static function addInstanceToPool(ArService $obj, $key = null)
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
	 * @param      mixed $value A ArService object or a primary key value.
	 */
	public static function removeInstanceFromPool($value)
	{
		if (Propel::isInstancePoolingEnabled() && $value !== null) {
			if (is_object($value) && $value instanceof ArService) {
				$key = (string) $value->getId();
			} elseif (is_scalar($value)) {
				// assume we've been passed a primary key
				$key = (string) $value;
			} else {
				$e = new PropelException("Invalid value passed to removeInstanceFromPool().  Expected primary key or ArService object; got " . (is_object($value) ? get_class($value) . ' object.' : var_export($value,true)));
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
	 * @return     ArService Found object or NULL if 1) no instance exists for specified key or 2) instance pooling has been disabled.
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
	 * Method to invalidate the instance pool of all tables related to ar_service
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
		$cls = ArServicePeer::getOMClass(false);
		// populate the object(s)
		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key = ArServicePeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj = ArServicePeer::getInstanceFromPool($key))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj->hydrate($row, 0, true); // rehydrate
				$results[] = $obj;
			} else {
				$obj = new $cls();
				$obj->hydrate($row);
				$results[] = $obj;
				ArServicePeer::addInstanceToPool($obj, $key);
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
	  $dbMap = Propel::getDatabaseMap(BaseArServicePeer::DATABASE_NAME);
	  if (!$dbMap->hasTable(BaseArServicePeer::TABLE_NAME))
	  {
	    $dbMap->addTableObject(new ArServiceTableMap());
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
		return $withPrefix ? ArServicePeer::CLASS_DEFAULT : ArServicePeer::OM_CLASS;
	}

	/**
	 * Method perform an INSERT on the database, given a ArService or Criteria object.
	 *
	 * @param      mixed $values Criteria or ArService object containing data that is used to create the INSERT statement.
	 * @param      PropelPDO $con the PropelPDO connection to use
	 * @return     mixed The new primary key.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doInsert($values, PropelPDO $con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArServicePeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		if ($values instanceof Criteria) {
			$criteria = clone $values; // rename for clarity
		} else {
			$criteria = $values->buildCriteria(); // build Criteria from ArService object
		}

		if ($criteria->containsKey(ArServicePeer::ID) && $criteria->keyContainsValue(ArServicePeer::ID) ) {
			throw new PropelException('Cannot insert a value for auto-increment primary key ('.ArServicePeer::ID.')');
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
	 * Method perform an UPDATE on the database, given a ArService or Criteria object.
	 *
	 * @param      mixed $values Criteria or ArService object containing data that is used to create the UPDATE statement.
	 * @param      PropelPDO $con The connection to use (specify PropelPDO connection object to exert more control over transactions).
	 * @return     int The number of affected rows (if supported by underlying database driver).
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doUpdate($values, PropelPDO $con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArServicePeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		$selectCriteria = new Criteria(self::DATABASE_NAME);

		if ($values instanceof Criteria) {
			$criteria = clone $values; // rename for clarity

			$comparison = $criteria->getComparison(ArServicePeer::ID);
			$selectCriteria->add(ArServicePeer::ID, $criteria->remove(ArServicePeer::ID), $comparison);

		} else { // $values is ArService object
			$criteria = $values->buildCriteria(); // gets full criteria
			$selectCriteria = $values->buildPkeyCriteria(); // gets criteria w/ primary key(s)
		}

		// set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		return BasePeer::doUpdate($selectCriteria, $criteria, $con);
	}

	/**
	 * Method to DELETE all rows from the ar_service table.
	 *
	 * @return     int The number of affected rows (if supported by underlying database driver).
	 */
	public static function doDeleteAll($con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArServicePeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}
		$affectedRows = 0; // initialize var to track total num of affected rows
		try {
			// use transaction because $criteria could contain info
			// for more than one table or we could emulating ON DELETE CASCADE, etc.
			$con->beginTransaction();
			$affectedRows += BasePeer::doDeleteAll(ArServicePeer::TABLE_NAME, $con);
			// Because this db requires some delete cascade/set null emulation, we have to
			// clear the cached instance *after* the emulation has happened (since
			// instances get re-added by the select statement contained therein).
			ArServicePeer::clearInstancePool();
			ArServicePeer::clearRelatedInstancePool();
			$con->commit();
			return $affectedRows;
		} catch (PropelException $e) {
			$con->rollBack();
			throw $e;
		}
	}

	/**
	 * Method perform a DELETE on the database, given a ArService or Criteria object OR a primary key value.
	 *
	 * @param      mixed $values Criteria or ArService object or primary key or array of primary keys
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
			$con = Propel::getConnection(ArServicePeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		if ($values instanceof Criteria) {
			// invalidate the cache for all objects of this type, since we have no
			// way of knowing (without running a query) what objects should be invalidated
			// from the cache based on this Criteria.
			ArServicePeer::clearInstancePool();
			// rename for clarity
			$criteria = clone $values;
		} elseif ($values instanceof ArService) { // it's a model object
			// invalidate the cache for this single object
			ArServicePeer::removeInstanceFromPool($values);
			// create criteria based on pk values
			$criteria = $values->buildPkeyCriteria();
		} else { // it's a primary key, or an array of pks
			$criteria = new Criteria(self::DATABASE_NAME);
			$criteria->add(ArServicePeer::ID, (array) $values, Criteria::IN);
			// invalidate the cache for this object(s)
			foreach ((array) $values as $singleval) {
				ArServicePeer::removeInstanceFromPool($singleval);
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
			ArServicePeer::clearRelatedInstancePool();
			$con->commit();
			return $affectedRows;
		} catch (PropelException $e) {
			$con->rollBack();
			throw $e;
		}
	}

	/**
	 * Validates all modified columns of given ArService object.
	 * If parameter $columns is either a single column name or an array of column names
	 * than only those columns are validated.
	 *
	 * NOTICE: This does not apply to primary or foreign keys for now.
	 *
	 * @param      ArService $obj The object to validate.
	 * @param      mixed $cols Column name or array of column names.
	 *
	 * @return     mixed TRUE if all columns are valid or the error message of the first invalid column.
	 */
	public static function doValidate(ArService $obj, $cols = null)
	{
		$columns = array();

		if ($cols) {
			$dbMap = Propel::getDatabaseMap(ArServicePeer::DATABASE_NAME);
			$tableMap = $dbMap->getTable(ArServicePeer::TABLE_NAME);

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

		return BasePeer::doValidate(ArServicePeer::DATABASE_NAME, ArServicePeer::TABLE_NAME, $columns);
	}

	/**
	 * Retrieve a single object by pkey.
	 *
	 * @param      int $pk the primary key.
	 * @param      PropelPDO $con the connection to use
	 * @return     ArService
	 */
	public static function retrieveByPK($pk, PropelPDO $con = null)
	{

		if (null !== ($obj = ArServicePeer::getInstanceFromPool((string) $pk))) {
			return $obj;
		}

		if ($con === null) {
			$con = Propel::getConnection(ArServicePeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria = new Criteria(ArServicePeer::DATABASE_NAME);
		$criteria->add(ArServicePeer::ID, $pk);

		$v = ArServicePeer::doSelect($criteria, $con);

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
			$con = Propel::getConnection(ArServicePeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$objs = null;
		if (empty($pks)) {
			$objs = array();
		} else {
			$criteria = new Criteria(ArServicePeer::DATABASE_NAME);
			$criteria->add(ArServicePeer::ID, $pks, Criteria::IN);
			$objs = ArServicePeer::doSelect($criteria, $con);
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
	  return array(array('internal_name'));
	}

} // BaseArServicePeer

// This is the static code needed to register the TableMap for this table with the main Propel class.
//
BaseArServicePeer::buildTableMap();

