<?php

/**
 * Base static class for performing query and update operations on the 'list_cdrs_admin_3_fast' table.
 *
 * 
 *
 * @package    lib.model.om
 */
abstract class BaseListCdrsAdmin3FastPeer {

	/** the default database name for this class */
	const DATABASE_NAME = 'propel';

	/** the table name for this class */
	const TABLE_NAME = 'list_cdrs_admin_3_fast';

	/** the related Propel class for this table */
	const OM_CLASS = 'ListCdrsAdmin3Fast';

	/** A class that can be returned by this peer. */
	const CLASS_DEFAULT = 'lib.model.ListCdrsAdmin3Fast';

	/** the related TableMap class for this table */
	const TM_CLASS = 'ListCdrsAdmin3FastTableMap';
	
	/** The total number of columns. */
	const NUM_COLUMNS = 11;

	/** The number of lazy-loaded columns. */
	const NUM_LAZY_LOAD_COLUMNS = 0;

	/** the column name for the ID field */
	const ID = 'list_cdrs_admin_3_fast.ID';

	/** the column name for the DESTINATION_TYPE field */
	const DESTINATION_TYPE = 'list_cdrs_admin_3_fast.DESTINATION_TYPE';

	/** the column name for the ERROR_DESTINATION_TYPE field */
	const ERROR_DESTINATION_TYPE = 'list_cdrs_admin_3_fast.ERROR_DESTINATION_TYPE';

	/** the column name for the OPERATOR_TYPE field */
	const OPERATOR_TYPE = 'list_cdrs_admin_3_fast.OPERATOR_TYPE';

	/** the column name for the AR_COMMUNICATION_CHANNEL_TYPE_ID field */
	const AR_COMMUNICATION_CHANNEL_TYPE_ID = 'list_cdrs_admin_3_fast.AR_COMMUNICATION_CHANNEL_TYPE_ID';

	/** the column name for the VENDOR_ID field */
	const VENDOR_ID = 'list_cdrs_admin_3_fast.VENDOR_ID';

	/** the column name for the COUNT_OF_CALLS field */
	const COUNT_OF_CALLS = 'list_cdrs_admin_3_fast.COUNT_OF_CALLS';

	/** the column name for the BILLSEC field */
	const BILLSEC = 'list_cdrs_admin_3_fast.BILLSEC';

	/** the column name for the INCOME field */
	const INCOME = 'list_cdrs_admin_3_fast.INCOME';

	/** the column name for the COST field */
	const COST = 'list_cdrs_admin_3_fast.COST';

	/** the column name for the COST_SAVING field */
	const COST_SAVING = 'list_cdrs_admin_3_fast.COST_SAVING';

	/**
	 * An identiy map to hold any loaded instances of ListCdrsAdmin3Fast objects.
	 * This must be public so that other peer classes can access this when hydrating from JOIN
	 * queries.
	 * @var        array ListCdrsAdmin3Fast[]
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
		BasePeer::TYPE_PHPNAME => array ('Id', 'DestinationType', 'ErrorDestinationType', 'OperatorType', 'ArCommunicationChannelTypeId', 'VendorId', 'CountOfCalls', 'Billsec', 'Income', 'Cost', 'CostSaving', ),
		BasePeer::TYPE_STUDLYPHPNAME => array ('id', 'destinationType', 'errorDestinationType', 'operatorType', 'arCommunicationChannelTypeId', 'vendorId', 'countOfCalls', 'billsec', 'income', 'cost', 'costSaving', ),
		BasePeer::TYPE_COLNAME => array (self::ID, self::DESTINATION_TYPE, self::ERROR_DESTINATION_TYPE, self::OPERATOR_TYPE, self::AR_COMMUNICATION_CHANNEL_TYPE_ID, self::VENDOR_ID, self::COUNT_OF_CALLS, self::BILLSEC, self::INCOME, self::COST, self::COST_SAVING, ),
		BasePeer::TYPE_FIELDNAME => array ('id', 'destination_type', 'error_destination_type', 'operator_type', 'ar_communication_channel_type_id', 'vendor_id', 'count_of_calls', 'billsec', 'income', 'cost', 'cost_saving', ),
		BasePeer::TYPE_NUM => array (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, )
	);

	/**
	 * holds an array of keys for quick access to the fieldnames array
	 *
	 * first dimension keys are the type constants
	 * e.g. self::$fieldNames[BasePeer::TYPE_PHPNAME]['Id'] = 0
	 */
	private static $fieldKeys = array (
		BasePeer::TYPE_PHPNAME => array ('Id' => 0, 'DestinationType' => 1, 'ErrorDestinationType' => 2, 'OperatorType' => 3, 'ArCommunicationChannelTypeId' => 4, 'VendorId' => 5, 'CountOfCalls' => 6, 'Billsec' => 7, 'Income' => 8, 'Cost' => 9, 'CostSaving' => 10, ),
		BasePeer::TYPE_STUDLYPHPNAME => array ('id' => 0, 'destinationType' => 1, 'errorDestinationType' => 2, 'operatorType' => 3, 'arCommunicationChannelTypeId' => 4, 'vendorId' => 5, 'countOfCalls' => 6, 'billsec' => 7, 'income' => 8, 'cost' => 9, 'costSaving' => 10, ),
		BasePeer::TYPE_COLNAME => array (self::ID => 0, self::DESTINATION_TYPE => 1, self::ERROR_DESTINATION_TYPE => 2, self::OPERATOR_TYPE => 3, self::AR_COMMUNICATION_CHANNEL_TYPE_ID => 4, self::VENDOR_ID => 5, self::COUNT_OF_CALLS => 6, self::BILLSEC => 7, self::INCOME => 8, self::COST => 9, self::COST_SAVING => 10, ),
		BasePeer::TYPE_FIELDNAME => array ('id' => 0, 'destination_type' => 1, 'error_destination_type' => 2, 'operator_type' => 3, 'ar_communication_channel_type_id' => 4, 'vendor_id' => 5, 'count_of_calls' => 6, 'billsec' => 7, 'income' => 8, 'cost' => 9, 'cost_saving' => 10, ),
		BasePeer::TYPE_NUM => array (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, )
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
	 * @param      string $column The column name for current table. (i.e. ListCdrsAdmin3FastPeer::COLUMN_NAME).
	 * @return     string
	 */
	public static function alias($alias, $column)
	{
		return str_replace(ListCdrsAdmin3FastPeer::TABLE_NAME.'.', $alias.'.', $column);
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
		$criteria->addSelectColumn(ListCdrsAdmin3FastPeer::ID);
		$criteria->addSelectColumn(ListCdrsAdmin3FastPeer::DESTINATION_TYPE);
		$criteria->addSelectColumn(ListCdrsAdmin3FastPeer::ERROR_DESTINATION_TYPE);
		$criteria->addSelectColumn(ListCdrsAdmin3FastPeer::OPERATOR_TYPE);
		$criteria->addSelectColumn(ListCdrsAdmin3FastPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID);
		$criteria->addSelectColumn(ListCdrsAdmin3FastPeer::VENDOR_ID);
		$criteria->addSelectColumn(ListCdrsAdmin3FastPeer::COUNT_OF_CALLS);
		$criteria->addSelectColumn(ListCdrsAdmin3FastPeer::BILLSEC);
		$criteria->addSelectColumn(ListCdrsAdmin3FastPeer::INCOME);
		$criteria->addSelectColumn(ListCdrsAdmin3FastPeer::COST);
		$criteria->addSelectColumn(ListCdrsAdmin3FastPeer::COST_SAVING);
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
		$criteria->setPrimaryTableName(ListCdrsAdmin3FastPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ListCdrsAdmin3FastPeer::addSelectColumns($criteria);
		}

		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		$criteria->setDbName(self::DATABASE_NAME); // Set the correct dbName

		if ($con === null) {
			$con = Propel::getConnection(ListCdrsAdmin3FastPeer::DATABASE_NAME, Propel::CONNECTION_READ);
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
	 * @return     ListCdrsAdmin3Fast
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectOne(Criteria $criteria, PropelPDO $con = null)
	{
		$critcopy = clone $criteria;
		$critcopy->setLimit(1);
		$objects = ListCdrsAdmin3FastPeer::doSelect($critcopy, $con);
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
		return ListCdrsAdmin3FastPeer::populateObjects(ListCdrsAdmin3FastPeer::doSelectStmt($criteria, $con));
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
			$con = Propel::getConnection(ListCdrsAdmin3FastPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		if (!$criteria->hasSelectClause()) {
			$criteria = clone $criteria;
			ListCdrsAdmin3FastPeer::addSelectColumns($criteria);
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
	 * @param      ListCdrsAdmin3Fast $value A ListCdrsAdmin3Fast object.
	 * @param      string $key (optional) key to use for instance map (for performance boost if key was already calculated externally).
	 */
	public static function addInstanceToPool(ListCdrsAdmin3Fast $obj, $key = null)
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
	 * @param      mixed $value A ListCdrsAdmin3Fast object or a primary key value.
	 */
	public static function removeInstanceFromPool($value)
	{
		if (Propel::isInstancePoolingEnabled() && $value !== null) {
			if (is_object($value) && $value instanceof ListCdrsAdmin3Fast) {
				$key = (string) $value->getId();
			} elseif (is_scalar($value)) {
				// assume we've been passed a primary key
				$key = (string) $value;
			} else {
				$e = new PropelException("Invalid value passed to removeInstanceFromPool().  Expected primary key or ListCdrsAdmin3Fast object; got " . (is_object($value) ? get_class($value) . ' object.' : var_export($value,true)));
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
	 * @return     ListCdrsAdmin3Fast Found object or NULL if 1) no instance exists for specified key or 2) instance pooling has been disabled.
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
	 * Method to invalidate the instance pool of all tables related to list_cdrs_admin_3_fast
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
		$cls = ListCdrsAdmin3FastPeer::getOMClass(false);
		// populate the object(s)
		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key = ListCdrsAdmin3FastPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj = ListCdrsAdmin3FastPeer::getInstanceFromPool($key))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj->hydrate($row, 0, true); // rehydrate
				$results[] = $obj;
			} else {
				$obj = new $cls();
				$obj->hydrate($row);
				$results[] = $obj;
				ListCdrsAdmin3FastPeer::addInstanceToPool($obj, $key);
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
	  $dbMap = Propel::getDatabaseMap(BaseListCdrsAdmin3FastPeer::DATABASE_NAME);
	  if (!$dbMap->hasTable(BaseListCdrsAdmin3FastPeer::TABLE_NAME))
	  {
	    $dbMap->addTableObject(new ListCdrsAdmin3FastTableMap());
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
		return $withPrefix ? ListCdrsAdmin3FastPeer::CLASS_DEFAULT : ListCdrsAdmin3FastPeer::OM_CLASS;
	}

	/**
	 * Method perform an INSERT on the database, given a ListCdrsAdmin3Fast or Criteria object.
	 *
	 * @param      mixed $values Criteria or ListCdrsAdmin3Fast object containing data that is used to create the INSERT statement.
	 * @param      PropelPDO $con the PropelPDO connection to use
	 * @return     mixed The new primary key.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doInsert($values, PropelPDO $con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ListCdrsAdmin3FastPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		if ($values instanceof Criteria) {
			$criteria = clone $values; // rename for clarity
		} else {
			$criteria = $values->buildCriteria(); // build Criteria from ListCdrsAdmin3Fast object
		}

		if ($criteria->containsKey(ListCdrsAdmin3FastPeer::ID) && $criteria->keyContainsValue(ListCdrsAdmin3FastPeer::ID) ) {
			throw new PropelException('Cannot insert a value for auto-increment primary key ('.ListCdrsAdmin3FastPeer::ID.')');
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
	 * Method perform an UPDATE on the database, given a ListCdrsAdmin3Fast or Criteria object.
	 *
	 * @param      mixed $values Criteria or ListCdrsAdmin3Fast object containing data that is used to create the UPDATE statement.
	 * @param      PropelPDO $con The connection to use (specify PropelPDO connection object to exert more control over transactions).
	 * @return     int The number of affected rows (if supported by underlying database driver).
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doUpdate($values, PropelPDO $con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ListCdrsAdmin3FastPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		$selectCriteria = new Criteria(self::DATABASE_NAME);

		if ($values instanceof Criteria) {
			$criteria = clone $values; // rename for clarity

			$comparison = $criteria->getComparison(ListCdrsAdmin3FastPeer::ID);
			$selectCriteria->add(ListCdrsAdmin3FastPeer::ID, $criteria->remove(ListCdrsAdmin3FastPeer::ID), $comparison);

		} else { // $values is ListCdrsAdmin3Fast object
			$criteria = $values->buildCriteria(); // gets full criteria
			$selectCriteria = $values->buildPkeyCriteria(); // gets criteria w/ primary key(s)
		}

		// set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		return BasePeer::doUpdate($selectCriteria, $criteria, $con);
	}

	/**
	 * Method to DELETE all rows from the list_cdrs_admin_3_fast table.
	 *
	 * @return     int The number of affected rows (if supported by underlying database driver).
	 */
	public static function doDeleteAll($con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ListCdrsAdmin3FastPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}
		$affectedRows = 0; // initialize var to track total num of affected rows
		try {
			// use transaction because $criteria could contain info
			// for more than one table or we could emulating ON DELETE CASCADE, etc.
			$con->beginTransaction();
			$affectedRows += BasePeer::doDeleteAll(ListCdrsAdmin3FastPeer::TABLE_NAME, $con);
			// Because this db requires some delete cascade/set null emulation, we have to
			// clear the cached instance *after* the emulation has happened (since
			// instances get re-added by the select statement contained therein).
			ListCdrsAdmin3FastPeer::clearInstancePool();
			ListCdrsAdmin3FastPeer::clearRelatedInstancePool();
			$con->commit();
			return $affectedRows;
		} catch (PropelException $e) {
			$con->rollBack();
			throw $e;
		}
	}

	/**
	 * Method perform a DELETE on the database, given a ListCdrsAdmin3Fast or Criteria object OR a primary key value.
	 *
	 * @param      mixed $values Criteria or ListCdrsAdmin3Fast object or primary key or array of primary keys
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
			$con = Propel::getConnection(ListCdrsAdmin3FastPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		if ($values instanceof Criteria) {
			// invalidate the cache for all objects of this type, since we have no
			// way of knowing (without running a query) what objects should be invalidated
			// from the cache based on this Criteria.
			ListCdrsAdmin3FastPeer::clearInstancePool();
			// rename for clarity
			$criteria = clone $values;
		} elseif ($values instanceof ListCdrsAdmin3Fast) { // it's a model object
			// invalidate the cache for this single object
			ListCdrsAdmin3FastPeer::removeInstanceFromPool($values);
			// create criteria based on pk values
			$criteria = $values->buildPkeyCriteria();
		} else { // it's a primary key, or an array of pks
			$criteria = new Criteria(self::DATABASE_NAME);
			$criteria->add(ListCdrsAdmin3FastPeer::ID, (array) $values, Criteria::IN);
			// invalidate the cache for this object(s)
			foreach ((array) $values as $singleval) {
				ListCdrsAdmin3FastPeer::removeInstanceFromPool($singleval);
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
			ListCdrsAdmin3FastPeer::clearRelatedInstancePool();
			$con->commit();
			return $affectedRows;
		} catch (PropelException $e) {
			$con->rollBack();
			throw $e;
		}
	}

	/**
	 * Validates all modified columns of given ListCdrsAdmin3Fast object.
	 * If parameter $columns is either a single column name or an array of column names
	 * than only those columns are validated.
	 *
	 * NOTICE: This does not apply to primary or foreign keys for now.
	 *
	 * @param      ListCdrsAdmin3Fast $obj The object to validate.
	 * @param      mixed $cols Column name or array of column names.
	 *
	 * @return     mixed TRUE if all columns are valid or the error message of the first invalid column.
	 */
	public static function doValidate(ListCdrsAdmin3Fast $obj, $cols = null)
	{
		$columns = array();

		if ($cols) {
			$dbMap = Propel::getDatabaseMap(ListCdrsAdmin3FastPeer::DATABASE_NAME);
			$tableMap = $dbMap->getTable(ListCdrsAdmin3FastPeer::TABLE_NAME);

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

		return BasePeer::doValidate(ListCdrsAdmin3FastPeer::DATABASE_NAME, ListCdrsAdmin3FastPeer::TABLE_NAME, $columns);
	}

	/**
	 * Retrieve a single object by pkey.
	 *
	 * @param      int $pk the primary key.
	 * @param      PropelPDO $con the connection to use
	 * @return     ListCdrsAdmin3Fast
	 */
	public static function retrieveByPK($pk, PropelPDO $con = null)
	{

		if (null !== ($obj = ListCdrsAdmin3FastPeer::getInstanceFromPool((string) $pk))) {
			return $obj;
		}

		if ($con === null) {
			$con = Propel::getConnection(ListCdrsAdmin3FastPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria = new Criteria(ListCdrsAdmin3FastPeer::DATABASE_NAME);
		$criteria->add(ListCdrsAdmin3FastPeer::ID, $pk);

		$v = ListCdrsAdmin3FastPeer::doSelect($criteria, $con);

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
			$con = Propel::getConnection(ListCdrsAdmin3FastPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$objs = null;
		if (empty($pks)) {
			$objs = array();
		} else {
			$criteria = new Criteria(ListCdrsAdmin3FastPeer::DATABASE_NAME);
			$criteria->add(ListCdrsAdmin3FastPeer::ID, $pks, Criteria::IN);
			$objs = ListCdrsAdmin3FastPeer::doSelect($criteria, $con);
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

} // BaseListCdrsAdmin3FastPeer

// This is the static code needed to register the TableMap for this table with the main Propel class.
//
BaseListCdrsAdmin3FastPeer::buildTableMap();

