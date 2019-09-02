<?php

/**
 * Base static class for performing query and update operations on the 'ar_wholesale_number' table.
 *
 * 
 *
 * @package    lib.model.om
 */
abstract class BaseArWholesaleNumberPeer {

	/** the default database name for this class */
	const DATABASE_NAME = 'propel';

	/** the table name for this class */
	const TABLE_NAME = 'ar_wholesale_number';

	/** the related Propel class for this table */
	const OM_CLASS = 'ArWholesaleNumber';

	/** A class that can be returned by this peer. */
	const CLASS_DEFAULT = 'lib.model.ArWholesaleNumber';

	/** the related TableMap class for this table */
	const TM_CLASS = 'ArWholesaleNumberTableMap';
	
	/** The total number of columns. */
	const NUM_COLUMNS = 14;

	/** The number of lazy-loaded columns. */
	const NUM_LAZY_LOAD_COLUMNS = 0;

	/** the column name for the ID field */
	const ID = 'ar_wholesale_number.ID';

	/** the column name for the TELEPHONE_NUMBER field */
	const TELEPHONE_NUMBER = 'ar_wholesale_number.TELEPHONE_NUMBER';

	/** the column name for the FROM_DATE field */
	const FROM_DATE = 'ar_wholesale_number.FROM_DATE';

	/** the column name for the EXISTS field */
	const EXISTS = 'ar_wholesale_number.EXISTS';

	/** the column name for the EXTENSION_CODES field */
	const EXTENSION_CODES = 'ar_wholesale_number.EXTENSION_CODES';

	/** the column name for the USE_DEFAULT_EXTENSION_CODES field */
	const USE_DEFAULT_EXTENSION_CODES = 'ar_wholesale_number.USE_DEFAULT_EXTENSION_CODES';

	/** the column name for the AR_RESELLER_ID field */
	const AR_RESELLER_ID = 'ar_wholesale_number.AR_RESELLER_ID';

	/** the column name for the AR_WHOLESALE_CARRIER_ID field */
	const AR_WHOLESALE_CARRIER_ID = 'ar_wholesale_number.AR_WHOLESALE_CARRIER_ID';

	/** the column name for the INCOME_PRICE field */
	const INCOME_PRICE = 'ar_wholesale_number.INCOME_PRICE';

	/** the column name for the COST_PRICE field */
	const COST_PRICE = 'ar_wholesale_number.COST_PRICE';

	/** the column name for the CSV_COMMENT field */
	const CSV_COMMENT = 'ar_wholesale_number.CSV_COMMENT';

	/** the column name for the CSV_LAST_DATE field */
	const CSV_LAST_DATE = 'ar_wholesale_number.CSV_LAST_DATE';

	/** the column name for the CSV_TO_DELETE field */
	const CSV_TO_DELETE = 'ar_wholesale_number.CSV_TO_DELETE';

	/** the column name for the CSV_IS_CURRENT field */
	const CSV_IS_CURRENT = 'ar_wholesale_number.CSV_IS_CURRENT';

	/**
	 * An identiy map to hold any loaded instances of ArWholesaleNumber objects.
	 * This must be public so that other peer classes can access this when hydrating from JOIN
	 * queries.
	 * @var        array ArWholesaleNumber[]
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
		BasePeer::TYPE_PHPNAME => array ('Id', 'TelephoneNumber', 'FromDate', 'Exists', 'ExtensionCodes', 'UseDefaultExtensionCodes', 'ArResellerId', 'ArWholesaleCarrierId', 'IncomePrice', 'CostPrice', 'CsvComment', 'CsvLastDate', 'CsvToDelete', 'CsvIsCurrent', ),
		BasePeer::TYPE_STUDLYPHPNAME => array ('id', 'telephoneNumber', 'fromDate', 'exists', 'extensionCodes', 'useDefaultExtensionCodes', 'arResellerId', 'arWholesaleCarrierId', 'incomePrice', 'costPrice', 'csvComment', 'csvLastDate', 'csvToDelete', 'csvIsCurrent', ),
		BasePeer::TYPE_COLNAME => array (self::ID, self::TELEPHONE_NUMBER, self::FROM_DATE, self::EXISTS, self::EXTENSION_CODES, self::USE_DEFAULT_EXTENSION_CODES, self::AR_RESELLER_ID, self::AR_WHOLESALE_CARRIER_ID, self::INCOME_PRICE, self::COST_PRICE, self::CSV_COMMENT, self::CSV_LAST_DATE, self::CSV_TO_DELETE, self::CSV_IS_CURRENT, ),
		BasePeer::TYPE_FIELDNAME => array ('id', 'telephone_number', 'from_date', 'exists', 'extension_codes', 'use_default_extension_codes', 'ar_reseller_id', 'ar_wholesale_carrier_id', 'income_price', 'cost_price', 'csv_comment', 'csv_last_date', 'csv_to_delete', 'csv_is_current', ),
		BasePeer::TYPE_NUM => array (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, )
	);

	/**
	 * holds an array of keys for quick access to the fieldnames array
	 *
	 * first dimension keys are the type constants
	 * e.g. self::$fieldNames[BasePeer::TYPE_PHPNAME]['Id'] = 0
	 */
	private static $fieldKeys = array (
		BasePeer::TYPE_PHPNAME => array ('Id' => 0, 'TelephoneNumber' => 1, 'FromDate' => 2, 'Exists' => 3, 'ExtensionCodes' => 4, 'UseDefaultExtensionCodes' => 5, 'ArResellerId' => 6, 'ArWholesaleCarrierId' => 7, 'IncomePrice' => 8, 'CostPrice' => 9, 'CsvComment' => 10, 'CsvLastDate' => 11, 'CsvToDelete' => 12, 'CsvIsCurrent' => 13, ),
		BasePeer::TYPE_STUDLYPHPNAME => array ('id' => 0, 'telephoneNumber' => 1, 'fromDate' => 2, 'exists' => 3, 'extensionCodes' => 4, 'useDefaultExtensionCodes' => 5, 'arResellerId' => 6, 'arWholesaleCarrierId' => 7, 'incomePrice' => 8, 'costPrice' => 9, 'csvComment' => 10, 'csvLastDate' => 11, 'csvToDelete' => 12, 'csvIsCurrent' => 13, ),
		BasePeer::TYPE_COLNAME => array (self::ID => 0, self::TELEPHONE_NUMBER => 1, self::FROM_DATE => 2, self::EXISTS => 3, self::EXTENSION_CODES => 4, self::USE_DEFAULT_EXTENSION_CODES => 5, self::AR_RESELLER_ID => 6, self::AR_WHOLESALE_CARRIER_ID => 7, self::INCOME_PRICE => 8, self::COST_PRICE => 9, self::CSV_COMMENT => 10, self::CSV_LAST_DATE => 11, self::CSV_TO_DELETE => 12, self::CSV_IS_CURRENT => 13, ),
		BasePeer::TYPE_FIELDNAME => array ('id' => 0, 'telephone_number' => 1, 'from_date' => 2, 'exists' => 3, 'extension_codes' => 4, 'use_default_extension_codes' => 5, 'ar_reseller_id' => 6, 'ar_wholesale_carrier_id' => 7, 'income_price' => 8, 'cost_price' => 9, 'csv_comment' => 10, 'csv_last_date' => 11, 'csv_to_delete' => 12, 'csv_is_current' => 13, ),
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
	 * @param      string $column The column name for current table. (i.e. ArWholesaleNumberPeer::COLUMN_NAME).
	 * @return     string
	 */
	public static function alias($alias, $column)
	{
		return str_replace(ArWholesaleNumberPeer::TABLE_NAME.'.', $alias.'.', $column);
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
		$criteria->addSelectColumn(ArWholesaleNumberPeer::ID);
		$criteria->addSelectColumn(ArWholesaleNumberPeer::TELEPHONE_NUMBER);
		$criteria->addSelectColumn(ArWholesaleNumberPeer::FROM_DATE);
		$criteria->addSelectColumn(ArWholesaleNumberPeer::EXISTS);
		$criteria->addSelectColumn(ArWholesaleNumberPeer::EXTENSION_CODES);
		$criteria->addSelectColumn(ArWholesaleNumberPeer::USE_DEFAULT_EXTENSION_CODES);
		$criteria->addSelectColumn(ArWholesaleNumberPeer::AR_RESELLER_ID);
		$criteria->addSelectColumn(ArWholesaleNumberPeer::AR_WHOLESALE_CARRIER_ID);
		$criteria->addSelectColumn(ArWholesaleNumberPeer::INCOME_PRICE);
		$criteria->addSelectColumn(ArWholesaleNumberPeer::COST_PRICE);
		$criteria->addSelectColumn(ArWholesaleNumberPeer::CSV_COMMENT);
		$criteria->addSelectColumn(ArWholesaleNumberPeer::CSV_LAST_DATE);
		$criteria->addSelectColumn(ArWholesaleNumberPeer::CSV_TO_DELETE);
		$criteria->addSelectColumn(ArWholesaleNumberPeer::CSV_IS_CURRENT);
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
		$criteria->setPrimaryTableName(ArWholesaleNumberPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArWholesaleNumberPeer::addSelectColumns($criteria);
		}

		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		$criteria->setDbName(self::DATABASE_NAME); // Set the correct dbName

		if ($con === null) {
			$con = Propel::getConnection(ArWholesaleNumberPeer::DATABASE_NAME, Propel::CONNECTION_READ);
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
	 * @return     ArWholesaleNumber
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectOne(Criteria $criteria, PropelPDO $con = null)
	{
		$critcopy = clone $criteria;
		$critcopy->setLimit(1);
		$objects = ArWholesaleNumberPeer::doSelect($critcopy, $con);
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
		return ArWholesaleNumberPeer::populateObjects(ArWholesaleNumberPeer::doSelectStmt($criteria, $con));
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
			$con = Propel::getConnection(ArWholesaleNumberPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		if (!$criteria->hasSelectClause()) {
			$criteria = clone $criteria;
			ArWholesaleNumberPeer::addSelectColumns($criteria);
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
	 * @param      ArWholesaleNumber $value A ArWholesaleNumber object.
	 * @param      string $key (optional) key to use for instance map (for performance boost if key was already calculated externally).
	 */
	public static function addInstanceToPool(ArWholesaleNumber $obj, $key = null)
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
	 * @param      mixed $value A ArWholesaleNumber object or a primary key value.
	 */
	public static function removeInstanceFromPool($value)
	{
		if (Propel::isInstancePoolingEnabled() && $value !== null) {
			if (is_object($value) && $value instanceof ArWholesaleNumber) {
				$key = (string) $value->getId();
			} elseif (is_scalar($value)) {
				// assume we've been passed a primary key
				$key = (string) $value;
			} else {
				$e = new PropelException("Invalid value passed to removeInstanceFromPool().  Expected primary key or ArWholesaleNumber object; got " . (is_object($value) ? get_class($value) . ' object.' : var_export($value,true)));
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
	 * @return     ArWholesaleNumber Found object or NULL if 1) no instance exists for specified key or 2) instance pooling has been disabled.
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
	 * Method to invalidate the instance pool of all tables related to ar_wholesale_number
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
		$cls = ArWholesaleNumberPeer::getOMClass(false);
		// populate the object(s)
		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key = ArWholesaleNumberPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj = ArWholesaleNumberPeer::getInstanceFromPool($key))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj->hydrate($row, 0, true); // rehydrate
				$results[] = $obj;
			} else {
				$obj = new $cls();
				$obj->hydrate($row);
				$results[] = $obj;
				ArWholesaleNumberPeer::addInstanceToPool($obj, $key);
			} // if key exists
		}
		$stmt->closeCursor();
		return $results;
	}

	/**
	 * Returns the number of rows matching criteria, joining the related ArReseller table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinArReseller(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArWholesaleNumberPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArWholesaleNumberPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArWholesaleNumberPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArWholesaleNumberPeer::AR_RESELLER_ID, ArResellerPeer::ID, $join_behavior);

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
	 * Returns the number of rows matching criteria, joining the related ArWholesaleCarrier table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinArWholesaleCarrier(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArWholesaleNumberPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArWholesaleNumberPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArWholesaleNumberPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArWholesaleNumberPeer::AR_WHOLESALE_CARRIER_ID, ArWholesaleCarrierPeer::ID, $join_behavior);

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
	 * Selects a collection of ArWholesaleNumber objects pre-filled with their ArReseller objects.
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArWholesaleNumber objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinArReseller(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArWholesaleNumberPeer::addSelectColumns($criteria);
		$startcol = (ArWholesaleNumberPeer::NUM_COLUMNS - ArWholesaleNumberPeer::NUM_LAZY_LOAD_COLUMNS);
		ArResellerPeer::addSelectColumns($criteria);

		$criteria->addJoin(ArWholesaleNumberPeer::AR_RESELLER_ID, ArResellerPeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArWholesaleNumberPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArWholesaleNumberPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {

				$cls = ArWholesaleNumberPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArWholesaleNumberPeer::addInstanceToPool($obj1, $key1);
			} // if $obj1 already loaded

			$key2 = ArResellerPeer::getPrimaryKeyHashFromRow($row, $startcol);
			if ($key2 !== null) {
				$obj2 = ArResellerPeer::getInstanceFromPool($key2);
				if (!$obj2) {

					$cls = ArResellerPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol);
					ArResellerPeer::addInstanceToPool($obj2, $key2);
				} // if obj2 already loaded
				
				// Add the $obj1 (ArWholesaleNumber) to $obj2 (ArReseller)
				$obj2->addArWholesaleNumber($obj1);

			} // if joined row was not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArWholesaleNumber objects pre-filled with their ArWholesaleCarrier objects.
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArWholesaleNumber objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinArWholesaleCarrier(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArWholesaleNumberPeer::addSelectColumns($criteria);
		$startcol = (ArWholesaleNumberPeer::NUM_COLUMNS - ArWholesaleNumberPeer::NUM_LAZY_LOAD_COLUMNS);
		ArWholesaleCarrierPeer::addSelectColumns($criteria);

		$criteria->addJoin(ArWholesaleNumberPeer::AR_WHOLESALE_CARRIER_ID, ArWholesaleCarrierPeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArWholesaleNumberPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArWholesaleNumberPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {

				$cls = ArWholesaleNumberPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArWholesaleNumberPeer::addInstanceToPool($obj1, $key1);
			} // if $obj1 already loaded

			$key2 = ArWholesaleCarrierPeer::getPrimaryKeyHashFromRow($row, $startcol);
			if ($key2 !== null) {
				$obj2 = ArWholesaleCarrierPeer::getInstanceFromPool($key2);
				if (!$obj2) {

					$cls = ArWholesaleCarrierPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol);
					ArWholesaleCarrierPeer::addInstanceToPool($obj2, $key2);
				} // if obj2 already loaded
				
				// Add the $obj1 (ArWholesaleNumber) to $obj2 (ArWholesaleCarrier)
				$obj2->addArWholesaleNumber($obj1);

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
		$criteria->setPrimaryTableName(ArWholesaleNumberPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArWholesaleNumberPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArWholesaleNumberPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArWholesaleNumberPeer::AR_RESELLER_ID, ArResellerPeer::ID, $join_behavior);

		$criteria->addJoin(ArWholesaleNumberPeer::AR_WHOLESALE_CARRIER_ID, ArWholesaleCarrierPeer::ID, $join_behavior);

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
	 * Selects a collection of ArWholesaleNumber objects pre-filled with all related objects.
	 *
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArWholesaleNumber objects.
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

		ArWholesaleNumberPeer::addSelectColumns($criteria);
		$startcol2 = (ArWholesaleNumberPeer::NUM_COLUMNS - ArWholesaleNumberPeer::NUM_LAZY_LOAD_COLUMNS);

		ArResellerPeer::addSelectColumns($criteria);
		$startcol3 = $startcol2 + (ArResellerPeer::NUM_COLUMNS - ArResellerPeer::NUM_LAZY_LOAD_COLUMNS);

		ArWholesaleCarrierPeer::addSelectColumns($criteria);
		$startcol4 = $startcol3 + (ArWholesaleCarrierPeer::NUM_COLUMNS - ArWholesaleCarrierPeer::NUM_LAZY_LOAD_COLUMNS);

		$criteria->addJoin(ArWholesaleNumberPeer::AR_RESELLER_ID, ArResellerPeer::ID, $join_behavior);

		$criteria->addJoin(ArWholesaleNumberPeer::AR_WHOLESALE_CARRIER_ID, ArWholesaleCarrierPeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArWholesaleNumberPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArWholesaleNumberPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {
				$cls = ArWholesaleNumberPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArWholesaleNumberPeer::addInstanceToPool($obj1, $key1);
			} // if obj1 already loaded

			// Add objects for joined ArReseller rows

			$key2 = ArResellerPeer::getPrimaryKeyHashFromRow($row, $startcol2);
			if ($key2 !== null) {
				$obj2 = ArResellerPeer::getInstanceFromPool($key2);
				if (!$obj2) {

					$cls = ArResellerPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol2);
					ArResellerPeer::addInstanceToPool($obj2, $key2);
				} // if obj2 loaded

				// Add the $obj1 (ArWholesaleNumber) to the collection in $obj2 (ArReseller)
				$obj2->addArWholesaleNumber($obj1);
			} // if joined row not null

			// Add objects for joined ArWholesaleCarrier rows

			$key3 = ArWholesaleCarrierPeer::getPrimaryKeyHashFromRow($row, $startcol3);
			if ($key3 !== null) {
				$obj3 = ArWholesaleCarrierPeer::getInstanceFromPool($key3);
				if (!$obj3) {

					$cls = ArWholesaleCarrierPeer::getOMClass(false);

					$obj3 = new $cls();
					$obj3->hydrate($row, $startcol3);
					ArWholesaleCarrierPeer::addInstanceToPool($obj3, $key3);
				} // if obj3 loaded

				// Add the $obj1 (ArWholesaleNumber) to the collection in $obj3 (ArWholesaleCarrier)
				$obj3->addArWholesaleNumber($obj1);
			} // if joined row not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Returns the number of rows matching criteria, joining the related ArReseller table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinAllExceptArReseller(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArWholesaleNumberPeer::TABLE_NAME);
		
		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArWholesaleNumberPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY should not affect count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArWholesaleNumberPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}
	
		$criteria->addJoin(ArWholesaleNumberPeer::AR_WHOLESALE_CARRIER_ID, ArWholesaleCarrierPeer::ID, $join_behavior);

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
	 * Returns the number of rows matching criteria, joining the related ArWholesaleCarrier table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinAllExceptArWholesaleCarrier(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArWholesaleNumberPeer::TABLE_NAME);
		
		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArWholesaleNumberPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY should not affect count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArWholesaleNumberPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}
	
		$criteria->addJoin(ArWholesaleNumberPeer::AR_RESELLER_ID, ArResellerPeer::ID, $join_behavior);

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
	 * Selects a collection of ArWholesaleNumber objects pre-filled with all related objects except ArReseller.
	 *
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArWholesaleNumber objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinAllExceptArReseller(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		// $criteria->getDbName() will return the same object if not set to another value
		// so == check is okay and faster
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArWholesaleNumberPeer::addSelectColumns($criteria);
		$startcol2 = (ArWholesaleNumberPeer::NUM_COLUMNS - ArWholesaleNumberPeer::NUM_LAZY_LOAD_COLUMNS);

		ArWholesaleCarrierPeer::addSelectColumns($criteria);
		$startcol3 = $startcol2 + (ArWholesaleCarrierPeer::NUM_COLUMNS - ArWholesaleCarrierPeer::NUM_LAZY_LOAD_COLUMNS);

		$criteria->addJoin(ArWholesaleNumberPeer::AR_WHOLESALE_CARRIER_ID, ArWholesaleCarrierPeer::ID, $join_behavior);


		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArWholesaleNumberPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArWholesaleNumberPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {
				$cls = ArWholesaleNumberPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArWholesaleNumberPeer::addInstanceToPool($obj1, $key1);
			} // if obj1 already loaded

				// Add objects for joined ArWholesaleCarrier rows

				$key2 = ArWholesaleCarrierPeer::getPrimaryKeyHashFromRow($row, $startcol2);
				if ($key2 !== null) {
					$obj2 = ArWholesaleCarrierPeer::getInstanceFromPool($key2);
					if (!$obj2) {
	
						$cls = ArWholesaleCarrierPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol2);
					ArWholesaleCarrierPeer::addInstanceToPool($obj2, $key2);
				} // if $obj2 already loaded

				// Add the $obj1 (ArWholesaleNumber) to the collection in $obj2 (ArWholesaleCarrier)
				$obj2->addArWholesaleNumber($obj1);

			} // if joined row is not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArWholesaleNumber objects pre-filled with all related objects except ArWholesaleCarrier.
	 *
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArWholesaleNumber objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinAllExceptArWholesaleCarrier(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		// $criteria->getDbName() will return the same object if not set to another value
		// so == check is okay and faster
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArWholesaleNumberPeer::addSelectColumns($criteria);
		$startcol2 = (ArWholesaleNumberPeer::NUM_COLUMNS - ArWholesaleNumberPeer::NUM_LAZY_LOAD_COLUMNS);

		ArResellerPeer::addSelectColumns($criteria);
		$startcol3 = $startcol2 + (ArResellerPeer::NUM_COLUMNS - ArResellerPeer::NUM_LAZY_LOAD_COLUMNS);

		$criteria->addJoin(ArWholesaleNumberPeer::AR_RESELLER_ID, ArResellerPeer::ID, $join_behavior);


		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArWholesaleNumberPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArWholesaleNumberPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {
				$cls = ArWholesaleNumberPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArWholesaleNumberPeer::addInstanceToPool($obj1, $key1);
			} // if obj1 already loaded

				// Add objects for joined ArReseller rows

				$key2 = ArResellerPeer::getPrimaryKeyHashFromRow($row, $startcol2);
				if ($key2 !== null) {
					$obj2 = ArResellerPeer::getInstanceFromPool($key2);
					if (!$obj2) {
	
						$cls = ArResellerPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol2);
					ArResellerPeer::addInstanceToPool($obj2, $key2);
				} // if $obj2 already loaded

				// Add the $obj1 (ArWholesaleNumber) to the collection in $obj2 (ArReseller)
				$obj2->addArWholesaleNumber($obj1);

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
	  $dbMap = Propel::getDatabaseMap(BaseArWholesaleNumberPeer::DATABASE_NAME);
	  if (!$dbMap->hasTable(BaseArWholesaleNumberPeer::TABLE_NAME))
	  {
	    $dbMap->addTableObject(new ArWholesaleNumberTableMap());
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
		return $withPrefix ? ArWholesaleNumberPeer::CLASS_DEFAULT : ArWholesaleNumberPeer::OM_CLASS;
	}

	/**
	 * Method perform an INSERT on the database, given a ArWholesaleNumber or Criteria object.
	 *
	 * @param      mixed $values Criteria or ArWholesaleNumber object containing data that is used to create the INSERT statement.
	 * @param      PropelPDO $con the PropelPDO connection to use
	 * @return     mixed The new primary key.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doInsert($values, PropelPDO $con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArWholesaleNumberPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		if ($values instanceof Criteria) {
			$criteria = clone $values; // rename for clarity
		} else {
			$criteria = $values->buildCriteria(); // build Criteria from ArWholesaleNumber object
		}

		if ($criteria->containsKey(ArWholesaleNumberPeer::ID) && $criteria->keyContainsValue(ArWholesaleNumberPeer::ID) ) {
			throw new PropelException('Cannot insert a value for auto-increment primary key ('.ArWholesaleNumberPeer::ID.')');
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
	 * Method perform an UPDATE on the database, given a ArWholesaleNumber or Criteria object.
	 *
	 * @param      mixed $values Criteria or ArWholesaleNumber object containing data that is used to create the UPDATE statement.
	 * @param      PropelPDO $con The connection to use (specify PropelPDO connection object to exert more control over transactions).
	 * @return     int The number of affected rows (if supported by underlying database driver).
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doUpdate($values, PropelPDO $con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArWholesaleNumberPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		$selectCriteria = new Criteria(self::DATABASE_NAME);

		if ($values instanceof Criteria) {
			$criteria = clone $values; // rename for clarity

			$comparison = $criteria->getComparison(ArWholesaleNumberPeer::ID);
			$selectCriteria->add(ArWholesaleNumberPeer::ID, $criteria->remove(ArWholesaleNumberPeer::ID), $comparison);

		} else { // $values is ArWholesaleNumber object
			$criteria = $values->buildCriteria(); // gets full criteria
			$selectCriteria = $values->buildPkeyCriteria(); // gets criteria w/ primary key(s)
		}

		// set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		return BasePeer::doUpdate($selectCriteria, $criteria, $con);
	}

	/**
	 * Method to DELETE all rows from the ar_wholesale_number table.
	 *
	 * @return     int The number of affected rows (if supported by underlying database driver).
	 */
	public static function doDeleteAll($con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArWholesaleNumberPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}
		$affectedRows = 0; // initialize var to track total num of affected rows
		try {
			// use transaction because $criteria could contain info
			// for more than one table or we could emulating ON DELETE CASCADE, etc.
			$con->beginTransaction();
			$affectedRows += BasePeer::doDeleteAll(ArWholesaleNumberPeer::TABLE_NAME, $con);
			// Because this db requires some delete cascade/set null emulation, we have to
			// clear the cached instance *after* the emulation has happened (since
			// instances get re-added by the select statement contained therein).
			ArWholesaleNumberPeer::clearInstancePool();
			ArWholesaleNumberPeer::clearRelatedInstancePool();
			$con->commit();
			return $affectedRows;
		} catch (PropelException $e) {
			$con->rollBack();
			throw $e;
		}
	}

	/**
	 * Method perform a DELETE on the database, given a ArWholesaleNumber or Criteria object OR a primary key value.
	 *
	 * @param      mixed $values Criteria or ArWholesaleNumber object or primary key or array of primary keys
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
			$con = Propel::getConnection(ArWholesaleNumberPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		if ($values instanceof Criteria) {
			// invalidate the cache for all objects of this type, since we have no
			// way of knowing (without running a query) what objects should be invalidated
			// from the cache based on this Criteria.
			ArWholesaleNumberPeer::clearInstancePool();
			// rename for clarity
			$criteria = clone $values;
		} elseif ($values instanceof ArWholesaleNumber) { // it's a model object
			// invalidate the cache for this single object
			ArWholesaleNumberPeer::removeInstanceFromPool($values);
			// create criteria based on pk values
			$criteria = $values->buildPkeyCriteria();
		} else { // it's a primary key, or an array of pks
			$criteria = new Criteria(self::DATABASE_NAME);
			$criteria->add(ArWholesaleNumberPeer::ID, (array) $values, Criteria::IN);
			// invalidate the cache for this object(s)
			foreach ((array) $values as $singleval) {
				ArWholesaleNumberPeer::removeInstanceFromPool($singleval);
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
			ArWholesaleNumberPeer::clearRelatedInstancePool();
			$con->commit();
			return $affectedRows;
		} catch (PropelException $e) {
			$con->rollBack();
			throw $e;
		}
	}

	/**
	 * Validates all modified columns of given ArWholesaleNumber object.
	 * If parameter $columns is either a single column name or an array of column names
	 * than only those columns are validated.
	 *
	 * NOTICE: This does not apply to primary or foreign keys for now.
	 *
	 * @param      ArWholesaleNumber $obj The object to validate.
	 * @param      mixed $cols Column name or array of column names.
	 *
	 * @return     mixed TRUE if all columns are valid or the error message of the first invalid column.
	 */
	public static function doValidate(ArWholesaleNumber $obj, $cols = null)
	{
		$columns = array();

		if ($cols) {
			$dbMap = Propel::getDatabaseMap(ArWholesaleNumberPeer::DATABASE_NAME);
			$tableMap = $dbMap->getTable(ArWholesaleNumberPeer::TABLE_NAME);

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

		return BasePeer::doValidate(ArWholesaleNumberPeer::DATABASE_NAME, ArWholesaleNumberPeer::TABLE_NAME, $columns);
	}

	/**
	 * Retrieve a single object by pkey.
	 *
	 * @param      int $pk the primary key.
	 * @param      PropelPDO $con the connection to use
	 * @return     ArWholesaleNumber
	 */
	public static function retrieveByPK($pk, PropelPDO $con = null)
	{

		if (null !== ($obj = ArWholesaleNumberPeer::getInstanceFromPool((string) $pk))) {
			return $obj;
		}

		if ($con === null) {
			$con = Propel::getConnection(ArWholesaleNumberPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria = new Criteria(ArWholesaleNumberPeer::DATABASE_NAME);
		$criteria->add(ArWholesaleNumberPeer::ID, $pk);

		$v = ArWholesaleNumberPeer::doSelect($criteria, $con);

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
			$con = Propel::getConnection(ArWholesaleNumberPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$objs = null;
		if (empty($pks)) {
			$objs = array();
		} else {
			$criteria = new Criteria(ArWholesaleNumberPeer::DATABASE_NAME);
			$criteria->add(ArWholesaleNumberPeer::ID, $pks, Criteria::IN);
			$objs = ArWholesaleNumberPeer::doSelect($criteria, $con);
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

} // BaseArWholesaleNumberPeer

// This is the static code needed to register the TableMap for this table with the main Propel class.
//
BaseArWholesaleNumberPeer::buildTableMap();

