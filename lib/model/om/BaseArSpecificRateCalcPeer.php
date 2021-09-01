<?php

/**
 * Base static class for performing query and update operations on the 'ar_specific_rate_calc' table.
 *
 * 
 *
 * @package    lib.model.om
 */
abstract class BaseArSpecificRateCalcPeer {

	/** the default database name for this class */
	const DATABASE_NAME = 'propel';

	/** the table name for this class */
	const TABLE_NAME = 'ar_specific_rate_calc';

	/** the related Propel class for this table */
	const OM_CLASS = 'ArSpecificRateCalc';

	/** A class that can be returned by this peer. */
	const CLASS_DEFAULT = 'lib.model.ArSpecificRateCalc';

	/** the related TableMap class for this table */
	const TM_CLASS = 'ArSpecificRateCalcTableMap';
	
	/** The total number of columns. */
	const NUM_COLUMNS = 13;

	/** The number of lazy-loaded columns. */
	const NUM_LAZY_LOAD_COLUMNS = 0;

	/** the column name for the ID field */
	const ID = 'ar_specific_rate_calc.ID';

	/** the column name for the NOTE field */
	const NOTE = 'ar_specific_rate_calc.NOTE';

	/** the column name for the AR_RATE_ID field */
	const AR_RATE_ID = 'ar_specific_rate_calc.AR_RATE_ID';

	/** the column name for the SPECIFIC_RATE_NAME field */
	const SPECIFIC_RATE_NAME = 'ar_specific_rate_calc.SPECIFIC_RATE_NAME';

	/** the column name for the PRICE_CATEGORY_NAME field */
	const PRICE_CATEGORY_NAME = 'ar_specific_rate_calc.PRICE_CATEGORY_NAME';

	/** the column name for the MEDIUMTEXT_SPECIFIC_RATE_IN_MATCH_ALL field */
	const MEDIUMTEXT_SPECIFIC_RATE_IN_MATCH_ALL = 'ar_specific_rate_calc.MEDIUMTEXT_SPECIFIC_RATE_IN_MATCH_ALL';

	/** the column name for the MEDIUMTEXT_SPECIFIC_RATE_IN_MATCH_EXACT field */
	const MEDIUMTEXT_SPECIFIC_RATE_IN_MATCH_EXACT = 'ar_specific_rate_calc.MEDIUMTEXT_SPECIFIC_RATE_IN_MATCH_EXACT';

	/** the column name for the MEDIUMTEXT_SPECIFIC_RATE_OUT field */
	const MEDIUMTEXT_SPECIFIC_RATE_OUT = 'ar_specific_rate_calc.MEDIUMTEXT_SPECIFIC_RATE_OUT';

	/** the column name for the RATE_PLAN_OUT field */
	const RATE_PLAN_OUT = 'ar_specific_rate_calc.RATE_PLAN_OUT';

	/** the column name for the MEDIUMTEXT_BASE_RATE_DIFF field */
	const MEDIUMTEXT_BASE_RATE_DIFF = 'ar_specific_rate_calc.MEDIUMTEXT_BASE_RATE_DIFF';

	/** the column name for the CALC_INFO field */
	const CALC_INFO = 'ar_specific_rate_calc.CALC_INFO';

	/** the column name for the CALC_ERROR field */
	const CALC_ERROR = 'ar_specific_rate_calc.CALC_ERROR';

	/** the column name for the IS_RECALC field */
	const IS_RECALC = 'ar_specific_rate_calc.IS_RECALC';

	/**
	 * An identiy map to hold any loaded instances of ArSpecificRateCalc objects.
	 * This must be public so that other peer classes can access this when hydrating from JOIN
	 * queries.
	 * @var        array ArSpecificRateCalc[]
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
		BasePeer::TYPE_PHPNAME => array ('Id', 'Note', 'ArRateId', 'SpecificRateName', 'PriceCategoryName', 'MediumtextSpecificRateInMatchAll', 'MediumtextSpecificRateInMatchExact', 'MediumtextSpecificRateOut', 'RatePlanOut', 'MediumtextBaseRateDiff', 'CalcInfo', 'CalcError', 'IsRecalc', ),
		BasePeer::TYPE_STUDLYPHPNAME => array ('id', 'note', 'arRateId', 'specificRateName', 'priceCategoryName', 'mediumtextSpecificRateInMatchAll', 'mediumtextSpecificRateInMatchExact', 'mediumtextSpecificRateOut', 'ratePlanOut', 'mediumtextBaseRateDiff', 'calcInfo', 'calcError', 'isRecalc', ),
		BasePeer::TYPE_COLNAME => array (self::ID, self::NOTE, self::AR_RATE_ID, self::SPECIFIC_RATE_NAME, self::PRICE_CATEGORY_NAME, self::MEDIUMTEXT_SPECIFIC_RATE_IN_MATCH_ALL, self::MEDIUMTEXT_SPECIFIC_RATE_IN_MATCH_EXACT, self::MEDIUMTEXT_SPECIFIC_RATE_OUT, self::RATE_PLAN_OUT, self::MEDIUMTEXT_BASE_RATE_DIFF, self::CALC_INFO, self::CALC_ERROR, self::IS_RECALC, ),
		BasePeer::TYPE_FIELDNAME => array ('id', 'note', 'ar_rate_id', 'specific_rate_name', 'price_category_name', 'mediumtext_specific_rate_in_match_all', 'mediumtext_specific_rate_in_match_exact', 'mediumtext_specific_rate_out', 'rate_plan_out', 'mediumtext_base_rate_diff', 'calc_info', 'calc_error', 'is_recalc', ),
		BasePeer::TYPE_NUM => array (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, )
	);

	/**
	 * holds an array of keys for quick access to the fieldnames array
	 *
	 * first dimension keys are the type constants
	 * e.g. self::$fieldNames[BasePeer::TYPE_PHPNAME]['Id'] = 0
	 */
	private static $fieldKeys = array (
		BasePeer::TYPE_PHPNAME => array ('Id' => 0, 'Note' => 1, 'ArRateId' => 2, 'SpecificRateName' => 3, 'PriceCategoryName' => 4, 'MediumtextSpecificRateInMatchAll' => 5, 'MediumtextSpecificRateInMatchExact' => 6, 'MediumtextSpecificRateOut' => 7, 'RatePlanOut' => 8, 'MediumtextBaseRateDiff' => 9, 'CalcInfo' => 10, 'CalcError' => 11, 'IsRecalc' => 12, ),
		BasePeer::TYPE_STUDLYPHPNAME => array ('id' => 0, 'note' => 1, 'arRateId' => 2, 'specificRateName' => 3, 'priceCategoryName' => 4, 'mediumtextSpecificRateInMatchAll' => 5, 'mediumtextSpecificRateInMatchExact' => 6, 'mediumtextSpecificRateOut' => 7, 'ratePlanOut' => 8, 'mediumtextBaseRateDiff' => 9, 'calcInfo' => 10, 'calcError' => 11, 'isRecalc' => 12, ),
		BasePeer::TYPE_COLNAME => array (self::ID => 0, self::NOTE => 1, self::AR_RATE_ID => 2, self::SPECIFIC_RATE_NAME => 3, self::PRICE_CATEGORY_NAME => 4, self::MEDIUMTEXT_SPECIFIC_RATE_IN_MATCH_ALL => 5, self::MEDIUMTEXT_SPECIFIC_RATE_IN_MATCH_EXACT => 6, self::MEDIUMTEXT_SPECIFIC_RATE_OUT => 7, self::RATE_PLAN_OUT => 8, self::MEDIUMTEXT_BASE_RATE_DIFF => 9, self::CALC_INFO => 10, self::CALC_ERROR => 11, self::IS_RECALC => 12, ),
		BasePeer::TYPE_FIELDNAME => array ('id' => 0, 'note' => 1, 'ar_rate_id' => 2, 'specific_rate_name' => 3, 'price_category_name' => 4, 'mediumtext_specific_rate_in_match_all' => 5, 'mediumtext_specific_rate_in_match_exact' => 6, 'mediumtext_specific_rate_out' => 7, 'rate_plan_out' => 8, 'mediumtext_base_rate_diff' => 9, 'calc_info' => 10, 'calc_error' => 11, 'is_recalc' => 12, ),
		BasePeer::TYPE_NUM => array (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, )
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
	 * @param      string $column The column name for current table. (i.e. ArSpecificRateCalcPeer::COLUMN_NAME).
	 * @return     string
	 */
	public static function alias($alias, $column)
	{
		return str_replace(ArSpecificRateCalcPeer::TABLE_NAME.'.', $alias.'.', $column);
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
		$criteria->addSelectColumn(ArSpecificRateCalcPeer::ID);
		$criteria->addSelectColumn(ArSpecificRateCalcPeer::NOTE);
		$criteria->addSelectColumn(ArSpecificRateCalcPeer::AR_RATE_ID);
		$criteria->addSelectColumn(ArSpecificRateCalcPeer::SPECIFIC_RATE_NAME);
		$criteria->addSelectColumn(ArSpecificRateCalcPeer::PRICE_CATEGORY_NAME);
		$criteria->addSelectColumn(ArSpecificRateCalcPeer::MEDIUMTEXT_SPECIFIC_RATE_IN_MATCH_ALL);
		$criteria->addSelectColumn(ArSpecificRateCalcPeer::MEDIUMTEXT_SPECIFIC_RATE_IN_MATCH_EXACT);
		$criteria->addSelectColumn(ArSpecificRateCalcPeer::MEDIUMTEXT_SPECIFIC_RATE_OUT);
		$criteria->addSelectColumn(ArSpecificRateCalcPeer::RATE_PLAN_OUT);
		$criteria->addSelectColumn(ArSpecificRateCalcPeer::MEDIUMTEXT_BASE_RATE_DIFF);
		$criteria->addSelectColumn(ArSpecificRateCalcPeer::CALC_INFO);
		$criteria->addSelectColumn(ArSpecificRateCalcPeer::CALC_ERROR);
		$criteria->addSelectColumn(ArSpecificRateCalcPeer::IS_RECALC);
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
		$criteria->setPrimaryTableName(ArSpecificRateCalcPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArSpecificRateCalcPeer::addSelectColumns($criteria);
		}

		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		$criteria->setDbName(self::DATABASE_NAME); // Set the correct dbName

		if ($con === null) {
			$con = Propel::getConnection(ArSpecificRateCalcPeer::DATABASE_NAME, Propel::CONNECTION_READ);
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
	 * @return     ArSpecificRateCalc
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectOne(Criteria $criteria, PropelPDO $con = null)
	{
		$critcopy = clone $criteria;
		$critcopy->setLimit(1);
		$objects = ArSpecificRateCalcPeer::doSelect($critcopy, $con);
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
		return ArSpecificRateCalcPeer::populateObjects(ArSpecificRateCalcPeer::doSelectStmt($criteria, $con));
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
			$con = Propel::getConnection(ArSpecificRateCalcPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		if (!$criteria->hasSelectClause()) {
			$criteria = clone $criteria;
			ArSpecificRateCalcPeer::addSelectColumns($criteria);
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
	 * @param      ArSpecificRateCalc $value A ArSpecificRateCalc object.
	 * @param      string $key (optional) key to use for instance map (for performance boost if key was already calculated externally).
	 */
	public static function addInstanceToPool(ArSpecificRateCalc $obj, $key = null)
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
	 * @param      mixed $value A ArSpecificRateCalc object or a primary key value.
	 */
	public static function removeInstanceFromPool($value)
	{
		if (Propel::isInstancePoolingEnabled() && $value !== null) {
			if (is_object($value) && $value instanceof ArSpecificRateCalc) {
				$key = (string) $value->getId();
			} elseif (is_scalar($value)) {
				// assume we've been passed a primary key
				$key = (string) $value;
			} else {
				$e = new PropelException("Invalid value passed to removeInstanceFromPool().  Expected primary key or ArSpecificRateCalc object; got " . (is_object($value) ? get_class($value) . ' object.' : var_export($value,true)));
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
	 * @return     ArSpecificRateCalc Found object or NULL if 1) no instance exists for specified key or 2) instance pooling has been disabled.
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
	 * Method to invalidate the instance pool of all tables related to ar_specific_rate_calc
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
		$cls = ArSpecificRateCalcPeer::getOMClass(false);
		// populate the object(s)
		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key = ArSpecificRateCalcPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj = ArSpecificRateCalcPeer::getInstanceFromPool($key))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj->hydrate($row, 0, true); // rehydrate
				$results[] = $obj;
			} else {
				$obj = new $cls();
				$obj->hydrate($row);
				$results[] = $obj;
				ArSpecificRateCalcPeer::addInstanceToPool($obj, $key);
			} // if key exists
		}
		$stmt->closeCursor();
		return $results;
	}

	/**
	 * Returns the number of rows matching criteria, joining the related ArRate table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinArRate(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArSpecificRateCalcPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArSpecificRateCalcPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArSpecificRateCalcPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArSpecificRateCalcPeer::AR_RATE_ID, ArRatePeer::ID, $join_behavior);

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
	 * Selects a collection of ArSpecificRateCalc objects pre-filled with their ArRate objects.
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArSpecificRateCalc objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinArRate(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArSpecificRateCalcPeer::addSelectColumns($criteria);
		$startcol = (ArSpecificRateCalcPeer::NUM_COLUMNS - ArSpecificRateCalcPeer::NUM_LAZY_LOAD_COLUMNS);
		ArRatePeer::addSelectColumns($criteria);

		$criteria->addJoin(ArSpecificRateCalcPeer::AR_RATE_ID, ArRatePeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArSpecificRateCalcPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArSpecificRateCalcPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {

				$cls = ArSpecificRateCalcPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArSpecificRateCalcPeer::addInstanceToPool($obj1, $key1);
			} // if $obj1 already loaded

			$key2 = ArRatePeer::getPrimaryKeyHashFromRow($row, $startcol);
			if ($key2 !== null) {
				$obj2 = ArRatePeer::getInstanceFromPool($key2);
				if (!$obj2) {

					$cls = ArRatePeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol);
					ArRatePeer::addInstanceToPool($obj2, $key2);
				} // if obj2 already loaded
				
				// Add the $obj1 (ArSpecificRateCalc) to $obj2 (ArRate)
				$obj2->addArSpecificRateCalc($obj1);

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
		$criteria->setPrimaryTableName(ArSpecificRateCalcPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArSpecificRateCalcPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArSpecificRateCalcPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArSpecificRateCalcPeer::AR_RATE_ID, ArRatePeer::ID, $join_behavior);

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
	 * Selects a collection of ArSpecificRateCalc objects pre-filled with all related objects.
	 *
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArSpecificRateCalc objects.
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

		ArSpecificRateCalcPeer::addSelectColumns($criteria);
		$startcol2 = (ArSpecificRateCalcPeer::NUM_COLUMNS - ArSpecificRateCalcPeer::NUM_LAZY_LOAD_COLUMNS);

		ArRatePeer::addSelectColumns($criteria);
		$startcol3 = $startcol2 + (ArRatePeer::NUM_COLUMNS - ArRatePeer::NUM_LAZY_LOAD_COLUMNS);

		$criteria->addJoin(ArSpecificRateCalcPeer::AR_RATE_ID, ArRatePeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArSpecificRateCalcPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArSpecificRateCalcPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {
				$cls = ArSpecificRateCalcPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArSpecificRateCalcPeer::addInstanceToPool($obj1, $key1);
			} // if obj1 already loaded

			// Add objects for joined ArRate rows

			$key2 = ArRatePeer::getPrimaryKeyHashFromRow($row, $startcol2);
			if ($key2 !== null) {
				$obj2 = ArRatePeer::getInstanceFromPool($key2);
				if (!$obj2) {

					$cls = ArRatePeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol2);
					ArRatePeer::addInstanceToPool($obj2, $key2);
				} // if obj2 loaded

				// Add the $obj1 (ArSpecificRateCalc) to the collection in $obj2 (ArRate)
				$obj2->addArSpecificRateCalc($obj1);
			} // if joined row not null

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
	  $dbMap = Propel::getDatabaseMap(BaseArSpecificRateCalcPeer::DATABASE_NAME);
	  if (!$dbMap->hasTable(BaseArSpecificRateCalcPeer::TABLE_NAME))
	  {
	    $dbMap->addTableObject(new ArSpecificRateCalcTableMap());
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
		return $withPrefix ? ArSpecificRateCalcPeer::CLASS_DEFAULT : ArSpecificRateCalcPeer::OM_CLASS;
	}

	/**
	 * Method perform an INSERT on the database, given a ArSpecificRateCalc or Criteria object.
	 *
	 * @param      mixed $values Criteria or ArSpecificRateCalc object containing data that is used to create the INSERT statement.
	 * @param      PropelPDO $con the PropelPDO connection to use
	 * @return     mixed The new primary key.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doInsert($values, PropelPDO $con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArSpecificRateCalcPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		if ($values instanceof Criteria) {
			$criteria = clone $values; // rename for clarity
		} else {
			$criteria = $values->buildCriteria(); // build Criteria from ArSpecificRateCalc object
		}

		if ($criteria->containsKey(ArSpecificRateCalcPeer::ID) && $criteria->keyContainsValue(ArSpecificRateCalcPeer::ID) ) {
			throw new PropelException('Cannot insert a value for auto-increment primary key ('.ArSpecificRateCalcPeer::ID.')');
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
	 * Method perform an UPDATE on the database, given a ArSpecificRateCalc or Criteria object.
	 *
	 * @param      mixed $values Criteria or ArSpecificRateCalc object containing data that is used to create the UPDATE statement.
	 * @param      PropelPDO $con The connection to use (specify PropelPDO connection object to exert more control over transactions).
	 * @return     int The number of affected rows (if supported by underlying database driver).
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doUpdate($values, PropelPDO $con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArSpecificRateCalcPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		$selectCriteria = new Criteria(self::DATABASE_NAME);

		if ($values instanceof Criteria) {
			$criteria = clone $values; // rename for clarity

			$comparison = $criteria->getComparison(ArSpecificRateCalcPeer::ID);
			$selectCriteria->add(ArSpecificRateCalcPeer::ID, $criteria->remove(ArSpecificRateCalcPeer::ID), $comparison);

		} else { // $values is ArSpecificRateCalc object
			$criteria = $values->buildCriteria(); // gets full criteria
			$selectCriteria = $values->buildPkeyCriteria(); // gets criteria w/ primary key(s)
		}

		// set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		return BasePeer::doUpdate($selectCriteria, $criteria, $con);
	}

	/**
	 * Method to DELETE all rows from the ar_specific_rate_calc table.
	 *
	 * @return     int The number of affected rows (if supported by underlying database driver).
	 */
	public static function doDeleteAll($con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArSpecificRateCalcPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}
		$affectedRows = 0; // initialize var to track total num of affected rows
		try {
			// use transaction because $criteria could contain info
			// for more than one table or we could emulating ON DELETE CASCADE, etc.
			$con->beginTransaction();
			$affectedRows += BasePeer::doDeleteAll(ArSpecificRateCalcPeer::TABLE_NAME, $con);
			// Because this db requires some delete cascade/set null emulation, we have to
			// clear the cached instance *after* the emulation has happened (since
			// instances get re-added by the select statement contained therein).
			ArSpecificRateCalcPeer::clearInstancePool();
			ArSpecificRateCalcPeer::clearRelatedInstancePool();
			$con->commit();
			return $affectedRows;
		} catch (PropelException $e) {
			$con->rollBack();
			throw $e;
		}
	}

	/**
	 * Method perform a DELETE on the database, given a ArSpecificRateCalc or Criteria object OR a primary key value.
	 *
	 * @param      mixed $values Criteria or ArSpecificRateCalc object or primary key or array of primary keys
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
			$con = Propel::getConnection(ArSpecificRateCalcPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		if ($values instanceof Criteria) {
			// invalidate the cache for all objects of this type, since we have no
			// way of knowing (without running a query) what objects should be invalidated
			// from the cache based on this Criteria.
			ArSpecificRateCalcPeer::clearInstancePool();
			// rename for clarity
			$criteria = clone $values;
		} elseif ($values instanceof ArSpecificRateCalc) { // it's a model object
			// invalidate the cache for this single object
			ArSpecificRateCalcPeer::removeInstanceFromPool($values);
			// create criteria based on pk values
			$criteria = $values->buildPkeyCriteria();
		} else { // it's a primary key, or an array of pks
			$criteria = new Criteria(self::DATABASE_NAME);
			$criteria->add(ArSpecificRateCalcPeer::ID, (array) $values, Criteria::IN);
			// invalidate the cache for this object(s)
			foreach ((array) $values as $singleval) {
				ArSpecificRateCalcPeer::removeInstanceFromPool($singleval);
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
			ArSpecificRateCalcPeer::clearRelatedInstancePool();
			$con->commit();
			return $affectedRows;
		} catch (PropelException $e) {
			$con->rollBack();
			throw $e;
		}
	}

	/**
	 * Validates all modified columns of given ArSpecificRateCalc object.
	 * If parameter $columns is either a single column name or an array of column names
	 * than only those columns are validated.
	 *
	 * NOTICE: This does not apply to primary or foreign keys for now.
	 *
	 * @param      ArSpecificRateCalc $obj The object to validate.
	 * @param      mixed $cols Column name or array of column names.
	 *
	 * @return     mixed TRUE if all columns are valid or the error message of the first invalid column.
	 */
	public static function doValidate(ArSpecificRateCalc $obj, $cols = null)
	{
		$columns = array();

		if ($cols) {
			$dbMap = Propel::getDatabaseMap(ArSpecificRateCalcPeer::DATABASE_NAME);
			$tableMap = $dbMap->getTable(ArSpecificRateCalcPeer::TABLE_NAME);

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

		return BasePeer::doValidate(ArSpecificRateCalcPeer::DATABASE_NAME, ArSpecificRateCalcPeer::TABLE_NAME, $columns);
	}

	/**
	 * Retrieve a single object by pkey.
	 *
	 * @param      int $pk the primary key.
	 * @param      PropelPDO $con the connection to use
	 * @return     ArSpecificRateCalc
	 */
	public static function retrieveByPK($pk, PropelPDO $con = null)
	{

		if (null !== ($obj = ArSpecificRateCalcPeer::getInstanceFromPool((string) $pk))) {
			return $obj;
		}

		if ($con === null) {
			$con = Propel::getConnection(ArSpecificRateCalcPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria = new Criteria(ArSpecificRateCalcPeer::DATABASE_NAME);
		$criteria->add(ArSpecificRateCalcPeer::ID, $pk);

		$v = ArSpecificRateCalcPeer::doSelect($criteria, $con);

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
			$con = Propel::getConnection(ArSpecificRateCalcPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$objs = null;
		if (empty($pks)) {
			$objs = array();
		} else {
			$criteria = new Criteria(ArSpecificRateCalcPeer::DATABASE_NAME);
			$criteria->add(ArSpecificRateCalcPeer::ID, $pks, Criteria::IN);
			$objs = ArSpecificRateCalcPeer::doSelect($criteria, $con);
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

} // BaseArSpecificRateCalcPeer

// This is the static code needed to register the TableMap for this table with the main Propel class.
//
BaseArSpecificRateCalcPeer::buildTableMap();

