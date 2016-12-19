<?php

/**
 * Base static class for performing query and update operations on the 'ar_organization_unit_has_structure' table.
 *
 * 
 *
 * @package    lib.model.om
 */
abstract class BaseArOrganizationUnitHasStructurePeer {

	/** the default database name for this class */
	const DATABASE_NAME = 'propel';

	/** the table name for this class */
	const TABLE_NAME = 'ar_organization_unit_has_structure';

	/** the related Propel class for this table */
	const OM_CLASS = 'ArOrganizationUnitHasStructure';

	/** A class that can be returned by this peer. */
	const CLASS_DEFAULT = 'lib.model.ArOrganizationUnitHasStructure';

	/** the related TableMap class for this table */
	const TM_CLASS = 'ArOrganizationUnitHasStructureTableMap';
	
	/** The total number of columns. */
	const NUM_COLUMNS = 11;

	/** The number of lazy-loaded columns. */
	const NUM_LAZY_LOAD_COLUMNS = 0;

	/** the column name for the ID field */
	const ID = 'ar_organization_unit_has_structure.ID';

	/** the column name for the AR_ORGANIZATION_UNIT_ID field */
	const AR_ORGANIZATION_UNIT_ID = 'ar_organization_unit_has_structure.AR_ORGANIZATION_UNIT_ID';

	/** the column name for the AR_ORGANIZATION_UNIT_TYPE_ID field */
	const AR_ORGANIZATION_UNIT_TYPE_ID = 'ar_organization_unit_has_structure.AR_ORGANIZATION_UNIT_TYPE_ID';

	/** the column name for the AR_PARENT_ORGANIZATION_UNIT_ID field */
	const AR_PARENT_ORGANIZATION_UNIT_ID = 'ar_organization_unit_has_structure.AR_PARENT_ORGANIZATION_UNIT_ID';

	/** the column name for the FROM field */
	const FROM = 'ar_organization_unit_has_structure.FROM';

	/** the column name for the EXISTS field */
	const EXISTS = 'ar_organization_unit_has_structure.EXISTS';

	/** the column name for the AR_RATE_CATEGORY_ID field */
	const AR_RATE_CATEGORY_ID = 'ar_organization_unit_has_structure.AR_RATE_CATEGORY_ID';

	/** the column name for the AR_PARTY_ID field */
	const AR_PARTY_ID = 'ar_organization_unit_has_structure.AR_PARTY_ID';

	/** the column name for the EXTENSION_CODES field */
	const EXTENSION_CODES = 'ar_organization_unit_has_structure.EXTENSION_CODES';

	/** the column name for the EXTENSION_NAME field */
	const EXTENSION_NAME = 'ar_organization_unit_has_structure.EXTENSION_NAME';

	/** the column name for the EXTENSION_USER_CODE field */
	const EXTENSION_USER_CODE = 'ar_organization_unit_has_structure.EXTENSION_USER_CODE';

	/**
	 * An identiy map to hold any loaded instances of ArOrganizationUnitHasStructure objects.
	 * This must be public so that other peer classes can access this when hydrating from JOIN
	 * queries.
	 * @var        array ArOrganizationUnitHasStructure[]
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
		BasePeer::TYPE_PHPNAME => array ('Id', 'ArOrganizationUnitId', 'ArOrganizationUnitTypeId', 'ArParentOrganizationUnitId', 'From', 'Exists', 'ArRateCategoryId', 'ArPartyId', 'ExtensionCodes', 'ExtensionName', 'ExtensionUserCode', ),
		BasePeer::TYPE_STUDLYPHPNAME => array ('id', 'arOrganizationUnitId', 'arOrganizationUnitTypeId', 'arParentOrganizationUnitId', 'from', 'exists', 'arRateCategoryId', 'arPartyId', 'extensionCodes', 'extensionName', 'extensionUserCode', ),
		BasePeer::TYPE_COLNAME => array (self::ID, self::AR_ORGANIZATION_UNIT_ID, self::AR_ORGANIZATION_UNIT_TYPE_ID, self::AR_PARENT_ORGANIZATION_UNIT_ID, self::FROM, self::EXISTS, self::AR_RATE_CATEGORY_ID, self::AR_PARTY_ID, self::EXTENSION_CODES, self::EXTENSION_NAME, self::EXTENSION_USER_CODE, ),
		BasePeer::TYPE_FIELDNAME => array ('id', 'ar_organization_unit_id', 'ar_organization_unit_type_id', 'ar_parent_organization_unit_id', 'from', 'exists', 'ar_rate_category_id', 'ar_party_id', 'extension_codes', 'extension_name', 'extension_user_code', ),
		BasePeer::TYPE_NUM => array (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, )
	);

	/**
	 * holds an array of keys for quick access to the fieldnames array
	 *
	 * first dimension keys are the type constants
	 * e.g. self::$fieldNames[BasePeer::TYPE_PHPNAME]['Id'] = 0
	 */
	private static $fieldKeys = array (
		BasePeer::TYPE_PHPNAME => array ('Id' => 0, 'ArOrganizationUnitId' => 1, 'ArOrganizationUnitTypeId' => 2, 'ArParentOrganizationUnitId' => 3, 'From' => 4, 'Exists' => 5, 'ArRateCategoryId' => 6, 'ArPartyId' => 7, 'ExtensionCodes' => 8, 'ExtensionName' => 9, 'ExtensionUserCode' => 10, ),
		BasePeer::TYPE_STUDLYPHPNAME => array ('id' => 0, 'arOrganizationUnitId' => 1, 'arOrganizationUnitTypeId' => 2, 'arParentOrganizationUnitId' => 3, 'from' => 4, 'exists' => 5, 'arRateCategoryId' => 6, 'arPartyId' => 7, 'extensionCodes' => 8, 'extensionName' => 9, 'extensionUserCode' => 10, ),
		BasePeer::TYPE_COLNAME => array (self::ID => 0, self::AR_ORGANIZATION_UNIT_ID => 1, self::AR_ORGANIZATION_UNIT_TYPE_ID => 2, self::AR_PARENT_ORGANIZATION_UNIT_ID => 3, self::FROM => 4, self::EXISTS => 5, self::AR_RATE_CATEGORY_ID => 6, self::AR_PARTY_ID => 7, self::EXTENSION_CODES => 8, self::EXTENSION_NAME => 9, self::EXTENSION_USER_CODE => 10, ),
		BasePeer::TYPE_FIELDNAME => array ('id' => 0, 'ar_organization_unit_id' => 1, 'ar_organization_unit_type_id' => 2, 'ar_parent_organization_unit_id' => 3, 'from' => 4, 'exists' => 5, 'ar_rate_category_id' => 6, 'ar_party_id' => 7, 'extension_codes' => 8, 'extension_name' => 9, 'extension_user_code' => 10, ),
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
	 * @param      string $column The column name for current table. (i.e. ArOrganizationUnitHasStructurePeer::COLUMN_NAME).
	 * @return     string
	 */
	public static function alias($alias, $column)
	{
		return str_replace(ArOrganizationUnitHasStructurePeer::TABLE_NAME.'.', $alias.'.', $column);
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
		$criteria->addSelectColumn(ArOrganizationUnitHasStructurePeer::ID);
		$criteria->addSelectColumn(ArOrganizationUnitHasStructurePeer::AR_ORGANIZATION_UNIT_ID);
		$criteria->addSelectColumn(ArOrganizationUnitHasStructurePeer::AR_ORGANIZATION_UNIT_TYPE_ID);
		$criteria->addSelectColumn(ArOrganizationUnitHasStructurePeer::AR_PARENT_ORGANIZATION_UNIT_ID);
		$criteria->addSelectColumn(ArOrganizationUnitHasStructurePeer::FROM);
		$criteria->addSelectColumn(ArOrganizationUnitHasStructurePeer::EXISTS);
		$criteria->addSelectColumn(ArOrganizationUnitHasStructurePeer::AR_RATE_CATEGORY_ID);
		$criteria->addSelectColumn(ArOrganizationUnitHasStructurePeer::AR_PARTY_ID);
		$criteria->addSelectColumn(ArOrganizationUnitHasStructurePeer::EXTENSION_CODES);
		$criteria->addSelectColumn(ArOrganizationUnitHasStructurePeer::EXTENSION_NAME);
		$criteria->addSelectColumn(ArOrganizationUnitHasStructurePeer::EXTENSION_USER_CODE);
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
		$criteria->setPrimaryTableName(ArOrganizationUnitHasStructurePeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArOrganizationUnitHasStructurePeer::addSelectColumns($criteria);
		}

		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		$criteria->setDbName(self::DATABASE_NAME); // Set the correct dbName

		if ($con === null) {
			$con = Propel::getConnection(ArOrganizationUnitHasStructurePeer::DATABASE_NAME, Propel::CONNECTION_READ);
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
	 * @return     ArOrganizationUnitHasStructure
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectOne(Criteria $criteria, PropelPDO $con = null)
	{
		$critcopy = clone $criteria;
		$critcopy->setLimit(1);
		$objects = ArOrganizationUnitHasStructurePeer::doSelect($critcopy, $con);
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
		return ArOrganizationUnitHasStructurePeer::populateObjects(ArOrganizationUnitHasStructurePeer::doSelectStmt($criteria, $con));
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
			$con = Propel::getConnection(ArOrganizationUnitHasStructurePeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		if (!$criteria->hasSelectClause()) {
			$criteria = clone $criteria;
			ArOrganizationUnitHasStructurePeer::addSelectColumns($criteria);
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
	 * @param      ArOrganizationUnitHasStructure $value A ArOrganizationUnitHasStructure object.
	 * @param      string $key (optional) key to use for instance map (for performance boost if key was already calculated externally).
	 */
	public static function addInstanceToPool(ArOrganizationUnitHasStructure $obj, $key = null)
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
	 * @param      mixed $value A ArOrganizationUnitHasStructure object or a primary key value.
	 */
	public static function removeInstanceFromPool($value)
	{
		if (Propel::isInstancePoolingEnabled() && $value !== null) {
			if (is_object($value) && $value instanceof ArOrganizationUnitHasStructure) {
				$key = (string) $value->getId();
			} elseif (is_scalar($value)) {
				// assume we've been passed a primary key
				$key = (string) $value;
			} else {
				$e = new PropelException("Invalid value passed to removeInstanceFromPool().  Expected primary key or ArOrganizationUnitHasStructure object; got " . (is_object($value) ? get_class($value) . ' object.' : var_export($value,true)));
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
	 * @return     ArOrganizationUnitHasStructure Found object or NULL if 1) no instance exists for specified key or 2) instance pooling has been disabled.
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
	 * Method to invalidate the instance pool of all tables related to ar_organization_unit_has_structure
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
		$cls = ArOrganizationUnitHasStructurePeer::getOMClass(false);
		// populate the object(s)
		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key = ArOrganizationUnitHasStructurePeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj = ArOrganizationUnitHasStructurePeer::getInstanceFromPool($key))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj->hydrate($row, 0, true); // rehydrate
				$results[] = $obj;
			} else {
				$obj = new $cls();
				$obj->hydrate($row);
				$results[] = $obj;
				ArOrganizationUnitHasStructurePeer::addInstanceToPool($obj, $key);
			} // if key exists
		}
		$stmt->closeCursor();
		return $results;
	}

	/**
	 * Returns the number of rows matching criteria, joining the related ArOrganizationUnitRelatedByArOrganizationUnitId table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinArOrganizationUnitRelatedByArOrganizationUnitId(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArOrganizationUnitHasStructurePeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArOrganizationUnitHasStructurePeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArOrganizationUnitHasStructurePeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

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
	 * Returns the number of rows matching criteria, joining the related ArOrganizationUnitType table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinArOrganizationUnitType(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArOrganizationUnitHasStructurePeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArOrganizationUnitHasStructurePeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArOrganizationUnitHasStructurePeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_ORGANIZATION_UNIT_TYPE_ID, ArOrganizationUnitTypePeer::ID, $join_behavior);

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
	 * Returns the number of rows matching criteria, joining the related ArOrganizationUnitRelatedByArParentOrganizationUnitId table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinArOrganizationUnitRelatedByArParentOrganizationUnitId(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArOrganizationUnitHasStructurePeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArOrganizationUnitHasStructurePeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArOrganizationUnitHasStructurePeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_PARENT_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

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
	 * Returns the number of rows matching criteria, joining the related ArRateCategory table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinArRateCategory(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArOrganizationUnitHasStructurePeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArOrganizationUnitHasStructurePeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArOrganizationUnitHasStructurePeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_RATE_CATEGORY_ID, ArRateCategoryPeer::ID, $join_behavior);

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
	 * Returns the number of rows matching criteria, joining the related ArParty table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinArParty(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArOrganizationUnitHasStructurePeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArOrganizationUnitHasStructurePeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArOrganizationUnitHasStructurePeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_PARTY_ID, ArPartyPeer::ID, $join_behavior);

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
	 * Selects a collection of ArOrganizationUnitHasStructure objects pre-filled with their ArOrganizationUnit objects.
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArOrganizationUnitHasStructure objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinArOrganizationUnitRelatedByArOrganizationUnitId(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArOrganizationUnitHasStructurePeer::addSelectColumns($criteria);
		$startcol = (ArOrganizationUnitHasStructurePeer::NUM_COLUMNS - ArOrganizationUnitHasStructurePeer::NUM_LAZY_LOAD_COLUMNS);
		ArOrganizationUnitPeer::addSelectColumns($criteria);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArOrganizationUnitHasStructurePeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArOrganizationUnitHasStructurePeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {

				$cls = ArOrganizationUnitHasStructurePeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArOrganizationUnitHasStructurePeer::addInstanceToPool($obj1, $key1);
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
				
				// Add the $obj1 (ArOrganizationUnitHasStructure) to $obj2 (ArOrganizationUnit)
				$obj2->addArOrganizationUnitHasStructureRelatedByArOrganizationUnitId($obj1);

			} // if joined row was not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArOrganizationUnitHasStructure objects pre-filled with their ArOrganizationUnitType objects.
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArOrganizationUnitHasStructure objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinArOrganizationUnitType(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArOrganizationUnitHasStructurePeer::addSelectColumns($criteria);
		$startcol = (ArOrganizationUnitHasStructurePeer::NUM_COLUMNS - ArOrganizationUnitHasStructurePeer::NUM_LAZY_LOAD_COLUMNS);
		ArOrganizationUnitTypePeer::addSelectColumns($criteria);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_ORGANIZATION_UNIT_TYPE_ID, ArOrganizationUnitTypePeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArOrganizationUnitHasStructurePeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArOrganizationUnitHasStructurePeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {

				$cls = ArOrganizationUnitHasStructurePeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArOrganizationUnitHasStructurePeer::addInstanceToPool($obj1, $key1);
			} // if $obj1 already loaded

			$key2 = ArOrganizationUnitTypePeer::getPrimaryKeyHashFromRow($row, $startcol);
			if ($key2 !== null) {
				$obj2 = ArOrganizationUnitTypePeer::getInstanceFromPool($key2);
				if (!$obj2) {

					$cls = ArOrganizationUnitTypePeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol);
					ArOrganizationUnitTypePeer::addInstanceToPool($obj2, $key2);
				} // if obj2 already loaded
				
				// Add the $obj1 (ArOrganizationUnitHasStructure) to $obj2 (ArOrganizationUnitType)
				$obj2->addArOrganizationUnitHasStructure($obj1);

			} // if joined row was not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArOrganizationUnitHasStructure objects pre-filled with their ArOrganizationUnit objects.
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArOrganizationUnitHasStructure objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinArOrganizationUnitRelatedByArParentOrganizationUnitId(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArOrganizationUnitHasStructurePeer::addSelectColumns($criteria);
		$startcol = (ArOrganizationUnitHasStructurePeer::NUM_COLUMNS - ArOrganizationUnitHasStructurePeer::NUM_LAZY_LOAD_COLUMNS);
		ArOrganizationUnitPeer::addSelectColumns($criteria);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_PARENT_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArOrganizationUnitHasStructurePeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArOrganizationUnitHasStructurePeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {

				$cls = ArOrganizationUnitHasStructurePeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArOrganizationUnitHasStructurePeer::addInstanceToPool($obj1, $key1);
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
				
				// Add the $obj1 (ArOrganizationUnitHasStructure) to $obj2 (ArOrganizationUnit)
				$obj2->addArOrganizationUnitHasStructureRelatedByArParentOrganizationUnitId($obj1);

			} // if joined row was not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArOrganizationUnitHasStructure objects pre-filled with their ArRateCategory objects.
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArOrganizationUnitHasStructure objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinArRateCategory(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArOrganizationUnitHasStructurePeer::addSelectColumns($criteria);
		$startcol = (ArOrganizationUnitHasStructurePeer::NUM_COLUMNS - ArOrganizationUnitHasStructurePeer::NUM_LAZY_LOAD_COLUMNS);
		ArRateCategoryPeer::addSelectColumns($criteria);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_RATE_CATEGORY_ID, ArRateCategoryPeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArOrganizationUnitHasStructurePeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArOrganizationUnitHasStructurePeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {

				$cls = ArOrganizationUnitHasStructurePeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArOrganizationUnitHasStructurePeer::addInstanceToPool($obj1, $key1);
			} // if $obj1 already loaded

			$key2 = ArRateCategoryPeer::getPrimaryKeyHashFromRow($row, $startcol);
			if ($key2 !== null) {
				$obj2 = ArRateCategoryPeer::getInstanceFromPool($key2);
				if (!$obj2) {

					$cls = ArRateCategoryPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol);
					ArRateCategoryPeer::addInstanceToPool($obj2, $key2);
				} // if obj2 already loaded
				
				// Add the $obj1 (ArOrganizationUnitHasStructure) to $obj2 (ArRateCategory)
				$obj2->addArOrganizationUnitHasStructure($obj1);

			} // if joined row was not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArOrganizationUnitHasStructure objects pre-filled with their ArParty objects.
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArOrganizationUnitHasStructure objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinArParty(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArOrganizationUnitHasStructurePeer::addSelectColumns($criteria);
		$startcol = (ArOrganizationUnitHasStructurePeer::NUM_COLUMNS - ArOrganizationUnitHasStructurePeer::NUM_LAZY_LOAD_COLUMNS);
		ArPartyPeer::addSelectColumns($criteria);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_PARTY_ID, ArPartyPeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArOrganizationUnitHasStructurePeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArOrganizationUnitHasStructurePeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {

				$cls = ArOrganizationUnitHasStructurePeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArOrganizationUnitHasStructurePeer::addInstanceToPool($obj1, $key1);
			} // if $obj1 already loaded

			$key2 = ArPartyPeer::getPrimaryKeyHashFromRow($row, $startcol);
			if ($key2 !== null) {
				$obj2 = ArPartyPeer::getInstanceFromPool($key2);
				if (!$obj2) {

					$cls = ArPartyPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol);
					ArPartyPeer::addInstanceToPool($obj2, $key2);
				} // if obj2 already loaded
				
				// Add the $obj1 (ArOrganizationUnitHasStructure) to $obj2 (ArParty)
				$obj2->addArOrganizationUnitHasStructure($obj1);

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
		$criteria->setPrimaryTableName(ArOrganizationUnitHasStructurePeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArOrganizationUnitHasStructurePeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArOrganizationUnitHasStructurePeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_ORGANIZATION_UNIT_TYPE_ID, ArOrganizationUnitTypePeer::ID, $join_behavior);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_PARENT_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_RATE_CATEGORY_ID, ArRateCategoryPeer::ID, $join_behavior);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_PARTY_ID, ArPartyPeer::ID, $join_behavior);

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
	 * Selects a collection of ArOrganizationUnitHasStructure objects pre-filled with all related objects.
	 *
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArOrganizationUnitHasStructure objects.
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

		ArOrganizationUnitHasStructurePeer::addSelectColumns($criteria);
		$startcol2 = (ArOrganizationUnitHasStructurePeer::NUM_COLUMNS - ArOrganizationUnitHasStructurePeer::NUM_LAZY_LOAD_COLUMNS);

		ArOrganizationUnitPeer::addSelectColumns($criteria);
		$startcol3 = $startcol2 + (ArOrganizationUnitPeer::NUM_COLUMNS - ArOrganizationUnitPeer::NUM_LAZY_LOAD_COLUMNS);

		ArOrganizationUnitTypePeer::addSelectColumns($criteria);
		$startcol4 = $startcol3 + (ArOrganizationUnitTypePeer::NUM_COLUMNS - ArOrganizationUnitTypePeer::NUM_LAZY_LOAD_COLUMNS);

		ArOrganizationUnitPeer::addSelectColumns($criteria);
		$startcol5 = $startcol4 + (ArOrganizationUnitPeer::NUM_COLUMNS - ArOrganizationUnitPeer::NUM_LAZY_LOAD_COLUMNS);

		ArRateCategoryPeer::addSelectColumns($criteria);
		$startcol6 = $startcol5 + (ArRateCategoryPeer::NUM_COLUMNS - ArRateCategoryPeer::NUM_LAZY_LOAD_COLUMNS);

		ArPartyPeer::addSelectColumns($criteria);
		$startcol7 = $startcol6 + (ArPartyPeer::NUM_COLUMNS - ArPartyPeer::NUM_LAZY_LOAD_COLUMNS);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_ORGANIZATION_UNIT_TYPE_ID, ArOrganizationUnitTypePeer::ID, $join_behavior);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_PARENT_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_RATE_CATEGORY_ID, ArRateCategoryPeer::ID, $join_behavior);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_PARTY_ID, ArPartyPeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArOrganizationUnitHasStructurePeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArOrganizationUnitHasStructurePeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {
				$cls = ArOrganizationUnitHasStructurePeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArOrganizationUnitHasStructurePeer::addInstanceToPool($obj1, $key1);
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

				// Add the $obj1 (ArOrganizationUnitHasStructure) to the collection in $obj2 (ArOrganizationUnit)
				$obj2->addArOrganizationUnitHasStructureRelatedByArOrganizationUnitId($obj1);
			} // if joined row not null

			// Add objects for joined ArOrganizationUnitType rows

			$key3 = ArOrganizationUnitTypePeer::getPrimaryKeyHashFromRow($row, $startcol3);
			if ($key3 !== null) {
				$obj3 = ArOrganizationUnitTypePeer::getInstanceFromPool($key3);
				if (!$obj3) {

					$cls = ArOrganizationUnitTypePeer::getOMClass(false);

					$obj3 = new $cls();
					$obj3->hydrate($row, $startcol3);
					ArOrganizationUnitTypePeer::addInstanceToPool($obj3, $key3);
				} // if obj3 loaded

				// Add the $obj1 (ArOrganizationUnitHasStructure) to the collection in $obj3 (ArOrganizationUnitType)
				$obj3->addArOrganizationUnitHasStructure($obj1);
			} // if joined row not null

			// Add objects for joined ArOrganizationUnit rows

			$key4 = ArOrganizationUnitPeer::getPrimaryKeyHashFromRow($row, $startcol4);
			if ($key4 !== null) {
				$obj4 = ArOrganizationUnitPeer::getInstanceFromPool($key4);
				if (!$obj4) {

					$cls = ArOrganizationUnitPeer::getOMClass(false);

					$obj4 = new $cls();
					$obj4->hydrate($row, $startcol4);
					ArOrganizationUnitPeer::addInstanceToPool($obj4, $key4);
				} // if obj4 loaded

				// Add the $obj1 (ArOrganizationUnitHasStructure) to the collection in $obj4 (ArOrganizationUnit)
				$obj4->addArOrganizationUnitHasStructureRelatedByArParentOrganizationUnitId($obj1);
			} // if joined row not null

			// Add objects for joined ArRateCategory rows

			$key5 = ArRateCategoryPeer::getPrimaryKeyHashFromRow($row, $startcol5);
			if ($key5 !== null) {
				$obj5 = ArRateCategoryPeer::getInstanceFromPool($key5);
				if (!$obj5) {

					$cls = ArRateCategoryPeer::getOMClass(false);

					$obj5 = new $cls();
					$obj5->hydrate($row, $startcol5);
					ArRateCategoryPeer::addInstanceToPool($obj5, $key5);
				} // if obj5 loaded

				// Add the $obj1 (ArOrganizationUnitHasStructure) to the collection in $obj5 (ArRateCategory)
				$obj5->addArOrganizationUnitHasStructure($obj1);
			} // if joined row not null

			// Add objects for joined ArParty rows

			$key6 = ArPartyPeer::getPrimaryKeyHashFromRow($row, $startcol6);
			if ($key6 !== null) {
				$obj6 = ArPartyPeer::getInstanceFromPool($key6);
				if (!$obj6) {

					$cls = ArPartyPeer::getOMClass(false);

					$obj6 = new $cls();
					$obj6->hydrate($row, $startcol6);
					ArPartyPeer::addInstanceToPool($obj6, $key6);
				} // if obj6 loaded

				// Add the $obj1 (ArOrganizationUnitHasStructure) to the collection in $obj6 (ArParty)
				$obj6->addArOrganizationUnitHasStructure($obj1);
			} // if joined row not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Returns the number of rows matching criteria, joining the related ArOrganizationUnitRelatedByArOrganizationUnitId table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinAllExceptArOrganizationUnitRelatedByArOrganizationUnitId(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArOrganizationUnitHasStructurePeer::TABLE_NAME);
		
		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArOrganizationUnitHasStructurePeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY should not affect count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArOrganizationUnitHasStructurePeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}
	
		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_ORGANIZATION_UNIT_TYPE_ID, ArOrganizationUnitTypePeer::ID, $join_behavior);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_RATE_CATEGORY_ID, ArRateCategoryPeer::ID, $join_behavior);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_PARTY_ID, ArPartyPeer::ID, $join_behavior);

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
	 * Returns the number of rows matching criteria, joining the related ArOrganizationUnitType table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinAllExceptArOrganizationUnitType(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArOrganizationUnitHasStructurePeer::TABLE_NAME);
		
		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArOrganizationUnitHasStructurePeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY should not affect count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArOrganizationUnitHasStructurePeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}
	
		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_PARENT_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_RATE_CATEGORY_ID, ArRateCategoryPeer::ID, $join_behavior);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_PARTY_ID, ArPartyPeer::ID, $join_behavior);

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
	 * Returns the number of rows matching criteria, joining the related ArOrganizationUnitRelatedByArParentOrganizationUnitId table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinAllExceptArOrganizationUnitRelatedByArParentOrganizationUnitId(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArOrganizationUnitHasStructurePeer::TABLE_NAME);
		
		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArOrganizationUnitHasStructurePeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY should not affect count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArOrganizationUnitHasStructurePeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}
	
		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_ORGANIZATION_UNIT_TYPE_ID, ArOrganizationUnitTypePeer::ID, $join_behavior);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_RATE_CATEGORY_ID, ArRateCategoryPeer::ID, $join_behavior);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_PARTY_ID, ArPartyPeer::ID, $join_behavior);

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
	 * Returns the number of rows matching criteria, joining the related ArRateCategory table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinAllExceptArRateCategory(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArOrganizationUnitHasStructurePeer::TABLE_NAME);
		
		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArOrganizationUnitHasStructurePeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY should not affect count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArOrganizationUnitHasStructurePeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}
	
		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_ORGANIZATION_UNIT_TYPE_ID, ArOrganizationUnitTypePeer::ID, $join_behavior);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_PARENT_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_PARTY_ID, ArPartyPeer::ID, $join_behavior);

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
	 * Returns the number of rows matching criteria, joining the related ArParty table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinAllExceptArParty(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArOrganizationUnitHasStructurePeer::TABLE_NAME);
		
		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArOrganizationUnitHasStructurePeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY should not affect count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArOrganizationUnitHasStructurePeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}
	
		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_ORGANIZATION_UNIT_TYPE_ID, ArOrganizationUnitTypePeer::ID, $join_behavior);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_PARENT_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_RATE_CATEGORY_ID, ArRateCategoryPeer::ID, $join_behavior);

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
	 * Selects a collection of ArOrganizationUnitHasStructure objects pre-filled with all related objects except ArOrganizationUnitRelatedByArOrganizationUnitId.
	 *
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArOrganizationUnitHasStructure objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinAllExceptArOrganizationUnitRelatedByArOrganizationUnitId(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		// $criteria->getDbName() will return the same object if not set to another value
		// so == check is okay and faster
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArOrganizationUnitHasStructurePeer::addSelectColumns($criteria);
		$startcol2 = (ArOrganizationUnitHasStructurePeer::NUM_COLUMNS - ArOrganizationUnitHasStructurePeer::NUM_LAZY_LOAD_COLUMNS);

		ArOrganizationUnitTypePeer::addSelectColumns($criteria);
		$startcol3 = $startcol2 + (ArOrganizationUnitTypePeer::NUM_COLUMNS - ArOrganizationUnitTypePeer::NUM_LAZY_LOAD_COLUMNS);

		ArRateCategoryPeer::addSelectColumns($criteria);
		$startcol4 = $startcol3 + (ArRateCategoryPeer::NUM_COLUMNS - ArRateCategoryPeer::NUM_LAZY_LOAD_COLUMNS);

		ArPartyPeer::addSelectColumns($criteria);
		$startcol5 = $startcol4 + (ArPartyPeer::NUM_COLUMNS - ArPartyPeer::NUM_LAZY_LOAD_COLUMNS);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_ORGANIZATION_UNIT_TYPE_ID, ArOrganizationUnitTypePeer::ID, $join_behavior);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_RATE_CATEGORY_ID, ArRateCategoryPeer::ID, $join_behavior);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_PARTY_ID, ArPartyPeer::ID, $join_behavior);


		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArOrganizationUnitHasStructurePeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArOrganizationUnitHasStructurePeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {
				$cls = ArOrganizationUnitHasStructurePeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArOrganizationUnitHasStructurePeer::addInstanceToPool($obj1, $key1);
			} // if obj1 already loaded

				// Add objects for joined ArOrganizationUnitType rows

				$key2 = ArOrganizationUnitTypePeer::getPrimaryKeyHashFromRow($row, $startcol2);
				if ($key2 !== null) {
					$obj2 = ArOrganizationUnitTypePeer::getInstanceFromPool($key2);
					if (!$obj2) {
	
						$cls = ArOrganizationUnitTypePeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol2);
					ArOrganizationUnitTypePeer::addInstanceToPool($obj2, $key2);
				} // if $obj2 already loaded

				// Add the $obj1 (ArOrganizationUnitHasStructure) to the collection in $obj2 (ArOrganizationUnitType)
				$obj2->addArOrganizationUnitHasStructure($obj1);

			} // if joined row is not null

				// Add objects for joined ArRateCategory rows

				$key3 = ArRateCategoryPeer::getPrimaryKeyHashFromRow($row, $startcol3);
				if ($key3 !== null) {
					$obj3 = ArRateCategoryPeer::getInstanceFromPool($key3);
					if (!$obj3) {
	
						$cls = ArRateCategoryPeer::getOMClass(false);

					$obj3 = new $cls();
					$obj3->hydrate($row, $startcol3);
					ArRateCategoryPeer::addInstanceToPool($obj3, $key3);
				} // if $obj3 already loaded

				// Add the $obj1 (ArOrganizationUnitHasStructure) to the collection in $obj3 (ArRateCategory)
				$obj3->addArOrganizationUnitHasStructure($obj1);

			} // if joined row is not null

				// Add objects for joined ArParty rows

				$key4 = ArPartyPeer::getPrimaryKeyHashFromRow($row, $startcol4);
				if ($key4 !== null) {
					$obj4 = ArPartyPeer::getInstanceFromPool($key4);
					if (!$obj4) {
	
						$cls = ArPartyPeer::getOMClass(false);

					$obj4 = new $cls();
					$obj4->hydrate($row, $startcol4);
					ArPartyPeer::addInstanceToPool($obj4, $key4);
				} // if $obj4 already loaded

				// Add the $obj1 (ArOrganizationUnitHasStructure) to the collection in $obj4 (ArParty)
				$obj4->addArOrganizationUnitHasStructure($obj1);

			} // if joined row is not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArOrganizationUnitHasStructure objects pre-filled with all related objects except ArOrganizationUnitType.
	 *
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArOrganizationUnitHasStructure objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinAllExceptArOrganizationUnitType(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		// $criteria->getDbName() will return the same object if not set to another value
		// so == check is okay and faster
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArOrganizationUnitHasStructurePeer::addSelectColumns($criteria);
		$startcol2 = (ArOrganizationUnitHasStructurePeer::NUM_COLUMNS - ArOrganizationUnitHasStructurePeer::NUM_LAZY_LOAD_COLUMNS);

		ArOrganizationUnitPeer::addSelectColumns($criteria);
		$startcol3 = $startcol2 + (ArOrganizationUnitPeer::NUM_COLUMNS - ArOrganizationUnitPeer::NUM_LAZY_LOAD_COLUMNS);

		ArOrganizationUnitPeer::addSelectColumns($criteria);
		$startcol4 = $startcol3 + (ArOrganizationUnitPeer::NUM_COLUMNS - ArOrganizationUnitPeer::NUM_LAZY_LOAD_COLUMNS);

		ArRateCategoryPeer::addSelectColumns($criteria);
		$startcol5 = $startcol4 + (ArRateCategoryPeer::NUM_COLUMNS - ArRateCategoryPeer::NUM_LAZY_LOAD_COLUMNS);

		ArPartyPeer::addSelectColumns($criteria);
		$startcol6 = $startcol5 + (ArPartyPeer::NUM_COLUMNS - ArPartyPeer::NUM_LAZY_LOAD_COLUMNS);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_PARENT_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_RATE_CATEGORY_ID, ArRateCategoryPeer::ID, $join_behavior);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_PARTY_ID, ArPartyPeer::ID, $join_behavior);


		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArOrganizationUnitHasStructurePeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArOrganizationUnitHasStructurePeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {
				$cls = ArOrganizationUnitHasStructurePeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArOrganizationUnitHasStructurePeer::addInstanceToPool($obj1, $key1);
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

				// Add the $obj1 (ArOrganizationUnitHasStructure) to the collection in $obj2 (ArOrganizationUnit)
				$obj2->addArOrganizationUnitHasStructureRelatedByArOrganizationUnitId($obj1);

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

				// Add the $obj1 (ArOrganizationUnitHasStructure) to the collection in $obj3 (ArOrganizationUnit)
				$obj3->addArOrganizationUnitHasStructureRelatedByArParentOrganizationUnitId($obj1);

			} // if joined row is not null

				// Add objects for joined ArRateCategory rows

				$key4 = ArRateCategoryPeer::getPrimaryKeyHashFromRow($row, $startcol4);
				if ($key4 !== null) {
					$obj4 = ArRateCategoryPeer::getInstanceFromPool($key4);
					if (!$obj4) {
	
						$cls = ArRateCategoryPeer::getOMClass(false);

					$obj4 = new $cls();
					$obj4->hydrate($row, $startcol4);
					ArRateCategoryPeer::addInstanceToPool($obj4, $key4);
				} // if $obj4 already loaded

				// Add the $obj1 (ArOrganizationUnitHasStructure) to the collection in $obj4 (ArRateCategory)
				$obj4->addArOrganizationUnitHasStructure($obj1);

			} // if joined row is not null

				// Add objects for joined ArParty rows

				$key5 = ArPartyPeer::getPrimaryKeyHashFromRow($row, $startcol5);
				if ($key5 !== null) {
					$obj5 = ArPartyPeer::getInstanceFromPool($key5);
					if (!$obj5) {
	
						$cls = ArPartyPeer::getOMClass(false);

					$obj5 = new $cls();
					$obj5->hydrate($row, $startcol5);
					ArPartyPeer::addInstanceToPool($obj5, $key5);
				} // if $obj5 already loaded

				// Add the $obj1 (ArOrganizationUnitHasStructure) to the collection in $obj5 (ArParty)
				$obj5->addArOrganizationUnitHasStructure($obj1);

			} // if joined row is not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArOrganizationUnitHasStructure objects pre-filled with all related objects except ArOrganizationUnitRelatedByArParentOrganizationUnitId.
	 *
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArOrganizationUnitHasStructure objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinAllExceptArOrganizationUnitRelatedByArParentOrganizationUnitId(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		// $criteria->getDbName() will return the same object if not set to another value
		// so == check is okay and faster
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArOrganizationUnitHasStructurePeer::addSelectColumns($criteria);
		$startcol2 = (ArOrganizationUnitHasStructurePeer::NUM_COLUMNS - ArOrganizationUnitHasStructurePeer::NUM_LAZY_LOAD_COLUMNS);

		ArOrganizationUnitTypePeer::addSelectColumns($criteria);
		$startcol3 = $startcol2 + (ArOrganizationUnitTypePeer::NUM_COLUMNS - ArOrganizationUnitTypePeer::NUM_LAZY_LOAD_COLUMNS);

		ArRateCategoryPeer::addSelectColumns($criteria);
		$startcol4 = $startcol3 + (ArRateCategoryPeer::NUM_COLUMNS - ArRateCategoryPeer::NUM_LAZY_LOAD_COLUMNS);

		ArPartyPeer::addSelectColumns($criteria);
		$startcol5 = $startcol4 + (ArPartyPeer::NUM_COLUMNS - ArPartyPeer::NUM_LAZY_LOAD_COLUMNS);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_ORGANIZATION_UNIT_TYPE_ID, ArOrganizationUnitTypePeer::ID, $join_behavior);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_RATE_CATEGORY_ID, ArRateCategoryPeer::ID, $join_behavior);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_PARTY_ID, ArPartyPeer::ID, $join_behavior);


		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArOrganizationUnitHasStructurePeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArOrganizationUnitHasStructurePeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {
				$cls = ArOrganizationUnitHasStructurePeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArOrganizationUnitHasStructurePeer::addInstanceToPool($obj1, $key1);
			} // if obj1 already loaded

				// Add objects for joined ArOrganizationUnitType rows

				$key2 = ArOrganizationUnitTypePeer::getPrimaryKeyHashFromRow($row, $startcol2);
				if ($key2 !== null) {
					$obj2 = ArOrganizationUnitTypePeer::getInstanceFromPool($key2);
					if (!$obj2) {
	
						$cls = ArOrganizationUnitTypePeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol2);
					ArOrganizationUnitTypePeer::addInstanceToPool($obj2, $key2);
				} // if $obj2 already loaded

				// Add the $obj1 (ArOrganizationUnitHasStructure) to the collection in $obj2 (ArOrganizationUnitType)
				$obj2->addArOrganizationUnitHasStructure($obj1);

			} // if joined row is not null

				// Add objects for joined ArRateCategory rows

				$key3 = ArRateCategoryPeer::getPrimaryKeyHashFromRow($row, $startcol3);
				if ($key3 !== null) {
					$obj3 = ArRateCategoryPeer::getInstanceFromPool($key3);
					if (!$obj3) {
	
						$cls = ArRateCategoryPeer::getOMClass(false);

					$obj3 = new $cls();
					$obj3->hydrate($row, $startcol3);
					ArRateCategoryPeer::addInstanceToPool($obj3, $key3);
				} // if $obj3 already loaded

				// Add the $obj1 (ArOrganizationUnitHasStructure) to the collection in $obj3 (ArRateCategory)
				$obj3->addArOrganizationUnitHasStructure($obj1);

			} // if joined row is not null

				// Add objects for joined ArParty rows

				$key4 = ArPartyPeer::getPrimaryKeyHashFromRow($row, $startcol4);
				if ($key4 !== null) {
					$obj4 = ArPartyPeer::getInstanceFromPool($key4);
					if (!$obj4) {
	
						$cls = ArPartyPeer::getOMClass(false);

					$obj4 = new $cls();
					$obj4->hydrate($row, $startcol4);
					ArPartyPeer::addInstanceToPool($obj4, $key4);
				} // if $obj4 already loaded

				// Add the $obj1 (ArOrganizationUnitHasStructure) to the collection in $obj4 (ArParty)
				$obj4->addArOrganizationUnitHasStructure($obj1);

			} // if joined row is not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArOrganizationUnitHasStructure objects pre-filled with all related objects except ArRateCategory.
	 *
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArOrganizationUnitHasStructure objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinAllExceptArRateCategory(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		// $criteria->getDbName() will return the same object if not set to another value
		// so == check is okay and faster
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArOrganizationUnitHasStructurePeer::addSelectColumns($criteria);
		$startcol2 = (ArOrganizationUnitHasStructurePeer::NUM_COLUMNS - ArOrganizationUnitHasStructurePeer::NUM_LAZY_LOAD_COLUMNS);

		ArOrganizationUnitPeer::addSelectColumns($criteria);
		$startcol3 = $startcol2 + (ArOrganizationUnitPeer::NUM_COLUMNS - ArOrganizationUnitPeer::NUM_LAZY_LOAD_COLUMNS);

		ArOrganizationUnitTypePeer::addSelectColumns($criteria);
		$startcol4 = $startcol3 + (ArOrganizationUnitTypePeer::NUM_COLUMNS - ArOrganizationUnitTypePeer::NUM_LAZY_LOAD_COLUMNS);

		ArOrganizationUnitPeer::addSelectColumns($criteria);
		$startcol5 = $startcol4 + (ArOrganizationUnitPeer::NUM_COLUMNS - ArOrganizationUnitPeer::NUM_LAZY_LOAD_COLUMNS);

		ArPartyPeer::addSelectColumns($criteria);
		$startcol6 = $startcol5 + (ArPartyPeer::NUM_COLUMNS - ArPartyPeer::NUM_LAZY_LOAD_COLUMNS);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_ORGANIZATION_UNIT_TYPE_ID, ArOrganizationUnitTypePeer::ID, $join_behavior);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_PARENT_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_PARTY_ID, ArPartyPeer::ID, $join_behavior);


		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArOrganizationUnitHasStructurePeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArOrganizationUnitHasStructurePeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {
				$cls = ArOrganizationUnitHasStructurePeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArOrganizationUnitHasStructurePeer::addInstanceToPool($obj1, $key1);
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

				// Add the $obj1 (ArOrganizationUnitHasStructure) to the collection in $obj2 (ArOrganizationUnit)
				$obj2->addArOrganizationUnitHasStructureRelatedByArOrganizationUnitId($obj1);

			} // if joined row is not null

				// Add objects for joined ArOrganizationUnitType rows

				$key3 = ArOrganizationUnitTypePeer::getPrimaryKeyHashFromRow($row, $startcol3);
				if ($key3 !== null) {
					$obj3 = ArOrganizationUnitTypePeer::getInstanceFromPool($key3);
					if (!$obj3) {
	
						$cls = ArOrganizationUnitTypePeer::getOMClass(false);

					$obj3 = new $cls();
					$obj3->hydrate($row, $startcol3);
					ArOrganizationUnitTypePeer::addInstanceToPool($obj3, $key3);
				} // if $obj3 already loaded

				// Add the $obj1 (ArOrganizationUnitHasStructure) to the collection in $obj3 (ArOrganizationUnitType)
				$obj3->addArOrganizationUnitHasStructure($obj1);

			} // if joined row is not null

				// Add objects for joined ArOrganizationUnit rows

				$key4 = ArOrganizationUnitPeer::getPrimaryKeyHashFromRow($row, $startcol4);
				if ($key4 !== null) {
					$obj4 = ArOrganizationUnitPeer::getInstanceFromPool($key4);
					if (!$obj4) {
	
						$cls = ArOrganizationUnitPeer::getOMClass(false);

					$obj4 = new $cls();
					$obj4->hydrate($row, $startcol4);
					ArOrganizationUnitPeer::addInstanceToPool($obj4, $key4);
				} // if $obj4 already loaded

				// Add the $obj1 (ArOrganizationUnitHasStructure) to the collection in $obj4 (ArOrganizationUnit)
				$obj4->addArOrganizationUnitHasStructureRelatedByArParentOrganizationUnitId($obj1);

			} // if joined row is not null

				// Add objects for joined ArParty rows

				$key5 = ArPartyPeer::getPrimaryKeyHashFromRow($row, $startcol5);
				if ($key5 !== null) {
					$obj5 = ArPartyPeer::getInstanceFromPool($key5);
					if (!$obj5) {
	
						$cls = ArPartyPeer::getOMClass(false);

					$obj5 = new $cls();
					$obj5->hydrate($row, $startcol5);
					ArPartyPeer::addInstanceToPool($obj5, $key5);
				} // if $obj5 already loaded

				// Add the $obj1 (ArOrganizationUnitHasStructure) to the collection in $obj5 (ArParty)
				$obj5->addArOrganizationUnitHasStructure($obj1);

			} // if joined row is not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArOrganizationUnitHasStructure objects pre-filled with all related objects except ArParty.
	 *
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArOrganizationUnitHasStructure objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinAllExceptArParty(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		// $criteria->getDbName() will return the same object if not set to another value
		// so == check is okay and faster
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArOrganizationUnitHasStructurePeer::addSelectColumns($criteria);
		$startcol2 = (ArOrganizationUnitHasStructurePeer::NUM_COLUMNS - ArOrganizationUnitHasStructurePeer::NUM_LAZY_LOAD_COLUMNS);

		ArOrganizationUnitPeer::addSelectColumns($criteria);
		$startcol3 = $startcol2 + (ArOrganizationUnitPeer::NUM_COLUMNS - ArOrganizationUnitPeer::NUM_LAZY_LOAD_COLUMNS);

		ArOrganizationUnitTypePeer::addSelectColumns($criteria);
		$startcol4 = $startcol3 + (ArOrganizationUnitTypePeer::NUM_COLUMNS - ArOrganizationUnitTypePeer::NUM_LAZY_LOAD_COLUMNS);

		ArOrganizationUnitPeer::addSelectColumns($criteria);
		$startcol5 = $startcol4 + (ArOrganizationUnitPeer::NUM_COLUMNS - ArOrganizationUnitPeer::NUM_LAZY_LOAD_COLUMNS);

		ArRateCategoryPeer::addSelectColumns($criteria);
		$startcol6 = $startcol5 + (ArRateCategoryPeer::NUM_COLUMNS - ArRateCategoryPeer::NUM_LAZY_LOAD_COLUMNS);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_ORGANIZATION_UNIT_TYPE_ID, ArOrganizationUnitTypePeer::ID, $join_behavior);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_PARENT_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArOrganizationUnitHasStructurePeer::AR_RATE_CATEGORY_ID, ArRateCategoryPeer::ID, $join_behavior);


		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArOrganizationUnitHasStructurePeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArOrganizationUnitHasStructurePeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {
				$cls = ArOrganizationUnitHasStructurePeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArOrganizationUnitHasStructurePeer::addInstanceToPool($obj1, $key1);
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

				// Add the $obj1 (ArOrganizationUnitHasStructure) to the collection in $obj2 (ArOrganizationUnit)
				$obj2->addArOrganizationUnitHasStructureRelatedByArOrganizationUnitId($obj1);

			} // if joined row is not null

				// Add objects for joined ArOrganizationUnitType rows

				$key3 = ArOrganizationUnitTypePeer::getPrimaryKeyHashFromRow($row, $startcol3);
				if ($key3 !== null) {
					$obj3 = ArOrganizationUnitTypePeer::getInstanceFromPool($key3);
					if (!$obj3) {
	
						$cls = ArOrganizationUnitTypePeer::getOMClass(false);

					$obj3 = new $cls();
					$obj3->hydrate($row, $startcol3);
					ArOrganizationUnitTypePeer::addInstanceToPool($obj3, $key3);
				} // if $obj3 already loaded

				// Add the $obj1 (ArOrganizationUnitHasStructure) to the collection in $obj3 (ArOrganizationUnitType)
				$obj3->addArOrganizationUnitHasStructure($obj1);

			} // if joined row is not null

				// Add objects for joined ArOrganizationUnit rows

				$key4 = ArOrganizationUnitPeer::getPrimaryKeyHashFromRow($row, $startcol4);
				if ($key4 !== null) {
					$obj4 = ArOrganizationUnitPeer::getInstanceFromPool($key4);
					if (!$obj4) {
	
						$cls = ArOrganizationUnitPeer::getOMClass(false);

					$obj4 = new $cls();
					$obj4->hydrate($row, $startcol4);
					ArOrganizationUnitPeer::addInstanceToPool($obj4, $key4);
				} // if $obj4 already loaded

				// Add the $obj1 (ArOrganizationUnitHasStructure) to the collection in $obj4 (ArOrganizationUnit)
				$obj4->addArOrganizationUnitHasStructureRelatedByArParentOrganizationUnitId($obj1);

			} // if joined row is not null

				// Add objects for joined ArRateCategory rows

				$key5 = ArRateCategoryPeer::getPrimaryKeyHashFromRow($row, $startcol5);
				if ($key5 !== null) {
					$obj5 = ArRateCategoryPeer::getInstanceFromPool($key5);
					if (!$obj5) {
	
						$cls = ArRateCategoryPeer::getOMClass(false);

					$obj5 = new $cls();
					$obj5->hydrate($row, $startcol5);
					ArRateCategoryPeer::addInstanceToPool($obj5, $key5);
				} // if $obj5 already loaded

				// Add the $obj1 (ArOrganizationUnitHasStructure) to the collection in $obj5 (ArRateCategory)
				$obj5->addArOrganizationUnitHasStructure($obj1);

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
	  $dbMap = Propel::getDatabaseMap(BaseArOrganizationUnitHasStructurePeer::DATABASE_NAME);
	  if (!$dbMap->hasTable(BaseArOrganizationUnitHasStructurePeer::TABLE_NAME))
	  {
	    $dbMap->addTableObject(new ArOrganizationUnitHasStructureTableMap());
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
		return $withPrefix ? ArOrganizationUnitHasStructurePeer::CLASS_DEFAULT : ArOrganizationUnitHasStructurePeer::OM_CLASS;
	}

	/**
	 * Method perform an INSERT on the database, given a ArOrganizationUnitHasStructure or Criteria object.
	 *
	 * @param      mixed $values Criteria or ArOrganizationUnitHasStructure object containing data that is used to create the INSERT statement.
	 * @param      PropelPDO $con the PropelPDO connection to use
	 * @return     mixed The new primary key.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doInsert($values, PropelPDO $con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArOrganizationUnitHasStructurePeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		if ($values instanceof Criteria) {
			$criteria = clone $values; // rename for clarity
		} else {
			$criteria = $values->buildCriteria(); // build Criteria from ArOrganizationUnitHasStructure object
		}

		if ($criteria->containsKey(ArOrganizationUnitHasStructurePeer::ID) && $criteria->keyContainsValue(ArOrganizationUnitHasStructurePeer::ID) ) {
			throw new PropelException('Cannot insert a value for auto-increment primary key ('.ArOrganizationUnitHasStructurePeer::ID.')');
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
	 * Method perform an UPDATE on the database, given a ArOrganizationUnitHasStructure or Criteria object.
	 *
	 * @param      mixed $values Criteria or ArOrganizationUnitHasStructure object containing data that is used to create the UPDATE statement.
	 * @param      PropelPDO $con The connection to use (specify PropelPDO connection object to exert more control over transactions).
	 * @return     int The number of affected rows (if supported by underlying database driver).
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doUpdate($values, PropelPDO $con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArOrganizationUnitHasStructurePeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		$selectCriteria = new Criteria(self::DATABASE_NAME);

		if ($values instanceof Criteria) {
			$criteria = clone $values; // rename for clarity

			$comparison = $criteria->getComparison(ArOrganizationUnitHasStructurePeer::ID);
			$selectCriteria->add(ArOrganizationUnitHasStructurePeer::ID, $criteria->remove(ArOrganizationUnitHasStructurePeer::ID), $comparison);

		} else { // $values is ArOrganizationUnitHasStructure object
			$criteria = $values->buildCriteria(); // gets full criteria
			$selectCriteria = $values->buildPkeyCriteria(); // gets criteria w/ primary key(s)
		}

		// set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		return BasePeer::doUpdate($selectCriteria, $criteria, $con);
	}

	/**
	 * Method to DELETE all rows from the ar_organization_unit_has_structure table.
	 *
	 * @return     int The number of affected rows (if supported by underlying database driver).
	 */
	public static function doDeleteAll($con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArOrganizationUnitHasStructurePeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}
		$affectedRows = 0; // initialize var to track total num of affected rows
		try {
			// use transaction because $criteria could contain info
			// for more than one table or we could emulating ON DELETE CASCADE, etc.
			$con->beginTransaction();
			$affectedRows += BasePeer::doDeleteAll(ArOrganizationUnitHasStructurePeer::TABLE_NAME, $con);
			// Because this db requires some delete cascade/set null emulation, we have to
			// clear the cached instance *after* the emulation has happened (since
			// instances get re-added by the select statement contained therein).
			ArOrganizationUnitHasStructurePeer::clearInstancePool();
			ArOrganizationUnitHasStructurePeer::clearRelatedInstancePool();
			$con->commit();
			return $affectedRows;
		} catch (PropelException $e) {
			$con->rollBack();
			throw $e;
		}
	}

	/**
	 * Method perform a DELETE on the database, given a ArOrganizationUnitHasStructure or Criteria object OR a primary key value.
	 *
	 * @param      mixed $values Criteria or ArOrganizationUnitHasStructure object or primary key or array of primary keys
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
			$con = Propel::getConnection(ArOrganizationUnitHasStructurePeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		if ($values instanceof Criteria) {
			// invalidate the cache for all objects of this type, since we have no
			// way of knowing (without running a query) what objects should be invalidated
			// from the cache based on this Criteria.
			ArOrganizationUnitHasStructurePeer::clearInstancePool();
			// rename for clarity
			$criteria = clone $values;
		} elseif ($values instanceof ArOrganizationUnitHasStructure) { // it's a model object
			// invalidate the cache for this single object
			ArOrganizationUnitHasStructurePeer::removeInstanceFromPool($values);
			// create criteria based on pk values
			$criteria = $values->buildPkeyCriteria();
		} else { // it's a primary key, or an array of pks
			$criteria = new Criteria(self::DATABASE_NAME);
			$criteria->add(ArOrganizationUnitHasStructurePeer::ID, (array) $values, Criteria::IN);
			// invalidate the cache for this object(s)
			foreach ((array) $values as $singleval) {
				ArOrganizationUnitHasStructurePeer::removeInstanceFromPool($singleval);
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
			ArOrganizationUnitHasStructurePeer::clearRelatedInstancePool();
			$con->commit();
			return $affectedRows;
		} catch (PropelException $e) {
			$con->rollBack();
			throw $e;
		}
	}

	/**
	 * Validates all modified columns of given ArOrganizationUnitHasStructure object.
	 * If parameter $columns is either a single column name or an array of column names
	 * than only those columns are validated.
	 *
	 * NOTICE: This does not apply to primary or foreign keys for now.
	 *
	 * @param      ArOrganizationUnitHasStructure $obj The object to validate.
	 * @param      mixed $cols Column name or array of column names.
	 *
	 * @return     mixed TRUE if all columns are valid or the error message of the first invalid column.
	 */
	public static function doValidate(ArOrganizationUnitHasStructure $obj, $cols = null)
	{
		$columns = array();

		if ($cols) {
			$dbMap = Propel::getDatabaseMap(ArOrganizationUnitHasStructurePeer::DATABASE_NAME);
			$tableMap = $dbMap->getTable(ArOrganizationUnitHasStructurePeer::TABLE_NAME);

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

		return BasePeer::doValidate(ArOrganizationUnitHasStructurePeer::DATABASE_NAME, ArOrganizationUnitHasStructurePeer::TABLE_NAME, $columns);
	}

	/**
	 * Retrieve a single object by pkey.
	 *
	 * @param      int $pk the primary key.
	 * @param      PropelPDO $con the connection to use
	 * @return     ArOrganizationUnitHasStructure
	 */
	public static function retrieveByPK($pk, PropelPDO $con = null)
	{

		if (null !== ($obj = ArOrganizationUnitHasStructurePeer::getInstanceFromPool((string) $pk))) {
			return $obj;
		}

		if ($con === null) {
			$con = Propel::getConnection(ArOrganizationUnitHasStructurePeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria = new Criteria(ArOrganizationUnitHasStructurePeer::DATABASE_NAME);
		$criteria->add(ArOrganizationUnitHasStructurePeer::ID, $pk);

		$v = ArOrganizationUnitHasStructurePeer::doSelect($criteria, $con);

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
			$con = Propel::getConnection(ArOrganizationUnitHasStructurePeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$objs = null;
		if (empty($pks)) {
			$objs = array();
		} else {
			$criteria = new Criteria(ArOrganizationUnitHasStructurePeer::DATABASE_NAME);
			$criteria->add(ArOrganizationUnitHasStructurePeer::ID, $pks, Criteria::IN);
			$objs = ArOrganizationUnitHasStructurePeer::doSelect($criteria, $con);
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
	  return array(array('ar_organization_unit_id', 'from'));
	}

} // BaseArOrganizationUnitHasStructurePeer

// This is the static code needed to register the TableMap for this table with the main Propel class.
//
BaseArOrganizationUnitHasStructurePeer::buildTableMap();

