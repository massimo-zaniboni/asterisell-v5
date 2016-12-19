<?php

/**
 * Base static class for performing query and update operations on the 'ar_new_problem' table.
 *
 * 
 *
 * @package    lib.model.om
 */
abstract class BaseArNewProblemPeer {

	/** the default database name for this class */
	const DATABASE_NAME = 'propel';

	/** the table name for this class */
	const TABLE_NAME = 'ar_new_problem';

	/** the related Propel class for this table */
	const OM_CLASS = 'ArNewProblem';

	/** A class that can be returned by this peer. */
	const CLASS_DEFAULT = 'lib.model.ArNewProblem';

	/** the related TableMap class for this table */
	const TM_CLASS = 'ArNewProblemTableMap';
	
	/** The total number of columns. */
	const NUM_COLUMNS = 13;

	/** The number of lazy-loaded columns. */
	const NUM_LAZY_LOAD_COLUMNS = 0;

	/** the column name for the DUPLICATION_KEY field */
	const DUPLICATION_KEY = 'ar_new_problem.DUPLICATION_KEY';

	/** the column name for the AR_PROBLEM_TYPE_ID field */
	const AR_PROBLEM_TYPE_ID = 'ar_new_problem.AR_PROBLEM_TYPE_ID';

	/** the column name for the AR_PROBLEM_DOMAIN_ID field */
	const AR_PROBLEM_DOMAIN_ID = 'ar_new_problem.AR_PROBLEM_DOMAIN_ID';

	/** the column name for the AR_PROBLEM_RESPONSIBLE_ID field */
	const AR_PROBLEM_RESPONSIBLE_ID = 'ar_new_problem.AR_PROBLEM_RESPONSIBLE_ID';

	/** the column name for the CREATED_AT field */
	const CREATED_AT = 'ar_new_problem.CREATED_AT';

	/** the column name for the GARBAGE_COLLECTION_KEY field */
	const GARBAGE_COLLECTION_KEY = 'ar_new_problem.GARBAGE_COLLECTION_KEY';

	/** the column name for the GARBAGE_COLLECTION_FROM field */
	const GARBAGE_COLLECTION_FROM = 'ar_new_problem.GARBAGE_COLLECTION_FROM';

	/** the column name for the GARBAGE_COLLECTION_TO field */
	const GARBAGE_COLLECTION_TO = 'ar_new_problem.GARBAGE_COLLECTION_TO';

	/** the column name for the DESCRIPTION field */
	const DESCRIPTION = 'ar_new_problem.DESCRIPTION';

	/** the column name for the EFFECT field */
	const EFFECT = 'ar_new_problem.EFFECT';

	/** the column name for the PROPOSED_SOLUTION field */
	const PROPOSED_SOLUTION = 'ar_new_problem.PROPOSED_SOLUTION';

	/** the column name for the SIGNALED_TO_ADMIN field */
	const SIGNALED_TO_ADMIN = 'ar_new_problem.SIGNALED_TO_ADMIN';

	/** the column name for the COUNT_OF_CDRS field */
	const COUNT_OF_CDRS = 'ar_new_problem.COUNT_OF_CDRS';

	/**
	 * An identiy map to hold any loaded instances of ArNewProblem objects.
	 * This must be public so that other peer classes can access this when hydrating from JOIN
	 * queries.
	 * @var        array ArNewProblem[]
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
		BasePeer::TYPE_PHPNAME => array ('DuplicationKey', 'ArProblemTypeId', 'ArProblemDomainId', 'ArProblemResponsibleId', 'CreatedAt', 'GarbageCollectionKey', 'GarbageCollectionFrom', 'GarbageCollectionTo', 'Description', 'Effect', 'ProposedSolution', 'SignaledToAdmin', 'CountOfCdrs', ),
		BasePeer::TYPE_STUDLYPHPNAME => array ('duplicationKey', 'arProblemTypeId', 'arProblemDomainId', 'arProblemResponsibleId', 'createdAt', 'garbageCollectionKey', 'garbageCollectionFrom', 'garbageCollectionTo', 'description', 'effect', 'proposedSolution', 'signaledToAdmin', 'countOfCdrs', ),
		BasePeer::TYPE_COLNAME => array (self::DUPLICATION_KEY, self::AR_PROBLEM_TYPE_ID, self::AR_PROBLEM_DOMAIN_ID, self::AR_PROBLEM_RESPONSIBLE_ID, self::CREATED_AT, self::GARBAGE_COLLECTION_KEY, self::GARBAGE_COLLECTION_FROM, self::GARBAGE_COLLECTION_TO, self::DESCRIPTION, self::EFFECT, self::PROPOSED_SOLUTION, self::SIGNALED_TO_ADMIN, self::COUNT_OF_CDRS, ),
		BasePeer::TYPE_FIELDNAME => array ('duplication_key', 'ar_problem_type_id', 'ar_problem_domain_id', 'ar_problem_responsible_id', 'created_at', 'garbage_collection_key', 'garbage_collection_from', 'garbage_collection_to', 'description', 'effect', 'proposed_solution', 'signaled_to_admin', 'count_of_cdrs', ),
		BasePeer::TYPE_NUM => array (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, )
	);

	/**
	 * holds an array of keys for quick access to the fieldnames array
	 *
	 * first dimension keys are the type constants
	 * e.g. self::$fieldNames[BasePeer::TYPE_PHPNAME]['Id'] = 0
	 */
	private static $fieldKeys = array (
		BasePeer::TYPE_PHPNAME => array ('DuplicationKey' => 0, 'ArProblemTypeId' => 1, 'ArProblemDomainId' => 2, 'ArProblemResponsibleId' => 3, 'CreatedAt' => 4, 'GarbageCollectionKey' => 5, 'GarbageCollectionFrom' => 6, 'GarbageCollectionTo' => 7, 'Description' => 8, 'Effect' => 9, 'ProposedSolution' => 10, 'SignaledToAdmin' => 11, 'CountOfCdrs' => 12, ),
		BasePeer::TYPE_STUDLYPHPNAME => array ('duplicationKey' => 0, 'arProblemTypeId' => 1, 'arProblemDomainId' => 2, 'arProblemResponsibleId' => 3, 'createdAt' => 4, 'garbageCollectionKey' => 5, 'garbageCollectionFrom' => 6, 'garbageCollectionTo' => 7, 'description' => 8, 'effect' => 9, 'proposedSolution' => 10, 'signaledToAdmin' => 11, 'countOfCdrs' => 12, ),
		BasePeer::TYPE_COLNAME => array (self::DUPLICATION_KEY => 0, self::AR_PROBLEM_TYPE_ID => 1, self::AR_PROBLEM_DOMAIN_ID => 2, self::AR_PROBLEM_RESPONSIBLE_ID => 3, self::CREATED_AT => 4, self::GARBAGE_COLLECTION_KEY => 5, self::GARBAGE_COLLECTION_FROM => 6, self::GARBAGE_COLLECTION_TO => 7, self::DESCRIPTION => 8, self::EFFECT => 9, self::PROPOSED_SOLUTION => 10, self::SIGNALED_TO_ADMIN => 11, self::COUNT_OF_CDRS => 12, ),
		BasePeer::TYPE_FIELDNAME => array ('duplication_key' => 0, 'ar_problem_type_id' => 1, 'ar_problem_domain_id' => 2, 'ar_problem_responsible_id' => 3, 'created_at' => 4, 'garbage_collection_key' => 5, 'garbage_collection_from' => 6, 'garbage_collection_to' => 7, 'description' => 8, 'effect' => 9, 'proposed_solution' => 10, 'signaled_to_admin' => 11, 'count_of_cdrs' => 12, ),
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
	 * @param      string $column The column name for current table. (i.e. ArNewProblemPeer::COLUMN_NAME).
	 * @return     string
	 */
	public static function alias($alias, $column)
	{
		return str_replace(ArNewProblemPeer::TABLE_NAME.'.', $alias.'.', $column);
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
		$criteria->addSelectColumn(ArNewProblemPeer::DUPLICATION_KEY);
		$criteria->addSelectColumn(ArNewProblemPeer::AR_PROBLEM_TYPE_ID);
		$criteria->addSelectColumn(ArNewProblemPeer::AR_PROBLEM_DOMAIN_ID);
		$criteria->addSelectColumn(ArNewProblemPeer::AR_PROBLEM_RESPONSIBLE_ID);
		$criteria->addSelectColumn(ArNewProblemPeer::CREATED_AT);
		$criteria->addSelectColumn(ArNewProblemPeer::GARBAGE_COLLECTION_KEY);
		$criteria->addSelectColumn(ArNewProblemPeer::GARBAGE_COLLECTION_FROM);
		$criteria->addSelectColumn(ArNewProblemPeer::GARBAGE_COLLECTION_TO);
		$criteria->addSelectColumn(ArNewProblemPeer::DESCRIPTION);
		$criteria->addSelectColumn(ArNewProblemPeer::EFFECT);
		$criteria->addSelectColumn(ArNewProblemPeer::PROPOSED_SOLUTION);
		$criteria->addSelectColumn(ArNewProblemPeer::SIGNALED_TO_ADMIN);
		$criteria->addSelectColumn(ArNewProblemPeer::COUNT_OF_CDRS);
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
		$criteria->setPrimaryTableName(ArNewProblemPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArNewProblemPeer::addSelectColumns($criteria);
		}

		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		$criteria->setDbName(self::DATABASE_NAME); // Set the correct dbName

		if ($con === null) {
			$con = Propel::getConnection(ArNewProblemPeer::DATABASE_NAME, Propel::CONNECTION_READ);
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
	 * @return     ArNewProblem
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectOne(Criteria $criteria, PropelPDO $con = null)
	{
		$critcopy = clone $criteria;
		$critcopy->setLimit(1);
		$objects = ArNewProblemPeer::doSelect($critcopy, $con);
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
		return ArNewProblemPeer::populateObjects(ArNewProblemPeer::doSelectStmt($criteria, $con));
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
			$con = Propel::getConnection(ArNewProblemPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		if (!$criteria->hasSelectClause()) {
			$criteria = clone $criteria;
			ArNewProblemPeer::addSelectColumns($criteria);
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
	 * @param      ArNewProblem $value A ArNewProblem object.
	 * @param      string $key (optional) key to use for instance map (for performance boost if key was already calculated externally).
	 */
	public static function addInstanceToPool(ArNewProblem $obj, $key = null)
	{
		if (Propel::isInstancePoolingEnabled()) {
			if ($key === null) {
				$key = (string) $obj->getDuplicationKey();
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
	 * @param      mixed $value A ArNewProblem object or a primary key value.
	 */
	public static function removeInstanceFromPool($value)
	{
		if (Propel::isInstancePoolingEnabled() && $value !== null) {
			if (is_object($value) && $value instanceof ArNewProblem) {
				$key = (string) $value->getDuplicationKey();
			} elseif (is_scalar($value)) {
				// assume we've been passed a primary key
				$key = (string) $value;
			} else {
				$e = new PropelException("Invalid value passed to removeInstanceFromPool().  Expected primary key or ArNewProblem object; got " . (is_object($value) ? get_class($value) . ' object.' : var_export($value,true)));
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
	 * @return     ArNewProblem Found object or NULL if 1) no instance exists for specified key or 2) instance pooling has been disabled.
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
	 * Method to invalidate the instance pool of all tables related to ar_new_problem
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
		$cls = ArNewProblemPeer::getOMClass(false);
		// populate the object(s)
		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key = ArNewProblemPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj = ArNewProblemPeer::getInstanceFromPool($key))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj->hydrate($row, 0, true); // rehydrate
				$results[] = $obj;
			} else {
				$obj = new $cls();
				$obj->hydrate($row);
				$results[] = $obj;
				ArNewProblemPeer::addInstanceToPool($obj, $key);
			} // if key exists
		}
		$stmt->closeCursor();
		return $results;
	}

	/**
	 * Returns the number of rows matching criteria, joining the related ArProblemType table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinArProblemType(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArNewProblemPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArNewProblemPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArNewProblemPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArNewProblemPeer::AR_PROBLEM_TYPE_ID, ArProblemTypePeer::ID, $join_behavior);

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
	 * Returns the number of rows matching criteria, joining the related ArProblemDomain table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinArProblemDomain(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArNewProblemPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArNewProblemPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArNewProblemPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArNewProblemPeer::AR_PROBLEM_DOMAIN_ID, ArProblemDomainPeer::ID, $join_behavior);

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
	 * Returns the number of rows matching criteria, joining the related ArProblemResponsible table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinArProblemResponsible(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArNewProblemPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArNewProblemPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArNewProblemPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArNewProblemPeer::AR_PROBLEM_RESPONSIBLE_ID, ArProblemResponsiblePeer::ID, $join_behavior);

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
	 * Selects a collection of ArNewProblem objects pre-filled with their ArProblemType objects.
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArNewProblem objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinArProblemType(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArNewProblemPeer::addSelectColumns($criteria);
		$startcol = (ArNewProblemPeer::NUM_COLUMNS - ArNewProblemPeer::NUM_LAZY_LOAD_COLUMNS);
		ArProblemTypePeer::addSelectColumns($criteria);

		$criteria->addJoin(ArNewProblemPeer::AR_PROBLEM_TYPE_ID, ArProblemTypePeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArNewProblemPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArNewProblemPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {

				$cls = ArNewProblemPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArNewProblemPeer::addInstanceToPool($obj1, $key1);
			} // if $obj1 already loaded

			$key2 = ArProblemTypePeer::getPrimaryKeyHashFromRow($row, $startcol);
			if ($key2 !== null) {
				$obj2 = ArProblemTypePeer::getInstanceFromPool($key2);
				if (!$obj2) {

					$cls = ArProblemTypePeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol);
					ArProblemTypePeer::addInstanceToPool($obj2, $key2);
				} // if obj2 already loaded
				
				// Add the $obj1 (ArNewProblem) to $obj2 (ArProblemType)
				$obj2->addArNewProblem($obj1);

			} // if joined row was not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArNewProblem objects pre-filled with their ArProblemDomain objects.
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArNewProblem objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinArProblemDomain(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArNewProblemPeer::addSelectColumns($criteria);
		$startcol = (ArNewProblemPeer::NUM_COLUMNS - ArNewProblemPeer::NUM_LAZY_LOAD_COLUMNS);
		ArProblemDomainPeer::addSelectColumns($criteria);

		$criteria->addJoin(ArNewProblemPeer::AR_PROBLEM_DOMAIN_ID, ArProblemDomainPeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArNewProblemPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArNewProblemPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {

				$cls = ArNewProblemPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArNewProblemPeer::addInstanceToPool($obj1, $key1);
			} // if $obj1 already loaded

			$key2 = ArProblemDomainPeer::getPrimaryKeyHashFromRow($row, $startcol);
			if ($key2 !== null) {
				$obj2 = ArProblemDomainPeer::getInstanceFromPool($key2);
				if (!$obj2) {

					$cls = ArProblemDomainPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol);
					ArProblemDomainPeer::addInstanceToPool($obj2, $key2);
				} // if obj2 already loaded
				
				// Add the $obj1 (ArNewProblem) to $obj2 (ArProblemDomain)
				$obj2->addArNewProblem($obj1);

			} // if joined row was not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArNewProblem objects pre-filled with their ArProblemResponsible objects.
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArNewProblem objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinArProblemResponsible(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArNewProblemPeer::addSelectColumns($criteria);
		$startcol = (ArNewProblemPeer::NUM_COLUMNS - ArNewProblemPeer::NUM_LAZY_LOAD_COLUMNS);
		ArProblemResponsiblePeer::addSelectColumns($criteria);

		$criteria->addJoin(ArNewProblemPeer::AR_PROBLEM_RESPONSIBLE_ID, ArProblemResponsiblePeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArNewProblemPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArNewProblemPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {

				$cls = ArNewProblemPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArNewProblemPeer::addInstanceToPool($obj1, $key1);
			} // if $obj1 already loaded

			$key2 = ArProblemResponsiblePeer::getPrimaryKeyHashFromRow($row, $startcol);
			if ($key2 !== null) {
				$obj2 = ArProblemResponsiblePeer::getInstanceFromPool($key2);
				if (!$obj2) {

					$cls = ArProblemResponsiblePeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol);
					ArProblemResponsiblePeer::addInstanceToPool($obj2, $key2);
				} // if obj2 already loaded
				
				// Add the $obj1 (ArNewProblem) to $obj2 (ArProblemResponsible)
				$obj2->addArNewProblem($obj1);

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
		$criteria->setPrimaryTableName(ArNewProblemPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArNewProblemPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArNewProblemPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArNewProblemPeer::AR_PROBLEM_TYPE_ID, ArProblemTypePeer::ID, $join_behavior);

		$criteria->addJoin(ArNewProblemPeer::AR_PROBLEM_DOMAIN_ID, ArProblemDomainPeer::ID, $join_behavior);

		$criteria->addJoin(ArNewProblemPeer::AR_PROBLEM_RESPONSIBLE_ID, ArProblemResponsiblePeer::ID, $join_behavior);

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
	 * Selects a collection of ArNewProblem objects pre-filled with all related objects.
	 *
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArNewProblem objects.
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

		ArNewProblemPeer::addSelectColumns($criteria);
		$startcol2 = (ArNewProblemPeer::NUM_COLUMNS - ArNewProblemPeer::NUM_LAZY_LOAD_COLUMNS);

		ArProblemTypePeer::addSelectColumns($criteria);
		$startcol3 = $startcol2 + (ArProblemTypePeer::NUM_COLUMNS - ArProblemTypePeer::NUM_LAZY_LOAD_COLUMNS);

		ArProblemDomainPeer::addSelectColumns($criteria);
		$startcol4 = $startcol3 + (ArProblemDomainPeer::NUM_COLUMNS - ArProblemDomainPeer::NUM_LAZY_LOAD_COLUMNS);

		ArProblemResponsiblePeer::addSelectColumns($criteria);
		$startcol5 = $startcol4 + (ArProblemResponsiblePeer::NUM_COLUMNS - ArProblemResponsiblePeer::NUM_LAZY_LOAD_COLUMNS);

		$criteria->addJoin(ArNewProblemPeer::AR_PROBLEM_TYPE_ID, ArProblemTypePeer::ID, $join_behavior);

		$criteria->addJoin(ArNewProblemPeer::AR_PROBLEM_DOMAIN_ID, ArProblemDomainPeer::ID, $join_behavior);

		$criteria->addJoin(ArNewProblemPeer::AR_PROBLEM_RESPONSIBLE_ID, ArProblemResponsiblePeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArNewProblemPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArNewProblemPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {
				$cls = ArNewProblemPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArNewProblemPeer::addInstanceToPool($obj1, $key1);
			} // if obj1 already loaded

			// Add objects for joined ArProblemType rows

			$key2 = ArProblemTypePeer::getPrimaryKeyHashFromRow($row, $startcol2);
			if ($key2 !== null) {
				$obj2 = ArProblemTypePeer::getInstanceFromPool($key2);
				if (!$obj2) {

					$cls = ArProblemTypePeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol2);
					ArProblemTypePeer::addInstanceToPool($obj2, $key2);
				} // if obj2 loaded

				// Add the $obj1 (ArNewProblem) to the collection in $obj2 (ArProblemType)
				$obj2->addArNewProblem($obj1);
			} // if joined row not null

			// Add objects for joined ArProblemDomain rows

			$key3 = ArProblemDomainPeer::getPrimaryKeyHashFromRow($row, $startcol3);
			if ($key3 !== null) {
				$obj3 = ArProblemDomainPeer::getInstanceFromPool($key3);
				if (!$obj3) {

					$cls = ArProblemDomainPeer::getOMClass(false);

					$obj3 = new $cls();
					$obj3->hydrate($row, $startcol3);
					ArProblemDomainPeer::addInstanceToPool($obj3, $key3);
				} // if obj3 loaded

				// Add the $obj1 (ArNewProblem) to the collection in $obj3 (ArProblemDomain)
				$obj3->addArNewProblem($obj1);
			} // if joined row not null

			// Add objects for joined ArProblemResponsible rows

			$key4 = ArProblemResponsiblePeer::getPrimaryKeyHashFromRow($row, $startcol4);
			if ($key4 !== null) {
				$obj4 = ArProblemResponsiblePeer::getInstanceFromPool($key4);
				if (!$obj4) {

					$cls = ArProblemResponsiblePeer::getOMClass(false);

					$obj4 = new $cls();
					$obj4->hydrate($row, $startcol4);
					ArProblemResponsiblePeer::addInstanceToPool($obj4, $key4);
				} // if obj4 loaded

				// Add the $obj1 (ArNewProblem) to the collection in $obj4 (ArProblemResponsible)
				$obj4->addArNewProblem($obj1);
			} // if joined row not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Returns the number of rows matching criteria, joining the related ArProblemType table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinAllExceptArProblemType(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArNewProblemPeer::TABLE_NAME);
		
		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArNewProblemPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY should not affect count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArNewProblemPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}
	
		$criteria->addJoin(ArNewProblemPeer::AR_PROBLEM_DOMAIN_ID, ArProblemDomainPeer::ID, $join_behavior);

		$criteria->addJoin(ArNewProblemPeer::AR_PROBLEM_RESPONSIBLE_ID, ArProblemResponsiblePeer::ID, $join_behavior);

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
	 * Returns the number of rows matching criteria, joining the related ArProblemDomain table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinAllExceptArProblemDomain(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArNewProblemPeer::TABLE_NAME);
		
		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArNewProblemPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY should not affect count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArNewProblemPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}
	
		$criteria->addJoin(ArNewProblemPeer::AR_PROBLEM_TYPE_ID, ArProblemTypePeer::ID, $join_behavior);

		$criteria->addJoin(ArNewProblemPeer::AR_PROBLEM_RESPONSIBLE_ID, ArProblemResponsiblePeer::ID, $join_behavior);

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
	 * Returns the number of rows matching criteria, joining the related ArProblemResponsible table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinAllExceptArProblemResponsible(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArNewProblemPeer::TABLE_NAME);
		
		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArNewProblemPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY should not affect count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArNewProblemPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}
	
		$criteria->addJoin(ArNewProblemPeer::AR_PROBLEM_TYPE_ID, ArProblemTypePeer::ID, $join_behavior);

		$criteria->addJoin(ArNewProblemPeer::AR_PROBLEM_DOMAIN_ID, ArProblemDomainPeer::ID, $join_behavior);

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
	 * Selects a collection of ArNewProblem objects pre-filled with all related objects except ArProblemType.
	 *
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArNewProblem objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinAllExceptArProblemType(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		// $criteria->getDbName() will return the same object if not set to another value
		// so == check is okay and faster
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArNewProblemPeer::addSelectColumns($criteria);
		$startcol2 = (ArNewProblemPeer::NUM_COLUMNS - ArNewProblemPeer::NUM_LAZY_LOAD_COLUMNS);

		ArProblemDomainPeer::addSelectColumns($criteria);
		$startcol3 = $startcol2 + (ArProblemDomainPeer::NUM_COLUMNS - ArProblemDomainPeer::NUM_LAZY_LOAD_COLUMNS);

		ArProblemResponsiblePeer::addSelectColumns($criteria);
		$startcol4 = $startcol3 + (ArProblemResponsiblePeer::NUM_COLUMNS - ArProblemResponsiblePeer::NUM_LAZY_LOAD_COLUMNS);

		$criteria->addJoin(ArNewProblemPeer::AR_PROBLEM_DOMAIN_ID, ArProblemDomainPeer::ID, $join_behavior);

		$criteria->addJoin(ArNewProblemPeer::AR_PROBLEM_RESPONSIBLE_ID, ArProblemResponsiblePeer::ID, $join_behavior);


		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArNewProblemPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArNewProblemPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {
				$cls = ArNewProblemPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArNewProblemPeer::addInstanceToPool($obj1, $key1);
			} // if obj1 already loaded

				// Add objects for joined ArProblemDomain rows

				$key2 = ArProblemDomainPeer::getPrimaryKeyHashFromRow($row, $startcol2);
				if ($key2 !== null) {
					$obj2 = ArProblemDomainPeer::getInstanceFromPool($key2);
					if (!$obj2) {
	
						$cls = ArProblemDomainPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol2);
					ArProblemDomainPeer::addInstanceToPool($obj2, $key2);
				} // if $obj2 already loaded

				// Add the $obj1 (ArNewProblem) to the collection in $obj2 (ArProblemDomain)
				$obj2->addArNewProblem($obj1);

			} // if joined row is not null

				// Add objects for joined ArProblemResponsible rows

				$key3 = ArProblemResponsiblePeer::getPrimaryKeyHashFromRow($row, $startcol3);
				if ($key3 !== null) {
					$obj3 = ArProblemResponsiblePeer::getInstanceFromPool($key3);
					if (!$obj3) {
	
						$cls = ArProblemResponsiblePeer::getOMClass(false);

					$obj3 = new $cls();
					$obj3->hydrate($row, $startcol3);
					ArProblemResponsiblePeer::addInstanceToPool($obj3, $key3);
				} // if $obj3 already loaded

				// Add the $obj1 (ArNewProblem) to the collection in $obj3 (ArProblemResponsible)
				$obj3->addArNewProblem($obj1);

			} // if joined row is not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArNewProblem objects pre-filled with all related objects except ArProblemDomain.
	 *
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArNewProblem objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinAllExceptArProblemDomain(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		// $criteria->getDbName() will return the same object if not set to another value
		// so == check is okay and faster
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArNewProblemPeer::addSelectColumns($criteria);
		$startcol2 = (ArNewProblemPeer::NUM_COLUMNS - ArNewProblemPeer::NUM_LAZY_LOAD_COLUMNS);

		ArProblemTypePeer::addSelectColumns($criteria);
		$startcol3 = $startcol2 + (ArProblemTypePeer::NUM_COLUMNS - ArProblemTypePeer::NUM_LAZY_LOAD_COLUMNS);

		ArProblemResponsiblePeer::addSelectColumns($criteria);
		$startcol4 = $startcol3 + (ArProblemResponsiblePeer::NUM_COLUMNS - ArProblemResponsiblePeer::NUM_LAZY_LOAD_COLUMNS);

		$criteria->addJoin(ArNewProblemPeer::AR_PROBLEM_TYPE_ID, ArProblemTypePeer::ID, $join_behavior);

		$criteria->addJoin(ArNewProblemPeer::AR_PROBLEM_RESPONSIBLE_ID, ArProblemResponsiblePeer::ID, $join_behavior);


		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArNewProblemPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArNewProblemPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {
				$cls = ArNewProblemPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArNewProblemPeer::addInstanceToPool($obj1, $key1);
			} // if obj1 already loaded

				// Add objects for joined ArProblemType rows

				$key2 = ArProblemTypePeer::getPrimaryKeyHashFromRow($row, $startcol2);
				if ($key2 !== null) {
					$obj2 = ArProblemTypePeer::getInstanceFromPool($key2);
					if (!$obj2) {
	
						$cls = ArProblemTypePeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol2);
					ArProblemTypePeer::addInstanceToPool($obj2, $key2);
				} // if $obj2 already loaded

				// Add the $obj1 (ArNewProblem) to the collection in $obj2 (ArProblemType)
				$obj2->addArNewProblem($obj1);

			} // if joined row is not null

				// Add objects for joined ArProblemResponsible rows

				$key3 = ArProblemResponsiblePeer::getPrimaryKeyHashFromRow($row, $startcol3);
				if ($key3 !== null) {
					$obj3 = ArProblemResponsiblePeer::getInstanceFromPool($key3);
					if (!$obj3) {
	
						$cls = ArProblemResponsiblePeer::getOMClass(false);

					$obj3 = new $cls();
					$obj3->hydrate($row, $startcol3);
					ArProblemResponsiblePeer::addInstanceToPool($obj3, $key3);
				} // if $obj3 already loaded

				// Add the $obj1 (ArNewProblem) to the collection in $obj3 (ArProblemResponsible)
				$obj3->addArNewProblem($obj1);

			} // if joined row is not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArNewProblem objects pre-filled with all related objects except ArProblemResponsible.
	 *
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArNewProblem objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinAllExceptArProblemResponsible(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		// $criteria->getDbName() will return the same object if not set to another value
		// so == check is okay and faster
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArNewProblemPeer::addSelectColumns($criteria);
		$startcol2 = (ArNewProblemPeer::NUM_COLUMNS - ArNewProblemPeer::NUM_LAZY_LOAD_COLUMNS);

		ArProblemTypePeer::addSelectColumns($criteria);
		$startcol3 = $startcol2 + (ArProblemTypePeer::NUM_COLUMNS - ArProblemTypePeer::NUM_LAZY_LOAD_COLUMNS);

		ArProblemDomainPeer::addSelectColumns($criteria);
		$startcol4 = $startcol3 + (ArProblemDomainPeer::NUM_COLUMNS - ArProblemDomainPeer::NUM_LAZY_LOAD_COLUMNS);

		$criteria->addJoin(ArNewProblemPeer::AR_PROBLEM_TYPE_ID, ArProblemTypePeer::ID, $join_behavior);

		$criteria->addJoin(ArNewProblemPeer::AR_PROBLEM_DOMAIN_ID, ArProblemDomainPeer::ID, $join_behavior);


		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArNewProblemPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArNewProblemPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {
				$cls = ArNewProblemPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArNewProblemPeer::addInstanceToPool($obj1, $key1);
			} // if obj1 already loaded

				// Add objects for joined ArProblemType rows

				$key2 = ArProblemTypePeer::getPrimaryKeyHashFromRow($row, $startcol2);
				if ($key2 !== null) {
					$obj2 = ArProblemTypePeer::getInstanceFromPool($key2);
					if (!$obj2) {
	
						$cls = ArProblemTypePeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol2);
					ArProblemTypePeer::addInstanceToPool($obj2, $key2);
				} // if $obj2 already loaded

				// Add the $obj1 (ArNewProblem) to the collection in $obj2 (ArProblemType)
				$obj2->addArNewProblem($obj1);

			} // if joined row is not null

				// Add objects for joined ArProblemDomain rows

				$key3 = ArProblemDomainPeer::getPrimaryKeyHashFromRow($row, $startcol3);
				if ($key3 !== null) {
					$obj3 = ArProblemDomainPeer::getInstanceFromPool($key3);
					if (!$obj3) {
	
						$cls = ArProblemDomainPeer::getOMClass(false);

					$obj3 = new $cls();
					$obj3->hydrate($row, $startcol3);
					ArProblemDomainPeer::addInstanceToPool($obj3, $key3);
				} // if $obj3 already loaded

				// Add the $obj1 (ArNewProblem) to the collection in $obj3 (ArProblemDomain)
				$obj3->addArNewProblem($obj1);

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
	  $dbMap = Propel::getDatabaseMap(BaseArNewProblemPeer::DATABASE_NAME);
	  if (!$dbMap->hasTable(BaseArNewProblemPeer::TABLE_NAME))
	  {
	    $dbMap->addTableObject(new ArNewProblemTableMap());
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
		return $withPrefix ? ArNewProblemPeer::CLASS_DEFAULT : ArNewProblemPeer::OM_CLASS;
	}

	/**
	 * Method perform an INSERT on the database, given a ArNewProblem or Criteria object.
	 *
	 * @param      mixed $values Criteria or ArNewProblem object containing data that is used to create the INSERT statement.
	 * @param      PropelPDO $con the PropelPDO connection to use
	 * @return     mixed The new primary key.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doInsert($values, PropelPDO $con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArNewProblemPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		if ($values instanceof Criteria) {
			$criteria = clone $values; // rename for clarity
		} else {
			$criteria = $values->buildCriteria(); // build Criteria from ArNewProblem object
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
	 * Method perform an UPDATE on the database, given a ArNewProblem or Criteria object.
	 *
	 * @param      mixed $values Criteria or ArNewProblem object containing data that is used to create the UPDATE statement.
	 * @param      PropelPDO $con The connection to use (specify PropelPDO connection object to exert more control over transactions).
	 * @return     int The number of affected rows (if supported by underlying database driver).
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doUpdate($values, PropelPDO $con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArNewProblemPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		$selectCriteria = new Criteria(self::DATABASE_NAME);

		if ($values instanceof Criteria) {
			$criteria = clone $values; // rename for clarity

			$comparison = $criteria->getComparison(ArNewProblemPeer::DUPLICATION_KEY);
			$selectCriteria->add(ArNewProblemPeer::DUPLICATION_KEY, $criteria->remove(ArNewProblemPeer::DUPLICATION_KEY), $comparison);

		} else { // $values is ArNewProblem object
			$criteria = $values->buildCriteria(); // gets full criteria
			$selectCriteria = $values->buildPkeyCriteria(); // gets criteria w/ primary key(s)
		}

		// set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		return BasePeer::doUpdate($selectCriteria, $criteria, $con);
	}

	/**
	 * Method to DELETE all rows from the ar_new_problem table.
	 *
	 * @return     int The number of affected rows (if supported by underlying database driver).
	 */
	public static function doDeleteAll($con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArNewProblemPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}
		$affectedRows = 0; // initialize var to track total num of affected rows
		try {
			// use transaction because $criteria could contain info
			// for more than one table or we could emulating ON DELETE CASCADE, etc.
			$con->beginTransaction();
			$affectedRows += BasePeer::doDeleteAll(ArNewProblemPeer::TABLE_NAME, $con);
			// Because this db requires some delete cascade/set null emulation, we have to
			// clear the cached instance *after* the emulation has happened (since
			// instances get re-added by the select statement contained therein).
			ArNewProblemPeer::clearInstancePool();
			ArNewProblemPeer::clearRelatedInstancePool();
			$con->commit();
			return $affectedRows;
		} catch (PropelException $e) {
			$con->rollBack();
			throw $e;
		}
	}

	/**
	 * Method perform a DELETE on the database, given a ArNewProblem or Criteria object OR a primary key value.
	 *
	 * @param      mixed $values Criteria or ArNewProblem object or primary key or array of primary keys
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
			$con = Propel::getConnection(ArNewProblemPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		if ($values instanceof Criteria) {
			// invalidate the cache for all objects of this type, since we have no
			// way of knowing (without running a query) what objects should be invalidated
			// from the cache based on this Criteria.
			ArNewProblemPeer::clearInstancePool();
			// rename for clarity
			$criteria = clone $values;
		} elseif ($values instanceof ArNewProblem) { // it's a model object
			// invalidate the cache for this single object
			ArNewProblemPeer::removeInstanceFromPool($values);
			// create criteria based on pk values
			$criteria = $values->buildPkeyCriteria();
		} else { // it's a primary key, or an array of pks
			$criteria = new Criteria(self::DATABASE_NAME);
			$criteria->add(ArNewProblemPeer::DUPLICATION_KEY, (array) $values, Criteria::IN);
			// invalidate the cache for this object(s)
			foreach ((array) $values as $singleval) {
				ArNewProblemPeer::removeInstanceFromPool($singleval);
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
			ArNewProblemPeer::clearRelatedInstancePool();
			$con->commit();
			return $affectedRows;
		} catch (PropelException $e) {
			$con->rollBack();
			throw $e;
		}
	}

	/**
	 * Validates all modified columns of given ArNewProblem object.
	 * If parameter $columns is either a single column name or an array of column names
	 * than only those columns are validated.
	 *
	 * NOTICE: This does not apply to primary or foreign keys for now.
	 *
	 * @param      ArNewProblem $obj The object to validate.
	 * @param      mixed $cols Column name or array of column names.
	 *
	 * @return     mixed TRUE if all columns are valid or the error message of the first invalid column.
	 */
	public static function doValidate(ArNewProblem $obj, $cols = null)
	{
		$columns = array();

		if ($cols) {
			$dbMap = Propel::getDatabaseMap(ArNewProblemPeer::DATABASE_NAME);
			$tableMap = $dbMap->getTable(ArNewProblemPeer::TABLE_NAME);

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

		return BasePeer::doValidate(ArNewProblemPeer::DATABASE_NAME, ArNewProblemPeer::TABLE_NAME, $columns);
	}

	/**
	 * Retrieve a single object by pkey.
	 *
	 * @param      string $pk the primary key.
	 * @param      PropelPDO $con the connection to use
	 * @return     ArNewProblem
	 */
	public static function retrieveByPK($pk, PropelPDO $con = null)
	{

		if (null !== ($obj = ArNewProblemPeer::getInstanceFromPool((string) $pk))) {
			return $obj;
		}

		if ($con === null) {
			$con = Propel::getConnection(ArNewProblemPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria = new Criteria(ArNewProblemPeer::DATABASE_NAME);
		$criteria->add(ArNewProblemPeer::DUPLICATION_KEY, $pk);

		$v = ArNewProblemPeer::doSelect($criteria, $con);

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
			$con = Propel::getConnection(ArNewProblemPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$objs = null;
		if (empty($pks)) {
			$objs = array();
		} else {
			$criteria = new Criteria(ArNewProblemPeer::DATABASE_NAME);
			$criteria->add(ArNewProblemPeer::DUPLICATION_KEY, $pks, Criteria::IN);
			$objs = ArNewProblemPeer::doSelect($criteria, $con);
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

} // BaseArNewProblemPeer

// This is the static code needed to register the TableMap for this table with the main Propel class.
//
BaseArNewProblemPeer::buildTableMap();

