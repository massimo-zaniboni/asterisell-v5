<?php

/**
 * Base static class for performing query and update operations on the 'ar_source_csv_file' table.
 *
 * 
 *
 * @package    lib.model.om
 */
abstract class BaseArSourceCsvFilePeer {

	/** the default database name for this class */
	const DATABASE_NAME = 'propel';

	/** the table name for this class */
	const TABLE_NAME = 'ar_source_csv_file';

	/** the related Propel class for this table */
	const OM_CLASS = 'ArSourceCsvFile';

	/** A class that can be returned by this peer. */
	const CLASS_DEFAULT = 'lib.model.ArSourceCsvFile';

	/** the related TableMap class for this table */
	const TM_CLASS = 'ArSourceCsvFileTableMap';
	
	/** The total number of columns. */
	const NUM_COLUMNS = 17;

	/** The number of lazy-loaded columns. */
	const NUM_LAZY_LOAD_COLUMNS = 0;

	/** the column name for the ID field */
	const ID = 'ar_source_csv_file.ID';

	/** the column name for the AR_CDR_PROVIDER_ID field */
	const AR_CDR_PROVIDER_ID = 'ar_source_csv_file.AR_CDR_PROVIDER_ID';

	/** the column name for the AR_PHYSICAL_FORMAT_ID field */
	const AR_PHYSICAL_FORMAT_ID = 'ar_source_csv_file.AR_PHYSICAL_FORMAT_ID';

	/** the column name for the RETRIEVED_FROM_SERVER field */
	const RETRIEVED_FROM_SERVER = 'ar_source_csv_file.RETRIEVED_FROM_SERVER';

	/** the column name for the IS_STATUS field */
	const IS_STATUS = 'ar_source_csv_file.IS_STATUS';

	/** the column name for the IS_CALLDATE_PROCESSED field */
	const IS_CALLDATE_PROCESSED = 'ar_source_csv_file.IS_CALLDATE_PROCESSED';

	/** the column name for the IS_IMPORTED field */
	const IS_IMPORTED = 'ar_source_csv_file.IS_IMPORTED';

	/** the column name for the IS_ACTIVE_INFO field */
	const IS_ACTIVE_INFO = 'ar_source_csv_file.IS_ACTIVE_INFO';

	/** the column name for the MIN_CALLDATE field */
	const MIN_CALLDATE = 'ar_source_csv_file.MIN_CALLDATE';

	/** the column name for the MAX_CALLDATE field */
	const MAX_CALLDATE = 'ar_source_csv_file.MAX_CALLDATE';

	/** the column name for the NAME field */
	const NAME = 'ar_source_csv_file.NAME';

	/** the column name for the ARCHIVE_DIRECTORY field */
	const ARCHIVE_DIRECTORY = 'ar_source_csv_file.ARCHIVE_DIRECTORY';

	/** the column name for the CHECKSUM field */
	const CHECKSUM = 'ar_source_csv_file.CHECKSUM';

	/** the column name for the RECEIVING_DATE field */
	const RECEIVING_DATE = 'ar_source_csv_file.RECEIVING_DATE';

	/** the column name for the SERIOUS_PROCESSING_ERRORS field */
	const SERIOUS_PROCESSING_ERRORS = 'ar_source_csv_file.SERIOUS_PROCESSING_ERRORS';

	/** the column name for the TOT_LINES field */
	const TOT_LINES = 'ar_source_csv_file.TOT_LINES';

	/** the column name for the LINES_WITH_ERRORS field */
	const LINES_WITH_ERRORS = 'ar_source_csv_file.LINES_WITH_ERRORS';

	/**
	 * An identiy map to hold any loaded instances of ArSourceCsvFile objects.
	 * This must be public so that other peer classes can access this when hydrating from JOIN
	 * queries.
	 * @var        array ArSourceCsvFile[]
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
		BasePeer::TYPE_PHPNAME => array ('Id', 'ArCdrProviderId', 'ArPhysicalFormatId', 'RetrievedFromServer', 'IsStatus', 'IsCalldateProcessed', 'IsImported', 'IsActiveInfo', 'MinCalldate', 'MaxCalldate', 'Name', 'ArchiveDirectory', 'Checksum', 'ReceivingDate', 'SeriousProcessingErrors', 'TotLines', 'LinesWithErrors', ),
		BasePeer::TYPE_STUDLYPHPNAME => array ('id', 'arCdrProviderId', 'arPhysicalFormatId', 'retrievedFromServer', 'isStatus', 'isCalldateProcessed', 'isImported', 'isActiveInfo', 'minCalldate', 'maxCalldate', 'name', 'archiveDirectory', 'checksum', 'receivingDate', 'seriousProcessingErrors', 'totLines', 'linesWithErrors', ),
		BasePeer::TYPE_COLNAME => array (self::ID, self::AR_CDR_PROVIDER_ID, self::AR_PHYSICAL_FORMAT_ID, self::RETRIEVED_FROM_SERVER, self::IS_STATUS, self::IS_CALLDATE_PROCESSED, self::IS_IMPORTED, self::IS_ACTIVE_INFO, self::MIN_CALLDATE, self::MAX_CALLDATE, self::NAME, self::ARCHIVE_DIRECTORY, self::CHECKSUM, self::RECEIVING_DATE, self::SERIOUS_PROCESSING_ERRORS, self::TOT_LINES, self::LINES_WITH_ERRORS, ),
		BasePeer::TYPE_FIELDNAME => array ('id', 'ar_cdr_provider_id', 'ar_physical_format_id', 'retrieved_from_server', 'is_status', 'is_calldate_processed', 'is_imported', 'is_active_info', 'min_calldate', 'max_calldate', 'name', 'archive_directory', 'checksum', 'receiving_date', 'serious_processing_errors', 'tot_lines', 'lines_with_errors', ),
		BasePeer::TYPE_NUM => array (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, )
	);

	/**
	 * holds an array of keys for quick access to the fieldnames array
	 *
	 * first dimension keys are the type constants
	 * e.g. self::$fieldNames[BasePeer::TYPE_PHPNAME]['Id'] = 0
	 */
	private static $fieldKeys = array (
		BasePeer::TYPE_PHPNAME => array ('Id' => 0, 'ArCdrProviderId' => 1, 'ArPhysicalFormatId' => 2, 'RetrievedFromServer' => 3, 'IsStatus' => 4, 'IsCalldateProcessed' => 5, 'IsImported' => 6, 'IsActiveInfo' => 7, 'MinCalldate' => 8, 'MaxCalldate' => 9, 'Name' => 10, 'ArchiveDirectory' => 11, 'Checksum' => 12, 'ReceivingDate' => 13, 'SeriousProcessingErrors' => 14, 'TotLines' => 15, 'LinesWithErrors' => 16, ),
		BasePeer::TYPE_STUDLYPHPNAME => array ('id' => 0, 'arCdrProviderId' => 1, 'arPhysicalFormatId' => 2, 'retrievedFromServer' => 3, 'isStatus' => 4, 'isCalldateProcessed' => 5, 'isImported' => 6, 'isActiveInfo' => 7, 'minCalldate' => 8, 'maxCalldate' => 9, 'name' => 10, 'archiveDirectory' => 11, 'checksum' => 12, 'receivingDate' => 13, 'seriousProcessingErrors' => 14, 'totLines' => 15, 'linesWithErrors' => 16, ),
		BasePeer::TYPE_COLNAME => array (self::ID => 0, self::AR_CDR_PROVIDER_ID => 1, self::AR_PHYSICAL_FORMAT_ID => 2, self::RETRIEVED_FROM_SERVER => 3, self::IS_STATUS => 4, self::IS_CALLDATE_PROCESSED => 5, self::IS_IMPORTED => 6, self::IS_ACTIVE_INFO => 7, self::MIN_CALLDATE => 8, self::MAX_CALLDATE => 9, self::NAME => 10, self::ARCHIVE_DIRECTORY => 11, self::CHECKSUM => 12, self::RECEIVING_DATE => 13, self::SERIOUS_PROCESSING_ERRORS => 14, self::TOT_LINES => 15, self::LINES_WITH_ERRORS => 16, ),
		BasePeer::TYPE_FIELDNAME => array ('id' => 0, 'ar_cdr_provider_id' => 1, 'ar_physical_format_id' => 2, 'retrieved_from_server' => 3, 'is_status' => 4, 'is_calldate_processed' => 5, 'is_imported' => 6, 'is_active_info' => 7, 'min_calldate' => 8, 'max_calldate' => 9, 'name' => 10, 'archive_directory' => 11, 'checksum' => 12, 'receiving_date' => 13, 'serious_processing_errors' => 14, 'tot_lines' => 15, 'lines_with_errors' => 16, ),
		BasePeer::TYPE_NUM => array (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, )
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
	 * @param      string $column The column name for current table. (i.e. ArSourceCsvFilePeer::COLUMN_NAME).
	 * @return     string
	 */
	public static function alias($alias, $column)
	{
		return str_replace(ArSourceCsvFilePeer::TABLE_NAME.'.', $alias.'.', $column);
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
		$criteria->addSelectColumn(ArSourceCsvFilePeer::ID);
		$criteria->addSelectColumn(ArSourceCsvFilePeer::AR_CDR_PROVIDER_ID);
		$criteria->addSelectColumn(ArSourceCsvFilePeer::AR_PHYSICAL_FORMAT_ID);
		$criteria->addSelectColumn(ArSourceCsvFilePeer::RETRIEVED_FROM_SERVER);
		$criteria->addSelectColumn(ArSourceCsvFilePeer::IS_STATUS);
		$criteria->addSelectColumn(ArSourceCsvFilePeer::IS_CALLDATE_PROCESSED);
		$criteria->addSelectColumn(ArSourceCsvFilePeer::IS_IMPORTED);
		$criteria->addSelectColumn(ArSourceCsvFilePeer::IS_ACTIVE_INFO);
		$criteria->addSelectColumn(ArSourceCsvFilePeer::MIN_CALLDATE);
		$criteria->addSelectColumn(ArSourceCsvFilePeer::MAX_CALLDATE);
		$criteria->addSelectColumn(ArSourceCsvFilePeer::NAME);
		$criteria->addSelectColumn(ArSourceCsvFilePeer::ARCHIVE_DIRECTORY);
		$criteria->addSelectColumn(ArSourceCsvFilePeer::CHECKSUM);
		$criteria->addSelectColumn(ArSourceCsvFilePeer::RECEIVING_DATE);
		$criteria->addSelectColumn(ArSourceCsvFilePeer::SERIOUS_PROCESSING_ERRORS);
		$criteria->addSelectColumn(ArSourceCsvFilePeer::TOT_LINES);
		$criteria->addSelectColumn(ArSourceCsvFilePeer::LINES_WITH_ERRORS);
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
		$criteria->setPrimaryTableName(ArSourceCsvFilePeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArSourceCsvFilePeer::addSelectColumns($criteria);
		}

		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		$criteria->setDbName(self::DATABASE_NAME); // Set the correct dbName

		if ($con === null) {
			$con = Propel::getConnection(ArSourceCsvFilePeer::DATABASE_NAME, Propel::CONNECTION_READ);
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
	 * @return     ArSourceCsvFile
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectOne(Criteria $criteria, PropelPDO $con = null)
	{
		$critcopy = clone $criteria;
		$critcopy->setLimit(1);
		$objects = ArSourceCsvFilePeer::doSelect($critcopy, $con);
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
		return ArSourceCsvFilePeer::populateObjects(ArSourceCsvFilePeer::doSelectStmt($criteria, $con));
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
			$con = Propel::getConnection(ArSourceCsvFilePeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		if (!$criteria->hasSelectClause()) {
			$criteria = clone $criteria;
			ArSourceCsvFilePeer::addSelectColumns($criteria);
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
	 * @param      ArSourceCsvFile $value A ArSourceCsvFile object.
	 * @param      string $key (optional) key to use for instance map (for performance boost if key was already calculated externally).
	 */
	public static function addInstanceToPool(ArSourceCsvFile $obj, $key = null)
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
	 * @param      mixed $value A ArSourceCsvFile object or a primary key value.
	 */
	public static function removeInstanceFromPool($value)
	{
		if (Propel::isInstancePoolingEnabled() && $value !== null) {
			if (is_object($value) && $value instanceof ArSourceCsvFile) {
				$key = (string) $value->getId();
			} elseif (is_scalar($value)) {
				// assume we've been passed a primary key
				$key = (string) $value;
			} else {
				$e = new PropelException("Invalid value passed to removeInstanceFromPool().  Expected primary key or ArSourceCsvFile object; got " . (is_object($value) ? get_class($value) . ' object.' : var_export($value,true)));
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
	 * @return     ArSourceCsvFile Found object or NULL if 1) no instance exists for specified key or 2) instance pooling has been disabled.
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
	 * Method to invalidate the instance pool of all tables related to ar_source_csv_file
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
		$cls = ArSourceCsvFilePeer::getOMClass(false);
		// populate the object(s)
		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key = ArSourceCsvFilePeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj = ArSourceCsvFilePeer::getInstanceFromPool($key))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj->hydrate($row, 0, true); // rehydrate
				$results[] = $obj;
			} else {
				$obj = new $cls();
				$obj->hydrate($row);
				$results[] = $obj;
				ArSourceCsvFilePeer::addInstanceToPool($obj, $key);
			} // if key exists
		}
		$stmt->closeCursor();
		return $results;
	}

	/**
	 * Returns the number of rows matching criteria, joining the related ArCdrProvider table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinArCdrProvider(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArSourceCsvFilePeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArSourceCsvFilePeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArSourceCsvFilePeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArSourceCsvFilePeer::AR_CDR_PROVIDER_ID, ArCdrProviderPeer::ID, $join_behavior);

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
	 * Returns the number of rows matching criteria, joining the related ArPhysicalFormat table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinArPhysicalFormat(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArSourceCsvFilePeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArSourceCsvFilePeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArSourceCsvFilePeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArSourceCsvFilePeer::AR_PHYSICAL_FORMAT_ID, ArPhysicalFormatPeer::ID, $join_behavior);

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
	 * Selects a collection of ArSourceCsvFile objects pre-filled with their ArCdrProvider objects.
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArSourceCsvFile objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinArCdrProvider(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArSourceCsvFilePeer::addSelectColumns($criteria);
		$startcol = (ArSourceCsvFilePeer::NUM_COLUMNS - ArSourceCsvFilePeer::NUM_LAZY_LOAD_COLUMNS);
		ArCdrProviderPeer::addSelectColumns($criteria);

		$criteria->addJoin(ArSourceCsvFilePeer::AR_CDR_PROVIDER_ID, ArCdrProviderPeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArSourceCsvFilePeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArSourceCsvFilePeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {

				$cls = ArSourceCsvFilePeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArSourceCsvFilePeer::addInstanceToPool($obj1, $key1);
			} // if $obj1 already loaded

			$key2 = ArCdrProviderPeer::getPrimaryKeyHashFromRow($row, $startcol);
			if ($key2 !== null) {
				$obj2 = ArCdrProviderPeer::getInstanceFromPool($key2);
				if (!$obj2) {

					$cls = ArCdrProviderPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol);
					ArCdrProviderPeer::addInstanceToPool($obj2, $key2);
				} // if obj2 already loaded
				
				// Add the $obj1 (ArSourceCsvFile) to $obj2 (ArCdrProvider)
				$obj2->addArSourceCsvFile($obj1);

			} // if joined row was not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArSourceCsvFile objects pre-filled with their ArPhysicalFormat objects.
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArSourceCsvFile objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinArPhysicalFormat(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArSourceCsvFilePeer::addSelectColumns($criteria);
		$startcol = (ArSourceCsvFilePeer::NUM_COLUMNS - ArSourceCsvFilePeer::NUM_LAZY_LOAD_COLUMNS);
		ArPhysicalFormatPeer::addSelectColumns($criteria);

		$criteria->addJoin(ArSourceCsvFilePeer::AR_PHYSICAL_FORMAT_ID, ArPhysicalFormatPeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArSourceCsvFilePeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArSourceCsvFilePeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {

				$cls = ArSourceCsvFilePeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArSourceCsvFilePeer::addInstanceToPool($obj1, $key1);
			} // if $obj1 already loaded

			$key2 = ArPhysicalFormatPeer::getPrimaryKeyHashFromRow($row, $startcol);
			if ($key2 !== null) {
				$obj2 = ArPhysicalFormatPeer::getInstanceFromPool($key2);
				if (!$obj2) {

					$cls = ArPhysicalFormatPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol);
					ArPhysicalFormatPeer::addInstanceToPool($obj2, $key2);
				} // if obj2 already loaded
				
				// Add the $obj1 (ArSourceCsvFile) to $obj2 (ArPhysicalFormat)
				$obj2->addArSourceCsvFile($obj1);

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
		$criteria->setPrimaryTableName(ArSourceCsvFilePeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArSourceCsvFilePeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArSourceCsvFilePeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArSourceCsvFilePeer::AR_CDR_PROVIDER_ID, ArCdrProviderPeer::ID, $join_behavior);

		$criteria->addJoin(ArSourceCsvFilePeer::AR_PHYSICAL_FORMAT_ID, ArPhysicalFormatPeer::ID, $join_behavior);

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
	 * Selects a collection of ArSourceCsvFile objects pre-filled with all related objects.
	 *
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArSourceCsvFile objects.
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

		ArSourceCsvFilePeer::addSelectColumns($criteria);
		$startcol2 = (ArSourceCsvFilePeer::NUM_COLUMNS - ArSourceCsvFilePeer::NUM_LAZY_LOAD_COLUMNS);

		ArCdrProviderPeer::addSelectColumns($criteria);
		$startcol3 = $startcol2 + (ArCdrProviderPeer::NUM_COLUMNS - ArCdrProviderPeer::NUM_LAZY_LOAD_COLUMNS);

		ArPhysicalFormatPeer::addSelectColumns($criteria);
		$startcol4 = $startcol3 + (ArPhysicalFormatPeer::NUM_COLUMNS - ArPhysicalFormatPeer::NUM_LAZY_LOAD_COLUMNS);

		$criteria->addJoin(ArSourceCsvFilePeer::AR_CDR_PROVIDER_ID, ArCdrProviderPeer::ID, $join_behavior);

		$criteria->addJoin(ArSourceCsvFilePeer::AR_PHYSICAL_FORMAT_ID, ArPhysicalFormatPeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArSourceCsvFilePeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArSourceCsvFilePeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {
				$cls = ArSourceCsvFilePeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArSourceCsvFilePeer::addInstanceToPool($obj1, $key1);
			} // if obj1 already loaded

			// Add objects for joined ArCdrProvider rows

			$key2 = ArCdrProviderPeer::getPrimaryKeyHashFromRow($row, $startcol2);
			if ($key2 !== null) {
				$obj2 = ArCdrProviderPeer::getInstanceFromPool($key2);
				if (!$obj2) {

					$cls = ArCdrProviderPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol2);
					ArCdrProviderPeer::addInstanceToPool($obj2, $key2);
				} // if obj2 loaded

				// Add the $obj1 (ArSourceCsvFile) to the collection in $obj2 (ArCdrProvider)
				$obj2->addArSourceCsvFile($obj1);
			} // if joined row not null

			// Add objects for joined ArPhysicalFormat rows

			$key3 = ArPhysicalFormatPeer::getPrimaryKeyHashFromRow($row, $startcol3);
			if ($key3 !== null) {
				$obj3 = ArPhysicalFormatPeer::getInstanceFromPool($key3);
				if (!$obj3) {

					$cls = ArPhysicalFormatPeer::getOMClass(false);

					$obj3 = new $cls();
					$obj3->hydrate($row, $startcol3);
					ArPhysicalFormatPeer::addInstanceToPool($obj3, $key3);
				} // if obj3 loaded

				// Add the $obj1 (ArSourceCsvFile) to the collection in $obj3 (ArPhysicalFormat)
				$obj3->addArSourceCsvFile($obj1);
			} // if joined row not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Returns the number of rows matching criteria, joining the related ArCdrProvider table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinAllExceptArCdrProvider(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArSourceCsvFilePeer::TABLE_NAME);
		
		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArSourceCsvFilePeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY should not affect count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArSourceCsvFilePeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}
	
		$criteria->addJoin(ArSourceCsvFilePeer::AR_PHYSICAL_FORMAT_ID, ArPhysicalFormatPeer::ID, $join_behavior);

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
	 * Returns the number of rows matching criteria, joining the related ArPhysicalFormat table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinAllExceptArPhysicalFormat(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArSourceCsvFilePeer::TABLE_NAME);
		
		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArSourceCsvFilePeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY should not affect count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArSourceCsvFilePeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}
	
		$criteria->addJoin(ArSourceCsvFilePeer::AR_CDR_PROVIDER_ID, ArCdrProviderPeer::ID, $join_behavior);

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
	 * Selects a collection of ArSourceCsvFile objects pre-filled with all related objects except ArCdrProvider.
	 *
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArSourceCsvFile objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinAllExceptArCdrProvider(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		// $criteria->getDbName() will return the same object if not set to another value
		// so == check is okay and faster
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArSourceCsvFilePeer::addSelectColumns($criteria);
		$startcol2 = (ArSourceCsvFilePeer::NUM_COLUMNS - ArSourceCsvFilePeer::NUM_LAZY_LOAD_COLUMNS);

		ArPhysicalFormatPeer::addSelectColumns($criteria);
		$startcol3 = $startcol2 + (ArPhysicalFormatPeer::NUM_COLUMNS - ArPhysicalFormatPeer::NUM_LAZY_LOAD_COLUMNS);

		$criteria->addJoin(ArSourceCsvFilePeer::AR_PHYSICAL_FORMAT_ID, ArPhysicalFormatPeer::ID, $join_behavior);


		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArSourceCsvFilePeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArSourceCsvFilePeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {
				$cls = ArSourceCsvFilePeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArSourceCsvFilePeer::addInstanceToPool($obj1, $key1);
			} // if obj1 already loaded

				// Add objects for joined ArPhysicalFormat rows

				$key2 = ArPhysicalFormatPeer::getPrimaryKeyHashFromRow($row, $startcol2);
				if ($key2 !== null) {
					$obj2 = ArPhysicalFormatPeer::getInstanceFromPool($key2);
					if (!$obj2) {
	
						$cls = ArPhysicalFormatPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol2);
					ArPhysicalFormatPeer::addInstanceToPool($obj2, $key2);
				} // if $obj2 already loaded

				// Add the $obj1 (ArSourceCsvFile) to the collection in $obj2 (ArPhysicalFormat)
				$obj2->addArSourceCsvFile($obj1);

			} // if joined row is not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArSourceCsvFile objects pre-filled with all related objects except ArPhysicalFormat.
	 *
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArSourceCsvFile objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinAllExceptArPhysicalFormat(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		// $criteria->getDbName() will return the same object if not set to another value
		// so == check is okay and faster
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArSourceCsvFilePeer::addSelectColumns($criteria);
		$startcol2 = (ArSourceCsvFilePeer::NUM_COLUMNS - ArSourceCsvFilePeer::NUM_LAZY_LOAD_COLUMNS);

		ArCdrProviderPeer::addSelectColumns($criteria);
		$startcol3 = $startcol2 + (ArCdrProviderPeer::NUM_COLUMNS - ArCdrProviderPeer::NUM_LAZY_LOAD_COLUMNS);

		$criteria->addJoin(ArSourceCsvFilePeer::AR_CDR_PROVIDER_ID, ArCdrProviderPeer::ID, $join_behavior);


		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArSourceCsvFilePeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArSourceCsvFilePeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {
				$cls = ArSourceCsvFilePeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArSourceCsvFilePeer::addInstanceToPool($obj1, $key1);
			} // if obj1 already loaded

				// Add objects for joined ArCdrProvider rows

				$key2 = ArCdrProviderPeer::getPrimaryKeyHashFromRow($row, $startcol2);
				if ($key2 !== null) {
					$obj2 = ArCdrProviderPeer::getInstanceFromPool($key2);
					if (!$obj2) {
	
						$cls = ArCdrProviderPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol2);
					ArCdrProviderPeer::addInstanceToPool($obj2, $key2);
				} // if $obj2 already loaded

				// Add the $obj1 (ArSourceCsvFile) to the collection in $obj2 (ArCdrProvider)
				$obj2->addArSourceCsvFile($obj1);

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
	  $dbMap = Propel::getDatabaseMap(BaseArSourceCsvFilePeer::DATABASE_NAME);
	  if (!$dbMap->hasTable(BaseArSourceCsvFilePeer::TABLE_NAME))
	  {
	    $dbMap->addTableObject(new ArSourceCsvFileTableMap());
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
		return $withPrefix ? ArSourceCsvFilePeer::CLASS_DEFAULT : ArSourceCsvFilePeer::OM_CLASS;
	}

	/**
	 * Method perform an INSERT on the database, given a ArSourceCsvFile or Criteria object.
	 *
	 * @param      mixed $values Criteria or ArSourceCsvFile object containing data that is used to create the INSERT statement.
	 * @param      PropelPDO $con the PropelPDO connection to use
	 * @return     mixed The new primary key.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doInsert($values, PropelPDO $con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArSourceCsvFilePeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		if ($values instanceof Criteria) {
			$criteria = clone $values; // rename for clarity
		} else {
			$criteria = $values->buildCriteria(); // build Criteria from ArSourceCsvFile object
		}

		if ($criteria->containsKey(ArSourceCsvFilePeer::ID) && $criteria->keyContainsValue(ArSourceCsvFilePeer::ID) ) {
			throw new PropelException('Cannot insert a value for auto-increment primary key ('.ArSourceCsvFilePeer::ID.')');
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
	 * Method perform an UPDATE on the database, given a ArSourceCsvFile or Criteria object.
	 *
	 * @param      mixed $values Criteria or ArSourceCsvFile object containing data that is used to create the UPDATE statement.
	 * @param      PropelPDO $con The connection to use (specify PropelPDO connection object to exert more control over transactions).
	 * @return     int The number of affected rows (if supported by underlying database driver).
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doUpdate($values, PropelPDO $con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArSourceCsvFilePeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		$selectCriteria = new Criteria(self::DATABASE_NAME);

		if ($values instanceof Criteria) {
			$criteria = clone $values; // rename for clarity

			$comparison = $criteria->getComparison(ArSourceCsvFilePeer::ID);
			$selectCriteria->add(ArSourceCsvFilePeer::ID, $criteria->remove(ArSourceCsvFilePeer::ID), $comparison);

		} else { // $values is ArSourceCsvFile object
			$criteria = $values->buildCriteria(); // gets full criteria
			$selectCriteria = $values->buildPkeyCriteria(); // gets criteria w/ primary key(s)
		}

		// set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		return BasePeer::doUpdate($selectCriteria, $criteria, $con);
	}

	/**
	 * Method to DELETE all rows from the ar_source_csv_file table.
	 *
	 * @return     int The number of affected rows (if supported by underlying database driver).
	 */
	public static function doDeleteAll($con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArSourceCsvFilePeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}
		$affectedRows = 0; // initialize var to track total num of affected rows
		try {
			// use transaction because $criteria could contain info
			// for more than one table or we could emulating ON DELETE CASCADE, etc.
			$con->beginTransaction();
			$affectedRows += BasePeer::doDeleteAll(ArSourceCsvFilePeer::TABLE_NAME, $con);
			// Because this db requires some delete cascade/set null emulation, we have to
			// clear the cached instance *after* the emulation has happened (since
			// instances get re-added by the select statement contained therein).
			ArSourceCsvFilePeer::clearInstancePool();
			ArSourceCsvFilePeer::clearRelatedInstancePool();
			$con->commit();
			return $affectedRows;
		} catch (PropelException $e) {
			$con->rollBack();
			throw $e;
		}
	}

	/**
	 * Method perform a DELETE on the database, given a ArSourceCsvFile or Criteria object OR a primary key value.
	 *
	 * @param      mixed $values Criteria or ArSourceCsvFile object or primary key or array of primary keys
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
			$con = Propel::getConnection(ArSourceCsvFilePeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		if ($values instanceof Criteria) {
			// invalidate the cache for all objects of this type, since we have no
			// way of knowing (without running a query) what objects should be invalidated
			// from the cache based on this Criteria.
			ArSourceCsvFilePeer::clearInstancePool();
			// rename for clarity
			$criteria = clone $values;
		} elseif ($values instanceof ArSourceCsvFile) { // it's a model object
			// invalidate the cache for this single object
			ArSourceCsvFilePeer::removeInstanceFromPool($values);
			// create criteria based on pk values
			$criteria = $values->buildPkeyCriteria();
		} else { // it's a primary key, or an array of pks
			$criteria = new Criteria(self::DATABASE_NAME);
			$criteria->add(ArSourceCsvFilePeer::ID, (array) $values, Criteria::IN);
			// invalidate the cache for this object(s)
			foreach ((array) $values as $singleval) {
				ArSourceCsvFilePeer::removeInstanceFromPool($singleval);
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
			ArSourceCsvFilePeer::clearRelatedInstancePool();
			$con->commit();
			return $affectedRows;
		} catch (PropelException $e) {
			$con->rollBack();
			throw $e;
		}
	}

	/**
	 * Validates all modified columns of given ArSourceCsvFile object.
	 * If parameter $columns is either a single column name or an array of column names
	 * than only those columns are validated.
	 *
	 * NOTICE: This does not apply to primary or foreign keys for now.
	 *
	 * @param      ArSourceCsvFile $obj The object to validate.
	 * @param      mixed $cols Column name or array of column names.
	 *
	 * @return     mixed TRUE if all columns are valid or the error message of the first invalid column.
	 */
	public static function doValidate(ArSourceCsvFile $obj, $cols = null)
	{
		$columns = array();

		if ($cols) {
			$dbMap = Propel::getDatabaseMap(ArSourceCsvFilePeer::DATABASE_NAME);
			$tableMap = $dbMap->getTable(ArSourceCsvFilePeer::TABLE_NAME);

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

		return BasePeer::doValidate(ArSourceCsvFilePeer::DATABASE_NAME, ArSourceCsvFilePeer::TABLE_NAME, $columns);
	}

	/**
	 * Retrieve a single object by pkey.
	 *
	 * @param      int $pk the primary key.
	 * @param      PropelPDO $con the connection to use
	 * @return     ArSourceCsvFile
	 */
	public static function retrieveByPK($pk, PropelPDO $con = null)
	{

		if (null !== ($obj = ArSourceCsvFilePeer::getInstanceFromPool((string) $pk))) {
			return $obj;
		}

		if ($con === null) {
			$con = Propel::getConnection(ArSourceCsvFilePeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria = new Criteria(ArSourceCsvFilePeer::DATABASE_NAME);
		$criteria->add(ArSourceCsvFilePeer::ID, $pk);

		$v = ArSourceCsvFilePeer::doSelect($criteria, $con);

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
			$con = Propel::getConnection(ArSourceCsvFilePeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$objs = null;
		if (empty($pks)) {
			$objs = array();
		} else {
			$criteria = new Criteria(ArSourceCsvFilePeer::DATABASE_NAME);
			$criteria->add(ArSourceCsvFilePeer::ID, $pks, Criteria::IN);
			$objs = ArSourceCsvFilePeer::doSelect($criteria, $con);
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
	  return array(array('name'));
	}

} // BaseArSourceCsvFilePeer

// This is the static code needed to register the TableMap for this table with the main Propel class.
//
BaseArSourceCsvFilePeer::buildTableMap();

