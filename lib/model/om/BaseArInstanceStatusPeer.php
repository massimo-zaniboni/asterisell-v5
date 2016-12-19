<?php

/**
 * Base static class for performing query and update operations on the 'ar_instance_status' table.
 *
 * 
 *
 * @package    lib.model.om
 */
abstract class BaseArInstanceStatusPeer {

	/** the default database name for this class */
	const DATABASE_NAME = 'propel';

	/** the table name for this class */
	const TABLE_NAME = 'ar_instance_status';

	/** the related Propel class for this table */
	const OM_CLASS = 'ArInstanceStatus';

	/** A class that can be returned by this peer. */
	const CLASS_DEFAULT = 'lib.model.ArInstanceStatus';

	/** the related TableMap class for this table */
	const TM_CLASS = 'ArInstanceStatusTableMap';
	
	/** The total number of columns. */
	const NUM_COLUMNS = 24;

	/** The number of lazy-loaded columns. */
	const NUM_LAZY_LOAD_COLUMNS = 0;

	/** the column name for the ID field */
	const ID = 'ar_instance_status.ID';

	/** the column name for the INTERNAL_NAME field */
	const INTERNAL_NAME = 'ar_instance_status.INTERNAL_NAME';

	/** the column name for the INSTANCE_CODE field */
	const INSTANCE_CODE = 'ar_instance_status.INSTANCE_CODE';

	/** the column name for the AR_ORGANIZATION_UNIT_ID field */
	const AR_ORGANIZATION_UNIT_ID = 'ar_instance_status.AR_ORGANIZATION_UNIT_ID';

	/** the column name for the APPLICATION_VERSION field */
	const APPLICATION_VERSION = 'ar_instance_status.APPLICATION_VERSION';

	/** the column name for the NR_OF_CRITICAL_ERRORS field */
	const NR_OF_CRITICAL_ERRORS = 'ar_instance_status.NR_OF_CRITICAL_ERRORS';

	/** the column name for the NR_OF_IMPORTANT_ERRORS field */
	const NR_OF_IMPORTANT_ERRORS = 'ar_instance_status.NR_OF_IMPORTANT_ERRORS';

	/** the column name for the NR_OF_WARNING_ERRORS field */
	const NR_OF_WARNING_ERRORS = 'ar_instance_status.NR_OF_WARNING_ERRORS';

	/** the column name for the NR_OF_EXTENSIONS field */
	const NR_OF_EXTENSIONS = 'ar_instance_status.NR_OF_EXTENSIONS';

	/** the column name for the NR_OF_UNSPECIFIED_EXTENSIONS field */
	const NR_OF_UNSPECIFIED_EXTENSIONS = 'ar_instance_status.NR_OF_UNSPECIFIED_EXTENSIONS';

	/** the column name for the PROPERTY_NR_ERRORS_OUTGOING_PREVIOUS_MONTH field */
	const PROPERTY_NR_ERRORS_OUTGOING_PREVIOUS_MONTH = 'ar_instance_status.PROPERTY_NR_ERRORS_OUTGOING_PREVIOUS_MONTH';

	/** the column name for the PROPERTY_NR_ERRORS_INCOMING_PREVIOUS_MONTH field */
	const PROPERTY_NR_ERRORS_INCOMING_PREVIOUS_MONTH = 'ar_instance_status.PROPERTY_NR_ERRORS_INCOMING_PREVIOUS_MONTH';

	/** the column name for the PROPERTY_NR_ERRORS_INTERNAL_PREVIOUS_MONTH field */
	const PROPERTY_NR_ERRORS_INTERNAL_PREVIOUS_MONTH = 'ar_instance_status.PROPERTY_NR_ERRORS_INTERNAL_PREVIOUS_MONTH';

	/** the column name for the PROPERTY_NR_ERRORS_OUTGOING_LAST_30_DAYS field */
	const PROPERTY_NR_ERRORS_OUTGOING_LAST_30_DAYS = 'ar_instance_status.PROPERTY_NR_ERRORS_OUTGOING_LAST_30_DAYS';

	/** the column name for the PROPERTY_NR_ERRORS_INCOMING_LAST_30_DAYS field */
	const PROPERTY_NR_ERRORS_INCOMING_LAST_30_DAYS = 'ar_instance_status.PROPERTY_NR_ERRORS_INCOMING_LAST_30_DAYS';

	/** the column name for the PROPERTY_NR_ERRORS_INTERNAL_LAST_30_DAYS field */
	const PROPERTY_NR_ERRORS_INTERNAL_LAST_30_DAYS = 'ar_instance_status.PROPERTY_NR_ERRORS_INTERNAL_LAST_30_DAYS';

	/** the column name for the PROPERTY_NR_OUTGOING_PREVIOUS_MONTH field */
	const PROPERTY_NR_OUTGOING_PREVIOUS_MONTH = 'ar_instance_status.PROPERTY_NR_OUTGOING_PREVIOUS_MONTH';

	/** the column name for the PROPERTY_NR_INCOMING_PREVIOUS_MONTH field */
	const PROPERTY_NR_INCOMING_PREVIOUS_MONTH = 'ar_instance_status.PROPERTY_NR_INCOMING_PREVIOUS_MONTH';

	/** the column name for the PROPERTY_NR_INTERNAL_PREVIOUS_MONTH field */
	const PROPERTY_NR_INTERNAL_PREVIOUS_MONTH = 'ar_instance_status.PROPERTY_NR_INTERNAL_PREVIOUS_MONTH';

	/** the column name for the PROPERTY_NR_OUTGOING_LAST_30_DAYS field */
	const PROPERTY_NR_OUTGOING_LAST_30_DAYS = 'ar_instance_status.PROPERTY_NR_OUTGOING_LAST_30_DAYS';

	/** the column name for the PROPERTY_NR_INCOMING_LAST_30_DAYS field */
	const PROPERTY_NR_INCOMING_LAST_30_DAYS = 'ar_instance_status.PROPERTY_NR_INCOMING_LAST_30_DAYS';

	/** the column name for the PROPERTY_NR_INTERNAL_LAST_30_DAYS field */
	const PROPERTY_NR_INTERNAL_LAST_30_DAYS = 'ar_instance_status.PROPERTY_NR_INTERNAL_LAST_30_DAYS';

	/** the column name for the LAST_PROCESSED_CDR_TIMESTAMP field */
	const LAST_PROCESSED_CDR_TIMESTAMP = 'ar_instance_status.LAST_PROCESSED_CDR_TIMESTAMP';

	/** the column name for the INFO_TIMESTAMP field */
	const INFO_TIMESTAMP = 'ar_instance_status.INFO_TIMESTAMP';

	/**
	 * An identiy map to hold any loaded instances of ArInstanceStatus objects.
	 * This must be public so that other peer classes can access this when hydrating from JOIN
	 * queries.
	 * @var        array ArInstanceStatus[]
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
		BasePeer::TYPE_PHPNAME => array ('Id', 'InternalName', 'InstanceCode', 'ArOrganizationUnitId', 'ApplicationVersion', 'NrOfCriticalErrors', 'NrOfImportantErrors', 'NrOfWarningErrors', 'NrOfExtensions', 'NrOfUnspecifiedExtensions', 'PropertyNrErrorsOutgoingPreviousMonth', 'PropertyNrErrorsIncomingPreviousMonth', 'PropertyNrErrorsInternalPreviousMonth', 'PropertyNrErrorsOutgoingLast30Days', 'PropertyNrErrorsIncomingLast30Days', 'PropertyNrErrorsInternalLast30Days', 'PropertyNrOutgoingPreviousMonth', 'PropertyNrIncomingPreviousMonth', 'PropertyNrInternalPreviousMonth', 'PropertyNrOutgoingLast30Days', 'PropertyNrIncomingLast30Days', 'PropertyNrInternalLast30Days', 'LastProcessedCdrTimestamp', 'InfoTimestamp', ),
		BasePeer::TYPE_STUDLYPHPNAME => array ('id', 'internalName', 'instanceCode', 'arOrganizationUnitId', 'applicationVersion', 'nrOfCriticalErrors', 'nrOfImportantErrors', 'nrOfWarningErrors', 'nrOfExtensions', 'nrOfUnspecifiedExtensions', 'propertyNrErrorsOutgoingPreviousMonth', 'propertyNrErrorsIncomingPreviousMonth', 'propertyNrErrorsInternalPreviousMonth', 'propertyNrErrorsOutgoingLast30Days', 'propertyNrErrorsIncomingLast30Days', 'propertyNrErrorsInternalLast30Days', 'propertyNrOutgoingPreviousMonth', 'propertyNrIncomingPreviousMonth', 'propertyNrInternalPreviousMonth', 'propertyNrOutgoingLast30Days', 'propertyNrIncomingLast30Days', 'propertyNrInternalLast30Days', 'lastProcessedCdrTimestamp', 'infoTimestamp', ),
		BasePeer::TYPE_COLNAME => array (self::ID, self::INTERNAL_NAME, self::INSTANCE_CODE, self::AR_ORGANIZATION_UNIT_ID, self::APPLICATION_VERSION, self::NR_OF_CRITICAL_ERRORS, self::NR_OF_IMPORTANT_ERRORS, self::NR_OF_WARNING_ERRORS, self::NR_OF_EXTENSIONS, self::NR_OF_UNSPECIFIED_EXTENSIONS, self::PROPERTY_NR_ERRORS_OUTGOING_PREVIOUS_MONTH, self::PROPERTY_NR_ERRORS_INCOMING_PREVIOUS_MONTH, self::PROPERTY_NR_ERRORS_INTERNAL_PREVIOUS_MONTH, self::PROPERTY_NR_ERRORS_OUTGOING_LAST_30_DAYS, self::PROPERTY_NR_ERRORS_INCOMING_LAST_30_DAYS, self::PROPERTY_NR_ERRORS_INTERNAL_LAST_30_DAYS, self::PROPERTY_NR_OUTGOING_PREVIOUS_MONTH, self::PROPERTY_NR_INCOMING_PREVIOUS_MONTH, self::PROPERTY_NR_INTERNAL_PREVIOUS_MONTH, self::PROPERTY_NR_OUTGOING_LAST_30_DAYS, self::PROPERTY_NR_INCOMING_LAST_30_DAYS, self::PROPERTY_NR_INTERNAL_LAST_30_DAYS, self::LAST_PROCESSED_CDR_TIMESTAMP, self::INFO_TIMESTAMP, ),
		BasePeer::TYPE_FIELDNAME => array ('id', 'internal_name', 'instance_code', 'ar_organization_unit_id', 'application_version', 'nr_of_critical_errors', 'nr_of_important_errors', 'nr_of_warning_errors', 'nr_of_extensions', 'nr_of_unspecified_extensions', 'property_nr_errors_outgoing_previous_month', 'property_nr_errors_incoming_previous_month', 'property_nr_errors_internal_previous_month', 'property_nr_errors_outgoing_last_30_days', 'property_nr_errors_incoming_last_30_days', 'property_nr_errors_internal_last_30_days', 'property_nr_outgoing_previous_month', 'property_nr_incoming_previous_month', 'property_nr_internal_previous_month', 'property_nr_outgoing_last_30_days', 'property_nr_incoming_last_30_days', 'property_nr_internal_last_30_days', 'last_processed_cdr_timestamp', 'info_timestamp', ),
		BasePeer::TYPE_NUM => array (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, )
	);

	/**
	 * holds an array of keys for quick access to the fieldnames array
	 *
	 * first dimension keys are the type constants
	 * e.g. self::$fieldNames[BasePeer::TYPE_PHPNAME]['Id'] = 0
	 */
	private static $fieldKeys = array (
		BasePeer::TYPE_PHPNAME => array ('Id' => 0, 'InternalName' => 1, 'InstanceCode' => 2, 'ArOrganizationUnitId' => 3, 'ApplicationVersion' => 4, 'NrOfCriticalErrors' => 5, 'NrOfImportantErrors' => 6, 'NrOfWarningErrors' => 7, 'NrOfExtensions' => 8, 'NrOfUnspecifiedExtensions' => 9, 'PropertyNrErrorsOutgoingPreviousMonth' => 10, 'PropertyNrErrorsIncomingPreviousMonth' => 11, 'PropertyNrErrorsInternalPreviousMonth' => 12, 'PropertyNrErrorsOutgoingLast30Days' => 13, 'PropertyNrErrorsIncomingLast30Days' => 14, 'PropertyNrErrorsInternalLast30Days' => 15, 'PropertyNrOutgoingPreviousMonth' => 16, 'PropertyNrIncomingPreviousMonth' => 17, 'PropertyNrInternalPreviousMonth' => 18, 'PropertyNrOutgoingLast30Days' => 19, 'PropertyNrIncomingLast30Days' => 20, 'PropertyNrInternalLast30Days' => 21, 'LastProcessedCdrTimestamp' => 22, 'InfoTimestamp' => 23, ),
		BasePeer::TYPE_STUDLYPHPNAME => array ('id' => 0, 'internalName' => 1, 'instanceCode' => 2, 'arOrganizationUnitId' => 3, 'applicationVersion' => 4, 'nrOfCriticalErrors' => 5, 'nrOfImportantErrors' => 6, 'nrOfWarningErrors' => 7, 'nrOfExtensions' => 8, 'nrOfUnspecifiedExtensions' => 9, 'propertyNrErrorsOutgoingPreviousMonth' => 10, 'propertyNrErrorsIncomingPreviousMonth' => 11, 'propertyNrErrorsInternalPreviousMonth' => 12, 'propertyNrErrorsOutgoingLast30Days' => 13, 'propertyNrErrorsIncomingLast30Days' => 14, 'propertyNrErrorsInternalLast30Days' => 15, 'propertyNrOutgoingPreviousMonth' => 16, 'propertyNrIncomingPreviousMonth' => 17, 'propertyNrInternalPreviousMonth' => 18, 'propertyNrOutgoingLast30Days' => 19, 'propertyNrIncomingLast30Days' => 20, 'propertyNrInternalLast30Days' => 21, 'lastProcessedCdrTimestamp' => 22, 'infoTimestamp' => 23, ),
		BasePeer::TYPE_COLNAME => array (self::ID => 0, self::INTERNAL_NAME => 1, self::INSTANCE_CODE => 2, self::AR_ORGANIZATION_UNIT_ID => 3, self::APPLICATION_VERSION => 4, self::NR_OF_CRITICAL_ERRORS => 5, self::NR_OF_IMPORTANT_ERRORS => 6, self::NR_OF_WARNING_ERRORS => 7, self::NR_OF_EXTENSIONS => 8, self::NR_OF_UNSPECIFIED_EXTENSIONS => 9, self::PROPERTY_NR_ERRORS_OUTGOING_PREVIOUS_MONTH => 10, self::PROPERTY_NR_ERRORS_INCOMING_PREVIOUS_MONTH => 11, self::PROPERTY_NR_ERRORS_INTERNAL_PREVIOUS_MONTH => 12, self::PROPERTY_NR_ERRORS_OUTGOING_LAST_30_DAYS => 13, self::PROPERTY_NR_ERRORS_INCOMING_LAST_30_DAYS => 14, self::PROPERTY_NR_ERRORS_INTERNAL_LAST_30_DAYS => 15, self::PROPERTY_NR_OUTGOING_PREVIOUS_MONTH => 16, self::PROPERTY_NR_INCOMING_PREVIOUS_MONTH => 17, self::PROPERTY_NR_INTERNAL_PREVIOUS_MONTH => 18, self::PROPERTY_NR_OUTGOING_LAST_30_DAYS => 19, self::PROPERTY_NR_INCOMING_LAST_30_DAYS => 20, self::PROPERTY_NR_INTERNAL_LAST_30_DAYS => 21, self::LAST_PROCESSED_CDR_TIMESTAMP => 22, self::INFO_TIMESTAMP => 23, ),
		BasePeer::TYPE_FIELDNAME => array ('id' => 0, 'internal_name' => 1, 'instance_code' => 2, 'ar_organization_unit_id' => 3, 'application_version' => 4, 'nr_of_critical_errors' => 5, 'nr_of_important_errors' => 6, 'nr_of_warning_errors' => 7, 'nr_of_extensions' => 8, 'nr_of_unspecified_extensions' => 9, 'property_nr_errors_outgoing_previous_month' => 10, 'property_nr_errors_incoming_previous_month' => 11, 'property_nr_errors_internal_previous_month' => 12, 'property_nr_errors_outgoing_last_30_days' => 13, 'property_nr_errors_incoming_last_30_days' => 14, 'property_nr_errors_internal_last_30_days' => 15, 'property_nr_outgoing_previous_month' => 16, 'property_nr_incoming_previous_month' => 17, 'property_nr_internal_previous_month' => 18, 'property_nr_outgoing_last_30_days' => 19, 'property_nr_incoming_last_30_days' => 20, 'property_nr_internal_last_30_days' => 21, 'last_processed_cdr_timestamp' => 22, 'info_timestamp' => 23, ),
		BasePeer::TYPE_NUM => array (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, )
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
	 * @param      string $column The column name for current table. (i.e. ArInstanceStatusPeer::COLUMN_NAME).
	 * @return     string
	 */
	public static function alias($alias, $column)
	{
		return str_replace(ArInstanceStatusPeer::TABLE_NAME.'.', $alias.'.', $column);
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
		$criteria->addSelectColumn(ArInstanceStatusPeer::ID);
		$criteria->addSelectColumn(ArInstanceStatusPeer::INTERNAL_NAME);
		$criteria->addSelectColumn(ArInstanceStatusPeer::INSTANCE_CODE);
		$criteria->addSelectColumn(ArInstanceStatusPeer::AR_ORGANIZATION_UNIT_ID);
		$criteria->addSelectColumn(ArInstanceStatusPeer::APPLICATION_VERSION);
		$criteria->addSelectColumn(ArInstanceStatusPeer::NR_OF_CRITICAL_ERRORS);
		$criteria->addSelectColumn(ArInstanceStatusPeer::NR_OF_IMPORTANT_ERRORS);
		$criteria->addSelectColumn(ArInstanceStatusPeer::NR_OF_WARNING_ERRORS);
		$criteria->addSelectColumn(ArInstanceStatusPeer::NR_OF_EXTENSIONS);
		$criteria->addSelectColumn(ArInstanceStatusPeer::NR_OF_UNSPECIFIED_EXTENSIONS);
		$criteria->addSelectColumn(ArInstanceStatusPeer::PROPERTY_NR_ERRORS_OUTGOING_PREVIOUS_MONTH);
		$criteria->addSelectColumn(ArInstanceStatusPeer::PROPERTY_NR_ERRORS_INCOMING_PREVIOUS_MONTH);
		$criteria->addSelectColumn(ArInstanceStatusPeer::PROPERTY_NR_ERRORS_INTERNAL_PREVIOUS_MONTH);
		$criteria->addSelectColumn(ArInstanceStatusPeer::PROPERTY_NR_ERRORS_OUTGOING_LAST_30_DAYS);
		$criteria->addSelectColumn(ArInstanceStatusPeer::PROPERTY_NR_ERRORS_INCOMING_LAST_30_DAYS);
		$criteria->addSelectColumn(ArInstanceStatusPeer::PROPERTY_NR_ERRORS_INTERNAL_LAST_30_DAYS);
		$criteria->addSelectColumn(ArInstanceStatusPeer::PROPERTY_NR_OUTGOING_PREVIOUS_MONTH);
		$criteria->addSelectColumn(ArInstanceStatusPeer::PROPERTY_NR_INCOMING_PREVIOUS_MONTH);
		$criteria->addSelectColumn(ArInstanceStatusPeer::PROPERTY_NR_INTERNAL_PREVIOUS_MONTH);
		$criteria->addSelectColumn(ArInstanceStatusPeer::PROPERTY_NR_OUTGOING_LAST_30_DAYS);
		$criteria->addSelectColumn(ArInstanceStatusPeer::PROPERTY_NR_INCOMING_LAST_30_DAYS);
		$criteria->addSelectColumn(ArInstanceStatusPeer::PROPERTY_NR_INTERNAL_LAST_30_DAYS);
		$criteria->addSelectColumn(ArInstanceStatusPeer::LAST_PROCESSED_CDR_TIMESTAMP);
		$criteria->addSelectColumn(ArInstanceStatusPeer::INFO_TIMESTAMP);
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
		$criteria->setPrimaryTableName(ArInstanceStatusPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArInstanceStatusPeer::addSelectColumns($criteria);
		}

		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		$criteria->setDbName(self::DATABASE_NAME); // Set the correct dbName

		if ($con === null) {
			$con = Propel::getConnection(ArInstanceStatusPeer::DATABASE_NAME, Propel::CONNECTION_READ);
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
	 * @return     ArInstanceStatus
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectOne(Criteria $criteria, PropelPDO $con = null)
	{
		$critcopy = clone $criteria;
		$critcopy->setLimit(1);
		$objects = ArInstanceStatusPeer::doSelect($critcopy, $con);
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
		return ArInstanceStatusPeer::populateObjects(ArInstanceStatusPeer::doSelectStmt($criteria, $con));
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
			$con = Propel::getConnection(ArInstanceStatusPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		if (!$criteria->hasSelectClause()) {
			$criteria = clone $criteria;
			ArInstanceStatusPeer::addSelectColumns($criteria);
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
	 * @param      ArInstanceStatus $value A ArInstanceStatus object.
	 * @param      string $key (optional) key to use for instance map (for performance boost if key was already calculated externally).
	 */
	public static function addInstanceToPool(ArInstanceStatus $obj, $key = null)
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
	 * @param      mixed $value A ArInstanceStatus object or a primary key value.
	 */
	public static function removeInstanceFromPool($value)
	{
		if (Propel::isInstancePoolingEnabled() && $value !== null) {
			if (is_object($value) && $value instanceof ArInstanceStatus) {
				$key = (string) $value->getId();
			} elseif (is_scalar($value)) {
				// assume we've been passed a primary key
				$key = (string) $value;
			} else {
				$e = new PropelException("Invalid value passed to removeInstanceFromPool().  Expected primary key or ArInstanceStatus object; got " . (is_object($value) ? get_class($value) . ' object.' : var_export($value,true)));
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
	 * @return     ArInstanceStatus Found object or NULL if 1) no instance exists for specified key or 2) instance pooling has been disabled.
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
	 * Method to invalidate the instance pool of all tables related to ar_instance_status
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
		$cls = ArInstanceStatusPeer::getOMClass(false);
		// populate the object(s)
		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key = ArInstanceStatusPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj = ArInstanceStatusPeer::getInstanceFromPool($key))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj->hydrate($row, 0, true); // rehydrate
				$results[] = $obj;
			} else {
				$obj = new $cls();
				$obj->hydrate($row);
				$results[] = $obj;
				ArInstanceStatusPeer::addInstanceToPool($obj, $key);
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
		$criteria->setPrimaryTableName(ArInstanceStatusPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArInstanceStatusPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArInstanceStatusPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArInstanceStatusPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

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
	 * Selects a collection of ArInstanceStatus objects pre-filled with their ArOrganizationUnit objects.
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArInstanceStatus objects.
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

		ArInstanceStatusPeer::addSelectColumns($criteria);
		$startcol = (ArInstanceStatusPeer::NUM_COLUMNS - ArInstanceStatusPeer::NUM_LAZY_LOAD_COLUMNS);
		ArOrganizationUnitPeer::addSelectColumns($criteria);

		$criteria->addJoin(ArInstanceStatusPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArInstanceStatusPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArInstanceStatusPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {

				$cls = ArInstanceStatusPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArInstanceStatusPeer::addInstanceToPool($obj1, $key1);
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
				
				// Add the $obj1 (ArInstanceStatus) to $obj2 (ArOrganizationUnit)
				$obj2->addArInstanceStatus($obj1);

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
		$criteria->setPrimaryTableName(ArInstanceStatusPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArInstanceStatusPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArInstanceStatusPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArInstanceStatusPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

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
	 * Selects a collection of ArInstanceStatus objects pre-filled with all related objects.
	 *
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArInstanceStatus objects.
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

		ArInstanceStatusPeer::addSelectColumns($criteria);
		$startcol2 = (ArInstanceStatusPeer::NUM_COLUMNS - ArInstanceStatusPeer::NUM_LAZY_LOAD_COLUMNS);

		ArOrganizationUnitPeer::addSelectColumns($criteria);
		$startcol3 = $startcol2 + (ArOrganizationUnitPeer::NUM_COLUMNS - ArOrganizationUnitPeer::NUM_LAZY_LOAD_COLUMNS);

		$criteria->addJoin(ArInstanceStatusPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArInstanceStatusPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArInstanceStatusPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {
				$cls = ArInstanceStatusPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArInstanceStatusPeer::addInstanceToPool($obj1, $key1);
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

				// Add the $obj1 (ArInstanceStatus) to the collection in $obj2 (ArOrganizationUnit)
				$obj2->addArInstanceStatus($obj1);
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
	  $dbMap = Propel::getDatabaseMap(BaseArInstanceStatusPeer::DATABASE_NAME);
	  if (!$dbMap->hasTable(BaseArInstanceStatusPeer::TABLE_NAME))
	  {
	    $dbMap->addTableObject(new ArInstanceStatusTableMap());
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
		return $withPrefix ? ArInstanceStatusPeer::CLASS_DEFAULT : ArInstanceStatusPeer::OM_CLASS;
	}

	/**
	 * Method perform an INSERT on the database, given a ArInstanceStatus or Criteria object.
	 *
	 * @param      mixed $values Criteria or ArInstanceStatus object containing data that is used to create the INSERT statement.
	 * @param      PropelPDO $con the PropelPDO connection to use
	 * @return     mixed The new primary key.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doInsert($values, PropelPDO $con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArInstanceStatusPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		if ($values instanceof Criteria) {
			$criteria = clone $values; // rename for clarity
		} else {
			$criteria = $values->buildCriteria(); // build Criteria from ArInstanceStatus object
		}

		if ($criteria->containsKey(ArInstanceStatusPeer::ID) && $criteria->keyContainsValue(ArInstanceStatusPeer::ID) ) {
			throw new PropelException('Cannot insert a value for auto-increment primary key ('.ArInstanceStatusPeer::ID.')');
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
	 * Method perform an UPDATE on the database, given a ArInstanceStatus or Criteria object.
	 *
	 * @param      mixed $values Criteria or ArInstanceStatus object containing data that is used to create the UPDATE statement.
	 * @param      PropelPDO $con The connection to use (specify PropelPDO connection object to exert more control over transactions).
	 * @return     int The number of affected rows (if supported by underlying database driver).
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doUpdate($values, PropelPDO $con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArInstanceStatusPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		$selectCriteria = new Criteria(self::DATABASE_NAME);

		if ($values instanceof Criteria) {
			$criteria = clone $values; // rename for clarity

			$comparison = $criteria->getComparison(ArInstanceStatusPeer::ID);
			$selectCriteria->add(ArInstanceStatusPeer::ID, $criteria->remove(ArInstanceStatusPeer::ID), $comparison);

		} else { // $values is ArInstanceStatus object
			$criteria = $values->buildCriteria(); // gets full criteria
			$selectCriteria = $values->buildPkeyCriteria(); // gets criteria w/ primary key(s)
		}

		// set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		return BasePeer::doUpdate($selectCriteria, $criteria, $con);
	}

	/**
	 * Method to DELETE all rows from the ar_instance_status table.
	 *
	 * @return     int The number of affected rows (if supported by underlying database driver).
	 */
	public static function doDeleteAll($con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArInstanceStatusPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}
		$affectedRows = 0; // initialize var to track total num of affected rows
		try {
			// use transaction because $criteria could contain info
			// for more than one table or we could emulating ON DELETE CASCADE, etc.
			$con->beginTransaction();
			$affectedRows += BasePeer::doDeleteAll(ArInstanceStatusPeer::TABLE_NAME, $con);
			// Because this db requires some delete cascade/set null emulation, we have to
			// clear the cached instance *after* the emulation has happened (since
			// instances get re-added by the select statement contained therein).
			ArInstanceStatusPeer::clearInstancePool();
			ArInstanceStatusPeer::clearRelatedInstancePool();
			$con->commit();
			return $affectedRows;
		} catch (PropelException $e) {
			$con->rollBack();
			throw $e;
		}
	}

	/**
	 * Method perform a DELETE on the database, given a ArInstanceStatus or Criteria object OR a primary key value.
	 *
	 * @param      mixed $values Criteria or ArInstanceStatus object or primary key or array of primary keys
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
			$con = Propel::getConnection(ArInstanceStatusPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		if ($values instanceof Criteria) {
			// invalidate the cache for all objects of this type, since we have no
			// way of knowing (without running a query) what objects should be invalidated
			// from the cache based on this Criteria.
			ArInstanceStatusPeer::clearInstancePool();
			// rename for clarity
			$criteria = clone $values;
		} elseif ($values instanceof ArInstanceStatus) { // it's a model object
			// invalidate the cache for this single object
			ArInstanceStatusPeer::removeInstanceFromPool($values);
			// create criteria based on pk values
			$criteria = $values->buildPkeyCriteria();
		} else { // it's a primary key, or an array of pks
			$criteria = new Criteria(self::DATABASE_NAME);
			$criteria->add(ArInstanceStatusPeer::ID, (array) $values, Criteria::IN);
			// invalidate the cache for this object(s)
			foreach ((array) $values as $singleval) {
				ArInstanceStatusPeer::removeInstanceFromPool($singleval);
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
			ArInstanceStatusPeer::clearRelatedInstancePool();
			$con->commit();
			return $affectedRows;
		} catch (PropelException $e) {
			$con->rollBack();
			throw $e;
		}
	}

	/**
	 * Validates all modified columns of given ArInstanceStatus object.
	 * If parameter $columns is either a single column name or an array of column names
	 * than only those columns are validated.
	 *
	 * NOTICE: This does not apply to primary or foreign keys for now.
	 *
	 * @param      ArInstanceStatus $obj The object to validate.
	 * @param      mixed $cols Column name or array of column names.
	 *
	 * @return     mixed TRUE if all columns are valid or the error message of the first invalid column.
	 */
	public static function doValidate(ArInstanceStatus $obj, $cols = null)
	{
		$columns = array();

		if ($cols) {
			$dbMap = Propel::getDatabaseMap(ArInstanceStatusPeer::DATABASE_NAME);
			$tableMap = $dbMap->getTable(ArInstanceStatusPeer::TABLE_NAME);

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

		return BasePeer::doValidate(ArInstanceStatusPeer::DATABASE_NAME, ArInstanceStatusPeer::TABLE_NAME, $columns);
	}

	/**
	 * Retrieve a single object by pkey.
	 *
	 * @param      int $pk the primary key.
	 * @param      PropelPDO $con the connection to use
	 * @return     ArInstanceStatus
	 */
	public static function retrieveByPK($pk, PropelPDO $con = null)
	{

		if (null !== ($obj = ArInstanceStatusPeer::getInstanceFromPool((string) $pk))) {
			return $obj;
		}

		if ($con === null) {
			$con = Propel::getConnection(ArInstanceStatusPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria = new Criteria(ArInstanceStatusPeer::DATABASE_NAME);
		$criteria->add(ArInstanceStatusPeer::ID, $pk);

		$v = ArInstanceStatusPeer::doSelect($criteria, $con);

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
			$con = Propel::getConnection(ArInstanceStatusPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$objs = null;
		if (empty($pks)) {
			$objs = array();
		} else {
			$criteria = new Criteria(ArInstanceStatusPeer::DATABASE_NAME);
			$criteria->add(ArInstanceStatusPeer::ID, $pks, Criteria::IN);
			$objs = ArInstanceStatusPeer::doSelect($criteria, $con);
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

} // BaseArInstanceStatusPeer

// This is the static code needed to register the TableMap for this table with the main Propel class.
//
BaseArInstanceStatusPeer::buildTableMap();

