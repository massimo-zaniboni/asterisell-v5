<?php

/**
 * Base static class for performing query and update operations on the 'ar_party' table.
 *
 * 
 *
 * @package    lib.model.om
 */
abstract class BaseArPartyPeer {

	/** the default database name for this class */
	const DATABASE_NAME = 'propel';

	/** the table name for this class */
	const TABLE_NAME = 'ar_party';

	/** the related Propel class for this table */
	const OM_CLASS = 'ArParty';

	/** A class that can be returned by this peer. */
	const CLASS_DEFAULT = 'lib.model.ArParty';

	/** the related TableMap class for this table */
	const TM_CLASS = 'ArPartyTableMap';
	
	/** The total number of columns. */
	const NUM_COLUMNS = 25;

	/** The number of lazy-loaded columns. */
	const NUM_LAZY_LOAD_COLUMNS = 0;

	/** the column name for the ID field */
	const ID = 'ar_party.ID';

	/** the column name for the NAME field */
	const NAME = 'ar_party.NAME';

	/** the column name for the COMPACT_NAME field */
	const COMPACT_NAME = 'ar_party.COMPACT_NAME';

	/** the column name for the EXTERNAL_CRM_CODE field */
	const EXTERNAL_CRM_CODE = 'ar_party.EXTERNAL_CRM_CODE';

	/** the column name for the VAT field */
	const VAT = 'ar_party.VAT';

	/** the column name for the IS_BILLABLE field */
	const IS_BILLABLE = 'ar_party.IS_BILLABLE';

	/** the column name for the LEGAL_ADDRESS field */
	const LEGAL_ADDRESS = 'ar_party.LEGAL_ADDRESS';

	/** the column name for the LEGAL_CITY field */
	const LEGAL_CITY = 'ar_party.LEGAL_CITY';

	/** the column name for the LEGAL_ZIPCODE field */
	const LEGAL_ZIPCODE = 'ar_party.LEGAL_ZIPCODE';

	/** the column name for the LEGAL_STATE_PROVINCE field */
	const LEGAL_STATE_PROVINCE = 'ar_party.LEGAL_STATE_PROVINCE';

	/** the column name for the LEGAL_COUNTRY field */
	const LEGAL_COUNTRY = 'ar_party.LEGAL_COUNTRY';

	/** the column name for the EMAIL field */
	const EMAIL = 'ar_party.EMAIL';

	/** the column name for the PHONE field */
	const PHONE = 'ar_party.PHONE';

	/** the column name for the PHONE2 field */
	const PHONE2 = 'ar_party.PHONE2';

	/** the column name for the FAX field */
	const FAX = 'ar_party.FAX';

	/** the column name for the MAX_LIMIT_30 field */
	const MAX_LIMIT_30 = 'ar_party.MAX_LIMIT_30';

	/** the column name for the LAST_EMAIL_ADVISE_FOR_MAX_LIMIT_30 field */
	const LAST_EMAIL_ADVISE_FOR_MAX_LIMIT_30 = 'ar_party.LAST_EMAIL_ADVISE_FOR_MAX_LIMIT_30';

	/** the column name for the IS_ACTIVE field */
	const IS_ACTIVE = 'ar_party.IS_ACTIVE';

	/** the column name for the AR_RESELLER_ID field */
	const AR_RESELLER_ID = 'ar_party.AR_RESELLER_ID';

	/** the column name for the MIGRATION_FIELD_FOR_TELEPHONE field */
	const MIGRATION_FIELD_FOR_TELEPHONE = 'ar_party.MIGRATION_FIELD_FOR_TELEPHONE';

	/** the column name for the MIGRATION_FIELD_FOR_ADSL field */
	const MIGRATION_FIELD_FOR_ADSL = 'ar_party.MIGRATION_FIELD_FOR_ADSL';

	/** the column name for the PAYMENT_IBAN field */
	const PAYMENT_IBAN = 'ar_party.PAYMENT_IBAN';

	/** the column name for the PAYMENT_BIC field */
	const PAYMENT_BIC = 'ar_party.PAYMENT_BIC';

	/** the column name for the PAYMENT_SEPA field */
	const PAYMENT_SEPA = 'ar_party.PAYMENT_SEPA';

	/** the column name for the PAYMENT_INFO field */
	const PAYMENT_INFO = 'ar_party.PAYMENT_INFO';

	/**
	 * An identiy map to hold any loaded instances of ArParty objects.
	 * This must be public so that other peer classes can access this when hydrating from JOIN
	 * queries.
	 * @var        array ArParty[]
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
		BasePeer::TYPE_PHPNAME => array ('Id', 'Name', 'CompactName', 'ExternalCrmCode', 'Vat', 'IsBillable', 'LegalAddress', 'LegalCity', 'LegalZipcode', 'LegalStateProvince', 'LegalCountry', 'Email', 'Phone', 'Phone2', 'Fax', 'MaxLimit30', 'LastEmailAdviseForMaxLimit30', 'IsActive', 'ArResellerId', 'MigrationFieldForTelephone', 'MigrationFieldForAdsl', 'PaymentIban', 'PaymentBic', 'PaymentSepa', 'PaymentInfo', ),
		BasePeer::TYPE_STUDLYPHPNAME => array ('id', 'name', 'compactName', 'externalCrmCode', 'vat', 'isBillable', 'legalAddress', 'legalCity', 'legalZipcode', 'legalStateProvince', 'legalCountry', 'email', 'phone', 'phone2', 'fax', 'maxLimit30', 'lastEmailAdviseForMaxLimit30', 'isActive', 'arResellerId', 'migrationFieldForTelephone', 'migrationFieldForAdsl', 'paymentIban', 'paymentBic', 'paymentSepa', 'paymentInfo', ),
		BasePeer::TYPE_COLNAME => array (self::ID, self::NAME, self::COMPACT_NAME, self::EXTERNAL_CRM_CODE, self::VAT, self::IS_BILLABLE, self::LEGAL_ADDRESS, self::LEGAL_CITY, self::LEGAL_ZIPCODE, self::LEGAL_STATE_PROVINCE, self::LEGAL_COUNTRY, self::EMAIL, self::PHONE, self::PHONE2, self::FAX, self::MAX_LIMIT_30, self::LAST_EMAIL_ADVISE_FOR_MAX_LIMIT_30, self::IS_ACTIVE, self::AR_RESELLER_ID, self::MIGRATION_FIELD_FOR_TELEPHONE, self::MIGRATION_FIELD_FOR_ADSL, self::PAYMENT_IBAN, self::PAYMENT_BIC, self::PAYMENT_SEPA, self::PAYMENT_INFO, ),
		BasePeer::TYPE_FIELDNAME => array ('id', 'name', 'compact_name', 'external_crm_code', 'vat', 'is_billable', 'legal_address', 'legal_city', 'legal_zipcode', 'legal_state_province', 'legal_country', 'email', 'phone', 'phone2', 'fax', 'max_limit_30', 'last_email_advise_for_max_limit_30', 'is_active', 'ar_reseller_id', 'migration_field_for_telephone', 'migration_field_for_adsl', 'payment_iban', 'payment_bic', 'payment_sepa', 'payment_info', ),
		BasePeer::TYPE_NUM => array (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, )
	);

	/**
	 * holds an array of keys for quick access to the fieldnames array
	 *
	 * first dimension keys are the type constants
	 * e.g. self::$fieldNames[BasePeer::TYPE_PHPNAME]['Id'] = 0
	 */
	private static $fieldKeys = array (
		BasePeer::TYPE_PHPNAME => array ('Id' => 0, 'Name' => 1, 'CompactName' => 2, 'ExternalCrmCode' => 3, 'Vat' => 4, 'IsBillable' => 5, 'LegalAddress' => 6, 'LegalCity' => 7, 'LegalZipcode' => 8, 'LegalStateProvince' => 9, 'LegalCountry' => 10, 'Email' => 11, 'Phone' => 12, 'Phone2' => 13, 'Fax' => 14, 'MaxLimit30' => 15, 'LastEmailAdviseForMaxLimit30' => 16, 'IsActive' => 17, 'ArResellerId' => 18, 'MigrationFieldForTelephone' => 19, 'MigrationFieldForAdsl' => 20, 'PaymentIban' => 21, 'PaymentBic' => 22, 'PaymentSepa' => 23, 'PaymentInfo' => 24, ),
		BasePeer::TYPE_STUDLYPHPNAME => array ('id' => 0, 'name' => 1, 'compactName' => 2, 'externalCrmCode' => 3, 'vat' => 4, 'isBillable' => 5, 'legalAddress' => 6, 'legalCity' => 7, 'legalZipcode' => 8, 'legalStateProvince' => 9, 'legalCountry' => 10, 'email' => 11, 'phone' => 12, 'phone2' => 13, 'fax' => 14, 'maxLimit30' => 15, 'lastEmailAdviseForMaxLimit30' => 16, 'isActive' => 17, 'arResellerId' => 18, 'migrationFieldForTelephone' => 19, 'migrationFieldForAdsl' => 20, 'paymentIban' => 21, 'paymentBic' => 22, 'paymentSepa' => 23, 'paymentInfo' => 24, ),
		BasePeer::TYPE_COLNAME => array (self::ID => 0, self::NAME => 1, self::COMPACT_NAME => 2, self::EXTERNAL_CRM_CODE => 3, self::VAT => 4, self::IS_BILLABLE => 5, self::LEGAL_ADDRESS => 6, self::LEGAL_CITY => 7, self::LEGAL_ZIPCODE => 8, self::LEGAL_STATE_PROVINCE => 9, self::LEGAL_COUNTRY => 10, self::EMAIL => 11, self::PHONE => 12, self::PHONE2 => 13, self::FAX => 14, self::MAX_LIMIT_30 => 15, self::LAST_EMAIL_ADVISE_FOR_MAX_LIMIT_30 => 16, self::IS_ACTIVE => 17, self::AR_RESELLER_ID => 18, self::MIGRATION_FIELD_FOR_TELEPHONE => 19, self::MIGRATION_FIELD_FOR_ADSL => 20, self::PAYMENT_IBAN => 21, self::PAYMENT_BIC => 22, self::PAYMENT_SEPA => 23, self::PAYMENT_INFO => 24, ),
		BasePeer::TYPE_FIELDNAME => array ('id' => 0, 'name' => 1, 'compact_name' => 2, 'external_crm_code' => 3, 'vat' => 4, 'is_billable' => 5, 'legal_address' => 6, 'legal_city' => 7, 'legal_zipcode' => 8, 'legal_state_province' => 9, 'legal_country' => 10, 'email' => 11, 'phone' => 12, 'phone2' => 13, 'fax' => 14, 'max_limit_30' => 15, 'last_email_advise_for_max_limit_30' => 16, 'is_active' => 17, 'ar_reseller_id' => 18, 'migration_field_for_telephone' => 19, 'migration_field_for_adsl' => 20, 'payment_iban' => 21, 'payment_bic' => 22, 'payment_sepa' => 23, 'payment_info' => 24, ),
		BasePeer::TYPE_NUM => array (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, )
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
	 * @param      string $column The column name for current table. (i.e. ArPartyPeer::COLUMN_NAME).
	 * @return     string
	 */
	public static function alias($alias, $column)
	{
		return str_replace(ArPartyPeer::TABLE_NAME.'.', $alias.'.', $column);
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
		$criteria->addSelectColumn(ArPartyPeer::ID);
		$criteria->addSelectColumn(ArPartyPeer::NAME);
		$criteria->addSelectColumn(ArPartyPeer::COMPACT_NAME);
		$criteria->addSelectColumn(ArPartyPeer::EXTERNAL_CRM_CODE);
		$criteria->addSelectColumn(ArPartyPeer::VAT);
		$criteria->addSelectColumn(ArPartyPeer::IS_BILLABLE);
		$criteria->addSelectColumn(ArPartyPeer::LEGAL_ADDRESS);
		$criteria->addSelectColumn(ArPartyPeer::LEGAL_CITY);
		$criteria->addSelectColumn(ArPartyPeer::LEGAL_ZIPCODE);
		$criteria->addSelectColumn(ArPartyPeer::LEGAL_STATE_PROVINCE);
		$criteria->addSelectColumn(ArPartyPeer::LEGAL_COUNTRY);
		$criteria->addSelectColumn(ArPartyPeer::EMAIL);
		$criteria->addSelectColumn(ArPartyPeer::PHONE);
		$criteria->addSelectColumn(ArPartyPeer::PHONE2);
		$criteria->addSelectColumn(ArPartyPeer::FAX);
		$criteria->addSelectColumn(ArPartyPeer::MAX_LIMIT_30);
		$criteria->addSelectColumn(ArPartyPeer::LAST_EMAIL_ADVISE_FOR_MAX_LIMIT_30);
		$criteria->addSelectColumn(ArPartyPeer::IS_ACTIVE);
		$criteria->addSelectColumn(ArPartyPeer::AR_RESELLER_ID);
		$criteria->addSelectColumn(ArPartyPeer::MIGRATION_FIELD_FOR_TELEPHONE);
		$criteria->addSelectColumn(ArPartyPeer::MIGRATION_FIELD_FOR_ADSL);
		$criteria->addSelectColumn(ArPartyPeer::PAYMENT_IBAN);
		$criteria->addSelectColumn(ArPartyPeer::PAYMENT_BIC);
		$criteria->addSelectColumn(ArPartyPeer::PAYMENT_SEPA);
		$criteria->addSelectColumn(ArPartyPeer::PAYMENT_INFO);
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
		$criteria->setPrimaryTableName(ArPartyPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArPartyPeer::addSelectColumns($criteria);
		}

		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		$criteria->setDbName(self::DATABASE_NAME); // Set the correct dbName

		if ($con === null) {
			$con = Propel::getConnection(ArPartyPeer::DATABASE_NAME, Propel::CONNECTION_READ);
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
	 * @return     ArParty
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectOne(Criteria $criteria, PropelPDO $con = null)
	{
		$critcopy = clone $criteria;
		$critcopy->setLimit(1);
		$objects = ArPartyPeer::doSelect($critcopy, $con);
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
		return ArPartyPeer::populateObjects(ArPartyPeer::doSelectStmt($criteria, $con));
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
			$con = Propel::getConnection(ArPartyPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		if (!$criteria->hasSelectClause()) {
			$criteria = clone $criteria;
			ArPartyPeer::addSelectColumns($criteria);
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
	 * @param      ArParty $value A ArParty object.
	 * @param      string $key (optional) key to use for instance map (for performance boost if key was already calculated externally).
	 */
	public static function addInstanceToPool(ArParty $obj, $key = null)
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
	 * @param      mixed $value A ArParty object or a primary key value.
	 */
	public static function removeInstanceFromPool($value)
	{
		if (Propel::isInstancePoolingEnabled() && $value !== null) {
			if (is_object($value) && $value instanceof ArParty) {
				$key = (string) $value->getId();
			} elseif (is_scalar($value)) {
				// assume we've been passed a primary key
				$key = (string) $value;
			} else {
				$e = new PropelException("Invalid value passed to removeInstanceFromPool().  Expected primary key or ArParty object; got " . (is_object($value) ? get_class($value) . ' object.' : var_export($value,true)));
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
	 * @return     ArParty Found object or NULL if 1) no instance exists for specified key or 2) instance pooling has been disabled.
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
	 * Method to invalidate the instance pool of all tables related to ar_party
	 * by a foreign key with ON DELETE CASCADE
	 */
	public static function clearRelatedInstancePool()
	{
		// invalidate objects in ArPartyHasTagPeer instance pool, since one or more of them may be deleted by ON DELETE CASCADE rule.
		ArPartyHasTagPeer::clearInstancePool();

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
		$cls = ArPartyPeer::getOMClass(false);
		// populate the object(s)
		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key = ArPartyPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj = ArPartyPeer::getInstanceFromPool($key))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj->hydrate($row, 0, true); // rehydrate
				$results[] = $obj;
			} else {
				$obj = new $cls();
				$obj->hydrate($row);
				$results[] = $obj;
				ArPartyPeer::addInstanceToPool($obj, $key);
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
		$criteria->setPrimaryTableName(ArPartyPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArPartyPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArPartyPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArPartyPeer::AR_RESELLER_ID, ArResellerPeer::ID, $join_behavior);

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
	 * Selects a collection of ArParty objects pre-filled with their ArReseller objects.
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArParty objects.
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

		ArPartyPeer::addSelectColumns($criteria);
		$startcol = (ArPartyPeer::NUM_COLUMNS - ArPartyPeer::NUM_LAZY_LOAD_COLUMNS);
		ArResellerPeer::addSelectColumns($criteria);

		$criteria->addJoin(ArPartyPeer::AR_RESELLER_ID, ArResellerPeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArPartyPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArPartyPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {

				$cls = ArPartyPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArPartyPeer::addInstanceToPool($obj1, $key1);
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
				
				// Add the $obj1 (ArParty) to $obj2 (ArReseller)
				$obj2->addArParty($obj1);

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
		$criteria->setPrimaryTableName(ArPartyPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArPartyPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArPartyPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArPartyPeer::AR_RESELLER_ID, ArResellerPeer::ID, $join_behavior);

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
	 * Selects a collection of ArParty objects pre-filled with all related objects.
	 *
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArParty objects.
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

		ArPartyPeer::addSelectColumns($criteria);
		$startcol2 = (ArPartyPeer::NUM_COLUMNS - ArPartyPeer::NUM_LAZY_LOAD_COLUMNS);

		ArResellerPeer::addSelectColumns($criteria);
		$startcol3 = $startcol2 + (ArResellerPeer::NUM_COLUMNS - ArResellerPeer::NUM_LAZY_LOAD_COLUMNS);

		$criteria->addJoin(ArPartyPeer::AR_RESELLER_ID, ArResellerPeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArPartyPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArPartyPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {
				$cls = ArPartyPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArPartyPeer::addInstanceToPool($obj1, $key1);
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

				// Add the $obj1 (ArParty) to the collection in $obj2 (ArReseller)
				$obj2->addArParty($obj1);
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
	  $dbMap = Propel::getDatabaseMap(BaseArPartyPeer::DATABASE_NAME);
	  if (!$dbMap->hasTable(BaseArPartyPeer::TABLE_NAME))
	  {
	    $dbMap->addTableObject(new ArPartyTableMap());
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
		return $withPrefix ? ArPartyPeer::CLASS_DEFAULT : ArPartyPeer::OM_CLASS;
	}

	/**
	 * Method perform an INSERT on the database, given a ArParty or Criteria object.
	 *
	 * @param      mixed $values Criteria or ArParty object containing data that is used to create the INSERT statement.
	 * @param      PropelPDO $con the PropelPDO connection to use
	 * @return     mixed The new primary key.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doInsert($values, PropelPDO $con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArPartyPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		if ($values instanceof Criteria) {
			$criteria = clone $values; // rename for clarity
		} else {
			$criteria = $values->buildCriteria(); // build Criteria from ArParty object
		}

		if ($criteria->containsKey(ArPartyPeer::ID) && $criteria->keyContainsValue(ArPartyPeer::ID) ) {
			throw new PropelException('Cannot insert a value for auto-increment primary key ('.ArPartyPeer::ID.')');
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
	 * Method perform an UPDATE on the database, given a ArParty or Criteria object.
	 *
	 * @param      mixed $values Criteria or ArParty object containing data that is used to create the UPDATE statement.
	 * @param      PropelPDO $con The connection to use (specify PropelPDO connection object to exert more control over transactions).
	 * @return     int The number of affected rows (if supported by underlying database driver).
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doUpdate($values, PropelPDO $con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArPartyPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		$selectCriteria = new Criteria(self::DATABASE_NAME);

		if ($values instanceof Criteria) {
			$criteria = clone $values; // rename for clarity

			$comparison = $criteria->getComparison(ArPartyPeer::ID);
			$selectCriteria->add(ArPartyPeer::ID, $criteria->remove(ArPartyPeer::ID), $comparison);

		} else { // $values is ArParty object
			$criteria = $values->buildCriteria(); // gets full criteria
			$selectCriteria = $values->buildPkeyCriteria(); // gets criteria w/ primary key(s)
		}

		// set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		return BasePeer::doUpdate($selectCriteria, $criteria, $con);
	}

	/**
	 * Method to DELETE all rows from the ar_party table.
	 *
	 * @return     int The number of affected rows (if supported by underlying database driver).
	 */
	public static function doDeleteAll($con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArPartyPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}
		$affectedRows = 0; // initialize var to track total num of affected rows
		try {
			// use transaction because $criteria could contain info
			// for more than one table or we could emulating ON DELETE CASCADE, etc.
			$con->beginTransaction();
			$affectedRows += BasePeer::doDeleteAll(ArPartyPeer::TABLE_NAME, $con);
			// Because this db requires some delete cascade/set null emulation, we have to
			// clear the cached instance *after* the emulation has happened (since
			// instances get re-added by the select statement contained therein).
			ArPartyPeer::clearInstancePool();
			ArPartyPeer::clearRelatedInstancePool();
			$con->commit();
			return $affectedRows;
		} catch (PropelException $e) {
			$con->rollBack();
			throw $e;
		}
	}

	/**
	 * Method perform a DELETE on the database, given a ArParty or Criteria object OR a primary key value.
	 *
	 * @param      mixed $values Criteria or ArParty object or primary key or array of primary keys
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
			$con = Propel::getConnection(ArPartyPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		if ($values instanceof Criteria) {
			// invalidate the cache for all objects of this type, since we have no
			// way of knowing (without running a query) what objects should be invalidated
			// from the cache based on this Criteria.
			ArPartyPeer::clearInstancePool();
			// rename for clarity
			$criteria = clone $values;
		} elseif ($values instanceof ArParty) { // it's a model object
			// invalidate the cache for this single object
			ArPartyPeer::removeInstanceFromPool($values);
			// create criteria based on pk values
			$criteria = $values->buildPkeyCriteria();
		} else { // it's a primary key, or an array of pks
			$criteria = new Criteria(self::DATABASE_NAME);
			$criteria->add(ArPartyPeer::ID, (array) $values, Criteria::IN);
			// invalidate the cache for this object(s)
			foreach ((array) $values as $singleval) {
				ArPartyPeer::removeInstanceFromPool($singleval);
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
			ArPartyPeer::clearRelatedInstancePool();
			$con->commit();
			return $affectedRows;
		} catch (PropelException $e) {
			$con->rollBack();
			throw $e;
		}
	}

	/**
	 * Validates all modified columns of given ArParty object.
	 * If parameter $columns is either a single column name or an array of column names
	 * than only those columns are validated.
	 *
	 * NOTICE: This does not apply to primary or foreign keys for now.
	 *
	 * @param      ArParty $obj The object to validate.
	 * @param      mixed $cols Column name or array of column names.
	 *
	 * @return     mixed TRUE if all columns are valid or the error message of the first invalid column.
	 */
	public static function doValidate(ArParty $obj, $cols = null)
	{
		$columns = array();

		if ($cols) {
			$dbMap = Propel::getDatabaseMap(ArPartyPeer::DATABASE_NAME);
			$tableMap = $dbMap->getTable(ArPartyPeer::TABLE_NAME);

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

		return BasePeer::doValidate(ArPartyPeer::DATABASE_NAME, ArPartyPeer::TABLE_NAME, $columns);
	}

	/**
	 * Retrieve a single object by pkey.
	 *
	 * @param      int $pk the primary key.
	 * @param      PropelPDO $con the connection to use
	 * @return     ArParty
	 */
	public static function retrieveByPK($pk, PropelPDO $con = null)
	{

		if (null !== ($obj = ArPartyPeer::getInstanceFromPool((string) $pk))) {
			return $obj;
		}

		if ($con === null) {
			$con = Propel::getConnection(ArPartyPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria = new Criteria(ArPartyPeer::DATABASE_NAME);
		$criteria->add(ArPartyPeer::ID, $pk);

		$v = ArPartyPeer::doSelect($criteria, $con);

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
			$con = Propel::getConnection(ArPartyPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$objs = null;
		if (empty($pks)) {
			$objs = array();
		} else {
			$criteria = new Criteria(ArPartyPeer::DATABASE_NAME);
			$criteria->add(ArPartyPeer::ID, $pks, Criteria::IN);
			$objs = ArPartyPeer::doSelect($criteria, $con);
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

} // BaseArPartyPeer

// This is the static code needed to register the TableMap for this table with the main Propel class.
//
BaseArPartyPeer::buildTableMap();

