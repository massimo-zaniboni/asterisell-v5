<?php

/**
 * Base class that represents a row from the 'ar_vendor_domain' table.
 *
 * 
 *
 * @package    lib.model.om
 */
abstract class BaseArVendorDomain extends BaseObject  implements Persistent {


	/**
	 * The Peer class.
	 * Instance provides a convenient way of calling static methods on a class
	 * that calling code may not be able to identify.
	 * @var        ArVendorDomainPeer
	 */
	protected static $peer;

	/**
	 * The value for the id field.
	 * @var        int
	 */
	protected $id;

	/**
	 * The value for the internal_name field.
	 * @var        string
	 */
	protected $internal_name;

	/**
	 * The value for the ar_vendor_id field.
	 * @var        int
	 */
	protected $ar_vendor_id;

	/**
	 * The value for the ar_communication_channel_type_id field.
	 * @var        int
	 */
	protected $ar_communication_channel_type_id;

	/**
	 * The value for the domain field.
	 * @var        string
	 */
	protected $domain;

	/**
	 * The value for the is_prefix field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $is_prefix;

	/**
	 * The value for the is_suffix field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $is_suffix;

	/**
	 * The value for the from field.
	 * @var        string
	 */
	protected $from;

	/**
	 * The value for the to field.
	 * @var        string
	 */
	protected $to;

	/**
	 * @var        ArVendor
	 */
	protected $aArVendor;

	/**
	 * @var        ArCommunicationChannelType
	 */
	protected $aArCommunicationChannelType;

	/**
	 * Flag to prevent endless save loop, if this object is referenced
	 * by another object which falls in this transaction.
	 * @var        boolean
	 */
	protected $alreadyInSave = false;

	/**
	 * Flag to prevent endless validation loop, if this object is referenced
	 * by another object which falls in this transaction.
	 * @var        boolean
	 */
	protected $alreadyInValidation = false;

	// symfony behavior
	
	const PEER = 'ArVendorDomainPeer';

	/**
	 * Applies default values to this object.
	 * This method should be called from the object's constructor (or
	 * equivalent initialization method).
	 * @see        __construct()
	 */
	public function applyDefaultValues()
	{
		$this->is_prefix = false;
		$this->is_suffix = false;
	}

	/**
	 * Initializes internal state of BaseArVendorDomain object.
	 * @see        applyDefaults()
	 */
	public function __construct()
	{
		parent::__construct();
		$this->applyDefaultValues();
	}

	/**
	 * Get the [id] column value.
	 * 
	 * @return     int
	 */
	public function getId()
	{
		return $this->id;
	}

	/**
	 * Get the [internal_name] column value.
	 * 
	 * @return     string
	 */
	public function getInternalName()
	{
		return $this->internal_name;
	}

	/**
	 * Get the [ar_vendor_id] column value.
	 * 
	 * @return     int
	 */
	public function getArVendorId()
	{
		return $this->ar_vendor_id;
	}

	/**
	 * Get the [ar_communication_channel_type_id] column value.
	 * 
	 * @return     int
	 */
	public function getArCommunicationChannelTypeId()
	{
		return $this->ar_communication_channel_type_id;
	}

	/**
	 * Get the [domain] column value.
	 * 
	 * @return     string
	 */
	public function getDomain()
	{
		return $this->domain;
	}

	/**
	 * Get the [is_prefix] column value.
	 * 
	 * @return     boolean
	 */
	public function getIsPrefix()
	{
		return $this->is_prefix;
	}

	/**
	 * Get the [is_suffix] column value.
	 * 
	 * @return     boolean
	 */
	public function getIsSuffix()
	{
		return $this->is_suffix;
	}

	/**
	 * Get the [optionally formatted] temporal [from] column value.
	 * 
	 *
	 * @param      string $format The date/time format string (either date()-style or strftime()-style).
	 *							If format is NULL, then the raw DateTime object will be returned.
	 * @return     mixed Formatted date/time value as string or DateTime object (if format is NULL), NULL if column is NULL, and 0 if column value is 0000-00-00 00:00:00
	 * @throws     PropelException - if unable to parse/validate the date/time value.
	 */
	public function getFrom($format = 'Y-m-d H:i:s')
	{
		if ($this->from === null) {
			return null;
		}


		if ($this->from === '0000-00-00 00:00:00') {
			// while technically this is not a default value of NULL,
			// this seems to be closest in meaning.
			return null;
		} else {
			try {
				$dt = new DateTime($this->from);
			} catch (Exception $x) {
				throw new PropelException("Internally stored date/time/timestamp value could not be converted to DateTime: " . var_export($this->from, true), $x);
			}
		}

		if ($format === null) {
			// Because propel.useDateTimeClass is TRUE, we return a DateTime object.
			return $dt;
		} elseif (strpos($format, '%') !== false) {
			return strftime($format, $dt->format('U'));
		} else {
			return $dt->format($format);
		}
	}

	/**
	 * Get the [optionally formatted] temporal [to] column value.
	 * 
	 *
	 * @param      string $format The date/time format string (either date()-style or strftime()-style).
	 *							If format is NULL, then the raw DateTime object will be returned.
	 * @return     mixed Formatted date/time value as string or DateTime object (if format is NULL), NULL if column is NULL, and 0 if column value is 0000-00-00 00:00:00
	 * @throws     PropelException - if unable to parse/validate the date/time value.
	 */
	public function getTo($format = 'Y-m-d H:i:s')
	{
		if ($this->to === null) {
			return null;
		}


		if ($this->to === '0000-00-00 00:00:00') {
			// while technically this is not a default value of NULL,
			// this seems to be closest in meaning.
			return null;
		} else {
			try {
				$dt = new DateTime($this->to);
			} catch (Exception $x) {
				throw new PropelException("Internally stored date/time/timestamp value could not be converted to DateTime: " . var_export($this->to, true), $x);
			}
		}

		if ($format === null) {
			// Because propel.useDateTimeClass is TRUE, we return a DateTime object.
			return $dt;
		} elseif (strpos($format, '%') !== false) {
			return strftime($format, $dt->format('U'));
		} else {
			return $dt->format($format);
		}
	}

	/**
	 * Set the value of [id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArVendorDomain The current object (for fluent API support)
	 */
	public function setId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->id !== $v) {
			$this->id = $v;
			$this->modifiedColumns[] = ArVendorDomainPeer::ID;
		}

		return $this;
	} // setId()

	/**
	 * Set the value of [internal_name] column.
	 * 
	 * @param      string $v new value
	 * @return     ArVendorDomain The current object (for fluent API support)
	 */
	public function setInternalName($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->internal_name !== $v) {
			$this->internal_name = $v;
			$this->modifiedColumns[] = ArVendorDomainPeer::INTERNAL_NAME;
		}

		return $this;
	} // setInternalName()

	/**
	 * Set the value of [ar_vendor_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArVendorDomain The current object (for fluent API support)
	 */
	public function setArVendorId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->ar_vendor_id !== $v) {
			$this->ar_vendor_id = $v;
			$this->modifiedColumns[] = ArVendorDomainPeer::AR_VENDOR_ID;
		}

		if ($this->aArVendor !== null && $this->aArVendor->getId() !== $v) {
			$this->aArVendor = null;
		}

		return $this;
	} // setArVendorId()

	/**
	 * Set the value of [ar_communication_channel_type_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArVendorDomain The current object (for fluent API support)
	 */
	public function setArCommunicationChannelTypeId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->ar_communication_channel_type_id !== $v) {
			$this->ar_communication_channel_type_id = $v;
			$this->modifiedColumns[] = ArVendorDomainPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID;
		}

		if ($this->aArCommunicationChannelType !== null && $this->aArCommunicationChannelType->getId() !== $v) {
			$this->aArCommunicationChannelType = null;
		}

		return $this;
	} // setArCommunicationChannelTypeId()

	/**
	 * Set the value of [domain] column.
	 * 
	 * @param      string $v new value
	 * @return     ArVendorDomain The current object (for fluent API support)
	 */
	public function setDomain($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->domain !== $v) {
			$this->domain = $v;
			$this->modifiedColumns[] = ArVendorDomainPeer::DOMAIN;
		}

		return $this;
	} // setDomain()

	/**
	 * Set the value of [is_prefix] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArVendorDomain The current object (for fluent API support)
	 */
	public function setIsPrefix($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->is_prefix !== $v || $this->isNew()) {
			$this->is_prefix = $v;
			$this->modifiedColumns[] = ArVendorDomainPeer::IS_PREFIX;
		}

		return $this;
	} // setIsPrefix()

	/**
	 * Set the value of [is_suffix] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArVendorDomain The current object (for fluent API support)
	 */
	public function setIsSuffix($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->is_suffix !== $v || $this->isNew()) {
			$this->is_suffix = $v;
			$this->modifiedColumns[] = ArVendorDomainPeer::IS_SUFFIX;
		}

		return $this;
	} // setIsSuffix()

	/**
	 * Sets the value of [from] column to a normalized version of the date/time value specified.
	 * 
	 * @param      mixed $v string, integer (timestamp), or DateTime value.  Empty string will
	 *						be treated as NULL for temporal objects.
	 * @return     ArVendorDomain The current object (for fluent API support)
	 */
	public function setFrom($v)
	{
		// we treat '' as NULL for temporal objects because DateTime('') == DateTime('now')
		// -- which is unexpected, to say the least.
		if ($v === null || $v === '') {
			$dt = null;
		} elseif ($v instanceof DateTime) {
			$dt = $v;
		} else {
			// some string/numeric value passed; we normalize that so that we can
			// validate it.
			try {
				if (is_numeric($v)) { // if it's a unix timestamp
					$dt = new DateTime('@'.$v, new DateTimeZone('UTC'));
					// We have to explicitly specify and then change the time zone because of a
					// DateTime bug: http://bugs.php.net/bug.php?id=43003
					$dt->setTimeZone(new DateTimeZone(date_default_timezone_get()));
				} else {
					$dt = new DateTime($v);
				}
			} catch (Exception $x) {
				throw new PropelException('Error parsing date/time value: ' . var_export($v, true), $x);
			}
		}

		if ( $this->from !== null || $dt !== null ) {
			// (nested ifs are a little easier to read in this case)

			$currNorm = ($this->from !== null && $tmpDt = new DateTime($this->from)) ? $tmpDt->format('Y-m-d H:i:s') : null;
			$newNorm = ($dt !== null) ? $dt->format('Y-m-d H:i:s') : null;

			if ( ($currNorm !== $newNorm) // normalized values don't match 
					)
			{
				$this->from = ($dt ? $dt->format('Y-m-d H:i:s') : null);
				$this->modifiedColumns[] = ArVendorDomainPeer::FROM;
			}
		} // if either are not null

		return $this;
	} // setFrom()

	/**
	 * Sets the value of [to] column to a normalized version of the date/time value specified.
	 * 
	 * @param      mixed $v string, integer (timestamp), or DateTime value.  Empty string will
	 *						be treated as NULL for temporal objects.
	 * @return     ArVendorDomain The current object (for fluent API support)
	 */
	public function setTo($v)
	{
		// we treat '' as NULL for temporal objects because DateTime('') == DateTime('now')
		// -- which is unexpected, to say the least.
		if ($v === null || $v === '') {
			$dt = null;
		} elseif ($v instanceof DateTime) {
			$dt = $v;
		} else {
			// some string/numeric value passed; we normalize that so that we can
			// validate it.
			try {
				if (is_numeric($v)) { // if it's a unix timestamp
					$dt = new DateTime('@'.$v, new DateTimeZone('UTC'));
					// We have to explicitly specify and then change the time zone because of a
					// DateTime bug: http://bugs.php.net/bug.php?id=43003
					$dt->setTimeZone(new DateTimeZone(date_default_timezone_get()));
				} else {
					$dt = new DateTime($v);
				}
			} catch (Exception $x) {
				throw new PropelException('Error parsing date/time value: ' . var_export($v, true), $x);
			}
		}

		if ( $this->to !== null || $dt !== null ) {
			// (nested ifs are a little easier to read in this case)

			$currNorm = ($this->to !== null && $tmpDt = new DateTime($this->to)) ? $tmpDt->format('Y-m-d H:i:s') : null;
			$newNorm = ($dt !== null) ? $dt->format('Y-m-d H:i:s') : null;

			if ( ($currNorm !== $newNorm) // normalized values don't match 
					)
			{
				$this->to = ($dt ? $dt->format('Y-m-d H:i:s') : null);
				$this->modifiedColumns[] = ArVendorDomainPeer::TO;
			}
		} // if either are not null

		return $this;
	} // setTo()

	/**
	 * Indicates whether the columns in this object are only set to default values.
	 *
	 * This method can be used in conjunction with isModified() to indicate whether an object is both
	 * modified _and_ has some values set which are non-default.
	 *
	 * @return     boolean Whether the columns in this object are only been set with default values.
	 */
	public function hasOnlyDefaultValues()
	{
			if ($this->is_prefix !== false) {
				return false;
			}

			if ($this->is_suffix !== false) {
				return false;
			}

		// otherwise, everything was equal, so return TRUE
		return true;
	} // hasOnlyDefaultValues()

	/**
	 * Hydrates (populates) the object variables with values from the database resultset.
	 *
	 * An offset (0-based "start column") is specified so that objects can be hydrated
	 * with a subset of the columns in the resultset rows.  This is needed, for example,
	 * for results of JOIN queries where the resultset row includes columns from two or
	 * more tables.
	 *
	 * @param      array $row The row returned by PDOStatement->fetch(PDO::FETCH_NUM)
	 * @param      int $startcol 0-based offset column which indicates which restultset column to start with.
	 * @param      boolean $rehydrate Whether this object is being re-hydrated from the database.
	 * @return     int next starting column
	 * @throws     PropelException  - Any caught Exception will be rewrapped as a PropelException.
	 */
	public function hydrate($row, $startcol = 0, $rehydrate = false)
	{
		try {

			$this->id = ($row[$startcol + 0] !== null) ? (int) $row[$startcol + 0] : null;
			$this->internal_name = ($row[$startcol + 1] !== null) ? (string) $row[$startcol + 1] : null;
			$this->ar_vendor_id = ($row[$startcol + 2] !== null) ? (int) $row[$startcol + 2] : null;
			$this->ar_communication_channel_type_id = ($row[$startcol + 3] !== null) ? (int) $row[$startcol + 3] : null;
			$this->domain = ($row[$startcol + 4] !== null) ? (string) $row[$startcol + 4] : null;
			$this->is_prefix = ($row[$startcol + 5] !== null) ? (boolean) $row[$startcol + 5] : null;
			$this->is_suffix = ($row[$startcol + 6] !== null) ? (boolean) $row[$startcol + 6] : null;
			$this->from = ($row[$startcol + 7] !== null) ? (string) $row[$startcol + 7] : null;
			$this->to = ($row[$startcol + 8] !== null) ? (string) $row[$startcol + 8] : null;
			$this->resetModified();

			$this->setNew(false);

			if ($rehydrate) {
				$this->ensureConsistency();
			}

			// FIXME - using NUM_COLUMNS may be clearer.
			return $startcol + 9; // 9 = ArVendorDomainPeer::NUM_COLUMNS - ArVendorDomainPeer::NUM_LAZY_LOAD_COLUMNS).

		} catch (Exception $e) {
			throw new PropelException("Error populating ArVendorDomain object", $e);
		}
	}

	/**
	 * Checks and repairs the internal consistency of the object.
	 *
	 * This method is executed after an already-instantiated object is re-hydrated
	 * from the database.  It exists to check any foreign keys to make sure that
	 * the objects related to the current object are correct based on foreign key.
	 *
	 * You can override this method in the stub class, but you should always invoke
	 * the base method from the overridden method (i.e. parent::ensureConsistency()),
	 * in case your model changes.
	 *
	 * @throws     PropelException
	 */
	public function ensureConsistency()
	{

		if ($this->aArVendor !== null && $this->ar_vendor_id !== $this->aArVendor->getId()) {
			$this->aArVendor = null;
		}
		if ($this->aArCommunicationChannelType !== null && $this->ar_communication_channel_type_id !== $this->aArCommunicationChannelType->getId()) {
			$this->aArCommunicationChannelType = null;
		}
	} // ensureConsistency

	/**
	 * Reloads this object from datastore based on primary key and (optionally) resets all associated objects.
	 *
	 * This will only work if the object has been saved and has a valid primary key set.
	 *
	 * @param      boolean $deep (optional) Whether to also de-associated any related objects.
	 * @param      PropelPDO $con (optional) The PropelPDO connection to use.
	 * @return     void
	 * @throws     PropelException - if this object is deleted, unsaved or doesn't have pk match in db
	 */
	public function reload($deep = false, PropelPDO $con = null)
	{
		if ($this->isDeleted()) {
			throw new PropelException("Cannot reload a deleted object.");
		}

		if ($this->isNew()) {
			throw new PropelException("Cannot reload an unsaved object.");
		}

		if ($con === null) {
			$con = Propel::getConnection(ArVendorDomainPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		// We don't need to alter the object instance pool; we're just modifying this instance
		// already in the pool.

		$stmt = ArVendorDomainPeer::doSelectStmt($this->buildPkeyCriteria(), $con);
		$row = $stmt->fetch(PDO::FETCH_NUM);
		$stmt->closeCursor();
		if (!$row) {
			throw new PropelException('Cannot find matching row in the database to reload object values.');
		}
		$this->hydrate($row, 0, true); // rehydrate

		if ($deep) {  // also de-associate any related objects?

			$this->aArVendor = null;
			$this->aArCommunicationChannelType = null;
		} // if (deep)
	}

	/**
	 * Removes this object from datastore and sets delete attribute.
	 *
	 * @param      PropelPDO $con
	 * @return     void
	 * @throws     PropelException
	 * @see        BaseObject::setDeleted()
	 * @see        BaseObject::isDeleted()
	 */
	public function delete(PropelPDO $con = null)
	{
		if ($this->isDeleted()) {
			throw new PropelException("This object has already been deleted.");
		}

		if ($con === null) {
			$con = Propel::getConnection(ArVendorDomainPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}
		
		$con->beginTransaction();
		try {
			$ret = $this->preDelete($con);
			if ($ret) {
				ArVendorDomainPeer::doDelete($this, $con);
				$this->postDelete($con);
				$this->setDeleted(true);
				$con->commit();
			} else {
				$con->commit();
			}
		} catch (PropelException $e) {
			$con->rollBack();
			throw $e;
		}
	}

	/**
	 * Persists this object to the database.
	 *
	 * If the object is new, it inserts it; otherwise an update is performed.
	 * All modified related objects will also be persisted in the doSave()
	 * method.  This method wraps all precipitate database operations in a
	 * single transaction.
	 *
	 * @param      PropelPDO $con
	 * @return     int The number of rows affected by this insert/update and any referring fk objects' save() operations.
	 * @throws     PropelException
	 * @see        doSave()
	 */
	public function save(PropelPDO $con = null)
	{
		if ($this->isDeleted()) {
			throw new PropelException("You cannot save an object that has been deleted.");
		}

		if ($con === null) {
			$con = Propel::getConnection(ArVendorDomainPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}
		
		$con->beginTransaction();
		$isInsert = $this->isNew();
		try {
			$ret = $this->preSave($con);
			if ($isInsert) {
				$ret = $ret && $this->preInsert($con);
			} else {
				$ret = $ret && $this->preUpdate($con);
			}
			if ($ret) {
				$affectedRows = $this->doSave($con);
				if ($isInsert) {
					$this->postInsert($con);
				} else {
					$this->postUpdate($con);
				}
				$this->postSave($con);
				ArVendorDomainPeer::addInstanceToPool($this);
			} else {
				$affectedRows = 0;
			}
			$con->commit();
			return $affectedRows;
		} catch (PropelException $e) {
			$con->rollBack();
			throw $e;
		}
	}

	/**
	 * Performs the work of inserting or updating the row in the database.
	 *
	 * If the object is new, it inserts it; otherwise an update is performed.
	 * All related objects are also updated in this method.
	 *
	 * @param      PropelPDO $con
	 * @return     int The number of rows affected by this insert/update and any referring fk objects' save() operations.
	 * @throws     PropelException
	 * @see        save()
	 */
	protected function doSave(PropelPDO $con)
	{
		$affectedRows = 0; // initialize var to track total num of affected rows
		if (!$this->alreadyInSave) {
			$this->alreadyInSave = true;

			// We call the save method on the following object(s) if they
			// were passed to this object by their coresponding set
			// method.  This object relates to these object(s) by a
			// foreign key reference.

			if ($this->aArVendor !== null) {
				if ($this->aArVendor->isModified() || $this->aArVendor->isNew()) {
					$affectedRows += $this->aArVendor->save($con);
				}
				$this->setArVendor($this->aArVendor);
			}

			if ($this->aArCommunicationChannelType !== null) {
				if ($this->aArCommunicationChannelType->isModified() || $this->aArCommunicationChannelType->isNew()) {
					$affectedRows += $this->aArCommunicationChannelType->save($con);
				}
				$this->setArCommunicationChannelType($this->aArCommunicationChannelType);
			}

			if ($this->isNew() ) {
				$this->modifiedColumns[] = ArVendorDomainPeer::ID;
			}

			// If this object has been modified, then save it to the database.
			if ($this->isModified()) {
				if ($this->isNew()) {
					$pk = ArVendorDomainPeer::doInsert($this, $con);
					$affectedRows += 1; // we are assuming that there is only 1 row per doInsert() which
										 // should always be true here (even though technically
										 // BasePeer::doInsert() can insert multiple rows).

					$this->setId($pk);  //[IMV] update autoincrement primary key

					$this->setNew(false);
				} else {
					$affectedRows += ArVendorDomainPeer::doUpdate($this, $con);
				}

				$this->resetModified(); // [HL] After being saved an object is no longer 'modified'
			}

			$this->alreadyInSave = false;

		}
		return $affectedRows;
	} // doSave()

	/**
	 * Array of ValidationFailed objects.
	 * @var        array ValidationFailed[]
	 */
	protected $validationFailures = array();

	/**
	 * Gets any ValidationFailed objects that resulted from last call to validate().
	 *
	 *
	 * @return     array ValidationFailed[]
	 * @see        validate()
	 */
	public function getValidationFailures()
	{
		return $this->validationFailures;
	}

	/**
	 * Validates the objects modified field values and all objects related to this table.
	 *
	 * If $columns is either a column name or an array of column names
	 * only those columns are validated.
	 *
	 * @param      mixed $columns Column name or an array of column names.
	 * @return     boolean Whether all columns pass validation.
	 * @see        doValidate()
	 * @see        getValidationFailures()
	 */
	public function validate($columns = null)
	{
		$res = $this->doValidate($columns);
		if ($res === true) {
			$this->validationFailures = array();
			return true;
		} else {
			$this->validationFailures = $res;
			return false;
		}
	}

	/**
	 * This function performs the validation work for complex object models.
	 *
	 * In addition to checking the current object, all related objects will
	 * also be validated.  If all pass then <code>true</code> is returned; otherwise
	 * an aggreagated array of ValidationFailed objects will be returned.
	 *
	 * @param      array $columns Array of column names to validate.
	 * @return     mixed <code>true</code> if all validations pass; array of <code>ValidationFailed</code> objets otherwise.
	 */
	protected function doValidate($columns = null)
	{
		if (!$this->alreadyInValidation) {
			$this->alreadyInValidation = true;
			$retval = null;

			$failureMap = array();


			// We call the validate method on the following object(s) if they
			// were passed to this object by their coresponding set
			// method.  This object relates to these object(s) by a
			// foreign key reference.

			if ($this->aArVendor !== null) {
				if (!$this->aArVendor->validate($columns)) {
					$failureMap = array_merge($failureMap, $this->aArVendor->getValidationFailures());
				}
			}

			if ($this->aArCommunicationChannelType !== null) {
				if (!$this->aArCommunicationChannelType->validate($columns)) {
					$failureMap = array_merge($failureMap, $this->aArCommunicationChannelType->getValidationFailures());
				}
			}


			if (($retval = ArVendorDomainPeer::doValidate($this, $columns)) !== true) {
				$failureMap = array_merge($failureMap, $retval);
			}



			$this->alreadyInValidation = false;
		}

		return (!empty($failureMap) ? $failureMap : true);
	}

	/**
	 * Retrieves a field from the object by name passed in as a string.
	 *
	 * @param      string $name name
	 * @param      string $type The type of fieldname the $name is of:
	 *                     one of the class type constants BasePeer::TYPE_PHPNAME, BasePeer::TYPE_STUDLYPHPNAME
	 *                     BasePeer::TYPE_COLNAME, BasePeer::TYPE_FIELDNAME, BasePeer::TYPE_NUM
	 * @return     mixed Value of field.
	 */
	public function getByName($name, $type = BasePeer::TYPE_PHPNAME)
	{
		$pos = ArVendorDomainPeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
		$field = $this->getByPosition($pos);
		return $field;
	}

	/**
	 * Retrieves a field from the object by Position as specified in the xml schema.
	 * Zero-based.
	 *
	 * @param      int $pos position in xml schema
	 * @return     mixed Value of field at $pos
	 */
	public function getByPosition($pos)
	{
		switch($pos) {
			case 0:
				return $this->getId();
				break;
			case 1:
				return $this->getInternalName();
				break;
			case 2:
				return $this->getArVendorId();
				break;
			case 3:
				return $this->getArCommunicationChannelTypeId();
				break;
			case 4:
				return $this->getDomain();
				break;
			case 5:
				return $this->getIsPrefix();
				break;
			case 6:
				return $this->getIsSuffix();
				break;
			case 7:
				return $this->getFrom();
				break;
			case 8:
				return $this->getTo();
				break;
			default:
				return null;
				break;
		} // switch()
	}

	/**
	 * Exports the object as an array.
	 *
	 * You can specify the key type of the array by passing one of the class
	 * type constants.
	 *
	 * @param      string $keyType (optional) One of the class type constants BasePeer::TYPE_PHPNAME, BasePeer::TYPE_STUDLYPHPNAME
	 *                        BasePeer::TYPE_COLNAME, BasePeer::TYPE_FIELDNAME, BasePeer::TYPE_NUM. Defaults to BasePeer::TYPE_PHPNAME.
	 * @param      boolean $includeLazyLoadColumns (optional) Whether to include lazy loaded columns.  Defaults to TRUE.
	 * @return     an associative array containing the field names (as keys) and field values
	 */
	public function toArray($keyType = BasePeer::TYPE_PHPNAME, $includeLazyLoadColumns = true)
	{
		$keys = ArVendorDomainPeer::getFieldNames($keyType);
		$result = array(
			$keys[0] => $this->getId(),
			$keys[1] => $this->getInternalName(),
			$keys[2] => $this->getArVendorId(),
			$keys[3] => $this->getArCommunicationChannelTypeId(),
			$keys[4] => $this->getDomain(),
			$keys[5] => $this->getIsPrefix(),
			$keys[6] => $this->getIsSuffix(),
			$keys[7] => $this->getFrom(),
			$keys[8] => $this->getTo(),
		);
		return $result;
	}

	/**
	 * Sets a field from the object by name passed in as a string.
	 *
	 * @param      string $name peer name
	 * @param      mixed $value field value
	 * @param      string $type The type of fieldname the $name is of:
	 *                     one of the class type constants BasePeer::TYPE_PHPNAME, BasePeer::TYPE_STUDLYPHPNAME
	 *                     BasePeer::TYPE_COLNAME, BasePeer::TYPE_FIELDNAME, BasePeer::TYPE_NUM
	 * @return     void
	 */
	public function setByName($name, $value, $type = BasePeer::TYPE_PHPNAME)
	{
		$pos = ArVendorDomainPeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
		return $this->setByPosition($pos, $value);
	}

	/**
	 * Sets a field from the object by Position as specified in the xml schema.
	 * Zero-based.
	 *
	 * @param      int $pos position in xml schema
	 * @param      mixed $value field value
	 * @return     void
	 */
	public function setByPosition($pos, $value)
	{
		switch($pos) {
			case 0:
				$this->setId($value);
				break;
			case 1:
				$this->setInternalName($value);
				break;
			case 2:
				$this->setArVendorId($value);
				break;
			case 3:
				$this->setArCommunicationChannelTypeId($value);
				break;
			case 4:
				$this->setDomain($value);
				break;
			case 5:
				$this->setIsPrefix($value);
				break;
			case 6:
				$this->setIsSuffix($value);
				break;
			case 7:
				$this->setFrom($value);
				break;
			case 8:
				$this->setTo($value);
				break;
		} // switch()
	}

	/**
	 * Populates the object using an array.
	 *
	 * This is particularly useful when populating an object from one of the
	 * request arrays (e.g. $_POST).  This method goes through the column
	 * names, checking to see whether a matching key exists in populated
	 * array. If so the setByName() method is called for that column.
	 *
	 * You can specify the key type of the array by additionally passing one
	 * of the class type constants BasePeer::TYPE_PHPNAME, BasePeer::TYPE_STUDLYPHPNAME,
	 * BasePeer::TYPE_COLNAME, BasePeer::TYPE_FIELDNAME, BasePeer::TYPE_NUM.
	 * The default key type is the column's phpname (e.g. 'AuthorId')
	 *
	 * @param      array  $arr     An array to populate the object from.
	 * @param      string $keyType The type of keys the array uses.
	 * @return     void
	 */
	public function fromArray($arr, $keyType = BasePeer::TYPE_PHPNAME)
	{
		$keys = ArVendorDomainPeer::getFieldNames($keyType);

		if (array_key_exists($keys[0], $arr)) $this->setId($arr[$keys[0]]);
		if (array_key_exists($keys[1], $arr)) $this->setInternalName($arr[$keys[1]]);
		if (array_key_exists($keys[2], $arr)) $this->setArVendorId($arr[$keys[2]]);
		if (array_key_exists($keys[3], $arr)) $this->setArCommunicationChannelTypeId($arr[$keys[3]]);
		if (array_key_exists($keys[4], $arr)) $this->setDomain($arr[$keys[4]]);
		if (array_key_exists($keys[5], $arr)) $this->setIsPrefix($arr[$keys[5]]);
		if (array_key_exists($keys[6], $arr)) $this->setIsSuffix($arr[$keys[6]]);
		if (array_key_exists($keys[7], $arr)) $this->setFrom($arr[$keys[7]]);
		if (array_key_exists($keys[8], $arr)) $this->setTo($arr[$keys[8]]);
	}

	/**
	 * Build a Criteria object containing the values of all modified columns in this object.
	 *
	 * @return     Criteria The Criteria object containing all modified values.
	 */
	public function buildCriteria()
	{
		$criteria = new Criteria(ArVendorDomainPeer::DATABASE_NAME);

		if ($this->isColumnModified(ArVendorDomainPeer::ID)) $criteria->add(ArVendorDomainPeer::ID, $this->id);
		if ($this->isColumnModified(ArVendorDomainPeer::INTERNAL_NAME)) $criteria->add(ArVendorDomainPeer::INTERNAL_NAME, $this->internal_name);
		if ($this->isColumnModified(ArVendorDomainPeer::AR_VENDOR_ID)) $criteria->add(ArVendorDomainPeer::AR_VENDOR_ID, $this->ar_vendor_id);
		if ($this->isColumnModified(ArVendorDomainPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID)) $criteria->add(ArVendorDomainPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID, $this->ar_communication_channel_type_id);
		if ($this->isColumnModified(ArVendorDomainPeer::DOMAIN)) $criteria->add(ArVendorDomainPeer::DOMAIN, $this->domain);
		if ($this->isColumnModified(ArVendorDomainPeer::IS_PREFIX)) $criteria->add(ArVendorDomainPeer::IS_PREFIX, $this->is_prefix);
		if ($this->isColumnModified(ArVendorDomainPeer::IS_SUFFIX)) $criteria->add(ArVendorDomainPeer::IS_SUFFIX, $this->is_suffix);
		if ($this->isColumnModified(ArVendorDomainPeer::FROM)) $criteria->add(ArVendorDomainPeer::FROM, $this->from);
		if ($this->isColumnModified(ArVendorDomainPeer::TO)) $criteria->add(ArVendorDomainPeer::TO, $this->to);

		return $criteria;
	}

	/**
	 * Builds a Criteria object containing the primary key for this object.
	 *
	 * Unlike buildCriteria() this method includes the primary key values regardless
	 * of whether or not they have been modified.
	 *
	 * @return     Criteria The Criteria object containing value(s) for primary key(s).
	 */
	public function buildPkeyCriteria()
	{
		$criteria = new Criteria(ArVendorDomainPeer::DATABASE_NAME);

		$criteria->add(ArVendorDomainPeer::ID, $this->id);

		return $criteria;
	}

	/**
	 * Returns the primary key for this object (row).
	 * @return     int
	 */
	public function getPrimaryKey()
	{
		return $this->getId();
	}

	/**
	 * Generic method to set the primary key (id column).
	 *
	 * @param      int $key Primary key.
	 * @return     void
	 */
	public function setPrimaryKey($key)
	{
		$this->setId($key);
	}

	/**
	 * Sets contents of passed object to values from current object.
	 *
	 * If desired, this method can also make copies of all associated (fkey referrers)
	 * objects.
	 *
	 * @param      object $copyObj An object of ArVendorDomain (or compatible) type.
	 * @param      boolean $deepCopy Whether to also copy all rows that refer (by fkey) to the current row.
	 * @throws     PropelException
	 */
	public function copyInto($copyObj, $deepCopy = false)
	{

		$copyObj->setInternalName($this->internal_name);

		$copyObj->setArVendorId($this->ar_vendor_id);

		$copyObj->setArCommunicationChannelTypeId($this->ar_communication_channel_type_id);

		$copyObj->setDomain($this->domain);

		$copyObj->setIsPrefix($this->is_prefix);

		$copyObj->setIsSuffix($this->is_suffix);

		$copyObj->setFrom($this->from);

		$copyObj->setTo($this->to);


		$copyObj->setNew(true);

		$copyObj->setId(NULL); // this is a auto-increment column, so set to default value

	}

	/**
	 * Makes a copy of this object that will be inserted as a new row in table when saved.
	 * It creates a new object filling in the simple attributes, but skipping any primary
	 * keys that are defined for the table.
	 *
	 * If desired, this method can also make copies of all associated (fkey referrers)
	 * objects.
	 *
	 * @param      boolean $deepCopy Whether to also copy all rows that refer (by fkey) to the current row.
	 * @return     ArVendorDomain Clone of current object.
	 * @throws     PropelException
	 */
	public function copy($deepCopy = false)
	{
		// we use get_class(), because this might be a subclass
		$clazz = get_class($this);
		$copyObj = new $clazz();
		$this->copyInto($copyObj, $deepCopy);
		return $copyObj;
	}

	/**
	 * Returns a peer instance associated with this om.
	 *
	 * Since Peer classes are not to have any instance attributes, this method returns the
	 * same instance for all member of this class. The method could therefore
	 * be static, but this would prevent one from overriding the behavior.
	 *
	 * @return     ArVendorDomainPeer
	 */
	public function getPeer()
	{
		if (self::$peer === null) {
			self::$peer = new ArVendorDomainPeer();
		}
		return self::$peer;
	}

	/**
	 * Declares an association between this object and a ArVendor object.
	 *
	 * @param      ArVendor $v
	 * @return     ArVendorDomain The current object (for fluent API support)
	 * @throws     PropelException
	 */
	public function setArVendor(ArVendor $v = null)
	{
		if ($v === null) {
			$this->setArVendorId(NULL);
		} else {
			$this->setArVendorId($v->getId());
		}

		$this->aArVendor = $v;

		// Add binding for other direction of this n:n relationship.
		// If this object has already been added to the ArVendor object, it will not be re-added.
		if ($v !== null) {
			$v->addArVendorDomain($this);
		}

		return $this;
	}


	/**
	 * Get the associated ArVendor object
	 *
	 * @param      PropelPDO Optional Connection object.
	 * @return     ArVendor The associated ArVendor object.
	 * @throws     PropelException
	 */
	public function getArVendor(PropelPDO $con = null)
	{
		if ($this->aArVendor === null && ($this->ar_vendor_id !== null)) {
			$this->aArVendor = ArVendorPeer::retrieveByPk($this->ar_vendor_id);
			/* The following can be used additionally to
			   guarantee the related object contains a reference
			   to this object.  This level of coupling may, however, be
			   undesirable since it could result in an only partially populated collection
			   in the referenced object.
			   $this->aArVendor->addArVendorDomains($this);
			 */
		}
		return $this->aArVendor;
	}

	/**
	 * Declares an association between this object and a ArCommunicationChannelType object.
	 *
	 * @param      ArCommunicationChannelType $v
	 * @return     ArVendorDomain The current object (for fluent API support)
	 * @throws     PropelException
	 */
	public function setArCommunicationChannelType(ArCommunicationChannelType $v = null)
	{
		if ($v === null) {
			$this->setArCommunicationChannelTypeId(NULL);
		} else {
			$this->setArCommunicationChannelTypeId($v->getId());
		}

		$this->aArCommunicationChannelType = $v;

		// Add binding for other direction of this n:n relationship.
		// If this object has already been added to the ArCommunicationChannelType object, it will not be re-added.
		if ($v !== null) {
			$v->addArVendorDomain($this);
		}

		return $this;
	}


	/**
	 * Get the associated ArCommunicationChannelType object
	 *
	 * @param      PropelPDO Optional Connection object.
	 * @return     ArCommunicationChannelType The associated ArCommunicationChannelType object.
	 * @throws     PropelException
	 */
	public function getArCommunicationChannelType(PropelPDO $con = null)
	{
		if ($this->aArCommunicationChannelType === null && ($this->ar_communication_channel_type_id !== null)) {
			$this->aArCommunicationChannelType = ArCommunicationChannelTypePeer::retrieveByPk($this->ar_communication_channel_type_id);
			/* The following can be used additionally to
			   guarantee the related object contains a reference
			   to this object.  This level of coupling may, however, be
			   undesirable since it could result in an only partially populated collection
			   in the referenced object.
			   $this->aArCommunicationChannelType->addArVendorDomains($this);
			 */
		}
		return $this->aArCommunicationChannelType;
	}

	/**
	 * Resets all collections of referencing foreign keys.
	 *
	 * This method is a user-space workaround for PHP's inability to garbage collect objects
	 * with circular references.  This is currently necessary when using Propel in certain
	 * daemon or large-volumne/high-memory operations.
	 *
	 * @param      boolean $deep Whether to also clear the references on all associated objects.
	 */
	public function clearAllReferences($deep = false)
	{
		if ($deep) {
		} // if ($deep)

			$this->aArVendor = null;
			$this->aArCommunicationChannelType = null;
	}

} // BaseArVendorDomain
