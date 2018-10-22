<?php

/**
 * Base class that represents a row from the 'ar_cached_grouped_cdr' table.
 *
 * 
 *
 * @package    lib.model.om
 */
abstract class BaseArCachedGroupedCdr extends BaseObject  implements Persistent {


	/**
	 * The Peer class.
	 * Instance provides a convenient way of calling static methods on a class
	 * that calling code may not be able to identify.
	 * @var        ArCachedGroupedCdrPeer
	 */
	protected static $peer;

	/**
	 * The value for the cached_parent_id_hierarchy field.
	 * @var        string
	 */
	protected $cached_parent_id_hierarchy;

	/**
	 * The value for the billable_ar_organization_unit_id field.
	 * @var        int
	 */
	protected $billable_ar_organization_unit_id;

	/**
	 * The value for the calldate field.
	 * @var        string
	 */
	protected $calldate;

	/**
	 * The value for the destination_type field.
	 * @var        int
	 */
	protected $destination_type;

	/**
	 * The value for the ar_communication_channel_type_id field.
	 * @var        int
	 */
	protected $ar_communication_channel_type_id;

	/**
	 * The value for the operator_type field.
	 * @var        string
	 */
	protected $operator_type;

	/**
	 * The value for the ar_vendor_id field.
	 * @var        int
	 */
	protected $ar_vendor_id;

	/**
	 * The value for the geographic_location field.
	 * @var        string
	 */
	protected $geographic_location;

	/**
	 * The value for the count_of_calls field.
	 * @var        string
	 */
	protected $count_of_calls;

	/**
	 * The value for the billsec field.
	 * @var        string
	 */
	protected $billsec;

	/**
	 * The value for the income field.
	 * @var        string
	 */
	protected $income;

	/**
	 * The value for the cost_saving field.
	 * @var        string
	 */
	protected $cost_saving;

	/**
	 * The value for the cost field.
	 * @var        string
	 */
	protected $cost;

	/**
	 * The value for the id field.
	 * Note: this column has a database default value of: 0
	 * @var        int
	 */
	protected $id;

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
	
	const PEER = 'ArCachedGroupedCdrPeer';

	/**
	 * Applies default values to this object.
	 * This method should be called from the object's constructor (or
	 * equivalent initialization method).
	 * @see        __construct()
	 */
	public function applyDefaultValues()
	{
		$this->id = 0;
	}

	/**
	 * Initializes internal state of BaseArCachedGroupedCdr object.
	 * @see        applyDefaults()
	 */
	public function __construct()
	{
		parent::__construct();
		$this->applyDefaultValues();
	}

	/**
	 * Get the [cached_parent_id_hierarchy] column value.
	 * 
	 * @return     string
	 */
	public function getCachedParentIdHierarchy()
	{
		return $this->cached_parent_id_hierarchy;
	}

	/**
	 * Get the [billable_ar_organization_unit_id] column value.
	 * 
	 * @return     int
	 */
	public function getBillableArOrganizationUnitId()
	{
		return $this->billable_ar_organization_unit_id;
	}

	/**
	 * Get the [optionally formatted] temporal [calldate] column value.
	 * 
	 *
	 * @param      string $format The date/time format string (either date()-style or strftime()-style).
	 *							If format is NULL, then the raw DateTime object will be returned.
	 * @return     mixed Formatted date/time value as string or DateTime object (if format is NULL), NULL if column is NULL, and 0 if column value is 0000-00-00
	 * @throws     PropelException - if unable to parse/validate the date/time value.
	 */
	public function getCalldate($format = 'Y-m-d')
	{
		if ($this->calldate === null) {
			return null;
		}


		if ($this->calldate === '0000-00-00') {
			// while technically this is not a default value of NULL,
			// this seems to be closest in meaning.
			return null;
		} else {
			try {
				$dt = new DateTime($this->calldate);
			} catch (Exception $x) {
				throw new PropelException("Internally stored date/time/timestamp value could not be converted to DateTime: " . var_export($this->calldate, true), $x);
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
	 * Get the [destination_type] column value.
	 * 
	 * @return     int
	 */
	public function getDestinationType()
	{
		return $this->destination_type;
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
	 * Get the [operator_type] column value.
	 * 
	 * @return     string
	 */
	public function getOperatorType()
	{
		return $this->operator_type;
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
	 * Get the [geographic_location] column value.
	 * 
	 * @return     string
	 */
	public function getGeographicLocation()
	{
		return $this->geographic_location;
	}

	/**
	 * Get the [count_of_calls] column value.
	 * 
	 * @return     string
	 */
	public function getCountOfCalls()
	{
		return $this->count_of_calls;
	}

	/**
	 * Get the [billsec] column value.
	 * 
	 * @return     string
	 */
	public function getBillsec()
	{
		return $this->billsec;
	}

	/**
	 * Get the [income] column value.
	 * 
	 * @return     string
	 */
	public function getIncome()
	{
		return $this->income;
	}

	/**
	 * Get the [cost_saving] column value.
	 * 
	 * @return     string
	 */
	public function getCostSaving()
	{
		return $this->cost_saving;
	}

	/**
	 * Get the [cost] column value.
	 * 
	 * @return     string
	 */
	public function getCost()
	{
		return $this->cost;
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
	 * Set the value of [cached_parent_id_hierarchy] column.
	 * 
	 * @param      string $v new value
	 * @return     ArCachedGroupedCdr The current object (for fluent API support)
	 */
	public function setCachedParentIdHierarchy($v)
	{
		// Because BLOB columns are streams in PDO we have to assume that they are
		// always modified when a new value is passed in.  For example, the contents
		// of the stream itself may have changed externally.
		if (!is_resource($v) && $v !== null) {
			$this->cached_parent_id_hierarchy = fopen('php://memory', 'r+');
			fwrite($this->cached_parent_id_hierarchy, $v);
			rewind($this->cached_parent_id_hierarchy);
		} else { // it's already a stream
			$this->cached_parent_id_hierarchy = $v;
		}
		$this->modifiedColumns[] = ArCachedGroupedCdrPeer::CACHED_PARENT_ID_HIERARCHY;

		return $this;
	} // setCachedParentIdHierarchy()

	/**
	 * Set the value of [billable_ar_organization_unit_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArCachedGroupedCdr The current object (for fluent API support)
	 */
	public function setBillableArOrganizationUnitId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->billable_ar_organization_unit_id !== $v) {
			$this->billable_ar_organization_unit_id = $v;
			$this->modifiedColumns[] = ArCachedGroupedCdrPeer::BILLABLE_AR_ORGANIZATION_UNIT_ID;
		}

		return $this;
	} // setBillableArOrganizationUnitId()

	/**
	 * Sets the value of [calldate] column to a normalized version of the date/time value specified.
	 * 
	 * @param      mixed $v string, integer (timestamp), or DateTime value.  Empty string will
	 *						be treated as NULL for temporal objects.
	 * @return     ArCachedGroupedCdr The current object (for fluent API support)
	 */
	public function setCalldate($v)
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

		if ( $this->calldate !== null || $dt !== null ) {
			// (nested ifs are a little easier to read in this case)

			$currNorm = ($this->calldate !== null && $tmpDt = new DateTime($this->calldate)) ? $tmpDt->format('Y-m-d') : null;
			$newNorm = ($dt !== null) ? $dt->format('Y-m-d') : null;

			if ( ($currNorm !== $newNorm) // normalized values don't match 
					)
			{
				$this->calldate = ($dt ? $dt->format('Y-m-d') : null);
				$this->modifiedColumns[] = ArCachedGroupedCdrPeer::CALLDATE;
			}
		} // if either are not null

		return $this;
	} // setCalldate()

	/**
	 * Set the value of [destination_type] column.
	 * 
	 * @param      int $v new value
	 * @return     ArCachedGroupedCdr The current object (for fluent API support)
	 */
	public function setDestinationType($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->destination_type !== $v) {
			$this->destination_type = $v;
			$this->modifiedColumns[] = ArCachedGroupedCdrPeer::DESTINATION_TYPE;
		}

		return $this;
	} // setDestinationType()

	/**
	 * Set the value of [ar_communication_channel_type_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArCachedGroupedCdr The current object (for fluent API support)
	 */
	public function setArCommunicationChannelTypeId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->ar_communication_channel_type_id !== $v) {
			$this->ar_communication_channel_type_id = $v;
			$this->modifiedColumns[] = ArCachedGroupedCdrPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID;
		}

		return $this;
	} // setArCommunicationChannelTypeId()

	/**
	 * Set the value of [operator_type] column.
	 * 
	 * @param      string $v new value
	 * @return     ArCachedGroupedCdr The current object (for fluent API support)
	 */
	public function setOperatorType($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->operator_type !== $v) {
			$this->operator_type = $v;
			$this->modifiedColumns[] = ArCachedGroupedCdrPeer::OPERATOR_TYPE;
		}

		return $this;
	} // setOperatorType()

	/**
	 * Set the value of [ar_vendor_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArCachedGroupedCdr The current object (for fluent API support)
	 */
	public function setArVendorId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->ar_vendor_id !== $v) {
			$this->ar_vendor_id = $v;
			$this->modifiedColumns[] = ArCachedGroupedCdrPeer::AR_VENDOR_ID;
		}

		return $this;
	} // setArVendorId()

	/**
	 * Set the value of [geographic_location] column.
	 * 
	 * @param      string $v new value
	 * @return     ArCachedGroupedCdr The current object (for fluent API support)
	 */
	public function setGeographicLocation($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->geographic_location !== $v) {
			$this->geographic_location = $v;
			$this->modifiedColumns[] = ArCachedGroupedCdrPeer::GEOGRAPHIC_LOCATION;
		}

		return $this;
	} // setGeographicLocation()

	/**
	 * Set the value of [count_of_calls] column.
	 * 
	 * @param      string $v new value
	 * @return     ArCachedGroupedCdr The current object (for fluent API support)
	 */
	public function setCountOfCalls($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->count_of_calls !== $v) {
			$this->count_of_calls = $v;
			$this->modifiedColumns[] = ArCachedGroupedCdrPeer::COUNT_OF_CALLS;
		}

		return $this;
	} // setCountOfCalls()

	/**
	 * Set the value of [billsec] column.
	 * 
	 * @param      string $v new value
	 * @return     ArCachedGroupedCdr The current object (for fluent API support)
	 */
	public function setBillsec($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->billsec !== $v) {
			$this->billsec = $v;
			$this->modifiedColumns[] = ArCachedGroupedCdrPeer::BILLSEC;
		}

		return $this;
	} // setBillsec()

	/**
	 * Set the value of [income] column.
	 * 
	 * @param      string $v new value
	 * @return     ArCachedGroupedCdr The current object (for fluent API support)
	 */
	public function setIncome($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->income !== $v) {
			$this->income = $v;
			$this->modifiedColumns[] = ArCachedGroupedCdrPeer::INCOME;
		}

		return $this;
	} // setIncome()

	/**
	 * Set the value of [cost_saving] column.
	 * 
	 * @param      string $v new value
	 * @return     ArCachedGroupedCdr The current object (for fluent API support)
	 */
	public function setCostSaving($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->cost_saving !== $v) {
			$this->cost_saving = $v;
			$this->modifiedColumns[] = ArCachedGroupedCdrPeer::COST_SAVING;
		}

		return $this;
	} // setCostSaving()

	/**
	 * Set the value of [cost] column.
	 * 
	 * @param      string $v new value
	 * @return     ArCachedGroupedCdr The current object (for fluent API support)
	 */
	public function setCost($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->cost !== $v) {
			$this->cost = $v;
			$this->modifiedColumns[] = ArCachedGroupedCdrPeer::COST;
		}

		return $this;
	} // setCost()

	/**
	 * Set the value of [id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArCachedGroupedCdr The current object (for fluent API support)
	 */
	public function setId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->id !== $v || $this->isNew()) {
			$this->id = $v;
			$this->modifiedColumns[] = ArCachedGroupedCdrPeer::ID;
		}

		return $this;
	} // setId()

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
			if ($this->id !== 0) {
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

			if ($row[$startcol + 0] !== null) {
				$this->cached_parent_id_hierarchy = fopen('php://memory', 'r+');
				fwrite($this->cached_parent_id_hierarchy, $row[$startcol + 0]);
				rewind($this->cached_parent_id_hierarchy);
			} else {
				$this->cached_parent_id_hierarchy = null;
			}
			$this->billable_ar_organization_unit_id = ($row[$startcol + 1] !== null) ? (int) $row[$startcol + 1] : null;
			$this->calldate = ($row[$startcol + 2] !== null) ? (string) $row[$startcol + 2] : null;
			$this->destination_type = ($row[$startcol + 3] !== null) ? (int) $row[$startcol + 3] : null;
			$this->ar_communication_channel_type_id = ($row[$startcol + 4] !== null) ? (int) $row[$startcol + 4] : null;
			$this->operator_type = ($row[$startcol + 5] !== null) ? (string) $row[$startcol + 5] : null;
			$this->ar_vendor_id = ($row[$startcol + 6] !== null) ? (int) $row[$startcol + 6] : null;
			$this->geographic_location = ($row[$startcol + 7] !== null) ? (string) $row[$startcol + 7] : null;
			$this->count_of_calls = ($row[$startcol + 8] !== null) ? (string) $row[$startcol + 8] : null;
			$this->billsec = ($row[$startcol + 9] !== null) ? (string) $row[$startcol + 9] : null;
			$this->income = ($row[$startcol + 10] !== null) ? (string) $row[$startcol + 10] : null;
			$this->cost_saving = ($row[$startcol + 11] !== null) ? (string) $row[$startcol + 11] : null;
			$this->cost = ($row[$startcol + 12] !== null) ? (string) $row[$startcol + 12] : null;
			$this->id = ($row[$startcol + 13] !== null) ? (int) $row[$startcol + 13] : null;
			$this->resetModified();

			$this->setNew(false);

			if ($rehydrate) {
				$this->ensureConsistency();
			}

			// FIXME - using NUM_COLUMNS may be clearer.
			return $startcol + 14; // 14 = ArCachedGroupedCdrPeer::NUM_COLUMNS - ArCachedGroupedCdrPeer::NUM_LAZY_LOAD_COLUMNS).

		} catch (Exception $e) {
			throw new PropelException("Error populating ArCachedGroupedCdr object", $e);
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
			$con = Propel::getConnection(ArCachedGroupedCdrPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		// We don't need to alter the object instance pool; we're just modifying this instance
		// already in the pool.

		$stmt = ArCachedGroupedCdrPeer::doSelectStmt($this->buildPkeyCriteria(), $con);
		$row = $stmt->fetch(PDO::FETCH_NUM);
		$stmt->closeCursor();
		if (!$row) {
			throw new PropelException('Cannot find matching row in the database to reload object values.');
		}
		$this->hydrate($row, 0, true); // rehydrate

		if ($deep) {  // also de-associate any related objects?

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
			$con = Propel::getConnection(ArCachedGroupedCdrPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}
		
		$con->beginTransaction();
		try {
			$ret = $this->preDelete($con);
			if ($ret) {
				ArCachedGroupedCdrPeer::doDelete($this, $con);
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
			$con = Propel::getConnection(ArCachedGroupedCdrPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
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
				ArCachedGroupedCdrPeer::addInstanceToPool($this);
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


			// If this object has been modified, then save it to the database.
			if ($this->isModified()) {
				if ($this->isNew()) {
					$pk = ArCachedGroupedCdrPeer::doInsert($this, $con);
					$affectedRows += 1; // we are assuming that there is only 1 row per doInsert() which
										 // should always be true here (even though technically
										 // BasePeer::doInsert() can insert multiple rows).

					$this->setNew(false);
				} else {
					$affectedRows += ArCachedGroupedCdrPeer::doUpdate($this, $con);
				}

				// Rewind the cached_parent_id_hierarchy LOB column, since PDO does not rewind after inserting value.
				if ($this->cached_parent_id_hierarchy !== null && is_resource($this->cached_parent_id_hierarchy)) {
					rewind($this->cached_parent_id_hierarchy);
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


			if (($retval = ArCachedGroupedCdrPeer::doValidate($this, $columns)) !== true) {
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
		$pos = ArCachedGroupedCdrPeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
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
				return $this->getCachedParentIdHierarchy();
				break;
			case 1:
				return $this->getBillableArOrganizationUnitId();
				break;
			case 2:
				return $this->getCalldate();
				break;
			case 3:
				return $this->getDestinationType();
				break;
			case 4:
				return $this->getArCommunicationChannelTypeId();
				break;
			case 5:
				return $this->getOperatorType();
				break;
			case 6:
				return $this->getArVendorId();
				break;
			case 7:
				return $this->getGeographicLocation();
				break;
			case 8:
				return $this->getCountOfCalls();
				break;
			case 9:
				return $this->getBillsec();
				break;
			case 10:
				return $this->getIncome();
				break;
			case 11:
				return $this->getCostSaving();
				break;
			case 12:
				return $this->getCost();
				break;
			case 13:
				return $this->getId();
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
		$keys = ArCachedGroupedCdrPeer::getFieldNames($keyType);
		$result = array(
			$keys[0] => $this->getCachedParentIdHierarchy(),
			$keys[1] => $this->getBillableArOrganizationUnitId(),
			$keys[2] => $this->getCalldate(),
			$keys[3] => $this->getDestinationType(),
			$keys[4] => $this->getArCommunicationChannelTypeId(),
			$keys[5] => $this->getOperatorType(),
			$keys[6] => $this->getArVendorId(),
			$keys[7] => $this->getGeographicLocation(),
			$keys[8] => $this->getCountOfCalls(),
			$keys[9] => $this->getBillsec(),
			$keys[10] => $this->getIncome(),
			$keys[11] => $this->getCostSaving(),
			$keys[12] => $this->getCost(),
			$keys[13] => $this->getId(),
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
		$pos = ArCachedGroupedCdrPeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
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
				$this->setCachedParentIdHierarchy($value);
				break;
			case 1:
				$this->setBillableArOrganizationUnitId($value);
				break;
			case 2:
				$this->setCalldate($value);
				break;
			case 3:
				$this->setDestinationType($value);
				break;
			case 4:
				$this->setArCommunicationChannelTypeId($value);
				break;
			case 5:
				$this->setOperatorType($value);
				break;
			case 6:
				$this->setArVendorId($value);
				break;
			case 7:
				$this->setGeographicLocation($value);
				break;
			case 8:
				$this->setCountOfCalls($value);
				break;
			case 9:
				$this->setBillsec($value);
				break;
			case 10:
				$this->setIncome($value);
				break;
			case 11:
				$this->setCostSaving($value);
				break;
			case 12:
				$this->setCost($value);
				break;
			case 13:
				$this->setId($value);
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
		$keys = ArCachedGroupedCdrPeer::getFieldNames($keyType);

		if (array_key_exists($keys[0], $arr)) $this->setCachedParentIdHierarchy($arr[$keys[0]]);
		if (array_key_exists($keys[1], $arr)) $this->setBillableArOrganizationUnitId($arr[$keys[1]]);
		if (array_key_exists($keys[2], $arr)) $this->setCalldate($arr[$keys[2]]);
		if (array_key_exists($keys[3], $arr)) $this->setDestinationType($arr[$keys[3]]);
		if (array_key_exists($keys[4], $arr)) $this->setArCommunicationChannelTypeId($arr[$keys[4]]);
		if (array_key_exists($keys[5], $arr)) $this->setOperatorType($arr[$keys[5]]);
		if (array_key_exists($keys[6], $arr)) $this->setArVendorId($arr[$keys[6]]);
		if (array_key_exists($keys[7], $arr)) $this->setGeographicLocation($arr[$keys[7]]);
		if (array_key_exists($keys[8], $arr)) $this->setCountOfCalls($arr[$keys[8]]);
		if (array_key_exists($keys[9], $arr)) $this->setBillsec($arr[$keys[9]]);
		if (array_key_exists($keys[10], $arr)) $this->setIncome($arr[$keys[10]]);
		if (array_key_exists($keys[11], $arr)) $this->setCostSaving($arr[$keys[11]]);
		if (array_key_exists($keys[12], $arr)) $this->setCost($arr[$keys[12]]);
		if (array_key_exists($keys[13], $arr)) $this->setId($arr[$keys[13]]);
	}

	/**
	 * Build a Criteria object containing the values of all modified columns in this object.
	 *
	 * @return     Criteria The Criteria object containing all modified values.
	 */
	public function buildCriteria()
	{
		$criteria = new Criteria(ArCachedGroupedCdrPeer::DATABASE_NAME);

		if ($this->isColumnModified(ArCachedGroupedCdrPeer::CACHED_PARENT_ID_HIERARCHY)) $criteria->add(ArCachedGroupedCdrPeer::CACHED_PARENT_ID_HIERARCHY, $this->cached_parent_id_hierarchy);
		if ($this->isColumnModified(ArCachedGroupedCdrPeer::BILLABLE_AR_ORGANIZATION_UNIT_ID)) $criteria->add(ArCachedGroupedCdrPeer::BILLABLE_AR_ORGANIZATION_UNIT_ID, $this->billable_ar_organization_unit_id);
		if ($this->isColumnModified(ArCachedGroupedCdrPeer::CALLDATE)) $criteria->add(ArCachedGroupedCdrPeer::CALLDATE, $this->calldate);
		if ($this->isColumnModified(ArCachedGroupedCdrPeer::DESTINATION_TYPE)) $criteria->add(ArCachedGroupedCdrPeer::DESTINATION_TYPE, $this->destination_type);
		if ($this->isColumnModified(ArCachedGroupedCdrPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID)) $criteria->add(ArCachedGroupedCdrPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID, $this->ar_communication_channel_type_id);
		if ($this->isColumnModified(ArCachedGroupedCdrPeer::OPERATOR_TYPE)) $criteria->add(ArCachedGroupedCdrPeer::OPERATOR_TYPE, $this->operator_type);
		if ($this->isColumnModified(ArCachedGroupedCdrPeer::AR_VENDOR_ID)) $criteria->add(ArCachedGroupedCdrPeer::AR_VENDOR_ID, $this->ar_vendor_id);
		if ($this->isColumnModified(ArCachedGroupedCdrPeer::GEOGRAPHIC_LOCATION)) $criteria->add(ArCachedGroupedCdrPeer::GEOGRAPHIC_LOCATION, $this->geographic_location);
		if ($this->isColumnModified(ArCachedGroupedCdrPeer::COUNT_OF_CALLS)) $criteria->add(ArCachedGroupedCdrPeer::COUNT_OF_CALLS, $this->count_of_calls);
		if ($this->isColumnModified(ArCachedGroupedCdrPeer::BILLSEC)) $criteria->add(ArCachedGroupedCdrPeer::BILLSEC, $this->billsec);
		if ($this->isColumnModified(ArCachedGroupedCdrPeer::INCOME)) $criteria->add(ArCachedGroupedCdrPeer::INCOME, $this->income);
		if ($this->isColumnModified(ArCachedGroupedCdrPeer::COST_SAVING)) $criteria->add(ArCachedGroupedCdrPeer::COST_SAVING, $this->cost_saving);
		if ($this->isColumnModified(ArCachedGroupedCdrPeer::COST)) $criteria->add(ArCachedGroupedCdrPeer::COST, $this->cost);
		if ($this->isColumnModified(ArCachedGroupedCdrPeer::ID)) $criteria->add(ArCachedGroupedCdrPeer::ID, $this->id);

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
		$criteria = new Criteria(ArCachedGroupedCdrPeer::DATABASE_NAME);

		$criteria->add(ArCachedGroupedCdrPeer::CACHED_PARENT_ID_HIERARCHY, $this->cached_parent_id_hierarchy);
		$criteria->add(ArCachedGroupedCdrPeer::BILLABLE_AR_ORGANIZATION_UNIT_ID, $this->billable_ar_organization_unit_id);
		$criteria->add(ArCachedGroupedCdrPeer::CALLDATE, $this->calldate);
		$criteria->add(ArCachedGroupedCdrPeer::DESTINATION_TYPE, $this->destination_type);
		$criteria->add(ArCachedGroupedCdrPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID, $this->ar_communication_channel_type_id);
		$criteria->add(ArCachedGroupedCdrPeer::OPERATOR_TYPE, $this->operator_type);
		$criteria->add(ArCachedGroupedCdrPeer::AR_VENDOR_ID, $this->ar_vendor_id);
		$criteria->add(ArCachedGroupedCdrPeer::GEOGRAPHIC_LOCATION, $this->geographic_location);

		return $criteria;
	}

	/**
	 * Returns the composite primary key for this object.
	 * The array elements will be in same order as specified in XML.
	 * @return     array
	 */
	public function getPrimaryKey()
	{
		$pks = array();

		$pks[0] = $this->getCachedParentIdHierarchy();

		$pks[1] = $this->getBillableArOrganizationUnitId();

		$pks[2] = $this->getCalldate();

		$pks[3] = $this->getDestinationType();

		$pks[4] = $this->getArCommunicationChannelTypeId();

		$pks[5] = $this->getOperatorType();

		$pks[6] = $this->getArVendorId();

		$pks[7] = $this->getGeographicLocation();

		return $pks;
	}

	/**
	 * Set the [composite] primary key.
	 *
	 * @param      array $keys The elements of the composite key (order must match the order in XML file).
	 * @return     void
	 */
	public function setPrimaryKey($keys)
	{

		$this->setCachedParentIdHierarchy($keys[0]);

		$this->setBillableArOrganizationUnitId($keys[1]);

		$this->setCalldate($keys[2]);

		$this->setDestinationType($keys[3]);

		$this->setArCommunicationChannelTypeId($keys[4]);

		$this->setOperatorType($keys[5]);

		$this->setArVendorId($keys[6]);

		$this->setGeographicLocation($keys[7]);

	}

	/**
	 * Sets contents of passed object to values from current object.
	 *
	 * If desired, this method can also make copies of all associated (fkey referrers)
	 * objects.
	 *
	 * @param      object $copyObj An object of ArCachedGroupedCdr (or compatible) type.
	 * @param      boolean $deepCopy Whether to also copy all rows that refer (by fkey) to the current row.
	 * @throws     PropelException
	 */
	public function copyInto($copyObj, $deepCopy = false)
	{

		$copyObj->setCachedParentIdHierarchy($this->cached_parent_id_hierarchy);

		$copyObj->setBillableArOrganizationUnitId($this->billable_ar_organization_unit_id);

		$copyObj->setCalldate($this->calldate);

		$copyObj->setDestinationType($this->destination_type);

		$copyObj->setArCommunicationChannelTypeId($this->ar_communication_channel_type_id);

		$copyObj->setOperatorType($this->operator_type);

		$copyObj->setArVendorId($this->ar_vendor_id);

		$copyObj->setGeographicLocation($this->geographic_location);

		$copyObj->setCountOfCalls($this->count_of_calls);

		$copyObj->setBillsec($this->billsec);

		$copyObj->setIncome($this->income);

		$copyObj->setCostSaving($this->cost_saving);

		$copyObj->setCost($this->cost);

		$copyObj->setId($this->id);


		$copyObj->setNew(true);

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
	 * @return     ArCachedGroupedCdr Clone of current object.
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
	 * @return     ArCachedGroupedCdrPeer
	 */
	public function getPeer()
	{
		if (self::$peer === null) {
			self::$peer = new ArCachedGroupedCdrPeer();
		}
		return self::$peer;
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

	}

} // BaseArCachedGroupedCdr
