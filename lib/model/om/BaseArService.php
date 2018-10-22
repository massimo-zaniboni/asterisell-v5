<?php

/**
 * Base class that represents a row from the 'ar_service' table.
 *
 * 
 *
 * @package    lib.model.om
 */
abstract class BaseArService extends BaseObject  implements Persistent {


	/**
	 * The Peer class.
	 * Instance provides a convenient way of calling static methods on a class
	 * that calling code may not be able to identify.
	 * @var        ArServicePeer
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
	 * The value for the customer_name field.
	 * @var        string
	 */
	protected $customer_name;

	/**
	 * The value for the customer_description field.
	 * @var        string
	 */
	protected $customer_description;

	/**
	 * The value for the vendor_name field.
	 * @var        string
	 */
	protected $vendor_name;

	/**
	 * The value for the vendor_description field.
	 * @var        string
	 */
	protected $vendor_description;

	/**
	 * The value for the external_crm_code field.
	 * @var        string
	 */
	protected $external_crm_code;

	/**
	 * The value for the customer_price_depend_from_activation_date field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $customer_price_depend_from_activation_date;

	/**
	 * The value for the customer_price_change_with_price_list field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $customer_price_change_with_price_list;

	/**
	 * The value for the is_enabled field.
	 * Note: this column has a database default value of: true
	 * @var        boolean
	 */
	protected $is_enabled;

	/**
	 * The value for the is_applied_only_one_time field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $is_applied_only_one_time;

	/**
	 * The value for the schedule_timeframe field.
	 * @var        string
	 */
	protected $schedule_timeframe;

	/**
	 * The value for the was_compiled field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $was_compiled;

	/**
	 * The value for the schedule_from field.
	 * @var        string
	 */
	protected $schedule_from;

	/**
	 * The value for the schedule_at field.
	 * Note: this column has a database default value of: '00:00:00'
	 * @var        string
	 */
	protected $schedule_at;

	/**
	 * @var        array ArServicePrice[] Collection to store aggregation of ArServicePrice objects.
	 */
	protected $collArServicePrices;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArServicePrices.
	 */
	private $lastArServicePriceCriteria = null;

	/**
	 * @var        array ArAssignedService[] Collection to store aggregation of ArAssignedService objects.
	 */
	protected $collArAssignedServices;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArAssignedServices.
	 */
	private $lastArAssignedServiceCriteria = null;

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
	
	const PEER = 'ArServicePeer';

	/**
	 * Applies default values to this object.
	 * This method should be called from the object's constructor (or
	 * equivalent initialization method).
	 * @see        __construct()
	 */
	public function applyDefaultValues()
	{
		$this->customer_price_depend_from_activation_date = false;
		$this->customer_price_change_with_price_list = false;
		$this->is_enabled = true;
		$this->is_applied_only_one_time = false;
		$this->was_compiled = false;
		$this->schedule_at = '00:00:00';
	}

	/**
	 * Initializes internal state of BaseArService object.
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
	 * Get the [customer_name] column value.
	 * 
	 * @return     string
	 */
	public function getCustomerName()
	{
		return $this->customer_name;
	}

	/**
	 * Get the [customer_description] column value.
	 * 
	 * @return     string
	 */
	public function getCustomerDescription()
	{
		return $this->customer_description;
	}

	/**
	 * Get the [vendor_name] column value.
	 * 
	 * @return     string
	 */
	public function getVendorName()
	{
		return $this->vendor_name;
	}

	/**
	 * Get the [vendor_description] column value.
	 * 
	 * @return     string
	 */
	public function getVendorDescription()
	{
		return $this->vendor_description;
	}

	/**
	 * Get the [external_crm_code] column value.
	 * 
	 * @return     string
	 */
	public function getExternalCrmCode()
	{
		return $this->external_crm_code;
	}

	/**
	 * Get the [customer_price_depend_from_activation_date] column value.
	 * 
	 * @return     boolean
	 */
	public function getCustomerPriceDependFromActivationDate()
	{
		return $this->customer_price_depend_from_activation_date;
	}

	/**
	 * Get the [customer_price_change_with_price_list] column value.
	 * 
	 * @return     boolean
	 */
	public function getCustomerPriceChangeWithPriceList()
	{
		return $this->customer_price_change_with_price_list;
	}

	/**
	 * Get the [is_enabled] column value.
	 * 
	 * @return     boolean
	 */
	public function getIsEnabled()
	{
		return $this->is_enabled;
	}

	/**
	 * Get the [is_applied_only_one_time] column value.
	 * 
	 * @return     boolean
	 */
	public function getIsAppliedOnlyOneTime()
	{
		return $this->is_applied_only_one_time;
	}

	/**
	 * Get the [schedule_timeframe] column value.
	 * 
	 * @return     string
	 */
	public function getScheduleTimeframe()
	{
		return $this->schedule_timeframe;
	}

	/**
	 * Get the [was_compiled] column value.
	 * 
	 * @return     boolean
	 */
	public function getWasCompiled()
	{
		return $this->was_compiled;
	}

	/**
	 * Get the [schedule_from] column value.
	 * 
	 * @return     string
	 */
	public function getScheduleFrom()
	{
		return $this->schedule_from;
	}

	/**
	 * Get the [optionally formatted] temporal [schedule_at] column value.
	 * 
	 *
	 * @param      string $format The date/time format string (either date()-style or strftime()-style).
	 *							If format is NULL, then the raw DateTime object will be returned.
	 * @return     mixed Formatted date/time value as string or DateTime object (if format is NULL), NULL if column is NULL
	 * @throws     PropelException - if unable to parse/validate the date/time value.
	 */
	public function getScheduleAt($format = 'H:i:s')
	{
		if ($this->schedule_at === null) {
			return null;
		}



		try {
			$dt = new DateTime($this->schedule_at);
		} catch (Exception $x) {
			throw new PropelException("Internally stored date/time/timestamp value could not be converted to DateTime: " . var_export($this->schedule_at, true), $x);
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
	 * @return     ArService The current object (for fluent API support)
	 */
	public function setId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->id !== $v) {
			$this->id = $v;
			$this->modifiedColumns[] = ArServicePeer::ID;
		}

		return $this;
	} // setId()

	/**
	 * Set the value of [internal_name] column.
	 * 
	 * @param      string $v new value
	 * @return     ArService The current object (for fluent API support)
	 */
	public function setInternalName($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->internal_name !== $v) {
			$this->internal_name = $v;
			$this->modifiedColumns[] = ArServicePeer::INTERNAL_NAME;
		}

		return $this;
	} // setInternalName()

	/**
	 * Set the value of [customer_name] column.
	 * 
	 * @param      string $v new value
	 * @return     ArService The current object (for fluent API support)
	 */
	public function setCustomerName($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->customer_name !== $v) {
			$this->customer_name = $v;
			$this->modifiedColumns[] = ArServicePeer::CUSTOMER_NAME;
		}

		return $this;
	} // setCustomerName()

	/**
	 * Set the value of [customer_description] column.
	 * 
	 * @param      string $v new value
	 * @return     ArService The current object (for fluent API support)
	 */
	public function setCustomerDescription($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->customer_description !== $v) {
			$this->customer_description = $v;
			$this->modifiedColumns[] = ArServicePeer::CUSTOMER_DESCRIPTION;
		}

		return $this;
	} // setCustomerDescription()

	/**
	 * Set the value of [vendor_name] column.
	 * 
	 * @param      string $v new value
	 * @return     ArService The current object (for fluent API support)
	 */
	public function setVendorName($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->vendor_name !== $v) {
			$this->vendor_name = $v;
			$this->modifiedColumns[] = ArServicePeer::VENDOR_NAME;
		}

		return $this;
	} // setVendorName()

	/**
	 * Set the value of [vendor_description] column.
	 * 
	 * @param      string $v new value
	 * @return     ArService The current object (for fluent API support)
	 */
	public function setVendorDescription($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->vendor_description !== $v) {
			$this->vendor_description = $v;
			$this->modifiedColumns[] = ArServicePeer::VENDOR_DESCRIPTION;
		}

		return $this;
	} // setVendorDescription()

	/**
	 * Set the value of [external_crm_code] column.
	 * 
	 * @param      string $v new value
	 * @return     ArService The current object (for fluent API support)
	 */
	public function setExternalCrmCode($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->external_crm_code !== $v) {
			$this->external_crm_code = $v;
			$this->modifiedColumns[] = ArServicePeer::EXTERNAL_CRM_CODE;
		}

		return $this;
	} // setExternalCrmCode()

	/**
	 * Set the value of [customer_price_depend_from_activation_date] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArService The current object (for fluent API support)
	 */
	public function setCustomerPriceDependFromActivationDate($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->customer_price_depend_from_activation_date !== $v || $this->isNew()) {
			$this->customer_price_depend_from_activation_date = $v;
			$this->modifiedColumns[] = ArServicePeer::CUSTOMER_PRICE_DEPEND_FROM_ACTIVATION_DATE;
		}

		return $this;
	} // setCustomerPriceDependFromActivationDate()

	/**
	 * Set the value of [customer_price_change_with_price_list] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArService The current object (for fluent API support)
	 */
	public function setCustomerPriceChangeWithPriceList($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->customer_price_change_with_price_list !== $v || $this->isNew()) {
			$this->customer_price_change_with_price_list = $v;
			$this->modifiedColumns[] = ArServicePeer::CUSTOMER_PRICE_CHANGE_WITH_PRICE_LIST;
		}

		return $this;
	} // setCustomerPriceChangeWithPriceList()

	/**
	 * Set the value of [is_enabled] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArService The current object (for fluent API support)
	 */
	public function setIsEnabled($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->is_enabled !== $v || $this->isNew()) {
			$this->is_enabled = $v;
			$this->modifiedColumns[] = ArServicePeer::IS_ENABLED;
		}

		return $this;
	} // setIsEnabled()

	/**
	 * Set the value of [is_applied_only_one_time] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArService The current object (for fluent API support)
	 */
	public function setIsAppliedOnlyOneTime($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->is_applied_only_one_time !== $v || $this->isNew()) {
			$this->is_applied_only_one_time = $v;
			$this->modifiedColumns[] = ArServicePeer::IS_APPLIED_ONLY_ONE_TIME;
		}

		return $this;
	} // setIsAppliedOnlyOneTime()

	/**
	 * Set the value of [schedule_timeframe] column.
	 * 
	 * @param      string $v new value
	 * @return     ArService The current object (for fluent API support)
	 */
	public function setScheduleTimeframe($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->schedule_timeframe !== $v) {
			$this->schedule_timeframe = $v;
			$this->modifiedColumns[] = ArServicePeer::SCHEDULE_TIMEFRAME;
		}

		return $this;
	} // setScheduleTimeframe()

	/**
	 * Set the value of [was_compiled] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArService The current object (for fluent API support)
	 */
	public function setWasCompiled($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->was_compiled !== $v || $this->isNew()) {
			$this->was_compiled = $v;
			$this->modifiedColumns[] = ArServicePeer::WAS_COMPILED;
		}

		return $this;
	} // setWasCompiled()

	/**
	 * Set the value of [schedule_from] column.
	 * 
	 * @param      string $v new value
	 * @return     ArService The current object (for fluent API support)
	 */
	public function setScheduleFrom($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->schedule_from !== $v) {
			$this->schedule_from = $v;
			$this->modifiedColumns[] = ArServicePeer::SCHEDULE_FROM;
		}

		return $this;
	} // setScheduleFrom()

	/**
	 * Sets the value of [schedule_at] column to a normalized version of the date/time value specified.
	 * 
	 * @param      mixed $v string, integer (timestamp), or DateTime value.  Empty string will
	 *						be treated as NULL for temporal objects.
	 * @return     ArService The current object (for fluent API support)
	 */
	public function setScheduleAt($v)
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

		if ( $this->schedule_at !== null || $dt !== null ) {
			// (nested ifs are a little easier to read in this case)

			$currNorm = ($this->schedule_at !== null && $tmpDt = new DateTime($this->schedule_at)) ? $tmpDt->format('H:i:s') : null;
			$newNorm = ($dt !== null) ? $dt->format('H:i:s') : null;

			if ( ($currNorm !== $newNorm) // normalized values don't match 
					|| ($dt->format('H:i:s') === '00:00:00') // or the entered value matches the default
					)
			{
				$this->schedule_at = ($dt ? $dt->format('H:i:s') : null);
				$this->modifiedColumns[] = ArServicePeer::SCHEDULE_AT;
			}
		} // if either are not null

		return $this;
	} // setScheduleAt()

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
			if ($this->customer_price_depend_from_activation_date !== false) {
				return false;
			}

			if ($this->customer_price_change_with_price_list !== false) {
				return false;
			}

			if ($this->is_enabled !== true) {
				return false;
			}

			if ($this->is_applied_only_one_time !== false) {
				return false;
			}

			if ($this->was_compiled !== false) {
				return false;
			}

			if ($this->schedule_at !== '00:00:00') {
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
			$this->customer_name = ($row[$startcol + 2] !== null) ? (string) $row[$startcol + 2] : null;
			$this->customer_description = ($row[$startcol + 3] !== null) ? (string) $row[$startcol + 3] : null;
			$this->vendor_name = ($row[$startcol + 4] !== null) ? (string) $row[$startcol + 4] : null;
			$this->vendor_description = ($row[$startcol + 5] !== null) ? (string) $row[$startcol + 5] : null;
			$this->external_crm_code = ($row[$startcol + 6] !== null) ? (string) $row[$startcol + 6] : null;
			$this->customer_price_depend_from_activation_date = ($row[$startcol + 7] !== null) ? (boolean) $row[$startcol + 7] : null;
			$this->customer_price_change_with_price_list = ($row[$startcol + 8] !== null) ? (boolean) $row[$startcol + 8] : null;
			$this->is_enabled = ($row[$startcol + 9] !== null) ? (boolean) $row[$startcol + 9] : null;
			$this->is_applied_only_one_time = ($row[$startcol + 10] !== null) ? (boolean) $row[$startcol + 10] : null;
			$this->schedule_timeframe = ($row[$startcol + 11] !== null) ? (string) $row[$startcol + 11] : null;
			$this->was_compiled = ($row[$startcol + 12] !== null) ? (boolean) $row[$startcol + 12] : null;
			$this->schedule_from = ($row[$startcol + 13] !== null) ? (string) $row[$startcol + 13] : null;
			$this->schedule_at = ($row[$startcol + 14] !== null) ? (string) $row[$startcol + 14] : null;
			$this->resetModified();

			$this->setNew(false);

			if ($rehydrate) {
				$this->ensureConsistency();
			}

			// FIXME - using NUM_COLUMNS may be clearer.
			return $startcol + 15; // 15 = ArServicePeer::NUM_COLUMNS - ArServicePeer::NUM_LAZY_LOAD_COLUMNS).

		} catch (Exception $e) {
			throw new PropelException("Error populating ArService object", $e);
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
			$con = Propel::getConnection(ArServicePeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		// We don't need to alter the object instance pool; we're just modifying this instance
		// already in the pool.

		$stmt = ArServicePeer::doSelectStmt($this->buildPkeyCriteria(), $con);
		$row = $stmt->fetch(PDO::FETCH_NUM);
		$stmt->closeCursor();
		if (!$row) {
			throw new PropelException('Cannot find matching row in the database to reload object values.');
		}
		$this->hydrate($row, 0, true); // rehydrate

		if ($deep) {  // also de-associate any related objects?

			$this->collArServicePrices = null;
			$this->lastArServicePriceCriteria = null;

			$this->collArAssignedServices = null;
			$this->lastArAssignedServiceCriteria = null;

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
			$con = Propel::getConnection(ArServicePeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}
		
		$con->beginTransaction();
		try {
			$ret = $this->preDelete($con);
			if ($ret) {
				ArServicePeer::doDelete($this, $con);
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
			$con = Propel::getConnection(ArServicePeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
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
				ArServicePeer::addInstanceToPool($this);
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

			if ($this->isNew() ) {
				$this->modifiedColumns[] = ArServicePeer::ID;
			}

			// If this object has been modified, then save it to the database.
			if ($this->isModified()) {
				if ($this->isNew()) {
					$pk = ArServicePeer::doInsert($this, $con);
					$affectedRows += 1; // we are assuming that there is only 1 row per doInsert() which
										 // should always be true here (even though technically
										 // BasePeer::doInsert() can insert multiple rows).

					$this->setId($pk);  //[IMV] update autoincrement primary key

					$this->setNew(false);
				} else {
					$affectedRows += ArServicePeer::doUpdate($this, $con);
				}

				$this->resetModified(); // [HL] After being saved an object is no longer 'modified'
			}

			if ($this->collArServicePrices !== null) {
				foreach ($this->collArServicePrices as $referrerFK) {
					if (!$referrerFK->isDeleted()) {
						$affectedRows += $referrerFK->save($con);
					}
				}
			}

			if ($this->collArAssignedServices !== null) {
				foreach ($this->collArAssignedServices as $referrerFK) {
					if (!$referrerFK->isDeleted()) {
						$affectedRows += $referrerFK->save($con);
					}
				}
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


			if (($retval = ArServicePeer::doValidate($this, $columns)) !== true) {
				$failureMap = array_merge($failureMap, $retval);
			}


				if ($this->collArServicePrices !== null) {
					foreach ($this->collArServicePrices as $referrerFK) {
						if (!$referrerFK->validate($columns)) {
							$failureMap = array_merge($failureMap, $referrerFK->getValidationFailures());
						}
					}
				}

				if ($this->collArAssignedServices !== null) {
					foreach ($this->collArAssignedServices as $referrerFK) {
						if (!$referrerFK->validate($columns)) {
							$failureMap = array_merge($failureMap, $referrerFK->getValidationFailures());
						}
					}
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
		$pos = ArServicePeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
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
				return $this->getCustomerName();
				break;
			case 3:
				return $this->getCustomerDescription();
				break;
			case 4:
				return $this->getVendorName();
				break;
			case 5:
				return $this->getVendorDescription();
				break;
			case 6:
				return $this->getExternalCrmCode();
				break;
			case 7:
				return $this->getCustomerPriceDependFromActivationDate();
				break;
			case 8:
				return $this->getCustomerPriceChangeWithPriceList();
				break;
			case 9:
				return $this->getIsEnabled();
				break;
			case 10:
				return $this->getIsAppliedOnlyOneTime();
				break;
			case 11:
				return $this->getScheduleTimeframe();
				break;
			case 12:
				return $this->getWasCompiled();
				break;
			case 13:
				return $this->getScheduleFrom();
				break;
			case 14:
				return $this->getScheduleAt();
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
		$keys = ArServicePeer::getFieldNames($keyType);
		$result = array(
			$keys[0] => $this->getId(),
			$keys[1] => $this->getInternalName(),
			$keys[2] => $this->getCustomerName(),
			$keys[3] => $this->getCustomerDescription(),
			$keys[4] => $this->getVendorName(),
			$keys[5] => $this->getVendorDescription(),
			$keys[6] => $this->getExternalCrmCode(),
			$keys[7] => $this->getCustomerPriceDependFromActivationDate(),
			$keys[8] => $this->getCustomerPriceChangeWithPriceList(),
			$keys[9] => $this->getIsEnabled(),
			$keys[10] => $this->getIsAppliedOnlyOneTime(),
			$keys[11] => $this->getScheduleTimeframe(),
			$keys[12] => $this->getWasCompiled(),
			$keys[13] => $this->getScheduleFrom(),
			$keys[14] => $this->getScheduleAt(),
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
		$pos = ArServicePeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
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
				$this->setCustomerName($value);
				break;
			case 3:
				$this->setCustomerDescription($value);
				break;
			case 4:
				$this->setVendorName($value);
				break;
			case 5:
				$this->setVendorDescription($value);
				break;
			case 6:
				$this->setExternalCrmCode($value);
				break;
			case 7:
				$this->setCustomerPriceDependFromActivationDate($value);
				break;
			case 8:
				$this->setCustomerPriceChangeWithPriceList($value);
				break;
			case 9:
				$this->setIsEnabled($value);
				break;
			case 10:
				$this->setIsAppliedOnlyOneTime($value);
				break;
			case 11:
				$this->setScheduleTimeframe($value);
				break;
			case 12:
				$this->setWasCompiled($value);
				break;
			case 13:
				$this->setScheduleFrom($value);
				break;
			case 14:
				$this->setScheduleAt($value);
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
		$keys = ArServicePeer::getFieldNames($keyType);

		if (array_key_exists($keys[0], $arr)) $this->setId($arr[$keys[0]]);
		if (array_key_exists($keys[1], $arr)) $this->setInternalName($arr[$keys[1]]);
		if (array_key_exists($keys[2], $arr)) $this->setCustomerName($arr[$keys[2]]);
		if (array_key_exists($keys[3], $arr)) $this->setCustomerDescription($arr[$keys[3]]);
		if (array_key_exists($keys[4], $arr)) $this->setVendorName($arr[$keys[4]]);
		if (array_key_exists($keys[5], $arr)) $this->setVendorDescription($arr[$keys[5]]);
		if (array_key_exists($keys[6], $arr)) $this->setExternalCrmCode($arr[$keys[6]]);
		if (array_key_exists($keys[7], $arr)) $this->setCustomerPriceDependFromActivationDate($arr[$keys[7]]);
		if (array_key_exists($keys[8], $arr)) $this->setCustomerPriceChangeWithPriceList($arr[$keys[8]]);
		if (array_key_exists($keys[9], $arr)) $this->setIsEnabled($arr[$keys[9]]);
		if (array_key_exists($keys[10], $arr)) $this->setIsAppliedOnlyOneTime($arr[$keys[10]]);
		if (array_key_exists($keys[11], $arr)) $this->setScheduleTimeframe($arr[$keys[11]]);
		if (array_key_exists($keys[12], $arr)) $this->setWasCompiled($arr[$keys[12]]);
		if (array_key_exists($keys[13], $arr)) $this->setScheduleFrom($arr[$keys[13]]);
		if (array_key_exists($keys[14], $arr)) $this->setScheduleAt($arr[$keys[14]]);
	}

	/**
	 * Build a Criteria object containing the values of all modified columns in this object.
	 *
	 * @return     Criteria The Criteria object containing all modified values.
	 */
	public function buildCriteria()
	{
		$criteria = new Criteria(ArServicePeer::DATABASE_NAME);

		if ($this->isColumnModified(ArServicePeer::ID)) $criteria->add(ArServicePeer::ID, $this->id);
		if ($this->isColumnModified(ArServicePeer::INTERNAL_NAME)) $criteria->add(ArServicePeer::INTERNAL_NAME, $this->internal_name);
		if ($this->isColumnModified(ArServicePeer::CUSTOMER_NAME)) $criteria->add(ArServicePeer::CUSTOMER_NAME, $this->customer_name);
		if ($this->isColumnModified(ArServicePeer::CUSTOMER_DESCRIPTION)) $criteria->add(ArServicePeer::CUSTOMER_DESCRIPTION, $this->customer_description);
		if ($this->isColumnModified(ArServicePeer::VENDOR_NAME)) $criteria->add(ArServicePeer::VENDOR_NAME, $this->vendor_name);
		if ($this->isColumnModified(ArServicePeer::VENDOR_DESCRIPTION)) $criteria->add(ArServicePeer::VENDOR_DESCRIPTION, $this->vendor_description);
		if ($this->isColumnModified(ArServicePeer::EXTERNAL_CRM_CODE)) $criteria->add(ArServicePeer::EXTERNAL_CRM_CODE, $this->external_crm_code);
		if ($this->isColumnModified(ArServicePeer::CUSTOMER_PRICE_DEPEND_FROM_ACTIVATION_DATE)) $criteria->add(ArServicePeer::CUSTOMER_PRICE_DEPEND_FROM_ACTIVATION_DATE, $this->customer_price_depend_from_activation_date);
		if ($this->isColumnModified(ArServicePeer::CUSTOMER_PRICE_CHANGE_WITH_PRICE_LIST)) $criteria->add(ArServicePeer::CUSTOMER_PRICE_CHANGE_WITH_PRICE_LIST, $this->customer_price_change_with_price_list);
		if ($this->isColumnModified(ArServicePeer::IS_ENABLED)) $criteria->add(ArServicePeer::IS_ENABLED, $this->is_enabled);
		if ($this->isColumnModified(ArServicePeer::IS_APPLIED_ONLY_ONE_TIME)) $criteria->add(ArServicePeer::IS_APPLIED_ONLY_ONE_TIME, $this->is_applied_only_one_time);
		if ($this->isColumnModified(ArServicePeer::SCHEDULE_TIMEFRAME)) $criteria->add(ArServicePeer::SCHEDULE_TIMEFRAME, $this->schedule_timeframe);
		if ($this->isColumnModified(ArServicePeer::WAS_COMPILED)) $criteria->add(ArServicePeer::WAS_COMPILED, $this->was_compiled);
		if ($this->isColumnModified(ArServicePeer::SCHEDULE_FROM)) $criteria->add(ArServicePeer::SCHEDULE_FROM, $this->schedule_from);
		if ($this->isColumnModified(ArServicePeer::SCHEDULE_AT)) $criteria->add(ArServicePeer::SCHEDULE_AT, $this->schedule_at);

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
		$criteria = new Criteria(ArServicePeer::DATABASE_NAME);

		$criteria->add(ArServicePeer::ID, $this->id);

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
	 * @param      object $copyObj An object of ArService (or compatible) type.
	 * @param      boolean $deepCopy Whether to also copy all rows that refer (by fkey) to the current row.
	 * @throws     PropelException
	 */
	public function copyInto($copyObj, $deepCopy = false)
	{

		$copyObj->setInternalName($this->internal_name);

		$copyObj->setCustomerName($this->customer_name);

		$copyObj->setCustomerDescription($this->customer_description);

		$copyObj->setVendorName($this->vendor_name);

		$copyObj->setVendorDescription($this->vendor_description);

		$copyObj->setExternalCrmCode($this->external_crm_code);

		$copyObj->setCustomerPriceDependFromActivationDate($this->customer_price_depend_from_activation_date);

		$copyObj->setCustomerPriceChangeWithPriceList($this->customer_price_change_with_price_list);

		$copyObj->setIsEnabled($this->is_enabled);

		$copyObj->setIsAppliedOnlyOneTime($this->is_applied_only_one_time);

		$copyObj->setScheduleTimeframe($this->schedule_timeframe);

		$copyObj->setWasCompiled($this->was_compiled);

		$copyObj->setScheduleFrom($this->schedule_from);

		$copyObj->setScheduleAt($this->schedule_at);


		if ($deepCopy) {
			// important: temporarily setNew(false) because this affects the behavior of
			// the getter/setter methods for fkey referrer objects.
			$copyObj->setNew(false);

			foreach ($this->getArServicePrices() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArServicePrice($relObj->copy($deepCopy));
				}
			}

			foreach ($this->getArAssignedServices() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArAssignedService($relObj->copy($deepCopy));
				}
			}

		} // if ($deepCopy)


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
	 * @return     ArService Clone of current object.
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
	 * @return     ArServicePeer
	 */
	public function getPeer()
	{
		if (self::$peer === null) {
			self::$peer = new ArServicePeer();
		}
		return self::$peer;
	}

	/**
	 * Clears out the collArServicePrices collection (array).
	 *
	 * This does not modify the database; however, it will remove any associated objects, causing
	 * them to be refetched by subsequent calls to accessor method.
	 *
	 * @return     void
	 * @see        addArServicePrices()
	 */
	public function clearArServicePrices()
	{
		$this->collArServicePrices = null; // important to set this to NULL since that means it is uninitialized
	}

	/**
	 * Initializes the collArServicePrices collection (array).
	 *
	 * By default this just sets the collArServicePrices collection to an empty array (like clearcollArServicePrices());
	 * however, you may wish to override this method in your stub class to provide setting appropriate
	 * to your application -- for example, setting the initial array to the values stored in database.
	 *
	 * @return     void
	 */
	public function initArServicePrices()
	{
		$this->collArServicePrices = array();
	}

	/**
	 * Gets an array of ArServicePrice objects which contain a foreign key that references this object.
	 *
	 * If this collection has already been initialized with an identical Criteria, it returns the collection.
	 * Otherwise if this ArService has previously been saved, it will retrieve
	 * related ArServicePrices from storage. If this ArService is new, it will return
	 * an empty collection or the current collection, the criteria is ignored on a new object.
	 *
	 * @param      PropelPDO $con
	 * @param      Criteria $criteria
	 * @return     array ArServicePrice[]
	 * @throws     PropelException
	 */
	public function getArServicePrices($criteria = null, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArServicePeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArServicePrices === null) {
			if ($this->isNew()) {
			   $this->collArServicePrices = array();
			} else {

				$criteria->add(ArServicePricePeer::AR_SERVICE_ID, $this->id);

				ArServicePricePeer::addSelectColumns($criteria);
				$this->collArServicePrices = ArServicePricePeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArServicePricePeer::AR_SERVICE_ID, $this->id);

				ArServicePricePeer::addSelectColumns($criteria);
				if (!isset($this->lastArServicePriceCriteria) || !$this->lastArServicePriceCriteria->equals($criteria)) {
					$this->collArServicePrices = ArServicePricePeer::doSelect($criteria, $con);
				}
			}
		}
		$this->lastArServicePriceCriteria = $criteria;
		return $this->collArServicePrices;
	}

	/**
	 * Returns the number of related ArServicePrice objects.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct
	 * @param      PropelPDO $con
	 * @return     int Count of related ArServicePrice objects.
	 * @throws     PropelException
	 */
	public function countArServicePrices(Criteria $criteria = null, $distinct = false, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArServicePeer::DATABASE_NAME);
		} else {
			$criteria = clone $criteria;
		}

		if ($distinct) {
			$criteria->setDistinct();
		}

		$count = null;

		if ($this->collArServicePrices === null) {
			if ($this->isNew()) {
				$count = 0;
			} else {

				$criteria->add(ArServicePricePeer::AR_SERVICE_ID, $this->id);

				$count = ArServicePricePeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArServicePricePeer::AR_SERVICE_ID, $this->id);

				if (!isset($this->lastArServicePriceCriteria) || !$this->lastArServicePriceCriteria->equals($criteria)) {
					$count = ArServicePricePeer::doCount($criteria, false, $con);
				} else {
					$count = count($this->collArServicePrices);
				}
			} else {
				$count = count($this->collArServicePrices);
			}
		}
		return $count;
	}

	/**
	 * Method called to associate a ArServicePrice object to this object
	 * through the ArServicePrice foreign key attribute.
	 *
	 * @param      ArServicePrice $l ArServicePrice
	 * @return     void
	 * @throws     PropelException
	 */
	public function addArServicePrice(ArServicePrice $l)
	{
		if ($this->collArServicePrices === null) {
			$this->initArServicePrices();
		}
		if (!in_array($l, $this->collArServicePrices, true)) { // only add it if the **same** object is not already associated
			array_push($this->collArServicePrices, $l);
			$l->setArService($this);
		}
	}

	/**
	 * Clears out the collArAssignedServices collection (array).
	 *
	 * This does not modify the database; however, it will remove any associated objects, causing
	 * them to be refetched by subsequent calls to accessor method.
	 *
	 * @return     void
	 * @see        addArAssignedServices()
	 */
	public function clearArAssignedServices()
	{
		$this->collArAssignedServices = null; // important to set this to NULL since that means it is uninitialized
	}

	/**
	 * Initializes the collArAssignedServices collection (array).
	 *
	 * By default this just sets the collArAssignedServices collection to an empty array (like clearcollArAssignedServices());
	 * however, you may wish to override this method in your stub class to provide setting appropriate
	 * to your application -- for example, setting the initial array to the values stored in database.
	 *
	 * @return     void
	 */
	public function initArAssignedServices()
	{
		$this->collArAssignedServices = array();
	}

	/**
	 * Gets an array of ArAssignedService objects which contain a foreign key that references this object.
	 *
	 * If this collection has already been initialized with an identical Criteria, it returns the collection.
	 * Otherwise if this ArService has previously been saved, it will retrieve
	 * related ArAssignedServices from storage. If this ArService is new, it will return
	 * an empty collection or the current collection, the criteria is ignored on a new object.
	 *
	 * @param      PropelPDO $con
	 * @param      Criteria $criteria
	 * @return     array ArAssignedService[]
	 * @throws     PropelException
	 */
	public function getArAssignedServices($criteria = null, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArServicePeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArAssignedServices === null) {
			if ($this->isNew()) {
			   $this->collArAssignedServices = array();
			} else {

				$criteria->add(ArAssignedServicePeer::AR_SERVICE_ID, $this->id);

				ArAssignedServicePeer::addSelectColumns($criteria);
				$this->collArAssignedServices = ArAssignedServicePeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArAssignedServicePeer::AR_SERVICE_ID, $this->id);

				ArAssignedServicePeer::addSelectColumns($criteria);
				if (!isset($this->lastArAssignedServiceCriteria) || !$this->lastArAssignedServiceCriteria->equals($criteria)) {
					$this->collArAssignedServices = ArAssignedServicePeer::doSelect($criteria, $con);
				}
			}
		}
		$this->lastArAssignedServiceCriteria = $criteria;
		return $this->collArAssignedServices;
	}

	/**
	 * Returns the number of related ArAssignedService objects.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct
	 * @param      PropelPDO $con
	 * @return     int Count of related ArAssignedService objects.
	 * @throws     PropelException
	 */
	public function countArAssignedServices(Criteria $criteria = null, $distinct = false, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArServicePeer::DATABASE_NAME);
		} else {
			$criteria = clone $criteria;
		}

		if ($distinct) {
			$criteria->setDistinct();
		}

		$count = null;

		if ($this->collArAssignedServices === null) {
			if ($this->isNew()) {
				$count = 0;
			} else {

				$criteria->add(ArAssignedServicePeer::AR_SERVICE_ID, $this->id);

				$count = ArAssignedServicePeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArAssignedServicePeer::AR_SERVICE_ID, $this->id);

				if (!isset($this->lastArAssignedServiceCriteria) || !$this->lastArAssignedServiceCriteria->equals($criteria)) {
					$count = ArAssignedServicePeer::doCount($criteria, false, $con);
				} else {
					$count = count($this->collArAssignedServices);
				}
			} else {
				$count = count($this->collArAssignedServices);
			}
		}
		return $count;
	}

	/**
	 * Method called to associate a ArAssignedService object to this object
	 * through the ArAssignedService foreign key attribute.
	 *
	 * @param      ArAssignedService $l ArAssignedService
	 * @return     void
	 * @throws     PropelException
	 */
	public function addArAssignedService(ArAssignedService $l)
	{
		if ($this->collArAssignedServices === null) {
			$this->initArAssignedServices();
		}
		if (!in_array($l, $this->collArAssignedServices, true)) { // only add it if the **same** object is not already associated
			array_push($this->collArAssignedServices, $l);
			$l->setArService($this);
		}
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArService is new, it will return
	 * an empty collection; or if this ArService has previously
	 * been saved, it will retrieve related ArAssignedServices from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArService.
	 */
	public function getArAssignedServicesJoinArOrganizationUnit($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArServicePeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArAssignedServices === null) {
			if ($this->isNew()) {
				$this->collArAssignedServices = array();
			} else {

				$criteria->add(ArAssignedServicePeer::AR_SERVICE_ID, $this->id);

				$this->collArAssignedServices = ArAssignedServicePeer::doSelectJoinArOrganizationUnit($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArAssignedServicePeer::AR_SERVICE_ID, $this->id);

			if (!isset($this->lastArAssignedServiceCriteria) || !$this->lastArAssignedServiceCriteria->equals($criteria)) {
				$this->collArAssignedServices = ArAssignedServicePeer::doSelectJoinArOrganizationUnit($criteria, $con, $join_behavior);
			}
		}
		$this->lastArAssignedServiceCriteria = $criteria;

		return $this->collArAssignedServices;
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
			if ($this->collArServicePrices) {
				foreach ((array) $this->collArServicePrices as $o) {
					$o->clearAllReferences($deep);
				}
			}
			if ($this->collArAssignedServices) {
				foreach ((array) $this->collArAssignedServices as $o) {
					$o->clearAllReferences($deep);
				}
			}
		} // if ($deep)

		$this->collArServicePrices = null;
		$this->collArAssignedServices = null;
	}

} // BaseArService
