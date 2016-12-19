<?php

/**
 * Base class that represents a row from the 'ar_asterisk_account_range' table.
 *
 * 
 *
 * @package    lib.model.om
 */
abstract class BaseArAsteriskAccountRange extends BaseObject  implements Persistent {


	/**
	 * The Peer class.
	 * Instance provides a convenient way of calling static methods on a class
	 * that calling code may not be able to identify.
	 * @var        ArAsteriskAccountRangePeer
	 */
	protected static $peer;

	/**
	 * The value for the id field.
	 * @var        int
	 */
	protected $id;

	/**
	 * The value for the ar_organization_unit_id field.
	 * @var        int
	 */
	protected $ar_organization_unit_id;

	/**
	 * The value for the system_prefix field.
	 * @var        string
	 */
	protected $system_prefix;

	/**
	 * The value for the system_suffix field.
	 * @var        string
	 */
	protected $system_suffix;

	/**
	 * The value for the system_start_range field.
	 * @var        string
	 */
	protected $system_start_range;

	/**
	 * The value for the system_end_range field.
	 * @var        string
	 */
	protected $system_end_range;

	/**
	 * The value for the system_leading_zero field.
	 * @var        int
	 */
	protected $system_leading_zero;

	/**
	 * The value for the is_delete field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $is_delete;

	/**
	 * The value for the is_physical_delete field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $is_physical_delete;

	/**
	 * The value for the user_prefix field.
	 * @var        string
	 */
	protected $user_prefix;

	/**
	 * The value for the user_suffix field.
	 * @var        string
	 */
	protected $user_suffix;

	/**
	 * The value for the user_start_range field.
	 * @var        string
	 */
	protected $user_start_range;

	/**
	 * The value for the generate_range_for_users field.
	 * Note: this column has a database default value of: true
	 * @var        boolean
	 */
	protected $generate_range_for_users;

	/**
	 * The value for the user_leading_zero field.
	 * @var        int
	 */
	protected $user_leading_zero;

	/**
	 * The value for the user_note field.
	 * @var        string
	 */
	protected $user_note;

	/**
	 * @var        ArOrganizationUnit
	 */
	protected $aArOrganizationUnit;

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
	
	const PEER = 'ArAsteriskAccountRangePeer';

	/**
	 * Applies default values to this object.
	 * This method should be called from the object's constructor (or
	 * equivalent initialization method).
	 * @see        __construct()
	 */
	public function applyDefaultValues()
	{
		$this->is_delete = false;
		$this->is_physical_delete = false;
		$this->generate_range_for_users = true;
	}

	/**
	 * Initializes internal state of BaseArAsteriskAccountRange object.
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
	 * Get the [ar_organization_unit_id] column value.
	 * 
	 * @return     int
	 */
	public function getArOrganizationUnitId()
	{
		return $this->ar_organization_unit_id;
	}

	/**
	 * Get the [system_prefix] column value.
	 * 
	 * @return     string
	 */
	public function getSystemPrefix()
	{
		return $this->system_prefix;
	}

	/**
	 * Get the [system_suffix] column value.
	 * 
	 * @return     string
	 */
	public function getSystemSuffix()
	{
		return $this->system_suffix;
	}

	/**
	 * Get the [system_start_range] column value.
	 * 
	 * @return     string
	 */
	public function getSystemStartRange()
	{
		return $this->system_start_range;
	}

	/**
	 * Get the [system_end_range] column value.
	 * 
	 * @return     string
	 */
	public function getSystemEndRange()
	{
		return $this->system_end_range;
	}

	/**
	 * Get the [system_leading_zero] column value.
	 * 
	 * @return     int
	 */
	public function getSystemLeadingZero()
	{
		return $this->system_leading_zero;
	}

	/**
	 * Get the [is_delete] column value.
	 * 
	 * @return     boolean
	 */
	public function getIsDelete()
	{
		return $this->is_delete;
	}

	/**
	 * Get the [is_physical_delete] column value.
	 * 
	 * @return     boolean
	 */
	public function getIsPhysicalDelete()
	{
		return $this->is_physical_delete;
	}

	/**
	 * Get the [user_prefix] column value.
	 * 
	 * @return     string
	 */
	public function getUserPrefix()
	{
		return $this->user_prefix;
	}

	/**
	 * Get the [user_suffix] column value.
	 * 
	 * @return     string
	 */
	public function getUserSuffix()
	{
		return $this->user_suffix;
	}

	/**
	 * Get the [user_start_range] column value.
	 * 
	 * @return     string
	 */
	public function getUserStartRange()
	{
		return $this->user_start_range;
	}

	/**
	 * Get the [generate_range_for_users] column value.
	 * 
	 * @return     boolean
	 */
	public function getGenerateRangeForUsers()
	{
		return $this->generate_range_for_users;
	}

	/**
	 * Get the [user_leading_zero] column value.
	 * 
	 * @return     int
	 */
	public function getUserLeadingZero()
	{
		return $this->user_leading_zero;
	}

	/**
	 * Get the [user_note] column value.
	 * 
	 * @return     string
	 */
	public function getUserNote()
	{
		return $this->user_note;
	}

	/**
	 * Set the value of [id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArAsteriskAccountRange The current object (for fluent API support)
	 */
	public function setId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->id !== $v) {
			$this->id = $v;
			$this->modifiedColumns[] = ArAsteriskAccountRangePeer::ID;
		}

		return $this;
	} // setId()

	/**
	 * Set the value of [ar_organization_unit_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArAsteriskAccountRange The current object (for fluent API support)
	 */
	public function setArOrganizationUnitId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->ar_organization_unit_id !== $v) {
			$this->ar_organization_unit_id = $v;
			$this->modifiedColumns[] = ArAsteriskAccountRangePeer::AR_ORGANIZATION_UNIT_ID;
		}

		if ($this->aArOrganizationUnit !== null && $this->aArOrganizationUnit->getId() !== $v) {
			$this->aArOrganizationUnit = null;
		}

		return $this;
	} // setArOrganizationUnitId()

	/**
	 * Set the value of [system_prefix] column.
	 * 
	 * @param      string $v new value
	 * @return     ArAsteriskAccountRange The current object (for fluent API support)
	 */
	public function setSystemPrefix($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->system_prefix !== $v) {
			$this->system_prefix = $v;
			$this->modifiedColumns[] = ArAsteriskAccountRangePeer::SYSTEM_PREFIX;
		}

		return $this;
	} // setSystemPrefix()

	/**
	 * Set the value of [system_suffix] column.
	 * 
	 * @param      string $v new value
	 * @return     ArAsteriskAccountRange The current object (for fluent API support)
	 */
	public function setSystemSuffix($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->system_suffix !== $v) {
			$this->system_suffix = $v;
			$this->modifiedColumns[] = ArAsteriskAccountRangePeer::SYSTEM_SUFFIX;
		}

		return $this;
	} // setSystemSuffix()

	/**
	 * Set the value of [system_start_range] column.
	 * 
	 * @param      string $v new value
	 * @return     ArAsteriskAccountRange The current object (for fluent API support)
	 */
	public function setSystemStartRange($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->system_start_range !== $v) {
			$this->system_start_range = $v;
			$this->modifiedColumns[] = ArAsteriskAccountRangePeer::SYSTEM_START_RANGE;
		}

		return $this;
	} // setSystemStartRange()

	/**
	 * Set the value of [system_end_range] column.
	 * 
	 * @param      string $v new value
	 * @return     ArAsteriskAccountRange The current object (for fluent API support)
	 */
	public function setSystemEndRange($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->system_end_range !== $v) {
			$this->system_end_range = $v;
			$this->modifiedColumns[] = ArAsteriskAccountRangePeer::SYSTEM_END_RANGE;
		}

		return $this;
	} // setSystemEndRange()

	/**
	 * Set the value of [system_leading_zero] column.
	 * 
	 * @param      int $v new value
	 * @return     ArAsteriskAccountRange The current object (for fluent API support)
	 */
	public function setSystemLeadingZero($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->system_leading_zero !== $v) {
			$this->system_leading_zero = $v;
			$this->modifiedColumns[] = ArAsteriskAccountRangePeer::SYSTEM_LEADING_ZERO;
		}

		return $this;
	} // setSystemLeadingZero()

	/**
	 * Set the value of [is_delete] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArAsteriskAccountRange The current object (for fluent API support)
	 */
	public function setIsDelete($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->is_delete !== $v || $this->isNew()) {
			$this->is_delete = $v;
			$this->modifiedColumns[] = ArAsteriskAccountRangePeer::IS_DELETE;
		}

		return $this;
	} // setIsDelete()

	/**
	 * Set the value of [is_physical_delete] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArAsteriskAccountRange The current object (for fluent API support)
	 */
	public function setIsPhysicalDelete($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->is_physical_delete !== $v || $this->isNew()) {
			$this->is_physical_delete = $v;
			$this->modifiedColumns[] = ArAsteriskAccountRangePeer::IS_PHYSICAL_DELETE;
		}

		return $this;
	} // setIsPhysicalDelete()

	/**
	 * Set the value of [user_prefix] column.
	 * 
	 * @param      string $v new value
	 * @return     ArAsteriskAccountRange The current object (for fluent API support)
	 */
	public function setUserPrefix($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->user_prefix !== $v) {
			$this->user_prefix = $v;
			$this->modifiedColumns[] = ArAsteriskAccountRangePeer::USER_PREFIX;
		}

		return $this;
	} // setUserPrefix()

	/**
	 * Set the value of [user_suffix] column.
	 * 
	 * @param      string $v new value
	 * @return     ArAsteriskAccountRange The current object (for fluent API support)
	 */
	public function setUserSuffix($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->user_suffix !== $v) {
			$this->user_suffix = $v;
			$this->modifiedColumns[] = ArAsteriskAccountRangePeer::USER_SUFFIX;
		}

		return $this;
	} // setUserSuffix()

	/**
	 * Set the value of [user_start_range] column.
	 * 
	 * @param      string $v new value
	 * @return     ArAsteriskAccountRange The current object (for fluent API support)
	 */
	public function setUserStartRange($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->user_start_range !== $v) {
			$this->user_start_range = $v;
			$this->modifiedColumns[] = ArAsteriskAccountRangePeer::USER_START_RANGE;
		}

		return $this;
	} // setUserStartRange()

	/**
	 * Set the value of [generate_range_for_users] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArAsteriskAccountRange The current object (for fluent API support)
	 */
	public function setGenerateRangeForUsers($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->generate_range_for_users !== $v || $this->isNew()) {
			$this->generate_range_for_users = $v;
			$this->modifiedColumns[] = ArAsteriskAccountRangePeer::GENERATE_RANGE_FOR_USERS;
		}

		return $this;
	} // setGenerateRangeForUsers()

	/**
	 * Set the value of [user_leading_zero] column.
	 * 
	 * @param      int $v new value
	 * @return     ArAsteriskAccountRange The current object (for fluent API support)
	 */
	public function setUserLeadingZero($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->user_leading_zero !== $v) {
			$this->user_leading_zero = $v;
			$this->modifiedColumns[] = ArAsteriskAccountRangePeer::USER_LEADING_ZERO;
		}

		return $this;
	} // setUserLeadingZero()

	/**
	 * Set the value of [user_note] column.
	 * 
	 * @param      string $v new value
	 * @return     ArAsteriskAccountRange The current object (for fluent API support)
	 */
	public function setUserNote($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->user_note !== $v) {
			$this->user_note = $v;
			$this->modifiedColumns[] = ArAsteriskAccountRangePeer::USER_NOTE;
		}

		return $this;
	} // setUserNote()

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
			if ($this->is_delete !== false) {
				return false;
			}

			if ($this->is_physical_delete !== false) {
				return false;
			}

			if ($this->generate_range_for_users !== true) {
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
			$this->ar_organization_unit_id = ($row[$startcol + 1] !== null) ? (int) $row[$startcol + 1] : null;
			$this->system_prefix = ($row[$startcol + 2] !== null) ? (string) $row[$startcol + 2] : null;
			$this->system_suffix = ($row[$startcol + 3] !== null) ? (string) $row[$startcol + 3] : null;
			$this->system_start_range = ($row[$startcol + 4] !== null) ? (string) $row[$startcol + 4] : null;
			$this->system_end_range = ($row[$startcol + 5] !== null) ? (string) $row[$startcol + 5] : null;
			$this->system_leading_zero = ($row[$startcol + 6] !== null) ? (int) $row[$startcol + 6] : null;
			$this->is_delete = ($row[$startcol + 7] !== null) ? (boolean) $row[$startcol + 7] : null;
			$this->is_physical_delete = ($row[$startcol + 8] !== null) ? (boolean) $row[$startcol + 8] : null;
			$this->user_prefix = ($row[$startcol + 9] !== null) ? (string) $row[$startcol + 9] : null;
			$this->user_suffix = ($row[$startcol + 10] !== null) ? (string) $row[$startcol + 10] : null;
			$this->user_start_range = ($row[$startcol + 11] !== null) ? (string) $row[$startcol + 11] : null;
			$this->generate_range_for_users = ($row[$startcol + 12] !== null) ? (boolean) $row[$startcol + 12] : null;
			$this->user_leading_zero = ($row[$startcol + 13] !== null) ? (int) $row[$startcol + 13] : null;
			$this->user_note = ($row[$startcol + 14] !== null) ? (string) $row[$startcol + 14] : null;
			$this->resetModified();

			$this->setNew(false);

			if ($rehydrate) {
				$this->ensureConsistency();
			}

			// FIXME - using NUM_COLUMNS may be clearer.
			return $startcol + 15; // 15 = ArAsteriskAccountRangePeer::NUM_COLUMNS - ArAsteriskAccountRangePeer::NUM_LAZY_LOAD_COLUMNS).

		} catch (Exception $e) {
			throw new PropelException("Error populating ArAsteriskAccountRange object", $e);
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

		if ($this->aArOrganizationUnit !== null && $this->ar_organization_unit_id !== $this->aArOrganizationUnit->getId()) {
			$this->aArOrganizationUnit = null;
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
			$con = Propel::getConnection(ArAsteriskAccountRangePeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		// We don't need to alter the object instance pool; we're just modifying this instance
		// already in the pool.

		$stmt = ArAsteriskAccountRangePeer::doSelectStmt($this->buildPkeyCriteria(), $con);
		$row = $stmt->fetch(PDO::FETCH_NUM);
		$stmt->closeCursor();
		if (!$row) {
			throw new PropelException('Cannot find matching row in the database to reload object values.');
		}
		$this->hydrate($row, 0, true); // rehydrate

		if ($deep) {  // also de-associate any related objects?

			$this->aArOrganizationUnit = null;
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
			$con = Propel::getConnection(ArAsteriskAccountRangePeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}
		
		$con->beginTransaction();
		try {
			$ret = $this->preDelete($con);
			if ($ret) {
				ArAsteriskAccountRangePeer::doDelete($this, $con);
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
			$con = Propel::getConnection(ArAsteriskAccountRangePeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
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
				ArAsteriskAccountRangePeer::addInstanceToPool($this);
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

			if ($this->aArOrganizationUnit !== null) {
				if ($this->aArOrganizationUnit->isModified() || $this->aArOrganizationUnit->isNew()) {
					$affectedRows += $this->aArOrganizationUnit->save($con);
				}
				$this->setArOrganizationUnit($this->aArOrganizationUnit);
			}

			if ($this->isNew() ) {
				$this->modifiedColumns[] = ArAsteriskAccountRangePeer::ID;
			}

			// If this object has been modified, then save it to the database.
			if ($this->isModified()) {
				if ($this->isNew()) {
					$pk = ArAsteriskAccountRangePeer::doInsert($this, $con);
					$affectedRows += 1; // we are assuming that there is only 1 row per doInsert() which
										 // should always be true here (even though technically
										 // BasePeer::doInsert() can insert multiple rows).

					$this->setId($pk);  //[IMV] update autoincrement primary key

					$this->setNew(false);
				} else {
					$affectedRows += ArAsteriskAccountRangePeer::doUpdate($this, $con);
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

			if ($this->aArOrganizationUnit !== null) {
				if (!$this->aArOrganizationUnit->validate($columns)) {
					$failureMap = array_merge($failureMap, $this->aArOrganizationUnit->getValidationFailures());
				}
			}


			if (($retval = ArAsteriskAccountRangePeer::doValidate($this, $columns)) !== true) {
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
		$pos = ArAsteriskAccountRangePeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
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
				return $this->getArOrganizationUnitId();
				break;
			case 2:
				return $this->getSystemPrefix();
				break;
			case 3:
				return $this->getSystemSuffix();
				break;
			case 4:
				return $this->getSystemStartRange();
				break;
			case 5:
				return $this->getSystemEndRange();
				break;
			case 6:
				return $this->getSystemLeadingZero();
				break;
			case 7:
				return $this->getIsDelete();
				break;
			case 8:
				return $this->getIsPhysicalDelete();
				break;
			case 9:
				return $this->getUserPrefix();
				break;
			case 10:
				return $this->getUserSuffix();
				break;
			case 11:
				return $this->getUserStartRange();
				break;
			case 12:
				return $this->getGenerateRangeForUsers();
				break;
			case 13:
				return $this->getUserLeadingZero();
				break;
			case 14:
				return $this->getUserNote();
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
		$keys = ArAsteriskAccountRangePeer::getFieldNames($keyType);
		$result = array(
			$keys[0] => $this->getId(),
			$keys[1] => $this->getArOrganizationUnitId(),
			$keys[2] => $this->getSystemPrefix(),
			$keys[3] => $this->getSystemSuffix(),
			$keys[4] => $this->getSystemStartRange(),
			$keys[5] => $this->getSystemEndRange(),
			$keys[6] => $this->getSystemLeadingZero(),
			$keys[7] => $this->getIsDelete(),
			$keys[8] => $this->getIsPhysicalDelete(),
			$keys[9] => $this->getUserPrefix(),
			$keys[10] => $this->getUserSuffix(),
			$keys[11] => $this->getUserStartRange(),
			$keys[12] => $this->getGenerateRangeForUsers(),
			$keys[13] => $this->getUserLeadingZero(),
			$keys[14] => $this->getUserNote(),
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
		$pos = ArAsteriskAccountRangePeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
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
				$this->setArOrganizationUnitId($value);
				break;
			case 2:
				$this->setSystemPrefix($value);
				break;
			case 3:
				$this->setSystemSuffix($value);
				break;
			case 4:
				$this->setSystemStartRange($value);
				break;
			case 5:
				$this->setSystemEndRange($value);
				break;
			case 6:
				$this->setSystemLeadingZero($value);
				break;
			case 7:
				$this->setIsDelete($value);
				break;
			case 8:
				$this->setIsPhysicalDelete($value);
				break;
			case 9:
				$this->setUserPrefix($value);
				break;
			case 10:
				$this->setUserSuffix($value);
				break;
			case 11:
				$this->setUserStartRange($value);
				break;
			case 12:
				$this->setGenerateRangeForUsers($value);
				break;
			case 13:
				$this->setUserLeadingZero($value);
				break;
			case 14:
				$this->setUserNote($value);
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
		$keys = ArAsteriskAccountRangePeer::getFieldNames($keyType);

		if (array_key_exists($keys[0], $arr)) $this->setId($arr[$keys[0]]);
		if (array_key_exists($keys[1], $arr)) $this->setArOrganizationUnitId($arr[$keys[1]]);
		if (array_key_exists($keys[2], $arr)) $this->setSystemPrefix($arr[$keys[2]]);
		if (array_key_exists($keys[3], $arr)) $this->setSystemSuffix($arr[$keys[3]]);
		if (array_key_exists($keys[4], $arr)) $this->setSystemStartRange($arr[$keys[4]]);
		if (array_key_exists($keys[5], $arr)) $this->setSystemEndRange($arr[$keys[5]]);
		if (array_key_exists($keys[6], $arr)) $this->setSystemLeadingZero($arr[$keys[6]]);
		if (array_key_exists($keys[7], $arr)) $this->setIsDelete($arr[$keys[7]]);
		if (array_key_exists($keys[8], $arr)) $this->setIsPhysicalDelete($arr[$keys[8]]);
		if (array_key_exists($keys[9], $arr)) $this->setUserPrefix($arr[$keys[9]]);
		if (array_key_exists($keys[10], $arr)) $this->setUserSuffix($arr[$keys[10]]);
		if (array_key_exists($keys[11], $arr)) $this->setUserStartRange($arr[$keys[11]]);
		if (array_key_exists($keys[12], $arr)) $this->setGenerateRangeForUsers($arr[$keys[12]]);
		if (array_key_exists($keys[13], $arr)) $this->setUserLeadingZero($arr[$keys[13]]);
		if (array_key_exists($keys[14], $arr)) $this->setUserNote($arr[$keys[14]]);
	}

	/**
	 * Build a Criteria object containing the values of all modified columns in this object.
	 *
	 * @return     Criteria The Criteria object containing all modified values.
	 */
	public function buildCriteria()
	{
		$criteria = new Criteria(ArAsteriskAccountRangePeer::DATABASE_NAME);

		if ($this->isColumnModified(ArAsteriskAccountRangePeer::ID)) $criteria->add(ArAsteriskAccountRangePeer::ID, $this->id);
		if ($this->isColumnModified(ArAsteriskAccountRangePeer::AR_ORGANIZATION_UNIT_ID)) $criteria->add(ArAsteriskAccountRangePeer::AR_ORGANIZATION_UNIT_ID, $this->ar_organization_unit_id);
		if ($this->isColumnModified(ArAsteriskAccountRangePeer::SYSTEM_PREFIX)) $criteria->add(ArAsteriskAccountRangePeer::SYSTEM_PREFIX, $this->system_prefix);
		if ($this->isColumnModified(ArAsteriskAccountRangePeer::SYSTEM_SUFFIX)) $criteria->add(ArAsteriskAccountRangePeer::SYSTEM_SUFFIX, $this->system_suffix);
		if ($this->isColumnModified(ArAsteriskAccountRangePeer::SYSTEM_START_RANGE)) $criteria->add(ArAsteriskAccountRangePeer::SYSTEM_START_RANGE, $this->system_start_range);
		if ($this->isColumnModified(ArAsteriskAccountRangePeer::SYSTEM_END_RANGE)) $criteria->add(ArAsteriskAccountRangePeer::SYSTEM_END_RANGE, $this->system_end_range);
		if ($this->isColumnModified(ArAsteriskAccountRangePeer::SYSTEM_LEADING_ZERO)) $criteria->add(ArAsteriskAccountRangePeer::SYSTEM_LEADING_ZERO, $this->system_leading_zero);
		if ($this->isColumnModified(ArAsteriskAccountRangePeer::IS_DELETE)) $criteria->add(ArAsteriskAccountRangePeer::IS_DELETE, $this->is_delete);
		if ($this->isColumnModified(ArAsteriskAccountRangePeer::IS_PHYSICAL_DELETE)) $criteria->add(ArAsteriskAccountRangePeer::IS_PHYSICAL_DELETE, $this->is_physical_delete);
		if ($this->isColumnModified(ArAsteriskAccountRangePeer::USER_PREFIX)) $criteria->add(ArAsteriskAccountRangePeer::USER_PREFIX, $this->user_prefix);
		if ($this->isColumnModified(ArAsteriskAccountRangePeer::USER_SUFFIX)) $criteria->add(ArAsteriskAccountRangePeer::USER_SUFFIX, $this->user_suffix);
		if ($this->isColumnModified(ArAsteriskAccountRangePeer::USER_START_RANGE)) $criteria->add(ArAsteriskAccountRangePeer::USER_START_RANGE, $this->user_start_range);
		if ($this->isColumnModified(ArAsteriskAccountRangePeer::GENERATE_RANGE_FOR_USERS)) $criteria->add(ArAsteriskAccountRangePeer::GENERATE_RANGE_FOR_USERS, $this->generate_range_for_users);
		if ($this->isColumnModified(ArAsteriskAccountRangePeer::USER_LEADING_ZERO)) $criteria->add(ArAsteriskAccountRangePeer::USER_LEADING_ZERO, $this->user_leading_zero);
		if ($this->isColumnModified(ArAsteriskAccountRangePeer::USER_NOTE)) $criteria->add(ArAsteriskAccountRangePeer::USER_NOTE, $this->user_note);

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
		$criteria = new Criteria(ArAsteriskAccountRangePeer::DATABASE_NAME);

		$criteria->add(ArAsteriskAccountRangePeer::ID, $this->id);

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
	 * @param      object $copyObj An object of ArAsteriskAccountRange (or compatible) type.
	 * @param      boolean $deepCopy Whether to also copy all rows that refer (by fkey) to the current row.
	 * @throws     PropelException
	 */
	public function copyInto($copyObj, $deepCopy = false)
	{

		$copyObj->setArOrganizationUnitId($this->ar_organization_unit_id);

		$copyObj->setSystemPrefix($this->system_prefix);

		$copyObj->setSystemSuffix($this->system_suffix);

		$copyObj->setSystemStartRange($this->system_start_range);

		$copyObj->setSystemEndRange($this->system_end_range);

		$copyObj->setSystemLeadingZero($this->system_leading_zero);

		$copyObj->setIsDelete($this->is_delete);

		$copyObj->setIsPhysicalDelete($this->is_physical_delete);

		$copyObj->setUserPrefix($this->user_prefix);

		$copyObj->setUserSuffix($this->user_suffix);

		$copyObj->setUserStartRange($this->user_start_range);

		$copyObj->setGenerateRangeForUsers($this->generate_range_for_users);

		$copyObj->setUserLeadingZero($this->user_leading_zero);

		$copyObj->setUserNote($this->user_note);


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
	 * @return     ArAsteriskAccountRange Clone of current object.
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
	 * @return     ArAsteriskAccountRangePeer
	 */
	public function getPeer()
	{
		if (self::$peer === null) {
			self::$peer = new ArAsteriskAccountRangePeer();
		}
		return self::$peer;
	}

	/**
	 * Declares an association between this object and a ArOrganizationUnit object.
	 *
	 * @param      ArOrganizationUnit $v
	 * @return     ArAsteriskAccountRange The current object (for fluent API support)
	 * @throws     PropelException
	 */
	public function setArOrganizationUnit(ArOrganizationUnit $v = null)
	{
		if ($v === null) {
			$this->setArOrganizationUnitId(NULL);
		} else {
			$this->setArOrganizationUnitId($v->getId());
		}

		$this->aArOrganizationUnit = $v;

		// Add binding for other direction of this n:n relationship.
		// If this object has already been added to the ArOrganizationUnit object, it will not be re-added.
		if ($v !== null) {
			$v->addArAsteriskAccountRange($this);
		}

		return $this;
	}


	/**
	 * Get the associated ArOrganizationUnit object
	 *
	 * @param      PropelPDO Optional Connection object.
	 * @return     ArOrganizationUnit The associated ArOrganizationUnit object.
	 * @throws     PropelException
	 */
	public function getArOrganizationUnit(PropelPDO $con = null)
	{
		if ($this->aArOrganizationUnit === null && ($this->ar_organization_unit_id !== null)) {
			$this->aArOrganizationUnit = ArOrganizationUnitPeer::retrieveByPk($this->ar_organization_unit_id);
			/* The following can be used additionally to
			   guarantee the related object contains a reference
			   to this object.  This level of coupling may, however, be
			   undesirable since it could result in an only partially populated collection
			   in the referenced object.
			   $this->aArOrganizationUnit->addArAsteriskAccountRanges($this);
			 */
		}
		return $this->aArOrganizationUnit;
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

			$this->aArOrganizationUnit = null;
	}

} // BaseArAsteriskAccountRange
