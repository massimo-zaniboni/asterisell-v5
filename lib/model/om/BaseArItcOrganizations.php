<?php

/**
 * Base class that represents a row from the 'ar_itc_organizations' table.
 *
 * 
 *
 * @package    lib.model.om
 */
abstract class BaseArItcOrganizations extends BaseObject  implements Persistent {


	/**
	 * The Peer class.
	 * Instance provides a convenient way of calling static methods on a class
	 * that calling code may not be able to identify.
	 * @var        ArItcOrganizationsPeer
	 */
	protected static $peer;

	/**
	 * The value for the id field.
	 * @var        int
	 */
	protected $id;

	/**
	 * The value for the account_code field.
	 * @var        string
	 */
	protected $account_code;

	/**
	 * The value for the definition_time field.
	 * @var        string
	 */
	protected $definition_time;

	/**
	 * The value for the org field.
	 * @var        string
	 */
	protected $org;

	/**
	 * The value for the name field.
	 * @var        string
	 */
	protected $name;

	/**
	 * The value for the description field.
	 * @var        string
	 */
	protected $description;

	/**
	 * The value for the email field.
	 * @var        string
	 */
	protected $email;

	/**
	 * The value for the parent field.
	 * @var        string
	 */
	protected $parent;

	/**
	 * The value for the calculated_account_code field.
	 * @var        string
	 */
	protected $calculated_account_code;

	/**
	 * The value for the is_new field.
	 * @var        boolean
	 */
	protected $is_new;

	/**
	 * The value for the is_maybe_modified field.
	 * @var        boolean
	 */
	protected $is_maybe_modified;

	/**
	 * The value for the is_to_remove field.
	 * @var        boolean
	 */
	protected $is_to_remove;

	/**
	 * The value for the can_be_removed field.
	 * @var        boolean
	 */
	protected $can_be_removed;

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
	
	const PEER = 'ArItcOrganizationsPeer';

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
	 * Get the [account_code] column value.
	 * 
	 * @return     string
	 */
	public function getAccountCode()
	{
		return $this->account_code;
	}

	/**
	 * Get the [optionally formatted] temporal [definition_time] column value.
	 * 
	 *
	 * @param      string $format The date/time format string (either date()-style or strftime()-style).
	 *							If format is NULL, then the raw DateTime object will be returned.
	 * @return     mixed Formatted date/time value as string or DateTime object (if format is NULL), NULL if column is NULL, and 0 if column value is 0000-00-00 00:00:00
	 * @throws     PropelException - if unable to parse/validate the date/time value.
	 */
	public function getDefinitionTime($format = 'Y-m-d H:i:s')
	{
		if ($this->definition_time === null) {
			return null;
		}


		if ($this->definition_time === '0000-00-00 00:00:00') {
			// while technically this is not a default value of NULL,
			// this seems to be closest in meaning.
			return null;
		} else {
			try {
				$dt = new DateTime($this->definition_time);
			} catch (Exception $x) {
				throw new PropelException("Internally stored date/time/timestamp value could not be converted to DateTime: " . var_export($this->definition_time, true), $x);
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
	 * Get the [org] column value.
	 * 
	 * @return     string
	 */
	public function getOrg()
	{
		return $this->org;
	}

	/**
	 * Get the [name] column value.
	 * 
	 * @return     string
	 */
	public function getName()
	{
		return $this->name;
	}

	/**
	 * Get the [description] column value.
	 * 
	 * @return     string
	 */
	public function getDescription()
	{
		return $this->description;
	}

	/**
	 * Get the [email] column value.
	 * 
	 * @return     string
	 */
	public function getEmail()
	{
		return $this->email;
	}

	/**
	 * Get the [parent] column value.
	 * 
	 * @return     string
	 */
	public function getParent()
	{
		return $this->parent;
	}

	/**
	 * Get the [calculated_account_code] column value.
	 * 
	 * @return     string
	 */
	public function getCalculatedAccountCode()
	{
		return $this->calculated_account_code;
	}

	/**
	 * Get the [is_new] column value.
	 * 
	 * @return     boolean
	 */
	public function getIsNew()
	{
		return $this->is_new;
	}

	/**
	 * Get the [is_maybe_modified] column value.
	 * 
	 * @return     boolean
	 */
	public function getIsMaybeModified()
	{
		return $this->is_maybe_modified;
	}

	/**
	 * Get the [is_to_remove] column value.
	 * 
	 * @return     boolean
	 */
	public function getIsToRemove()
	{
		return $this->is_to_remove;
	}

	/**
	 * Get the [can_be_removed] column value.
	 * 
	 * @return     boolean
	 */
	public function getCanBeRemoved()
	{
		return $this->can_be_removed;
	}

	/**
	 * Set the value of [id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArItcOrganizations The current object (for fluent API support)
	 */
	public function setId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->id !== $v) {
			$this->id = $v;
			$this->modifiedColumns[] = ArItcOrganizationsPeer::ID;
		}

		return $this;
	} // setId()

	/**
	 * Set the value of [account_code] column.
	 * 
	 * @param      string $v new value
	 * @return     ArItcOrganizations The current object (for fluent API support)
	 */
	public function setAccountCode($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->account_code !== $v) {
			$this->account_code = $v;
			$this->modifiedColumns[] = ArItcOrganizationsPeer::ACCOUNT_CODE;
		}

		return $this;
	} // setAccountCode()

	/**
	 * Sets the value of [definition_time] column to a normalized version of the date/time value specified.
	 * 
	 * @param      mixed $v string, integer (timestamp), or DateTime value.  Empty string will
	 *						be treated as NULL for temporal objects.
	 * @return     ArItcOrganizations The current object (for fluent API support)
	 */
	public function setDefinitionTime($v)
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

		if ( $this->definition_time !== null || $dt !== null ) {
			// (nested ifs are a little easier to read in this case)

			$currNorm = ($this->definition_time !== null && $tmpDt = new DateTime($this->definition_time)) ? $tmpDt->format('Y-m-d H:i:s') : null;
			$newNorm = ($dt !== null) ? $dt->format('Y-m-d H:i:s') : null;

			if ( ($currNorm !== $newNorm) // normalized values don't match 
					)
			{
				$this->definition_time = ($dt ? $dt->format('Y-m-d H:i:s') : null);
				$this->modifiedColumns[] = ArItcOrganizationsPeer::DEFINITION_TIME;
			}
		} // if either are not null

		return $this;
	} // setDefinitionTime()

	/**
	 * Set the value of [org] column.
	 * 
	 * @param      string $v new value
	 * @return     ArItcOrganizations The current object (for fluent API support)
	 */
	public function setOrg($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->org !== $v) {
			$this->org = $v;
			$this->modifiedColumns[] = ArItcOrganizationsPeer::ORG;
		}

		return $this;
	} // setOrg()

	/**
	 * Set the value of [name] column.
	 * 
	 * @param      string $v new value
	 * @return     ArItcOrganizations The current object (for fluent API support)
	 */
	public function setName($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->name !== $v) {
			$this->name = $v;
			$this->modifiedColumns[] = ArItcOrganizationsPeer::NAME;
		}

		return $this;
	} // setName()

	/**
	 * Set the value of [description] column.
	 * 
	 * @param      string $v new value
	 * @return     ArItcOrganizations The current object (for fluent API support)
	 */
	public function setDescription($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->description !== $v) {
			$this->description = $v;
			$this->modifiedColumns[] = ArItcOrganizationsPeer::DESCRIPTION;
		}

		return $this;
	} // setDescription()

	/**
	 * Set the value of [email] column.
	 * 
	 * @param      string $v new value
	 * @return     ArItcOrganizations The current object (for fluent API support)
	 */
	public function setEmail($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->email !== $v) {
			$this->email = $v;
			$this->modifiedColumns[] = ArItcOrganizationsPeer::EMAIL;
		}

		return $this;
	} // setEmail()

	/**
	 * Set the value of [parent] column.
	 * 
	 * @param      string $v new value
	 * @return     ArItcOrganizations The current object (for fluent API support)
	 */
	public function setParent($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->parent !== $v) {
			$this->parent = $v;
			$this->modifiedColumns[] = ArItcOrganizationsPeer::PARENT;
		}

		return $this;
	} // setParent()

	/**
	 * Set the value of [calculated_account_code] column.
	 * 
	 * @param      string $v new value
	 * @return     ArItcOrganizations The current object (for fluent API support)
	 */
	public function setCalculatedAccountCode($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->calculated_account_code !== $v) {
			$this->calculated_account_code = $v;
			$this->modifiedColumns[] = ArItcOrganizationsPeer::CALCULATED_ACCOUNT_CODE;
		}

		return $this;
	} // setCalculatedAccountCode()

	/**
	 * Set the value of [is_new] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArItcOrganizations The current object (for fluent API support)
	 */
	public function setIsNew($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->is_new !== $v) {
			$this->is_new = $v;
			$this->modifiedColumns[] = ArItcOrganizationsPeer::IS_NEW;
		}

		return $this;
	} // setIsNew()

	/**
	 * Set the value of [is_maybe_modified] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArItcOrganizations The current object (for fluent API support)
	 */
	public function setIsMaybeModified($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->is_maybe_modified !== $v) {
			$this->is_maybe_modified = $v;
			$this->modifiedColumns[] = ArItcOrganizationsPeer::IS_MAYBE_MODIFIED;
		}

		return $this;
	} // setIsMaybeModified()

	/**
	 * Set the value of [is_to_remove] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArItcOrganizations The current object (for fluent API support)
	 */
	public function setIsToRemove($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->is_to_remove !== $v) {
			$this->is_to_remove = $v;
			$this->modifiedColumns[] = ArItcOrganizationsPeer::IS_TO_REMOVE;
		}

		return $this;
	} // setIsToRemove()

	/**
	 * Set the value of [can_be_removed] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArItcOrganizations The current object (for fluent API support)
	 */
	public function setCanBeRemoved($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->can_be_removed !== $v) {
			$this->can_be_removed = $v;
			$this->modifiedColumns[] = ArItcOrganizationsPeer::CAN_BE_REMOVED;
		}

		return $this;
	} // setCanBeRemoved()

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
			$this->account_code = ($row[$startcol + 1] !== null) ? (string) $row[$startcol + 1] : null;
			$this->definition_time = ($row[$startcol + 2] !== null) ? (string) $row[$startcol + 2] : null;
			$this->org = ($row[$startcol + 3] !== null) ? (string) $row[$startcol + 3] : null;
			$this->name = ($row[$startcol + 4] !== null) ? (string) $row[$startcol + 4] : null;
			$this->description = ($row[$startcol + 5] !== null) ? (string) $row[$startcol + 5] : null;
			$this->email = ($row[$startcol + 6] !== null) ? (string) $row[$startcol + 6] : null;
			$this->parent = ($row[$startcol + 7] !== null) ? (string) $row[$startcol + 7] : null;
			$this->calculated_account_code = ($row[$startcol + 8] !== null) ? (string) $row[$startcol + 8] : null;
			$this->is_new = ($row[$startcol + 9] !== null) ? (boolean) $row[$startcol + 9] : null;
			$this->is_maybe_modified = ($row[$startcol + 10] !== null) ? (boolean) $row[$startcol + 10] : null;
			$this->is_to_remove = ($row[$startcol + 11] !== null) ? (boolean) $row[$startcol + 11] : null;
			$this->can_be_removed = ($row[$startcol + 12] !== null) ? (boolean) $row[$startcol + 12] : null;
			$this->resetModified();

			$this->setNew(false);

			if ($rehydrate) {
				$this->ensureConsistency();
			}

			// FIXME - using NUM_COLUMNS may be clearer.
			return $startcol + 13; // 13 = ArItcOrganizationsPeer::NUM_COLUMNS - ArItcOrganizationsPeer::NUM_LAZY_LOAD_COLUMNS).

		} catch (Exception $e) {
			throw new PropelException("Error populating ArItcOrganizations object", $e);
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
			$con = Propel::getConnection(ArItcOrganizationsPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		// We don't need to alter the object instance pool; we're just modifying this instance
		// already in the pool.

		$stmt = ArItcOrganizationsPeer::doSelectStmt($this->buildPkeyCriteria(), $con);
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
			$con = Propel::getConnection(ArItcOrganizationsPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}
		
		$con->beginTransaction();
		try {
			$ret = $this->preDelete($con);
			if ($ret) {
				ArItcOrganizationsPeer::doDelete($this, $con);
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
			$con = Propel::getConnection(ArItcOrganizationsPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
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
				ArItcOrganizationsPeer::addInstanceToPool($this);
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
				$this->modifiedColumns[] = ArItcOrganizationsPeer::ID;
			}

			// If this object has been modified, then save it to the database.
			if ($this->isModified()) {
				if ($this->isNew()) {
					$pk = ArItcOrganizationsPeer::doInsert($this, $con);
					$affectedRows += 1; // we are assuming that there is only 1 row per doInsert() which
										 // should always be true here (even though technically
										 // BasePeer::doInsert() can insert multiple rows).

					$this->setId($pk);  //[IMV] update autoincrement primary key

					$this->setNew(false);
				} else {
					$affectedRows += ArItcOrganizationsPeer::doUpdate($this, $con);
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


			if (($retval = ArItcOrganizationsPeer::doValidate($this, $columns)) !== true) {
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
		$pos = ArItcOrganizationsPeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
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
				return $this->getAccountCode();
				break;
			case 2:
				return $this->getDefinitionTime();
				break;
			case 3:
				return $this->getOrg();
				break;
			case 4:
				return $this->getName();
				break;
			case 5:
				return $this->getDescription();
				break;
			case 6:
				return $this->getEmail();
				break;
			case 7:
				return $this->getParent();
				break;
			case 8:
				return $this->getCalculatedAccountCode();
				break;
			case 9:
				return $this->getIsNew();
				break;
			case 10:
				return $this->getIsMaybeModified();
				break;
			case 11:
				return $this->getIsToRemove();
				break;
			case 12:
				return $this->getCanBeRemoved();
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
		$keys = ArItcOrganizationsPeer::getFieldNames($keyType);
		$result = array(
			$keys[0] => $this->getId(),
			$keys[1] => $this->getAccountCode(),
			$keys[2] => $this->getDefinitionTime(),
			$keys[3] => $this->getOrg(),
			$keys[4] => $this->getName(),
			$keys[5] => $this->getDescription(),
			$keys[6] => $this->getEmail(),
			$keys[7] => $this->getParent(),
			$keys[8] => $this->getCalculatedAccountCode(),
			$keys[9] => $this->getIsNew(),
			$keys[10] => $this->getIsMaybeModified(),
			$keys[11] => $this->getIsToRemove(),
			$keys[12] => $this->getCanBeRemoved(),
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
		$pos = ArItcOrganizationsPeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
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
				$this->setAccountCode($value);
				break;
			case 2:
				$this->setDefinitionTime($value);
				break;
			case 3:
				$this->setOrg($value);
				break;
			case 4:
				$this->setName($value);
				break;
			case 5:
				$this->setDescription($value);
				break;
			case 6:
				$this->setEmail($value);
				break;
			case 7:
				$this->setParent($value);
				break;
			case 8:
				$this->setCalculatedAccountCode($value);
				break;
			case 9:
				$this->setIsNew($value);
				break;
			case 10:
				$this->setIsMaybeModified($value);
				break;
			case 11:
				$this->setIsToRemove($value);
				break;
			case 12:
				$this->setCanBeRemoved($value);
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
		$keys = ArItcOrganizationsPeer::getFieldNames($keyType);

		if (array_key_exists($keys[0], $arr)) $this->setId($arr[$keys[0]]);
		if (array_key_exists($keys[1], $arr)) $this->setAccountCode($arr[$keys[1]]);
		if (array_key_exists($keys[2], $arr)) $this->setDefinitionTime($arr[$keys[2]]);
		if (array_key_exists($keys[3], $arr)) $this->setOrg($arr[$keys[3]]);
		if (array_key_exists($keys[4], $arr)) $this->setName($arr[$keys[4]]);
		if (array_key_exists($keys[5], $arr)) $this->setDescription($arr[$keys[5]]);
		if (array_key_exists($keys[6], $arr)) $this->setEmail($arr[$keys[6]]);
		if (array_key_exists($keys[7], $arr)) $this->setParent($arr[$keys[7]]);
		if (array_key_exists($keys[8], $arr)) $this->setCalculatedAccountCode($arr[$keys[8]]);
		if (array_key_exists($keys[9], $arr)) $this->setIsNew($arr[$keys[9]]);
		if (array_key_exists($keys[10], $arr)) $this->setIsMaybeModified($arr[$keys[10]]);
		if (array_key_exists($keys[11], $arr)) $this->setIsToRemove($arr[$keys[11]]);
		if (array_key_exists($keys[12], $arr)) $this->setCanBeRemoved($arr[$keys[12]]);
	}

	/**
	 * Build a Criteria object containing the values of all modified columns in this object.
	 *
	 * @return     Criteria The Criteria object containing all modified values.
	 */
	public function buildCriteria()
	{
		$criteria = new Criteria(ArItcOrganizationsPeer::DATABASE_NAME);

		if ($this->isColumnModified(ArItcOrganizationsPeer::ID)) $criteria->add(ArItcOrganizationsPeer::ID, $this->id);
		if ($this->isColumnModified(ArItcOrganizationsPeer::ACCOUNT_CODE)) $criteria->add(ArItcOrganizationsPeer::ACCOUNT_CODE, $this->account_code);
		if ($this->isColumnModified(ArItcOrganizationsPeer::DEFINITION_TIME)) $criteria->add(ArItcOrganizationsPeer::DEFINITION_TIME, $this->definition_time);
		if ($this->isColumnModified(ArItcOrganizationsPeer::ORG)) $criteria->add(ArItcOrganizationsPeer::ORG, $this->org);
		if ($this->isColumnModified(ArItcOrganizationsPeer::NAME)) $criteria->add(ArItcOrganizationsPeer::NAME, $this->name);
		if ($this->isColumnModified(ArItcOrganizationsPeer::DESCRIPTION)) $criteria->add(ArItcOrganizationsPeer::DESCRIPTION, $this->description);
		if ($this->isColumnModified(ArItcOrganizationsPeer::EMAIL)) $criteria->add(ArItcOrganizationsPeer::EMAIL, $this->email);
		if ($this->isColumnModified(ArItcOrganizationsPeer::PARENT)) $criteria->add(ArItcOrganizationsPeer::PARENT, $this->parent);
		if ($this->isColumnModified(ArItcOrganizationsPeer::CALCULATED_ACCOUNT_CODE)) $criteria->add(ArItcOrganizationsPeer::CALCULATED_ACCOUNT_CODE, $this->calculated_account_code);
		if ($this->isColumnModified(ArItcOrganizationsPeer::IS_NEW)) $criteria->add(ArItcOrganizationsPeer::IS_NEW, $this->is_new);
		if ($this->isColumnModified(ArItcOrganizationsPeer::IS_MAYBE_MODIFIED)) $criteria->add(ArItcOrganizationsPeer::IS_MAYBE_MODIFIED, $this->is_maybe_modified);
		if ($this->isColumnModified(ArItcOrganizationsPeer::IS_TO_REMOVE)) $criteria->add(ArItcOrganizationsPeer::IS_TO_REMOVE, $this->is_to_remove);
		if ($this->isColumnModified(ArItcOrganizationsPeer::CAN_BE_REMOVED)) $criteria->add(ArItcOrganizationsPeer::CAN_BE_REMOVED, $this->can_be_removed);

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
		$criteria = new Criteria(ArItcOrganizationsPeer::DATABASE_NAME);

		$criteria->add(ArItcOrganizationsPeer::ID, $this->id);

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
	 * @param      object $copyObj An object of ArItcOrganizations (or compatible) type.
	 * @param      boolean $deepCopy Whether to also copy all rows that refer (by fkey) to the current row.
	 * @throws     PropelException
	 */
	public function copyInto($copyObj, $deepCopy = false)
	{

		$copyObj->setAccountCode($this->account_code);

		$copyObj->setDefinitionTime($this->definition_time);

		$copyObj->setOrg($this->org);

		$copyObj->setName($this->name);

		$copyObj->setDescription($this->description);

		$copyObj->setEmail($this->email);

		$copyObj->setParent($this->parent);

		$copyObj->setCalculatedAccountCode($this->calculated_account_code);

		$copyObj->setIsNew($this->is_new);

		$copyObj->setIsMaybeModified($this->is_maybe_modified);

		$copyObj->setIsToRemove($this->is_to_remove);

		$copyObj->setCanBeRemoved($this->can_be_removed);


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
	 * @return     ArItcOrganizations Clone of current object.
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
	 * @return     ArItcOrganizationsPeer
	 */
	public function getPeer()
	{
		if (self::$peer === null) {
			self::$peer = new ArItcOrganizationsPeer();
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

} // BaseArItcOrganizations
