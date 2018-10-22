<?php

/**
 * Base class that represents a row from the 'list_cdrs_admin_3_fast' table.
 *
 * 
 *
 * @package    lib.model.om
 */
abstract class BaseListCdrsAdmin3Fast extends BaseObject  implements Persistent {


	/**
	 * The Peer class.
	 * Instance provides a convenient way of calling static methods on a class
	 * that calling code may not be able to identify.
	 * @var        ListCdrsAdmin3FastPeer
	 */
	protected static $peer;

	/**
	 * The value for the id field.
	 * @var        int
	 */
	protected $id;

	/**
	 * The value for the destination_type field.
	 * Note: this column has a database default value of: 0
	 * @var        int
	 */
	protected $destination_type;

	/**
	 * The value for the error_destination_type field.
	 * Note: this column has a database default value of: 0
	 * @var        int
	 */
	protected $error_destination_type;

	/**
	 * The value for the operator_type field.
	 * @var        string
	 */
	protected $operator_type;

	/**
	 * The value for the ar_communication_channel_type_id field.
	 * @var        int
	 */
	protected $ar_communication_channel_type_id;

	/**
	 * The value for the vendor_id field.
	 * @var        int
	 */
	protected $vendor_id;

	/**
	 * The value for the count_of_calls field.
	 * @var        int
	 */
	protected $count_of_calls;

	/**
	 * The value for the billsec field.
	 * @var        int
	 */
	protected $billsec;

	/**
	 * The value for the income field.
	 * @var        int
	 */
	protected $income;

	/**
	 * The value for the cost field.
	 * @var        int
	 */
	protected $cost;

	/**
	 * The value for the cost_saving field.
	 * @var        int
	 */
	protected $cost_saving;

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
	
	const PEER = 'ListCdrsAdmin3FastPeer';

	/**
	 * Applies default values to this object.
	 * This method should be called from the object's constructor (or
	 * equivalent initialization method).
	 * @see        __construct()
	 */
	public function applyDefaultValues()
	{
		$this->destination_type = 0;
		$this->error_destination_type = 0;
	}

	/**
	 * Initializes internal state of BaseListCdrsAdmin3Fast object.
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
	 * Get the [destination_type] column value.
	 * 
	 * @return     int
	 */
	public function getDestinationType()
	{
		return $this->destination_type;
	}

	/**
	 * Get the [error_destination_type] column value.
	 * 
	 * @return     int
	 */
	public function getErrorDestinationType()
	{
		return $this->error_destination_type;
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
	 * Get the [ar_communication_channel_type_id] column value.
	 * 
	 * @return     int
	 */
	public function getArCommunicationChannelTypeId()
	{
		return $this->ar_communication_channel_type_id;
	}

	/**
	 * Get the [vendor_id] column value.
	 * 
	 * @return     int
	 */
	public function getVendorId()
	{
		return $this->vendor_id;
	}

	/**
	 * Get the [count_of_calls] column value.
	 * 
	 * @return     int
	 */
	public function getCountOfCalls()
	{
		return $this->count_of_calls;
	}

	/**
	 * Get the [billsec] column value.
	 * 
	 * @return     int
	 */
	public function getBillsec()
	{
		return $this->billsec;
	}

	/**
	 * Get the [income] column value.
	 * 
	 * @return     int
	 */
	public function getIncome()
	{
		return $this->income;
	}

	/**
	 * Get the [cost] column value.
	 * 
	 * @return     int
	 */
	public function getCost()
	{
		return $this->cost;
	}

	/**
	 * Get the [cost_saving] column value.
	 * 
	 * @return     int
	 */
	public function getCostSaving()
	{
		return $this->cost_saving;
	}

	/**
	 * Set the value of [id] column.
	 * 
	 * @param      int $v new value
	 * @return     ListCdrsAdmin3Fast The current object (for fluent API support)
	 */
	public function setId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->id !== $v) {
			$this->id = $v;
			$this->modifiedColumns[] = ListCdrsAdmin3FastPeer::ID;
		}

		return $this;
	} // setId()

	/**
	 * Set the value of [destination_type] column.
	 * 
	 * @param      int $v new value
	 * @return     ListCdrsAdmin3Fast The current object (for fluent API support)
	 */
	public function setDestinationType($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->destination_type !== $v || $this->isNew()) {
			$this->destination_type = $v;
			$this->modifiedColumns[] = ListCdrsAdmin3FastPeer::DESTINATION_TYPE;
		}

		return $this;
	} // setDestinationType()

	/**
	 * Set the value of [error_destination_type] column.
	 * 
	 * @param      int $v new value
	 * @return     ListCdrsAdmin3Fast The current object (for fluent API support)
	 */
	public function setErrorDestinationType($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->error_destination_type !== $v || $this->isNew()) {
			$this->error_destination_type = $v;
			$this->modifiedColumns[] = ListCdrsAdmin3FastPeer::ERROR_DESTINATION_TYPE;
		}

		return $this;
	} // setErrorDestinationType()

	/**
	 * Set the value of [operator_type] column.
	 * 
	 * @param      string $v new value
	 * @return     ListCdrsAdmin3Fast The current object (for fluent API support)
	 */
	public function setOperatorType($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->operator_type !== $v) {
			$this->operator_type = $v;
			$this->modifiedColumns[] = ListCdrsAdmin3FastPeer::OPERATOR_TYPE;
		}

		return $this;
	} // setOperatorType()

	/**
	 * Set the value of [ar_communication_channel_type_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ListCdrsAdmin3Fast The current object (for fluent API support)
	 */
	public function setArCommunicationChannelTypeId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->ar_communication_channel_type_id !== $v) {
			$this->ar_communication_channel_type_id = $v;
			$this->modifiedColumns[] = ListCdrsAdmin3FastPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID;
		}

		return $this;
	} // setArCommunicationChannelTypeId()

	/**
	 * Set the value of [vendor_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ListCdrsAdmin3Fast The current object (for fluent API support)
	 */
	public function setVendorId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->vendor_id !== $v) {
			$this->vendor_id = $v;
			$this->modifiedColumns[] = ListCdrsAdmin3FastPeer::VENDOR_ID;
		}

		return $this;
	} // setVendorId()

	/**
	 * Set the value of [count_of_calls] column.
	 * 
	 * @param      int $v new value
	 * @return     ListCdrsAdmin3Fast The current object (for fluent API support)
	 */
	public function setCountOfCalls($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->count_of_calls !== $v) {
			$this->count_of_calls = $v;
			$this->modifiedColumns[] = ListCdrsAdmin3FastPeer::COUNT_OF_CALLS;
		}

		return $this;
	} // setCountOfCalls()

	/**
	 * Set the value of [billsec] column.
	 * 
	 * @param      int $v new value
	 * @return     ListCdrsAdmin3Fast The current object (for fluent API support)
	 */
	public function setBillsec($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->billsec !== $v) {
			$this->billsec = $v;
			$this->modifiedColumns[] = ListCdrsAdmin3FastPeer::BILLSEC;
		}

		return $this;
	} // setBillsec()

	/**
	 * Set the value of [income] column.
	 * 
	 * @param      int $v new value
	 * @return     ListCdrsAdmin3Fast The current object (for fluent API support)
	 */
	public function setIncome($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->income !== $v) {
			$this->income = $v;
			$this->modifiedColumns[] = ListCdrsAdmin3FastPeer::INCOME;
		}

		return $this;
	} // setIncome()

	/**
	 * Set the value of [cost] column.
	 * 
	 * @param      int $v new value
	 * @return     ListCdrsAdmin3Fast The current object (for fluent API support)
	 */
	public function setCost($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->cost !== $v) {
			$this->cost = $v;
			$this->modifiedColumns[] = ListCdrsAdmin3FastPeer::COST;
		}

		return $this;
	} // setCost()

	/**
	 * Set the value of [cost_saving] column.
	 * 
	 * @param      int $v new value
	 * @return     ListCdrsAdmin3Fast The current object (for fluent API support)
	 */
	public function setCostSaving($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->cost_saving !== $v) {
			$this->cost_saving = $v;
			$this->modifiedColumns[] = ListCdrsAdmin3FastPeer::COST_SAVING;
		}

		return $this;
	} // setCostSaving()

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
			if ($this->destination_type !== 0) {
				return false;
			}

			if ($this->error_destination_type !== 0) {
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
			$this->destination_type = ($row[$startcol + 1] !== null) ? (int) $row[$startcol + 1] : null;
			$this->error_destination_type = ($row[$startcol + 2] !== null) ? (int) $row[$startcol + 2] : null;
			$this->operator_type = ($row[$startcol + 3] !== null) ? (string) $row[$startcol + 3] : null;
			$this->ar_communication_channel_type_id = ($row[$startcol + 4] !== null) ? (int) $row[$startcol + 4] : null;
			$this->vendor_id = ($row[$startcol + 5] !== null) ? (int) $row[$startcol + 5] : null;
			$this->count_of_calls = ($row[$startcol + 6] !== null) ? (int) $row[$startcol + 6] : null;
			$this->billsec = ($row[$startcol + 7] !== null) ? (int) $row[$startcol + 7] : null;
			$this->income = ($row[$startcol + 8] !== null) ? (int) $row[$startcol + 8] : null;
			$this->cost = ($row[$startcol + 9] !== null) ? (int) $row[$startcol + 9] : null;
			$this->cost_saving = ($row[$startcol + 10] !== null) ? (int) $row[$startcol + 10] : null;
			$this->resetModified();

			$this->setNew(false);

			if ($rehydrate) {
				$this->ensureConsistency();
			}

			// FIXME - using NUM_COLUMNS may be clearer.
			return $startcol + 11; // 11 = ListCdrsAdmin3FastPeer::NUM_COLUMNS - ListCdrsAdmin3FastPeer::NUM_LAZY_LOAD_COLUMNS).

		} catch (Exception $e) {
			throw new PropelException("Error populating ListCdrsAdmin3Fast object", $e);
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
			$con = Propel::getConnection(ListCdrsAdmin3FastPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		// We don't need to alter the object instance pool; we're just modifying this instance
		// already in the pool.

		$stmt = ListCdrsAdmin3FastPeer::doSelectStmt($this->buildPkeyCriteria(), $con);
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
			$con = Propel::getConnection(ListCdrsAdmin3FastPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}
		
		$con->beginTransaction();
		try {
			$ret = $this->preDelete($con);
			if ($ret) {
				ListCdrsAdmin3FastPeer::doDelete($this, $con);
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
			$con = Propel::getConnection(ListCdrsAdmin3FastPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
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
				ListCdrsAdmin3FastPeer::addInstanceToPool($this);
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
				$this->modifiedColumns[] = ListCdrsAdmin3FastPeer::ID;
			}

			// If this object has been modified, then save it to the database.
			if ($this->isModified()) {
				if ($this->isNew()) {
					$pk = ListCdrsAdmin3FastPeer::doInsert($this, $con);
					$affectedRows += 1; // we are assuming that there is only 1 row per doInsert() which
										 // should always be true here (even though technically
										 // BasePeer::doInsert() can insert multiple rows).

					$this->setId($pk);  //[IMV] update autoincrement primary key

					$this->setNew(false);
				} else {
					$affectedRows += ListCdrsAdmin3FastPeer::doUpdate($this, $con);
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


			if (($retval = ListCdrsAdmin3FastPeer::doValidate($this, $columns)) !== true) {
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
		$pos = ListCdrsAdmin3FastPeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
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
				return $this->getDestinationType();
				break;
			case 2:
				return $this->getErrorDestinationType();
				break;
			case 3:
				return $this->getOperatorType();
				break;
			case 4:
				return $this->getArCommunicationChannelTypeId();
				break;
			case 5:
				return $this->getVendorId();
				break;
			case 6:
				return $this->getCountOfCalls();
				break;
			case 7:
				return $this->getBillsec();
				break;
			case 8:
				return $this->getIncome();
				break;
			case 9:
				return $this->getCost();
				break;
			case 10:
				return $this->getCostSaving();
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
		$keys = ListCdrsAdmin3FastPeer::getFieldNames($keyType);
		$result = array(
			$keys[0] => $this->getId(),
			$keys[1] => $this->getDestinationType(),
			$keys[2] => $this->getErrorDestinationType(),
			$keys[3] => $this->getOperatorType(),
			$keys[4] => $this->getArCommunicationChannelTypeId(),
			$keys[5] => $this->getVendorId(),
			$keys[6] => $this->getCountOfCalls(),
			$keys[7] => $this->getBillsec(),
			$keys[8] => $this->getIncome(),
			$keys[9] => $this->getCost(),
			$keys[10] => $this->getCostSaving(),
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
		$pos = ListCdrsAdmin3FastPeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
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
				$this->setDestinationType($value);
				break;
			case 2:
				$this->setErrorDestinationType($value);
				break;
			case 3:
				$this->setOperatorType($value);
				break;
			case 4:
				$this->setArCommunicationChannelTypeId($value);
				break;
			case 5:
				$this->setVendorId($value);
				break;
			case 6:
				$this->setCountOfCalls($value);
				break;
			case 7:
				$this->setBillsec($value);
				break;
			case 8:
				$this->setIncome($value);
				break;
			case 9:
				$this->setCost($value);
				break;
			case 10:
				$this->setCostSaving($value);
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
		$keys = ListCdrsAdmin3FastPeer::getFieldNames($keyType);

		if (array_key_exists($keys[0], $arr)) $this->setId($arr[$keys[0]]);
		if (array_key_exists($keys[1], $arr)) $this->setDestinationType($arr[$keys[1]]);
		if (array_key_exists($keys[2], $arr)) $this->setErrorDestinationType($arr[$keys[2]]);
		if (array_key_exists($keys[3], $arr)) $this->setOperatorType($arr[$keys[3]]);
		if (array_key_exists($keys[4], $arr)) $this->setArCommunicationChannelTypeId($arr[$keys[4]]);
		if (array_key_exists($keys[5], $arr)) $this->setVendorId($arr[$keys[5]]);
		if (array_key_exists($keys[6], $arr)) $this->setCountOfCalls($arr[$keys[6]]);
		if (array_key_exists($keys[7], $arr)) $this->setBillsec($arr[$keys[7]]);
		if (array_key_exists($keys[8], $arr)) $this->setIncome($arr[$keys[8]]);
		if (array_key_exists($keys[9], $arr)) $this->setCost($arr[$keys[9]]);
		if (array_key_exists($keys[10], $arr)) $this->setCostSaving($arr[$keys[10]]);
	}

	/**
	 * Build a Criteria object containing the values of all modified columns in this object.
	 *
	 * @return     Criteria The Criteria object containing all modified values.
	 */
	public function buildCriteria()
	{
		$criteria = new Criteria(ListCdrsAdmin3FastPeer::DATABASE_NAME);

		if ($this->isColumnModified(ListCdrsAdmin3FastPeer::ID)) $criteria->add(ListCdrsAdmin3FastPeer::ID, $this->id);
		if ($this->isColumnModified(ListCdrsAdmin3FastPeer::DESTINATION_TYPE)) $criteria->add(ListCdrsAdmin3FastPeer::DESTINATION_TYPE, $this->destination_type);
		if ($this->isColumnModified(ListCdrsAdmin3FastPeer::ERROR_DESTINATION_TYPE)) $criteria->add(ListCdrsAdmin3FastPeer::ERROR_DESTINATION_TYPE, $this->error_destination_type);
		if ($this->isColumnModified(ListCdrsAdmin3FastPeer::OPERATOR_TYPE)) $criteria->add(ListCdrsAdmin3FastPeer::OPERATOR_TYPE, $this->operator_type);
		if ($this->isColumnModified(ListCdrsAdmin3FastPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID)) $criteria->add(ListCdrsAdmin3FastPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID, $this->ar_communication_channel_type_id);
		if ($this->isColumnModified(ListCdrsAdmin3FastPeer::VENDOR_ID)) $criteria->add(ListCdrsAdmin3FastPeer::VENDOR_ID, $this->vendor_id);
		if ($this->isColumnModified(ListCdrsAdmin3FastPeer::COUNT_OF_CALLS)) $criteria->add(ListCdrsAdmin3FastPeer::COUNT_OF_CALLS, $this->count_of_calls);
		if ($this->isColumnModified(ListCdrsAdmin3FastPeer::BILLSEC)) $criteria->add(ListCdrsAdmin3FastPeer::BILLSEC, $this->billsec);
		if ($this->isColumnModified(ListCdrsAdmin3FastPeer::INCOME)) $criteria->add(ListCdrsAdmin3FastPeer::INCOME, $this->income);
		if ($this->isColumnModified(ListCdrsAdmin3FastPeer::COST)) $criteria->add(ListCdrsAdmin3FastPeer::COST, $this->cost);
		if ($this->isColumnModified(ListCdrsAdmin3FastPeer::COST_SAVING)) $criteria->add(ListCdrsAdmin3FastPeer::COST_SAVING, $this->cost_saving);

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
		$criteria = new Criteria(ListCdrsAdmin3FastPeer::DATABASE_NAME);

		$criteria->add(ListCdrsAdmin3FastPeer::ID, $this->id);

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
	 * @param      object $copyObj An object of ListCdrsAdmin3Fast (or compatible) type.
	 * @param      boolean $deepCopy Whether to also copy all rows that refer (by fkey) to the current row.
	 * @throws     PropelException
	 */
	public function copyInto($copyObj, $deepCopy = false)
	{

		$copyObj->setDestinationType($this->destination_type);

		$copyObj->setErrorDestinationType($this->error_destination_type);

		$copyObj->setOperatorType($this->operator_type);

		$copyObj->setArCommunicationChannelTypeId($this->ar_communication_channel_type_id);

		$copyObj->setVendorId($this->vendor_id);

		$copyObj->setCountOfCalls($this->count_of_calls);

		$copyObj->setBillsec($this->billsec);

		$copyObj->setIncome($this->income);

		$copyObj->setCost($this->cost);

		$copyObj->setCostSaving($this->cost_saving);


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
	 * @return     ListCdrsAdmin3Fast Clone of current object.
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
	 * @return     ListCdrsAdmin3FastPeer
	 */
	public function getPeer()
	{
		if (self::$peer === null) {
			self::$peer = new ListCdrsAdmin3FastPeer();
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

} // BaseListCdrsAdmin3Fast
