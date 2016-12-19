<?php

/**
 * Base class that represents a row from the 'ar_rate_incremental_info' table.
 *
 * 
 *
 * @package    lib.model.om
 */
abstract class BaseArRateIncrementalInfo extends BaseObject  implements Persistent {


	/**
	 * The Peer class.
	 * Instance provides a convenient way of calling static methods on a class
	 * that calling code may not be able to identify.
	 * @var        ArRateIncrementalInfoPeer
	 */
	protected static $peer;

	/**
	 * The value for the id field.
	 * @var        int
	 */
	protected $id;

	/**
	 * The value for the ar_party_id field.
	 * @var        int
	 */
	protected $ar_party_id;

	/**
	 * The value for the ar_rate_id field.
	 * @var        int
	 */
	protected $ar_rate_id;

	/**
	 * The value for the period field.
	 * @var        string
	 */
	protected $period;

	/**
	 * The value for the last_processed_ar_cdr_date field.
	 * @var        string
	 */
	protected $last_processed_ar_cdr_date;

	/**
	 * The value for the last_processed_ar_cdr_id field.
	 * @var        int
	 */
	protected $last_processed_ar_cdr_id;

	/**
	 * The value for the bundle_rate field.
	 * @var        string
	 */
	protected $bundle_rate;

	/**
	 * @var        ArParty
	 */
	protected $aArParty;

	/**
	 * @var        ArRate
	 */
	protected $aArRate;

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
	
	const PEER = 'ArRateIncrementalInfoPeer';

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
	 * Get the [ar_party_id] column value.
	 * 
	 * @return     int
	 */
	public function getArPartyId()
	{
		return $this->ar_party_id;
	}

	/**
	 * Get the [ar_rate_id] column value.
	 * 
	 * @return     int
	 */
	public function getArRateId()
	{
		return $this->ar_rate_id;
	}

	/**
	 * Get the [period] column value.
	 * 
	 * @return     string
	 */
	public function getPeriod()
	{
		return $this->period;
	}

	/**
	 * Get the [optionally formatted] temporal [last_processed_ar_cdr_date] column value.
	 * 
	 *
	 * @param      string $format The date/time format string (either date()-style or strftime()-style).
	 *							If format is NULL, then the raw DateTime object will be returned.
	 * @return     mixed Formatted date/time value as string or DateTime object (if format is NULL), NULL if column is NULL, and 0 if column value is 0000-00-00 00:00:00
	 * @throws     PropelException - if unable to parse/validate the date/time value.
	 */
	public function getLastProcessedArCdrDate($format = 'Y-m-d H:i:s')
	{
		if ($this->last_processed_ar_cdr_date === null) {
			return null;
		}


		if ($this->last_processed_ar_cdr_date === '0000-00-00 00:00:00') {
			// while technically this is not a default value of NULL,
			// this seems to be closest in meaning.
			return null;
		} else {
			try {
				$dt = new DateTime($this->last_processed_ar_cdr_date);
			} catch (Exception $x) {
				throw new PropelException("Internally stored date/time/timestamp value could not be converted to DateTime: " . var_export($this->last_processed_ar_cdr_date, true), $x);
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
	 * Get the [last_processed_ar_cdr_id] column value.
	 * 
	 * @return     int
	 */
	public function getLastProcessedArCdrId()
	{
		return $this->last_processed_ar_cdr_id;
	}

	/**
	 * Get the [bundle_rate] column value.
	 * 
	 * @return     string
	 */
	public function getBundleRate()
	{
		return $this->bundle_rate;
	}

	/**
	 * Set the value of [id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArRateIncrementalInfo The current object (for fluent API support)
	 */
	public function setId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->id !== $v) {
			$this->id = $v;
			$this->modifiedColumns[] = ArRateIncrementalInfoPeer::ID;
		}

		return $this;
	} // setId()

	/**
	 * Set the value of [ar_party_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArRateIncrementalInfo The current object (for fluent API support)
	 */
	public function setArPartyId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->ar_party_id !== $v) {
			$this->ar_party_id = $v;
			$this->modifiedColumns[] = ArRateIncrementalInfoPeer::AR_PARTY_ID;
		}

		if ($this->aArParty !== null && $this->aArParty->getId() !== $v) {
			$this->aArParty = null;
		}

		return $this;
	} // setArPartyId()

	/**
	 * Set the value of [ar_rate_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArRateIncrementalInfo The current object (for fluent API support)
	 */
	public function setArRateId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->ar_rate_id !== $v) {
			$this->ar_rate_id = $v;
			$this->modifiedColumns[] = ArRateIncrementalInfoPeer::AR_RATE_ID;
		}

		if ($this->aArRate !== null && $this->aArRate->getId() !== $v) {
			$this->aArRate = null;
		}

		return $this;
	} // setArRateId()

	/**
	 * Set the value of [period] column.
	 * 
	 * @param      string $v new value
	 * @return     ArRateIncrementalInfo The current object (for fluent API support)
	 */
	public function setPeriod($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->period !== $v) {
			$this->period = $v;
			$this->modifiedColumns[] = ArRateIncrementalInfoPeer::PERIOD;
		}

		return $this;
	} // setPeriod()

	/**
	 * Sets the value of [last_processed_ar_cdr_date] column to a normalized version of the date/time value specified.
	 * 
	 * @param      mixed $v string, integer (timestamp), or DateTime value.  Empty string will
	 *						be treated as NULL for temporal objects.
	 * @return     ArRateIncrementalInfo The current object (for fluent API support)
	 */
	public function setLastProcessedArCdrDate($v)
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

		if ( $this->last_processed_ar_cdr_date !== null || $dt !== null ) {
			// (nested ifs are a little easier to read in this case)

			$currNorm = ($this->last_processed_ar_cdr_date !== null && $tmpDt = new DateTime($this->last_processed_ar_cdr_date)) ? $tmpDt->format('Y-m-d H:i:s') : null;
			$newNorm = ($dt !== null) ? $dt->format('Y-m-d H:i:s') : null;

			if ( ($currNorm !== $newNorm) // normalized values don't match 
					)
			{
				$this->last_processed_ar_cdr_date = ($dt ? $dt->format('Y-m-d H:i:s') : null);
				$this->modifiedColumns[] = ArRateIncrementalInfoPeer::LAST_PROCESSED_AR_CDR_DATE;
			}
		} // if either are not null

		return $this;
	} // setLastProcessedArCdrDate()

	/**
	 * Set the value of [last_processed_ar_cdr_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArRateIncrementalInfo The current object (for fluent API support)
	 */
	public function setLastProcessedArCdrId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->last_processed_ar_cdr_id !== $v) {
			$this->last_processed_ar_cdr_id = $v;
			$this->modifiedColumns[] = ArRateIncrementalInfoPeer::LAST_PROCESSED_AR_CDR_ID;
		}

		return $this;
	} // setLastProcessedArCdrId()

	/**
	 * Set the value of [bundle_rate] column.
	 * 
	 * @param      string $v new value
	 * @return     ArRateIncrementalInfo The current object (for fluent API support)
	 */
	public function setBundleRate($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->bundle_rate !== $v) {
			$this->bundle_rate = $v;
			$this->modifiedColumns[] = ArRateIncrementalInfoPeer::BUNDLE_RATE;
		}

		return $this;
	} // setBundleRate()

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
			$this->ar_party_id = ($row[$startcol + 1] !== null) ? (int) $row[$startcol + 1] : null;
			$this->ar_rate_id = ($row[$startcol + 2] !== null) ? (int) $row[$startcol + 2] : null;
			$this->period = ($row[$startcol + 3] !== null) ? (string) $row[$startcol + 3] : null;
			$this->last_processed_ar_cdr_date = ($row[$startcol + 4] !== null) ? (string) $row[$startcol + 4] : null;
			$this->last_processed_ar_cdr_id = ($row[$startcol + 5] !== null) ? (int) $row[$startcol + 5] : null;
			$this->bundle_rate = ($row[$startcol + 6] !== null) ? (string) $row[$startcol + 6] : null;
			$this->resetModified();

			$this->setNew(false);

			if ($rehydrate) {
				$this->ensureConsistency();
			}

			// FIXME - using NUM_COLUMNS may be clearer.
			return $startcol + 7; // 7 = ArRateIncrementalInfoPeer::NUM_COLUMNS - ArRateIncrementalInfoPeer::NUM_LAZY_LOAD_COLUMNS).

		} catch (Exception $e) {
			throw new PropelException("Error populating ArRateIncrementalInfo object", $e);
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

		if ($this->aArParty !== null && $this->ar_party_id !== $this->aArParty->getId()) {
			$this->aArParty = null;
		}
		if ($this->aArRate !== null && $this->ar_rate_id !== $this->aArRate->getId()) {
			$this->aArRate = null;
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
			$con = Propel::getConnection(ArRateIncrementalInfoPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		// We don't need to alter the object instance pool; we're just modifying this instance
		// already in the pool.

		$stmt = ArRateIncrementalInfoPeer::doSelectStmt($this->buildPkeyCriteria(), $con);
		$row = $stmt->fetch(PDO::FETCH_NUM);
		$stmt->closeCursor();
		if (!$row) {
			throw new PropelException('Cannot find matching row in the database to reload object values.');
		}
		$this->hydrate($row, 0, true); // rehydrate

		if ($deep) {  // also de-associate any related objects?

			$this->aArParty = null;
			$this->aArRate = null;
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
			$con = Propel::getConnection(ArRateIncrementalInfoPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}
		
		$con->beginTransaction();
		try {
			$ret = $this->preDelete($con);
			if ($ret) {
				ArRateIncrementalInfoPeer::doDelete($this, $con);
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
			$con = Propel::getConnection(ArRateIncrementalInfoPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
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
				ArRateIncrementalInfoPeer::addInstanceToPool($this);
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

			if ($this->aArParty !== null) {
				if ($this->aArParty->isModified() || $this->aArParty->isNew()) {
					$affectedRows += $this->aArParty->save($con);
				}
				$this->setArParty($this->aArParty);
			}

			if ($this->aArRate !== null) {
				if ($this->aArRate->isModified() || $this->aArRate->isNew()) {
					$affectedRows += $this->aArRate->save($con);
				}
				$this->setArRate($this->aArRate);
			}

			if ($this->isNew() ) {
				$this->modifiedColumns[] = ArRateIncrementalInfoPeer::ID;
			}

			// If this object has been modified, then save it to the database.
			if ($this->isModified()) {
				if ($this->isNew()) {
					$pk = ArRateIncrementalInfoPeer::doInsert($this, $con);
					$affectedRows += 1; // we are assuming that there is only 1 row per doInsert() which
										 // should always be true here (even though technically
										 // BasePeer::doInsert() can insert multiple rows).

					$this->setId($pk);  //[IMV] update autoincrement primary key

					$this->setNew(false);
				} else {
					$affectedRows += ArRateIncrementalInfoPeer::doUpdate($this, $con);
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

			if ($this->aArParty !== null) {
				if (!$this->aArParty->validate($columns)) {
					$failureMap = array_merge($failureMap, $this->aArParty->getValidationFailures());
				}
			}

			if ($this->aArRate !== null) {
				if (!$this->aArRate->validate($columns)) {
					$failureMap = array_merge($failureMap, $this->aArRate->getValidationFailures());
				}
			}


			if (($retval = ArRateIncrementalInfoPeer::doValidate($this, $columns)) !== true) {
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
		$pos = ArRateIncrementalInfoPeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
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
				return $this->getArPartyId();
				break;
			case 2:
				return $this->getArRateId();
				break;
			case 3:
				return $this->getPeriod();
				break;
			case 4:
				return $this->getLastProcessedArCdrDate();
				break;
			case 5:
				return $this->getLastProcessedArCdrId();
				break;
			case 6:
				return $this->getBundleRate();
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
		$keys = ArRateIncrementalInfoPeer::getFieldNames($keyType);
		$result = array(
			$keys[0] => $this->getId(),
			$keys[1] => $this->getArPartyId(),
			$keys[2] => $this->getArRateId(),
			$keys[3] => $this->getPeriod(),
			$keys[4] => $this->getLastProcessedArCdrDate(),
			$keys[5] => $this->getLastProcessedArCdrId(),
			$keys[6] => $this->getBundleRate(),
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
		$pos = ArRateIncrementalInfoPeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
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
				$this->setArPartyId($value);
				break;
			case 2:
				$this->setArRateId($value);
				break;
			case 3:
				$this->setPeriod($value);
				break;
			case 4:
				$this->setLastProcessedArCdrDate($value);
				break;
			case 5:
				$this->setLastProcessedArCdrId($value);
				break;
			case 6:
				$this->setBundleRate($value);
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
		$keys = ArRateIncrementalInfoPeer::getFieldNames($keyType);

		if (array_key_exists($keys[0], $arr)) $this->setId($arr[$keys[0]]);
		if (array_key_exists($keys[1], $arr)) $this->setArPartyId($arr[$keys[1]]);
		if (array_key_exists($keys[2], $arr)) $this->setArRateId($arr[$keys[2]]);
		if (array_key_exists($keys[3], $arr)) $this->setPeriod($arr[$keys[3]]);
		if (array_key_exists($keys[4], $arr)) $this->setLastProcessedArCdrDate($arr[$keys[4]]);
		if (array_key_exists($keys[5], $arr)) $this->setLastProcessedArCdrId($arr[$keys[5]]);
		if (array_key_exists($keys[6], $arr)) $this->setBundleRate($arr[$keys[6]]);
	}

	/**
	 * Build a Criteria object containing the values of all modified columns in this object.
	 *
	 * @return     Criteria The Criteria object containing all modified values.
	 */
	public function buildCriteria()
	{
		$criteria = new Criteria(ArRateIncrementalInfoPeer::DATABASE_NAME);

		if ($this->isColumnModified(ArRateIncrementalInfoPeer::ID)) $criteria->add(ArRateIncrementalInfoPeer::ID, $this->id);
		if ($this->isColumnModified(ArRateIncrementalInfoPeer::AR_PARTY_ID)) $criteria->add(ArRateIncrementalInfoPeer::AR_PARTY_ID, $this->ar_party_id);
		if ($this->isColumnModified(ArRateIncrementalInfoPeer::AR_RATE_ID)) $criteria->add(ArRateIncrementalInfoPeer::AR_RATE_ID, $this->ar_rate_id);
		if ($this->isColumnModified(ArRateIncrementalInfoPeer::PERIOD)) $criteria->add(ArRateIncrementalInfoPeer::PERIOD, $this->period);
		if ($this->isColumnModified(ArRateIncrementalInfoPeer::LAST_PROCESSED_AR_CDR_DATE)) $criteria->add(ArRateIncrementalInfoPeer::LAST_PROCESSED_AR_CDR_DATE, $this->last_processed_ar_cdr_date);
		if ($this->isColumnModified(ArRateIncrementalInfoPeer::LAST_PROCESSED_AR_CDR_ID)) $criteria->add(ArRateIncrementalInfoPeer::LAST_PROCESSED_AR_CDR_ID, $this->last_processed_ar_cdr_id);
		if ($this->isColumnModified(ArRateIncrementalInfoPeer::BUNDLE_RATE)) $criteria->add(ArRateIncrementalInfoPeer::BUNDLE_RATE, $this->bundle_rate);

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
		$criteria = new Criteria(ArRateIncrementalInfoPeer::DATABASE_NAME);

		$criteria->add(ArRateIncrementalInfoPeer::ID, $this->id);

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
	 * @param      object $copyObj An object of ArRateIncrementalInfo (or compatible) type.
	 * @param      boolean $deepCopy Whether to also copy all rows that refer (by fkey) to the current row.
	 * @throws     PropelException
	 */
	public function copyInto($copyObj, $deepCopy = false)
	{

		$copyObj->setArPartyId($this->ar_party_id);

		$copyObj->setArRateId($this->ar_rate_id);

		$copyObj->setPeriod($this->period);

		$copyObj->setLastProcessedArCdrDate($this->last_processed_ar_cdr_date);

		$copyObj->setLastProcessedArCdrId($this->last_processed_ar_cdr_id);

		$copyObj->setBundleRate($this->bundle_rate);


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
	 * @return     ArRateIncrementalInfo Clone of current object.
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
	 * @return     ArRateIncrementalInfoPeer
	 */
	public function getPeer()
	{
		if (self::$peer === null) {
			self::$peer = new ArRateIncrementalInfoPeer();
		}
		return self::$peer;
	}

	/**
	 * Declares an association between this object and a ArParty object.
	 *
	 * @param      ArParty $v
	 * @return     ArRateIncrementalInfo The current object (for fluent API support)
	 * @throws     PropelException
	 */
	public function setArParty(ArParty $v = null)
	{
		if ($v === null) {
			$this->setArPartyId(NULL);
		} else {
			$this->setArPartyId($v->getId());
		}

		$this->aArParty = $v;

		// Add binding for other direction of this n:n relationship.
		// If this object has already been added to the ArParty object, it will not be re-added.
		if ($v !== null) {
			$v->addArRateIncrementalInfo($this);
		}

		return $this;
	}


	/**
	 * Get the associated ArParty object
	 *
	 * @param      PropelPDO Optional Connection object.
	 * @return     ArParty The associated ArParty object.
	 * @throws     PropelException
	 */
	public function getArParty(PropelPDO $con = null)
	{
		if ($this->aArParty === null && ($this->ar_party_id !== null)) {
			$this->aArParty = ArPartyPeer::retrieveByPk($this->ar_party_id);
			/* The following can be used additionally to
			   guarantee the related object contains a reference
			   to this object.  This level of coupling may, however, be
			   undesirable since it could result in an only partially populated collection
			   in the referenced object.
			   $this->aArParty->addArRateIncrementalInfos($this);
			 */
		}
		return $this->aArParty;
	}

	/**
	 * Declares an association between this object and a ArRate object.
	 *
	 * @param      ArRate $v
	 * @return     ArRateIncrementalInfo The current object (for fluent API support)
	 * @throws     PropelException
	 */
	public function setArRate(ArRate $v = null)
	{
		if ($v === null) {
			$this->setArRateId(NULL);
		} else {
			$this->setArRateId($v->getId());
		}

		$this->aArRate = $v;

		// Add binding for other direction of this n:n relationship.
		// If this object has already been added to the ArRate object, it will not be re-added.
		if ($v !== null) {
			$v->addArRateIncrementalInfo($this);
		}

		return $this;
	}


	/**
	 * Get the associated ArRate object
	 *
	 * @param      PropelPDO Optional Connection object.
	 * @return     ArRate The associated ArRate object.
	 * @throws     PropelException
	 */
	public function getArRate(PropelPDO $con = null)
	{
		if ($this->aArRate === null && ($this->ar_rate_id !== null)) {
			$this->aArRate = ArRatePeer::retrieveByPk($this->ar_rate_id);
			/* The following can be used additionally to
			   guarantee the related object contains a reference
			   to this object.  This level of coupling may, however, be
			   undesirable since it could result in an only partially populated collection
			   in the referenced object.
			   $this->aArRate->addArRateIncrementalInfos($this);
			 */
		}
		return $this->aArRate;
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

			$this->aArParty = null;
			$this->aArRate = null;
	}

} // BaseArRateIncrementalInfo
