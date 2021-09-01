<?php

/**
 * Base class that represents a row from the 'ar_specific_rate_calc' table.
 *
 * 
 *
 * @package    lib.model.om
 */
abstract class BaseArSpecificRateCalc extends BaseObject  implements Persistent {


	/**
	 * The Peer class.
	 * Instance provides a convenient way of calling static methods on a class
	 * that calling code may not be able to identify.
	 * @var        ArSpecificRateCalcPeer
	 */
	protected static $peer;

	/**
	 * The value for the id field.
	 * @var        int
	 */
	protected $id;

	/**
	 * The value for the note field.
	 * @var        string
	 */
	protected $note;

	/**
	 * The value for the ar_rate_id field.
	 * @var        int
	 */
	protected $ar_rate_id;

	/**
	 * The value for the specific_rate_name field.
	 * @var        string
	 */
	protected $specific_rate_name;

	/**
	 * The value for the price_category_name field.
	 * @var        string
	 */
	protected $price_category_name;

	/**
	 * The value for the mediumtext_specific_rate_in_match_all field.
	 * @var        string
	 */
	protected $mediumtext_specific_rate_in_match_all;

	/**
	 * The value for the mediumtext_specific_rate_in_match_exact field.
	 * @var        string
	 */
	protected $mediumtext_specific_rate_in_match_exact;

	/**
	 * The value for the mediumtext_specific_rate_out field.
	 * @var        string
	 */
	protected $mediumtext_specific_rate_out;

	/**
	 * The value for the rate_plan_out field.
	 * @var        string
	 */
	protected $rate_plan_out;

	/**
	 * The value for the mediumtext_base_rate_diff field.
	 * @var        string
	 */
	protected $mediumtext_base_rate_diff;

	/**
	 * The value for the calc_info field.
	 * @var        string
	 */
	protected $calc_info;

	/**
	 * The value for the calc_error field.
	 * @var        string
	 */
	protected $calc_error;

	/**
	 * The value for the is_recalc field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $is_recalc;

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
	
	const PEER = 'ArSpecificRateCalcPeer';

	/**
	 * Applies default values to this object.
	 * This method should be called from the object's constructor (or
	 * equivalent initialization method).
	 * @see        __construct()
	 */
	public function applyDefaultValues()
	{
		$this->is_recalc = false;
	}

	/**
	 * Initializes internal state of BaseArSpecificRateCalc object.
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
	 * Get the [note] column value.
	 * 
	 * @return     string
	 */
	public function getNote()
	{
		return $this->note;
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
	 * Get the [specific_rate_name] column value.
	 * 
	 * @return     string
	 */
	public function getSpecificRateName()
	{
		return $this->specific_rate_name;
	}

	/**
	 * Get the [price_category_name] column value.
	 * 
	 * @return     string
	 */
	public function getPriceCategoryName()
	{
		return $this->price_category_name;
	}

	/**
	 * Get the [mediumtext_specific_rate_in_match_all] column value.
	 * 
	 * @return     string
	 */
	public function getMediumtextSpecificRateInMatchAll()
	{
		return $this->mediumtext_specific_rate_in_match_all;
	}

	/**
	 * Get the [mediumtext_specific_rate_in_match_exact] column value.
	 * 
	 * @return     string
	 */
	public function getMediumtextSpecificRateInMatchExact()
	{
		return $this->mediumtext_specific_rate_in_match_exact;
	}

	/**
	 * Get the [mediumtext_specific_rate_out] column value.
	 * 
	 * @return     string
	 */
	public function getMediumtextSpecificRateOut()
	{
		return $this->mediumtext_specific_rate_out;
	}

	/**
	 * Get the [rate_plan_out] column value.
	 * 
	 * @return     string
	 */
	public function getRatePlanOut()
	{
		return $this->rate_plan_out;
	}

	/**
	 * Get the [mediumtext_base_rate_diff] column value.
	 * 
	 * @return     string
	 */
	public function getMediumtextBaseRateDiff()
	{
		return $this->mediumtext_base_rate_diff;
	}

	/**
	 * Get the [calc_info] column value.
	 * 
	 * @return     string
	 */
	public function getCalcInfo()
	{
		return $this->calc_info;
	}

	/**
	 * Get the [calc_error] column value.
	 * 
	 * @return     string
	 */
	public function getCalcError()
	{
		return $this->calc_error;
	}

	/**
	 * Get the [is_recalc] column value.
	 * 
	 * @return     boolean
	 */
	public function getIsRecalc()
	{
		return $this->is_recalc;
	}

	/**
	 * Set the value of [id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArSpecificRateCalc The current object (for fluent API support)
	 */
	public function setId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->id !== $v) {
			$this->id = $v;
			$this->modifiedColumns[] = ArSpecificRateCalcPeer::ID;
		}

		return $this;
	} // setId()

	/**
	 * Set the value of [note] column.
	 * 
	 * @param      string $v new value
	 * @return     ArSpecificRateCalc The current object (for fluent API support)
	 */
	public function setNote($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->note !== $v) {
			$this->note = $v;
			$this->modifiedColumns[] = ArSpecificRateCalcPeer::NOTE;
		}

		return $this;
	} // setNote()

	/**
	 * Set the value of [ar_rate_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArSpecificRateCalc The current object (for fluent API support)
	 */
	public function setArRateId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->ar_rate_id !== $v) {
			$this->ar_rate_id = $v;
			$this->modifiedColumns[] = ArSpecificRateCalcPeer::AR_RATE_ID;
		}

		if ($this->aArRate !== null && $this->aArRate->getId() !== $v) {
			$this->aArRate = null;
		}

		return $this;
	} // setArRateId()

	/**
	 * Set the value of [specific_rate_name] column.
	 * 
	 * @param      string $v new value
	 * @return     ArSpecificRateCalc The current object (for fluent API support)
	 */
	public function setSpecificRateName($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->specific_rate_name !== $v) {
			$this->specific_rate_name = $v;
			$this->modifiedColumns[] = ArSpecificRateCalcPeer::SPECIFIC_RATE_NAME;
		}

		return $this;
	} // setSpecificRateName()

	/**
	 * Set the value of [price_category_name] column.
	 * 
	 * @param      string $v new value
	 * @return     ArSpecificRateCalc The current object (for fluent API support)
	 */
	public function setPriceCategoryName($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->price_category_name !== $v) {
			$this->price_category_name = $v;
			$this->modifiedColumns[] = ArSpecificRateCalcPeer::PRICE_CATEGORY_NAME;
		}

		return $this;
	} // setPriceCategoryName()

	/**
	 * Set the value of [mediumtext_specific_rate_in_match_all] column.
	 * 
	 * @param      string $v new value
	 * @return     ArSpecificRateCalc The current object (for fluent API support)
	 */
	public function setMediumtextSpecificRateInMatchAll($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->mediumtext_specific_rate_in_match_all !== $v) {
			$this->mediumtext_specific_rate_in_match_all = $v;
			$this->modifiedColumns[] = ArSpecificRateCalcPeer::MEDIUMTEXT_SPECIFIC_RATE_IN_MATCH_ALL;
		}

		return $this;
	} // setMediumtextSpecificRateInMatchAll()

	/**
	 * Set the value of [mediumtext_specific_rate_in_match_exact] column.
	 * 
	 * @param      string $v new value
	 * @return     ArSpecificRateCalc The current object (for fluent API support)
	 */
	public function setMediumtextSpecificRateInMatchExact($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->mediumtext_specific_rate_in_match_exact !== $v) {
			$this->mediumtext_specific_rate_in_match_exact = $v;
			$this->modifiedColumns[] = ArSpecificRateCalcPeer::MEDIUMTEXT_SPECIFIC_RATE_IN_MATCH_EXACT;
		}

		return $this;
	} // setMediumtextSpecificRateInMatchExact()

	/**
	 * Set the value of [mediumtext_specific_rate_out] column.
	 * 
	 * @param      string $v new value
	 * @return     ArSpecificRateCalc The current object (for fluent API support)
	 */
	public function setMediumtextSpecificRateOut($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->mediumtext_specific_rate_out !== $v) {
			$this->mediumtext_specific_rate_out = $v;
			$this->modifiedColumns[] = ArSpecificRateCalcPeer::MEDIUMTEXT_SPECIFIC_RATE_OUT;
		}

		return $this;
	} // setMediumtextSpecificRateOut()

	/**
	 * Set the value of [rate_plan_out] column.
	 * 
	 * @param      string $v new value
	 * @return     ArSpecificRateCalc The current object (for fluent API support)
	 */
	public function setRatePlanOut($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->rate_plan_out !== $v) {
			$this->rate_plan_out = $v;
			$this->modifiedColumns[] = ArSpecificRateCalcPeer::RATE_PLAN_OUT;
		}

		return $this;
	} // setRatePlanOut()

	/**
	 * Set the value of [mediumtext_base_rate_diff] column.
	 * 
	 * @param      string $v new value
	 * @return     ArSpecificRateCalc The current object (for fluent API support)
	 */
	public function setMediumtextBaseRateDiff($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->mediumtext_base_rate_diff !== $v) {
			$this->mediumtext_base_rate_diff = $v;
			$this->modifiedColumns[] = ArSpecificRateCalcPeer::MEDIUMTEXT_BASE_RATE_DIFF;
		}

		return $this;
	} // setMediumtextBaseRateDiff()

	/**
	 * Set the value of [calc_info] column.
	 * 
	 * @param      string $v new value
	 * @return     ArSpecificRateCalc The current object (for fluent API support)
	 */
	public function setCalcInfo($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->calc_info !== $v) {
			$this->calc_info = $v;
			$this->modifiedColumns[] = ArSpecificRateCalcPeer::CALC_INFO;
		}

		return $this;
	} // setCalcInfo()

	/**
	 * Set the value of [calc_error] column.
	 * 
	 * @param      string $v new value
	 * @return     ArSpecificRateCalc The current object (for fluent API support)
	 */
	public function setCalcError($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->calc_error !== $v) {
			$this->calc_error = $v;
			$this->modifiedColumns[] = ArSpecificRateCalcPeer::CALC_ERROR;
		}

		return $this;
	} // setCalcError()

	/**
	 * Set the value of [is_recalc] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArSpecificRateCalc The current object (for fluent API support)
	 */
	public function setIsRecalc($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->is_recalc !== $v || $this->isNew()) {
			$this->is_recalc = $v;
			$this->modifiedColumns[] = ArSpecificRateCalcPeer::IS_RECALC;
		}

		return $this;
	} // setIsRecalc()

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
			if ($this->is_recalc !== false) {
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
			$this->note = ($row[$startcol + 1] !== null) ? (string) $row[$startcol + 1] : null;
			$this->ar_rate_id = ($row[$startcol + 2] !== null) ? (int) $row[$startcol + 2] : null;
			$this->specific_rate_name = ($row[$startcol + 3] !== null) ? (string) $row[$startcol + 3] : null;
			$this->price_category_name = ($row[$startcol + 4] !== null) ? (string) $row[$startcol + 4] : null;
			$this->mediumtext_specific_rate_in_match_all = ($row[$startcol + 5] !== null) ? (string) $row[$startcol + 5] : null;
			$this->mediumtext_specific_rate_in_match_exact = ($row[$startcol + 6] !== null) ? (string) $row[$startcol + 6] : null;
			$this->mediumtext_specific_rate_out = ($row[$startcol + 7] !== null) ? (string) $row[$startcol + 7] : null;
			$this->rate_plan_out = ($row[$startcol + 8] !== null) ? (string) $row[$startcol + 8] : null;
			$this->mediumtext_base_rate_diff = ($row[$startcol + 9] !== null) ? (string) $row[$startcol + 9] : null;
			$this->calc_info = ($row[$startcol + 10] !== null) ? (string) $row[$startcol + 10] : null;
			$this->calc_error = ($row[$startcol + 11] !== null) ? (string) $row[$startcol + 11] : null;
			$this->is_recalc = ($row[$startcol + 12] !== null) ? (boolean) $row[$startcol + 12] : null;
			$this->resetModified();

			$this->setNew(false);

			if ($rehydrate) {
				$this->ensureConsistency();
			}

			// FIXME - using NUM_COLUMNS may be clearer.
			return $startcol + 13; // 13 = ArSpecificRateCalcPeer::NUM_COLUMNS - ArSpecificRateCalcPeer::NUM_LAZY_LOAD_COLUMNS).

		} catch (Exception $e) {
			throw new PropelException("Error populating ArSpecificRateCalc object", $e);
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
			$con = Propel::getConnection(ArSpecificRateCalcPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		// We don't need to alter the object instance pool; we're just modifying this instance
		// already in the pool.

		$stmt = ArSpecificRateCalcPeer::doSelectStmt($this->buildPkeyCriteria(), $con);
		$row = $stmt->fetch(PDO::FETCH_NUM);
		$stmt->closeCursor();
		if (!$row) {
			throw new PropelException('Cannot find matching row in the database to reload object values.');
		}
		$this->hydrate($row, 0, true); // rehydrate

		if ($deep) {  // also de-associate any related objects?

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
			$con = Propel::getConnection(ArSpecificRateCalcPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}
		
		$con->beginTransaction();
		try {
			$ret = $this->preDelete($con);
			if ($ret) {
				ArSpecificRateCalcPeer::doDelete($this, $con);
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
			$con = Propel::getConnection(ArSpecificRateCalcPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
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
				ArSpecificRateCalcPeer::addInstanceToPool($this);
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

			if ($this->aArRate !== null) {
				if ($this->aArRate->isModified() || $this->aArRate->isNew()) {
					$affectedRows += $this->aArRate->save($con);
				}
				$this->setArRate($this->aArRate);
			}

			if ($this->isNew() ) {
				$this->modifiedColumns[] = ArSpecificRateCalcPeer::ID;
			}

			// If this object has been modified, then save it to the database.
			if ($this->isModified()) {
				if ($this->isNew()) {
					$pk = ArSpecificRateCalcPeer::doInsert($this, $con);
					$affectedRows += 1; // we are assuming that there is only 1 row per doInsert() which
										 // should always be true here (even though technically
										 // BasePeer::doInsert() can insert multiple rows).

					$this->setId($pk);  //[IMV] update autoincrement primary key

					$this->setNew(false);
				} else {
					$affectedRows += ArSpecificRateCalcPeer::doUpdate($this, $con);
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

			if ($this->aArRate !== null) {
				if (!$this->aArRate->validate($columns)) {
					$failureMap = array_merge($failureMap, $this->aArRate->getValidationFailures());
				}
			}


			if (($retval = ArSpecificRateCalcPeer::doValidate($this, $columns)) !== true) {
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
		$pos = ArSpecificRateCalcPeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
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
				return $this->getNote();
				break;
			case 2:
				return $this->getArRateId();
				break;
			case 3:
				return $this->getSpecificRateName();
				break;
			case 4:
				return $this->getPriceCategoryName();
				break;
			case 5:
				return $this->getMediumtextSpecificRateInMatchAll();
				break;
			case 6:
				return $this->getMediumtextSpecificRateInMatchExact();
				break;
			case 7:
				return $this->getMediumtextSpecificRateOut();
				break;
			case 8:
				return $this->getRatePlanOut();
				break;
			case 9:
				return $this->getMediumtextBaseRateDiff();
				break;
			case 10:
				return $this->getCalcInfo();
				break;
			case 11:
				return $this->getCalcError();
				break;
			case 12:
				return $this->getIsRecalc();
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
		$keys = ArSpecificRateCalcPeer::getFieldNames($keyType);
		$result = array(
			$keys[0] => $this->getId(),
			$keys[1] => $this->getNote(),
			$keys[2] => $this->getArRateId(),
			$keys[3] => $this->getSpecificRateName(),
			$keys[4] => $this->getPriceCategoryName(),
			$keys[5] => $this->getMediumtextSpecificRateInMatchAll(),
			$keys[6] => $this->getMediumtextSpecificRateInMatchExact(),
			$keys[7] => $this->getMediumtextSpecificRateOut(),
			$keys[8] => $this->getRatePlanOut(),
			$keys[9] => $this->getMediumtextBaseRateDiff(),
			$keys[10] => $this->getCalcInfo(),
			$keys[11] => $this->getCalcError(),
			$keys[12] => $this->getIsRecalc(),
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
		$pos = ArSpecificRateCalcPeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
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
				$this->setNote($value);
				break;
			case 2:
				$this->setArRateId($value);
				break;
			case 3:
				$this->setSpecificRateName($value);
				break;
			case 4:
				$this->setPriceCategoryName($value);
				break;
			case 5:
				$this->setMediumtextSpecificRateInMatchAll($value);
				break;
			case 6:
				$this->setMediumtextSpecificRateInMatchExact($value);
				break;
			case 7:
				$this->setMediumtextSpecificRateOut($value);
				break;
			case 8:
				$this->setRatePlanOut($value);
				break;
			case 9:
				$this->setMediumtextBaseRateDiff($value);
				break;
			case 10:
				$this->setCalcInfo($value);
				break;
			case 11:
				$this->setCalcError($value);
				break;
			case 12:
				$this->setIsRecalc($value);
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
		$keys = ArSpecificRateCalcPeer::getFieldNames($keyType);

		if (array_key_exists($keys[0], $arr)) $this->setId($arr[$keys[0]]);
		if (array_key_exists($keys[1], $arr)) $this->setNote($arr[$keys[1]]);
		if (array_key_exists($keys[2], $arr)) $this->setArRateId($arr[$keys[2]]);
		if (array_key_exists($keys[3], $arr)) $this->setSpecificRateName($arr[$keys[3]]);
		if (array_key_exists($keys[4], $arr)) $this->setPriceCategoryName($arr[$keys[4]]);
		if (array_key_exists($keys[5], $arr)) $this->setMediumtextSpecificRateInMatchAll($arr[$keys[5]]);
		if (array_key_exists($keys[6], $arr)) $this->setMediumtextSpecificRateInMatchExact($arr[$keys[6]]);
		if (array_key_exists($keys[7], $arr)) $this->setMediumtextSpecificRateOut($arr[$keys[7]]);
		if (array_key_exists($keys[8], $arr)) $this->setRatePlanOut($arr[$keys[8]]);
		if (array_key_exists($keys[9], $arr)) $this->setMediumtextBaseRateDiff($arr[$keys[9]]);
		if (array_key_exists($keys[10], $arr)) $this->setCalcInfo($arr[$keys[10]]);
		if (array_key_exists($keys[11], $arr)) $this->setCalcError($arr[$keys[11]]);
		if (array_key_exists($keys[12], $arr)) $this->setIsRecalc($arr[$keys[12]]);
	}

	/**
	 * Build a Criteria object containing the values of all modified columns in this object.
	 *
	 * @return     Criteria The Criteria object containing all modified values.
	 */
	public function buildCriteria()
	{
		$criteria = new Criteria(ArSpecificRateCalcPeer::DATABASE_NAME);

		if ($this->isColumnModified(ArSpecificRateCalcPeer::ID)) $criteria->add(ArSpecificRateCalcPeer::ID, $this->id);
		if ($this->isColumnModified(ArSpecificRateCalcPeer::NOTE)) $criteria->add(ArSpecificRateCalcPeer::NOTE, $this->note);
		if ($this->isColumnModified(ArSpecificRateCalcPeer::AR_RATE_ID)) $criteria->add(ArSpecificRateCalcPeer::AR_RATE_ID, $this->ar_rate_id);
		if ($this->isColumnModified(ArSpecificRateCalcPeer::SPECIFIC_RATE_NAME)) $criteria->add(ArSpecificRateCalcPeer::SPECIFIC_RATE_NAME, $this->specific_rate_name);
		if ($this->isColumnModified(ArSpecificRateCalcPeer::PRICE_CATEGORY_NAME)) $criteria->add(ArSpecificRateCalcPeer::PRICE_CATEGORY_NAME, $this->price_category_name);
		if ($this->isColumnModified(ArSpecificRateCalcPeer::MEDIUMTEXT_SPECIFIC_RATE_IN_MATCH_ALL)) $criteria->add(ArSpecificRateCalcPeer::MEDIUMTEXT_SPECIFIC_RATE_IN_MATCH_ALL, $this->mediumtext_specific_rate_in_match_all);
		if ($this->isColumnModified(ArSpecificRateCalcPeer::MEDIUMTEXT_SPECIFIC_RATE_IN_MATCH_EXACT)) $criteria->add(ArSpecificRateCalcPeer::MEDIUMTEXT_SPECIFIC_RATE_IN_MATCH_EXACT, $this->mediumtext_specific_rate_in_match_exact);
		if ($this->isColumnModified(ArSpecificRateCalcPeer::MEDIUMTEXT_SPECIFIC_RATE_OUT)) $criteria->add(ArSpecificRateCalcPeer::MEDIUMTEXT_SPECIFIC_RATE_OUT, $this->mediumtext_specific_rate_out);
		if ($this->isColumnModified(ArSpecificRateCalcPeer::RATE_PLAN_OUT)) $criteria->add(ArSpecificRateCalcPeer::RATE_PLAN_OUT, $this->rate_plan_out);
		if ($this->isColumnModified(ArSpecificRateCalcPeer::MEDIUMTEXT_BASE_RATE_DIFF)) $criteria->add(ArSpecificRateCalcPeer::MEDIUMTEXT_BASE_RATE_DIFF, $this->mediumtext_base_rate_diff);
		if ($this->isColumnModified(ArSpecificRateCalcPeer::CALC_INFO)) $criteria->add(ArSpecificRateCalcPeer::CALC_INFO, $this->calc_info);
		if ($this->isColumnModified(ArSpecificRateCalcPeer::CALC_ERROR)) $criteria->add(ArSpecificRateCalcPeer::CALC_ERROR, $this->calc_error);
		if ($this->isColumnModified(ArSpecificRateCalcPeer::IS_RECALC)) $criteria->add(ArSpecificRateCalcPeer::IS_RECALC, $this->is_recalc);

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
		$criteria = new Criteria(ArSpecificRateCalcPeer::DATABASE_NAME);

		$criteria->add(ArSpecificRateCalcPeer::ID, $this->id);

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
	 * @param      object $copyObj An object of ArSpecificRateCalc (or compatible) type.
	 * @param      boolean $deepCopy Whether to also copy all rows that refer (by fkey) to the current row.
	 * @throws     PropelException
	 */
	public function copyInto($copyObj, $deepCopy = false)
	{

		$copyObj->setNote($this->note);

		$copyObj->setArRateId($this->ar_rate_id);

		$copyObj->setSpecificRateName($this->specific_rate_name);

		$copyObj->setPriceCategoryName($this->price_category_name);

		$copyObj->setMediumtextSpecificRateInMatchAll($this->mediumtext_specific_rate_in_match_all);

		$copyObj->setMediumtextSpecificRateInMatchExact($this->mediumtext_specific_rate_in_match_exact);

		$copyObj->setMediumtextSpecificRateOut($this->mediumtext_specific_rate_out);

		$copyObj->setRatePlanOut($this->rate_plan_out);

		$copyObj->setMediumtextBaseRateDiff($this->mediumtext_base_rate_diff);

		$copyObj->setCalcInfo($this->calc_info);

		$copyObj->setCalcError($this->calc_error);

		$copyObj->setIsRecalc($this->is_recalc);


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
	 * @return     ArSpecificRateCalc Clone of current object.
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
	 * @return     ArSpecificRateCalcPeer
	 */
	public function getPeer()
	{
		if (self::$peer === null) {
			self::$peer = new ArSpecificRateCalcPeer();
		}
		return self::$peer;
	}

	/**
	 * Declares an association between this object and a ArRate object.
	 *
	 * @param      ArRate $v
	 * @return     ArSpecificRateCalc The current object (for fluent API support)
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
			$v->addArSpecificRateCalc($this);
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
			   $this->aArRate->addArSpecificRateCalcs($this);
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

			$this->aArRate = null;
	}

} // BaseArSpecificRateCalc
