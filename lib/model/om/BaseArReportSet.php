<?php

/**
 * Base class that represents a row from the 'ar_report_set' table.
 *
 * 
 *
 * @package    lib.model.om
 */
abstract class BaseArReportSet extends BaseObject  implements Persistent {


	/**
	 * The Peer class.
	 * Instance provides a convenient way of calling static methods on a class
	 * that calling code may not be able to identify.
	 * @var        ArReportSetPeer
	 */
	protected static $peer;

	/**
	 * The value for the id field.
	 * @var        int
	 */
	protected $id;

	/**
	 * The value for the ar_report_scheduler_id field.
	 * @var        int
	 */
	protected $ar_report_scheduler_id;

	/**
	 * The value for the from_date field.
	 * @var        string
	 */
	protected $from_date;

	/**
	 * The value for the to_date field.
	 * @var        string
	 */
	protected $to_date;

	/**
	 * The value for the must_be_reviewed field.
	 * Note: this column has a database default value of: true
	 * @var        boolean
	 */
	protected $must_be_reviewed;

	/**
	 * The value for the postponed_fields_are_updated field.
	 * Note: this column has a database default value of: true
	 * @var        boolean
	 */
	protected $postponed_fields_are_updated;

	/**
	 * The value for the postponed_reports field.
	 * Note: this column has a database default value of: 0
	 * @var        int
	 */
	protected $postponed_reports;

	/**
	 * The value for the postponed_amount field.
	 * Note: this column has a database default value of: '0'
	 * @var        string
	 */
	protected $postponed_amount;

	/**
	 * The value for the reports field.
	 * Note: this column has a database default value of: 0
	 * @var        int
	 */
	protected $reports;

	/**
	 * The value for the amount field.
	 * Note: this column has a database default value of: '0'
	 * @var        string
	 */
	protected $amount;

	/**
	 * @var        ArReportScheduler
	 */
	protected $aArReportScheduler;

	/**
	 * @var        array ArReport[] Collection to store aggregation of ArReport objects.
	 */
	protected $collArReportsRelatedByArReportSetId;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArReportsRelatedByArReportSetId.
	 */
	private $lastArReportRelatedByArReportSetIdCriteria = null;

	/**
	 * @var        array ArReport[] Collection to store aggregation of ArReport objects.
	 */
	protected $collArReportsRelatedByAboutArReportSetId;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArReportsRelatedByAboutArReportSetId.
	 */
	private $lastArReportRelatedByAboutArReportSetIdCriteria = null;

	/**
	 * @var        array ArPostponedReport[] Collection to store aggregation of ArPostponedReport objects.
	 */
	protected $collArPostponedReports;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArPostponedReports.
	 */
	private $lastArPostponedReportCriteria = null;

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
	
	const PEER = 'ArReportSetPeer';

	/**
	 * Applies default values to this object.
	 * This method should be called from the object's constructor (or
	 * equivalent initialization method).
	 * @see        __construct()
	 */
	public function applyDefaultValues()
	{
		$this->must_be_reviewed = true;
		$this->postponed_fields_are_updated = true;
		$this->postponed_reports = 0;
		$this->postponed_amount = '0';
		$this->reports = 0;
		$this->amount = '0';
	}

	/**
	 * Initializes internal state of BaseArReportSet object.
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
	 * Get the [ar_report_scheduler_id] column value.
	 * 
	 * @return     int
	 */
	public function getArReportSchedulerId()
	{
		return $this->ar_report_scheduler_id;
	}

	/**
	 * Get the [optionally formatted] temporal [from_date] column value.
	 * 
	 *
	 * @param      string $format The date/time format string (either date()-style or strftime()-style).
	 *							If format is NULL, then the raw DateTime object will be returned.
	 * @return     mixed Formatted date/time value as string or DateTime object (if format is NULL), NULL if column is NULL, and 0 if column value is 0000-00-00 00:00:00
	 * @throws     PropelException - if unable to parse/validate the date/time value.
	 */
	public function getFromDate($format = 'Y-m-d H:i:s')
	{
		if ($this->from_date === null) {
			return null;
		}


		if ($this->from_date === '0000-00-00 00:00:00') {
			// while technically this is not a default value of NULL,
			// this seems to be closest in meaning.
			return null;
		} else {
			try {
				$dt = new DateTime($this->from_date);
			} catch (Exception $x) {
				throw new PropelException("Internally stored date/time/timestamp value could not be converted to DateTime: " . var_export($this->from_date, true), $x);
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
	 * Get the [optionally formatted] temporal [to_date] column value.
	 * 
	 *
	 * @param      string $format The date/time format string (either date()-style or strftime()-style).
	 *							If format is NULL, then the raw DateTime object will be returned.
	 * @return     mixed Formatted date/time value as string or DateTime object (if format is NULL), NULL if column is NULL, and 0 if column value is 0000-00-00 00:00:00
	 * @throws     PropelException - if unable to parse/validate the date/time value.
	 */
	public function getToDate($format = 'Y-m-d H:i:s')
	{
		if ($this->to_date === null) {
			return null;
		}


		if ($this->to_date === '0000-00-00 00:00:00') {
			// while technically this is not a default value of NULL,
			// this seems to be closest in meaning.
			return null;
		} else {
			try {
				$dt = new DateTime($this->to_date);
			} catch (Exception $x) {
				throw new PropelException("Internally stored date/time/timestamp value could not be converted to DateTime: " . var_export($this->to_date, true), $x);
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
	 * Get the [must_be_reviewed] column value.
	 * 
	 * @return     boolean
	 */
	public function getMustBeReviewed()
	{
		return $this->must_be_reviewed;
	}

	/**
	 * Get the [postponed_fields_are_updated] column value.
	 * 
	 * @return     boolean
	 */
	public function getPostponedFieldsAreUpdated()
	{
		return $this->postponed_fields_are_updated;
	}

	/**
	 * Get the [postponed_reports] column value.
	 * 
	 * @return     int
	 */
	public function getPostponedReports()
	{
		return $this->postponed_reports;
	}

	/**
	 * Get the [postponed_amount] column value.
	 * 
	 * @return     string
	 */
	public function getPostponedAmount()
	{
		return $this->postponed_amount;
	}

	/**
	 * Get the [reports] column value.
	 * 
	 * @return     int
	 */
	public function getReports()
	{
		return $this->reports;
	}

	/**
	 * Get the [amount] column value.
	 * 
	 * @return     string
	 */
	public function getAmount()
	{
		return $this->amount;
	}

	/**
	 * Set the value of [id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArReportSet The current object (for fluent API support)
	 */
	public function setId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->id !== $v) {
			$this->id = $v;
			$this->modifiedColumns[] = ArReportSetPeer::ID;
		}

		return $this;
	} // setId()

	/**
	 * Set the value of [ar_report_scheduler_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArReportSet The current object (for fluent API support)
	 */
	public function setArReportSchedulerId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->ar_report_scheduler_id !== $v) {
			$this->ar_report_scheduler_id = $v;
			$this->modifiedColumns[] = ArReportSetPeer::AR_REPORT_SCHEDULER_ID;
		}

		if ($this->aArReportScheduler !== null && $this->aArReportScheduler->getId() !== $v) {
			$this->aArReportScheduler = null;
		}

		return $this;
	} // setArReportSchedulerId()

	/**
	 * Sets the value of [from_date] column to a normalized version of the date/time value specified.
	 * 
	 * @param      mixed $v string, integer (timestamp), or DateTime value.  Empty string will
	 *						be treated as NULL for temporal objects.
	 * @return     ArReportSet The current object (for fluent API support)
	 */
	public function setFromDate($v)
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

		if ( $this->from_date !== null || $dt !== null ) {
			// (nested ifs are a little easier to read in this case)

			$currNorm = ($this->from_date !== null && $tmpDt = new DateTime($this->from_date)) ? $tmpDt->format('Y-m-d H:i:s') : null;
			$newNorm = ($dt !== null) ? $dt->format('Y-m-d H:i:s') : null;

			if ( ($currNorm !== $newNorm) // normalized values don't match 
					)
			{
				$this->from_date = ($dt ? $dt->format('Y-m-d H:i:s') : null);
				$this->modifiedColumns[] = ArReportSetPeer::FROM_DATE;
			}
		} // if either are not null

		return $this;
	} // setFromDate()

	/**
	 * Sets the value of [to_date] column to a normalized version of the date/time value specified.
	 * 
	 * @param      mixed $v string, integer (timestamp), or DateTime value.  Empty string will
	 *						be treated as NULL for temporal objects.
	 * @return     ArReportSet The current object (for fluent API support)
	 */
	public function setToDate($v)
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

		if ( $this->to_date !== null || $dt !== null ) {
			// (nested ifs are a little easier to read in this case)

			$currNorm = ($this->to_date !== null && $tmpDt = new DateTime($this->to_date)) ? $tmpDt->format('Y-m-d H:i:s') : null;
			$newNorm = ($dt !== null) ? $dt->format('Y-m-d H:i:s') : null;

			if ( ($currNorm !== $newNorm) // normalized values don't match 
					)
			{
				$this->to_date = ($dt ? $dt->format('Y-m-d H:i:s') : null);
				$this->modifiedColumns[] = ArReportSetPeer::TO_DATE;
			}
		} // if either are not null

		return $this;
	} // setToDate()

	/**
	 * Set the value of [must_be_reviewed] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArReportSet The current object (for fluent API support)
	 */
	public function setMustBeReviewed($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->must_be_reviewed !== $v || $this->isNew()) {
			$this->must_be_reviewed = $v;
			$this->modifiedColumns[] = ArReportSetPeer::MUST_BE_REVIEWED;
		}

		return $this;
	} // setMustBeReviewed()

	/**
	 * Set the value of [postponed_fields_are_updated] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArReportSet The current object (for fluent API support)
	 */
	public function setPostponedFieldsAreUpdated($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->postponed_fields_are_updated !== $v || $this->isNew()) {
			$this->postponed_fields_are_updated = $v;
			$this->modifiedColumns[] = ArReportSetPeer::POSTPONED_FIELDS_ARE_UPDATED;
		}

		return $this;
	} // setPostponedFieldsAreUpdated()

	/**
	 * Set the value of [postponed_reports] column.
	 * 
	 * @param      int $v new value
	 * @return     ArReportSet The current object (for fluent API support)
	 */
	public function setPostponedReports($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->postponed_reports !== $v || $this->isNew()) {
			$this->postponed_reports = $v;
			$this->modifiedColumns[] = ArReportSetPeer::POSTPONED_REPORTS;
		}

		return $this;
	} // setPostponedReports()

	/**
	 * Set the value of [postponed_amount] column.
	 * 
	 * @param      string $v new value
	 * @return     ArReportSet The current object (for fluent API support)
	 */
	public function setPostponedAmount($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->postponed_amount !== $v || $this->isNew()) {
			$this->postponed_amount = $v;
			$this->modifiedColumns[] = ArReportSetPeer::POSTPONED_AMOUNT;
		}

		return $this;
	} // setPostponedAmount()

	/**
	 * Set the value of [reports] column.
	 * 
	 * @param      int $v new value
	 * @return     ArReportSet The current object (for fluent API support)
	 */
	public function setReports($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->reports !== $v || $this->isNew()) {
			$this->reports = $v;
			$this->modifiedColumns[] = ArReportSetPeer::REPORTS;
		}

		return $this;
	} // setReports()

	/**
	 * Set the value of [amount] column.
	 * 
	 * @param      string $v new value
	 * @return     ArReportSet The current object (for fluent API support)
	 */
	public function setAmount($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->amount !== $v || $this->isNew()) {
			$this->amount = $v;
			$this->modifiedColumns[] = ArReportSetPeer::AMOUNT;
		}

		return $this;
	} // setAmount()

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
			if ($this->must_be_reviewed !== true) {
				return false;
			}

			if ($this->postponed_fields_are_updated !== true) {
				return false;
			}

			if ($this->postponed_reports !== 0) {
				return false;
			}

			if ($this->postponed_amount !== '0') {
				return false;
			}

			if ($this->reports !== 0) {
				return false;
			}

			if ($this->amount !== '0') {
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
			$this->ar_report_scheduler_id = ($row[$startcol + 1] !== null) ? (int) $row[$startcol + 1] : null;
			$this->from_date = ($row[$startcol + 2] !== null) ? (string) $row[$startcol + 2] : null;
			$this->to_date = ($row[$startcol + 3] !== null) ? (string) $row[$startcol + 3] : null;
			$this->must_be_reviewed = ($row[$startcol + 4] !== null) ? (boolean) $row[$startcol + 4] : null;
			$this->postponed_fields_are_updated = ($row[$startcol + 5] !== null) ? (boolean) $row[$startcol + 5] : null;
			$this->postponed_reports = ($row[$startcol + 6] !== null) ? (int) $row[$startcol + 6] : null;
			$this->postponed_amount = ($row[$startcol + 7] !== null) ? (string) $row[$startcol + 7] : null;
			$this->reports = ($row[$startcol + 8] !== null) ? (int) $row[$startcol + 8] : null;
			$this->amount = ($row[$startcol + 9] !== null) ? (string) $row[$startcol + 9] : null;
			$this->resetModified();

			$this->setNew(false);

			if ($rehydrate) {
				$this->ensureConsistency();
			}

			// FIXME - using NUM_COLUMNS may be clearer.
			return $startcol + 10; // 10 = ArReportSetPeer::NUM_COLUMNS - ArReportSetPeer::NUM_LAZY_LOAD_COLUMNS).

		} catch (Exception $e) {
			throw new PropelException("Error populating ArReportSet object", $e);
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

		if ($this->aArReportScheduler !== null && $this->ar_report_scheduler_id !== $this->aArReportScheduler->getId()) {
			$this->aArReportScheduler = null;
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
			$con = Propel::getConnection(ArReportSetPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		// We don't need to alter the object instance pool; we're just modifying this instance
		// already in the pool.

		$stmt = ArReportSetPeer::doSelectStmt($this->buildPkeyCriteria(), $con);
		$row = $stmt->fetch(PDO::FETCH_NUM);
		$stmt->closeCursor();
		if (!$row) {
			throw new PropelException('Cannot find matching row in the database to reload object values.');
		}
		$this->hydrate($row, 0, true); // rehydrate

		if ($deep) {  // also de-associate any related objects?

			$this->aArReportScheduler = null;
			$this->collArReportsRelatedByArReportSetId = null;
			$this->lastArReportRelatedByArReportSetIdCriteria = null;

			$this->collArReportsRelatedByAboutArReportSetId = null;
			$this->lastArReportRelatedByAboutArReportSetIdCriteria = null;

			$this->collArPostponedReports = null;
			$this->lastArPostponedReportCriteria = null;

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
			$con = Propel::getConnection(ArReportSetPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}
		
		$con->beginTransaction();
		try {
			$ret = $this->preDelete($con);
			if ($ret) {
				ArReportSetPeer::doDelete($this, $con);
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
			$con = Propel::getConnection(ArReportSetPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
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
				ArReportSetPeer::addInstanceToPool($this);
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

			if ($this->aArReportScheduler !== null) {
				if ($this->aArReportScheduler->isModified() || $this->aArReportScheduler->isNew()) {
					$affectedRows += $this->aArReportScheduler->save($con);
				}
				$this->setArReportScheduler($this->aArReportScheduler);
			}

			if ($this->isNew() ) {
				$this->modifiedColumns[] = ArReportSetPeer::ID;
			}

			// If this object has been modified, then save it to the database.
			if ($this->isModified()) {
				if ($this->isNew()) {
					$pk = ArReportSetPeer::doInsert($this, $con);
					$affectedRows += 1; // we are assuming that there is only 1 row per doInsert() which
										 // should always be true here (even though technically
										 // BasePeer::doInsert() can insert multiple rows).

					$this->setId($pk);  //[IMV] update autoincrement primary key

					$this->setNew(false);
				} else {
					$affectedRows += ArReportSetPeer::doUpdate($this, $con);
				}

				$this->resetModified(); // [HL] After being saved an object is no longer 'modified'
			}

			if ($this->collArReportsRelatedByArReportSetId !== null) {
				foreach ($this->collArReportsRelatedByArReportSetId as $referrerFK) {
					if (!$referrerFK->isDeleted()) {
						$affectedRows += $referrerFK->save($con);
					}
				}
			}

			if ($this->collArReportsRelatedByAboutArReportSetId !== null) {
				foreach ($this->collArReportsRelatedByAboutArReportSetId as $referrerFK) {
					if (!$referrerFK->isDeleted()) {
						$affectedRows += $referrerFK->save($con);
					}
				}
			}

			if ($this->collArPostponedReports !== null) {
				foreach ($this->collArPostponedReports as $referrerFK) {
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


			// We call the validate method on the following object(s) if they
			// were passed to this object by their coresponding set
			// method.  This object relates to these object(s) by a
			// foreign key reference.

			if ($this->aArReportScheduler !== null) {
				if (!$this->aArReportScheduler->validate($columns)) {
					$failureMap = array_merge($failureMap, $this->aArReportScheduler->getValidationFailures());
				}
			}


			if (($retval = ArReportSetPeer::doValidate($this, $columns)) !== true) {
				$failureMap = array_merge($failureMap, $retval);
			}


				if ($this->collArReportsRelatedByArReportSetId !== null) {
					foreach ($this->collArReportsRelatedByArReportSetId as $referrerFK) {
						if (!$referrerFK->validate($columns)) {
							$failureMap = array_merge($failureMap, $referrerFK->getValidationFailures());
						}
					}
				}

				if ($this->collArReportsRelatedByAboutArReportSetId !== null) {
					foreach ($this->collArReportsRelatedByAboutArReportSetId as $referrerFK) {
						if (!$referrerFK->validate($columns)) {
							$failureMap = array_merge($failureMap, $referrerFK->getValidationFailures());
						}
					}
				}

				if ($this->collArPostponedReports !== null) {
					foreach ($this->collArPostponedReports as $referrerFK) {
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
		$pos = ArReportSetPeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
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
				return $this->getArReportSchedulerId();
				break;
			case 2:
				return $this->getFromDate();
				break;
			case 3:
				return $this->getToDate();
				break;
			case 4:
				return $this->getMustBeReviewed();
				break;
			case 5:
				return $this->getPostponedFieldsAreUpdated();
				break;
			case 6:
				return $this->getPostponedReports();
				break;
			case 7:
				return $this->getPostponedAmount();
				break;
			case 8:
				return $this->getReports();
				break;
			case 9:
				return $this->getAmount();
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
		$keys = ArReportSetPeer::getFieldNames($keyType);
		$result = array(
			$keys[0] => $this->getId(),
			$keys[1] => $this->getArReportSchedulerId(),
			$keys[2] => $this->getFromDate(),
			$keys[3] => $this->getToDate(),
			$keys[4] => $this->getMustBeReviewed(),
			$keys[5] => $this->getPostponedFieldsAreUpdated(),
			$keys[6] => $this->getPostponedReports(),
			$keys[7] => $this->getPostponedAmount(),
			$keys[8] => $this->getReports(),
			$keys[9] => $this->getAmount(),
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
		$pos = ArReportSetPeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
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
				$this->setArReportSchedulerId($value);
				break;
			case 2:
				$this->setFromDate($value);
				break;
			case 3:
				$this->setToDate($value);
				break;
			case 4:
				$this->setMustBeReviewed($value);
				break;
			case 5:
				$this->setPostponedFieldsAreUpdated($value);
				break;
			case 6:
				$this->setPostponedReports($value);
				break;
			case 7:
				$this->setPostponedAmount($value);
				break;
			case 8:
				$this->setReports($value);
				break;
			case 9:
				$this->setAmount($value);
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
		$keys = ArReportSetPeer::getFieldNames($keyType);

		if (array_key_exists($keys[0], $arr)) $this->setId($arr[$keys[0]]);
		if (array_key_exists($keys[1], $arr)) $this->setArReportSchedulerId($arr[$keys[1]]);
		if (array_key_exists($keys[2], $arr)) $this->setFromDate($arr[$keys[2]]);
		if (array_key_exists($keys[3], $arr)) $this->setToDate($arr[$keys[3]]);
		if (array_key_exists($keys[4], $arr)) $this->setMustBeReviewed($arr[$keys[4]]);
		if (array_key_exists($keys[5], $arr)) $this->setPostponedFieldsAreUpdated($arr[$keys[5]]);
		if (array_key_exists($keys[6], $arr)) $this->setPostponedReports($arr[$keys[6]]);
		if (array_key_exists($keys[7], $arr)) $this->setPostponedAmount($arr[$keys[7]]);
		if (array_key_exists($keys[8], $arr)) $this->setReports($arr[$keys[8]]);
		if (array_key_exists($keys[9], $arr)) $this->setAmount($arr[$keys[9]]);
	}

	/**
	 * Build a Criteria object containing the values of all modified columns in this object.
	 *
	 * @return     Criteria The Criteria object containing all modified values.
	 */
	public function buildCriteria()
	{
		$criteria = new Criteria(ArReportSetPeer::DATABASE_NAME);

		if ($this->isColumnModified(ArReportSetPeer::ID)) $criteria->add(ArReportSetPeer::ID, $this->id);
		if ($this->isColumnModified(ArReportSetPeer::AR_REPORT_SCHEDULER_ID)) $criteria->add(ArReportSetPeer::AR_REPORT_SCHEDULER_ID, $this->ar_report_scheduler_id);
		if ($this->isColumnModified(ArReportSetPeer::FROM_DATE)) $criteria->add(ArReportSetPeer::FROM_DATE, $this->from_date);
		if ($this->isColumnModified(ArReportSetPeer::TO_DATE)) $criteria->add(ArReportSetPeer::TO_DATE, $this->to_date);
		if ($this->isColumnModified(ArReportSetPeer::MUST_BE_REVIEWED)) $criteria->add(ArReportSetPeer::MUST_BE_REVIEWED, $this->must_be_reviewed);
		if ($this->isColumnModified(ArReportSetPeer::POSTPONED_FIELDS_ARE_UPDATED)) $criteria->add(ArReportSetPeer::POSTPONED_FIELDS_ARE_UPDATED, $this->postponed_fields_are_updated);
		if ($this->isColumnModified(ArReportSetPeer::POSTPONED_REPORTS)) $criteria->add(ArReportSetPeer::POSTPONED_REPORTS, $this->postponed_reports);
		if ($this->isColumnModified(ArReportSetPeer::POSTPONED_AMOUNT)) $criteria->add(ArReportSetPeer::POSTPONED_AMOUNT, $this->postponed_amount);
		if ($this->isColumnModified(ArReportSetPeer::REPORTS)) $criteria->add(ArReportSetPeer::REPORTS, $this->reports);
		if ($this->isColumnModified(ArReportSetPeer::AMOUNT)) $criteria->add(ArReportSetPeer::AMOUNT, $this->amount);

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
		$criteria = new Criteria(ArReportSetPeer::DATABASE_NAME);

		$criteria->add(ArReportSetPeer::ID, $this->id);

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
	 * @param      object $copyObj An object of ArReportSet (or compatible) type.
	 * @param      boolean $deepCopy Whether to also copy all rows that refer (by fkey) to the current row.
	 * @throws     PropelException
	 */
	public function copyInto($copyObj, $deepCopy = false)
	{

		$copyObj->setArReportSchedulerId($this->ar_report_scheduler_id);

		$copyObj->setFromDate($this->from_date);

		$copyObj->setToDate($this->to_date);

		$copyObj->setMustBeReviewed($this->must_be_reviewed);

		$copyObj->setPostponedFieldsAreUpdated($this->postponed_fields_are_updated);

		$copyObj->setPostponedReports($this->postponed_reports);

		$copyObj->setPostponedAmount($this->postponed_amount);

		$copyObj->setReports($this->reports);

		$copyObj->setAmount($this->amount);


		if ($deepCopy) {
			// important: temporarily setNew(false) because this affects the behavior of
			// the getter/setter methods for fkey referrer objects.
			$copyObj->setNew(false);

			foreach ($this->getArReportsRelatedByArReportSetId() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArReportRelatedByArReportSetId($relObj->copy($deepCopy));
				}
			}

			foreach ($this->getArReportsRelatedByAboutArReportSetId() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArReportRelatedByAboutArReportSetId($relObj->copy($deepCopy));
				}
			}

			foreach ($this->getArPostponedReports() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArPostponedReport($relObj->copy($deepCopy));
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
	 * @return     ArReportSet Clone of current object.
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
	 * @return     ArReportSetPeer
	 */
	public function getPeer()
	{
		if (self::$peer === null) {
			self::$peer = new ArReportSetPeer();
		}
		return self::$peer;
	}

	/**
	 * Declares an association between this object and a ArReportScheduler object.
	 *
	 * @param      ArReportScheduler $v
	 * @return     ArReportSet The current object (for fluent API support)
	 * @throws     PropelException
	 */
	public function setArReportScheduler(ArReportScheduler $v = null)
	{
		if ($v === null) {
			$this->setArReportSchedulerId(NULL);
		} else {
			$this->setArReportSchedulerId($v->getId());
		}

		$this->aArReportScheduler = $v;

		// Add binding for other direction of this n:n relationship.
		// If this object has already been added to the ArReportScheduler object, it will not be re-added.
		if ($v !== null) {
			$v->addArReportSet($this);
		}

		return $this;
	}


	/**
	 * Get the associated ArReportScheduler object
	 *
	 * @param      PropelPDO Optional Connection object.
	 * @return     ArReportScheduler The associated ArReportScheduler object.
	 * @throws     PropelException
	 */
	public function getArReportScheduler(PropelPDO $con = null)
	{
		if ($this->aArReportScheduler === null && ($this->ar_report_scheduler_id !== null)) {
			$this->aArReportScheduler = ArReportSchedulerPeer::retrieveByPk($this->ar_report_scheduler_id);
			/* The following can be used additionally to
			   guarantee the related object contains a reference
			   to this object.  This level of coupling may, however, be
			   undesirable since it could result in an only partially populated collection
			   in the referenced object.
			   $this->aArReportScheduler->addArReportSets($this);
			 */
		}
		return $this->aArReportScheduler;
	}

	/**
	 * Clears out the collArReportsRelatedByArReportSetId collection (array).
	 *
	 * This does not modify the database; however, it will remove any associated objects, causing
	 * them to be refetched by subsequent calls to accessor method.
	 *
	 * @return     void
	 * @see        addArReportsRelatedByArReportSetId()
	 */
	public function clearArReportsRelatedByArReportSetId()
	{
		$this->collArReportsRelatedByArReportSetId = null; // important to set this to NULL since that means it is uninitialized
	}

	/**
	 * Initializes the collArReportsRelatedByArReportSetId collection (array).
	 *
	 * By default this just sets the collArReportsRelatedByArReportSetId collection to an empty array (like clearcollArReportsRelatedByArReportSetId());
	 * however, you may wish to override this method in your stub class to provide setting appropriate
	 * to your application -- for example, setting the initial array to the values stored in database.
	 *
	 * @return     void
	 */
	public function initArReportsRelatedByArReportSetId()
	{
		$this->collArReportsRelatedByArReportSetId = array();
	}

	/**
	 * Gets an array of ArReport objects which contain a foreign key that references this object.
	 *
	 * If this collection has already been initialized with an identical Criteria, it returns the collection.
	 * Otherwise if this ArReportSet has previously been saved, it will retrieve
	 * related ArReportsRelatedByArReportSetId from storage. If this ArReportSet is new, it will return
	 * an empty collection or the current collection, the criteria is ignored on a new object.
	 *
	 * @param      PropelPDO $con
	 * @param      Criteria $criteria
	 * @return     array ArReport[]
	 * @throws     PropelException
	 */
	public function getArReportsRelatedByArReportSetId($criteria = null, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArReportSetPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReportsRelatedByArReportSetId === null) {
			if ($this->isNew()) {
			   $this->collArReportsRelatedByArReportSetId = array();
			} else {

				$criteria->add(ArReportPeer::AR_REPORT_SET_ID, $this->id);

				ArReportPeer::addSelectColumns($criteria);
				$this->collArReportsRelatedByArReportSetId = ArReportPeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArReportPeer::AR_REPORT_SET_ID, $this->id);

				ArReportPeer::addSelectColumns($criteria);
				if (!isset($this->lastArReportRelatedByArReportSetIdCriteria) || !$this->lastArReportRelatedByArReportSetIdCriteria->equals($criteria)) {
					$this->collArReportsRelatedByArReportSetId = ArReportPeer::doSelect($criteria, $con);
				}
			}
		}
		$this->lastArReportRelatedByArReportSetIdCriteria = $criteria;
		return $this->collArReportsRelatedByArReportSetId;
	}

	/**
	 * Returns the number of related ArReport objects.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct
	 * @param      PropelPDO $con
	 * @return     int Count of related ArReport objects.
	 * @throws     PropelException
	 */
	public function countArReportsRelatedByArReportSetId(Criteria $criteria = null, $distinct = false, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArReportSetPeer::DATABASE_NAME);
		} else {
			$criteria = clone $criteria;
		}

		if ($distinct) {
			$criteria->setDistinct();
		}

		$count = null;

		if ($this->collArReportsRelatedByArReportSetId === null) {
			if ($this->isNew()) {
				$count = 0;
			} else {

				$criteria->add(ArReportPeer::AR_REPORT_SET_ID, $this->id);

				$count = ArReportPeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArReportPeer::AR_REPORT_SET_ID, $this->id);

				if (!isset($this->lastArReportRelatedByArReportSetIdCriteria) || !$this->lastArReportRelatedByArReportSetIdCriteria->equals($criteria)) {
					$count = ArReportPeer::doCount($criteria, false, $con);
				} else {
					$count = count($this->collArReportsRelatedByArReportSetId);
				}
			} else {
				$count = count($this->collArReportsRelatedByArReportSetId);
			}
		}
		return $count;
	}

	/**
	 * Method called to associate a ArReport object to this object
	 * through the ArReport foreign key attribute.
	 *
	 * @param      ArReport $l ArReport
	 * @return     void
	 * @throws     PropelException
	 */
	public function addArReportRelatedByArReportSetId(ArReport $l)
	{
		if ($this->collArReportsRelatedByArReportSetId === null) {
			$this->initArReportsRelatedByArReportSetId();
		}
		if (!in_array($l, $this->collArReportsRelatedByArReportSetId, true)) { // only add it if the **same** object is not already associated
			array_push($this->collArReportsRelatedByArReportSetId, $l);
			$l->setArReportSetRelatedByArReportSetId($this);
		}
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArReportSet is new, it will return
	 * an empty collection; or if this ArReportSet has previously
	 * been saved, it will retrieve related ArReportsRelatedByArReportSetId from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArReportSet.
	 */
	public function getArReportsRelatedByArReportSetIdJoinArOrganizationUnit($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArReportSetPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReportsRelatedByArReportSetId === null) {
			if ($this->isNew()) {
				$this->collArReportsRelatedByArReportSetId = array();
			} else {

				$criteria->add(ArReportPeer::AR_REPORT_SET_ID, $this->id);

				$this->collArReportsRelatedByArReportSetId = ArReportPeer::doSelectJoinArOrganizationUnit($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArReportPeer::AR_REPORT_SET_ID, $this->id);

			if (!isset($this->lastArReportRelatedByArReportSetIdCriteria) || !$this->lastArReportRelatedByArReportSetIdCriteria->equals($criteria)) {
				$this->collArReportsRelatedByArReportSetId = ArReportPeer::doSelectJoinArOrganizationUnit($criteria, $con, $join_behavior);
			}
		}
		$this->lastArReportRelatedByArReportSetIdCriteria = $criteria;

		return $this->collArReportsRelatedByArReportSetId;
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArReportSet is new, it will return
	 * an empty collection; or if this ArReportSet has previously
	 * been saved, it will retrieve related ArReportsRelatedByArReportSetId from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArReportSet.
	 */
	public function getArReportsRelatedByArReportSetIdJoinArUser($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArReportSetPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReportsRelatedByArReportSetId === null) {
			if ($this->isNew()) {
				$this->collArReportsRelatedByArReportSetId = array();
			} else {

				$criteria->add(ArReportPeer::AR_REPORT_SET_ID, $this->id);

				$this->collArReportsRelatedByArReportSetId = ArReportPeer::doSelectJoinArUser($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArReportPeer::AR_REPORT_SET_ID, $this->id);

			if (!isset($this->lastArReportRelatedByArReportSetIdCriteria) || !$this->lastArReportRelatedByArReportSetIdCriteria->equals($criteria)) {
				$this->collArReportsRelatedByArReportSetId = ArReportPeer::doSelectJoinArUser($criteria, $con, $join_behavior);
			}
		}
		$this->lastArReportRelatedByArReportSetIdCriteria = $criteria;

		return $this->collArReportsRelatedByArReportSetId;
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArReportSet is new, it will return
	 * an empty collection; or if this ArReportSet has previously
	 * been saved, it will retrieve related ArReportsRelatedByArReportSetId from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArReportSet.
	 */
	public function getArReportsRelatedByArReportSetIdJoinArVendor($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArReportSetPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReportsRelatedByArReportSetId === null) {
			if ($this->isNew()) {
				$this->collArReportsRelatedByArReportSetId = array();
			} else {

				$criteria->add(ArReportPeer::AR_REPORT_SET_ID, $this->id);

				$this->collArReportsRelatedByArReportSetId = ArReportPeer::doSelectJoinArVendor($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArReportPeer::AR_REPORT_SET_ID, $this->id);

			if (!isset($this->lastArReportRelatedByArReportSetIdCriteria) || !$this->lastArReportRelatedByArReportSetIdCriteria->equals($criteria)) {
				$this->collArReportsRelatedByArReportSetId = ArReportPeer::doSelectJoinArVendor($criteria, $con, $join_behavior);
			}
		}
		$this->lastArReportRelatedByArReportSetIdCriteria = $criteria;

		return $this->collArReportsRelatedByArReportSetId;
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArReportSet is new, it will return
	 * an empty collection; or if this ArReportSet has previously
	 * been saved, it will retrieve related ArReportsRelatedByArReportSetId from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArReportSet.
	 */
	public function getArReportsRelatedByArReportSetIdJoinArTag($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArReportSetPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReportsRelatedByArReportSetId === null) {
			if ($this->isNew()) {
				$this->collArReportsRelatedByArReportSetId = array();
			} else {

				$criteria->add(ArReportPeer::AR_REPORT_SET_ID, $this->id);

				$this->collArReportsRelatedByArReportSetId = ArReportPeer::doSelectJoinArTag($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArReportPeer::AR_REPORT_SET_ID, $this->id);

			if (!isset($this->lastArReportRelatedByArReportSetIdCriteria) || !$this->lastArReportRelatedByArReportSetIdCriteria->equals($criteria)) {
				$this->collArReportsRelatedByArReportSetId = ArReportPeer::doSelectJoinArTag($criteria, $con, $join_behavior);
			}
		}
		$this->lastArReportRelatedByArReportSetIdCriteria = $criteria;

		return $this->collArReportsRelatedByArReportSetId;
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArReportSet is new, it will return
	 * an empty collection; or if this ArReportSet has previously
	 * been saved, it will retrieve related ArReportsRelatedByArReportSetId from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArReportSet.
	 */
	public function getArReportsRelatedByArReportSetIdJoinArReportOrderOfChildren($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArReportSetPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReportsRelatedByArReportSetId === null) {
			if ($this->isNew()) {
				$this->collArReportsRelatedByArReportSetId = array();
			} else {

				$criteria->add(ArReportPeer::AR_REPORT_SET_ID, $this->id);

				$this->collArReportsRelatedByArReportSetId = ArReportPeer::doSelectJoinArReportOrderOfChildren($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArReportPeer::AR_REPORT_SET_ID, $this->id);

			if (!isset($this->lastArReportRelatedByArReportSetIdCriteria) || !$this->lastArReportRelatedByArReportSetIdCriteria->equals($criteria)) {
				$this->collArReportsRelatedByArReportSetId = ArReportPeer::doSelectJoinArReportOrderOfChildren($criteria, $con, $join_behavior);
			}
		}
		$this->lastArReportRelatedByArReportSetIdCriteria = $criteria;

		return $this->collArReportsRelatedByArReportSetId;
	}

	/**
	 * Clears out the collArReportsRelatedByAboutArReportSetId collection (array).
	 *
	 * This does not modify the database; however, it will remove any associated objects, causing
	 * them to be refetched by subsequent calls to accessor method.
	 *
	 * @return     void
	 * @see        addArReportsRelatedByAboutArReportSetId()
	 */
	public function clearArReportsRelatedByAboutArReportSetId()
	{
		$this->collArReportsRelatedByAboutArReportSetId = null; // important to set this to NULL since that means it is uninitialized
	}

	/**
	 * Initializes the collArReportsRelatedByAboutArReportSetId collection (array).
	 *
	 * By default this just sets the collArReportsRelatedByAboutArReportSetId collection to an empty array (like clearcollArReportsRelatedByAboutArReportSetId());
	 * however, you may wish to override this method in your stub class to provide setting appropriate
	 * to your application -- for example, setting the initial array to the values stored in database.
	 *
	 * @return     void
	 */
	public function initArReportsRelatedByAboutArReportSetId()
	{
		$this->collArReportsRelatedByAboutArReportSetId = array();
	}

	/**
	 * Gets an array of ArReport objects which contain a foreign key that references this object.
	 *
	 * If this collection has already been initialized with an identical Criteria, it returns the collection.
	 * Otherwise if this ArReportSet has previously been saved, it will retrieve
	 * related ArReportsRelatedByAboutArReportSetId from storage. If this ArReportSet is new, it will return
	 * an empty collection or the current collection, the criteria is ignored on a new object.
	 *
	 * @param      PropelPDO $con
	 * @param      Criteria $criteria
	 * @return     array ArReport[]
	 * @throws     PropelException
	 */
	public function getArReportsRelatedByAboutArReportSetId($criteria = null, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArReportSetPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReportsRelatedByAboutArReportSetId === null) {
			if ($this->isNew()) {
			   $this->collArReportsRelatedByAboutArReportSetId = array();
			} else {

				$criteria->add(ArReportPeer::ABOUT_AR_REPORT_SET_ID, $this->id);

				ArReportPeer::addSelectColumns($criteria);
				$this->collArReportsRelatedByAboutArReportSetId = ArReportPeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArReportPeer::ABOUT_AR_REPORT_SET_ID, $this->id);

				ArReportPeer::addSelectColumns($criteria);
				if (!isset($this->lastArReportRelatedByAboutArReportSetIdCriteria) || !$this->lastArReportRelatedByAboutArReportSetIdCriteria->equals($criteria)) {
					$this->collArReportsRelatedByAboutArReportSetId = ArReportPeer::doSelect($criteria, $con);
				}
			}
		}
		$this->lastArReportRelatedByAboutArReportSetIdCriteria = $criteria;
		return $this->collArReportsRelatedByAboutArReportSetId;
	}

	/**
	 * Returns the number of related ArReport objects.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct
	 * @param      PropelPDO $con
	 * @return     int Count of related ArReport objects.
	 * @throws     PropelException
	 */
	public function countArReportsRelatedByAboutArReportSetId(Criteria $criteria = null, $distinct = false, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArReportSetPeer::DATABASE_NAME);
		} else {
			$criteria = clone $criteria;
		}

		if ($distinct) {
			$criteria->setDistinct();
		}

		$count = null;

		if ($this->collArReportsRelatedByAboutArReportSetId === null) {
			if ($this->isNew()) {
				$count = 0;
			} else {

				$criteria->add(ArReportPeer::ABOUT_AR_REPORT_SET_ID, $this->id);

				$count = ArReportPeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArReportPeer::ABOUT_AR_REPORT_SET_ID, $this->id);

				if (!isset($this->lastArReportRelatedByAboutArReportSetIdCriteria) || !$this->lastArReportRelatedByAboutArReportSetIdCriteria->equals($criteria)) {
					$count = ArReportPeer::doCount($criteria, false, $con);
				} else {
					$count = count($this->collArReportsRelatedByAboutArReportSetId);
				}
			} else {
				$count = count($this->collArReportsRelatedByAboutArReportSetId);
			}
		}
		return $count;
	}

	/**
	 * Method called to associate a ArReport object to this object
	 * through the ArReport foreign key attribute.
	 *
	 * @param      ArReport $l ArReport
	 * @return     void
	 * @throws     PropelException
	 */
	public function addArReportRelatedByAboutArReportSetId(ArReport $l)
	{
		if ($this->collArReportsRelatedByAboutArReportSetId === null) {
			$this->initArReportsRelatedByAboutArReportSetId();
		}
		if (!in_array($l, $this->collArReportsRelatedByAboutArReportSetId, true)) { // only add it if the **same** object is not already associated
			array_push($this->collArReportsRelatedByAboutArReportSetId, $l);
			$l->setArReportSetRelatedByAboutArReportSetId($this);
		}
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArReportSet is new, it will return
	 * an empty collection; or if this ArReportSet has previously
	 * been saved, it will retrieve related ArReportsRelatedByAboutArReportSetId from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArReportSet.
	 */
	public function getArReportsRelatedByAboutArReportSetIdJoinArOrganizationUnit($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArReportSetPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReportsRelatedByAboutArReportSetId === null) {
			if ($this->isNew()) {
				$this->collArReportsRelatedByAboutArReportSetId = array();
			} else {

				$criteria->add(ArReportPeer::ABOUT_AR_REPORT_SET_ID, $this->id);

				$this->collArReportsRelatedByAboutArReportSetId = ArReportPeer::doSelectJoinArOrganizationUnit($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArReportPeer::ABOUT_AR_REPORT_SET_ID, $this->id);

			if (!isset($this->lastArReportRelatedByAboutArReportSetIdCriteria) || !$this->lastArReportRelatedByAboutArReportSetIdCriteria->equals($criteria)) {
				$this->collArReportsRelatedByAboutArReportSetId = ArReportPeer::doSelectJoinArOrganizationUnit($criteria, $con, $join_behavior);
			}
		}
		$this->lastArReportRelatedByAboutArReportSetIdCriteria = $criteria;

		return $this->collArReportsRelatedByAboutArReportSetId;
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArReportSet is new, it will return
	 * an empty collection; or if this ArReportSet has previously
	 * been saved, it will retrieve related ArReportsRelatedByAboutArReportSetId from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArReportSet.
	 */
	public function getArReportsRelatedByAboutArReportSetIdJoinArUser($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArReportSetPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReportsRelatedByAboutArReportSetId === null) {
			if ($this->isNew()) {
				$this->collArReportsRelatedByAboutArReportSetId = array();
			} else {

				$criteria->add(ArReportPeer::ABOUT_AR_REPORT_SET_ID, $this->id);

				$this->collArReportsRelatedByAboutArReportSetId = ArReportPeer::doSelectJoinArUser($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArReportPeer::ABOUT_AR_REPORT_SET_ID, $this->id);

			if (!isset($this->lastArReportRelatedByAboutArReportSetIdCriteria) || !$this->lastArReportRelatedByAboutArReportSetIdCriteria->equals($criteria)) {
				$this->collArReportsRelatedByAboutArReportSetId = ArReportPeer::doSelectJoinArUser($criteria, $con, $join_behavior);
			}
		}
		$this->lastArReportRelatedByAboutArReportSetIdCriteria = $criteria;

		return $this->collArReportsRelatedByAboutArReportSetId;
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArReportSet is new, it will return
	 * an empty collection; or if this ArReportSet has previously
	 * been saved, it will retrieve related ArReportsRelatedByAboutArReportSetId from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArReportSet.
	 */
	public function getArReportsRelatedByAboutArReportSetIdJoinArVendor($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArReportSetPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReportsRelatedByAboutArReportSetId === null) {
			if ($this->isNew()) {
				$this->collArReportsRelatedByAboutArReportSetId = array();
			} else {

				$criteria->add(ArReportPeer::ABOUT_AR_REPORT_SET_ID, $this->id);

				$this->collArReportsRelatedByAboutArReportSetId = ArReportPeer::doSelectJoinArVendor($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArReportPeer::ABOUT_AR_REPORT_SET_ID, $this->id);

			if (!isset($this->lastArReportRelatedByAboutArReportSetIdCriteria) || !$this->lastArReportRelatedByAboutArReportSetIdCriteria->equals($criteria)) {
				$this->collArReportsRelatedByAboutArReportSetId = ArReportPeer::doSelectJoinArVendor($criteria, $con, $join_behavior);
			}
		}
		$this->lastArReportRelatedByAboutArReportSetIdCriteria = $criteria;

		return $this->collArReportsRelatedByAboutArReportSetId;
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArReportSet is new, it will return
	 * an empty collection; or if this ArReportSet has previously
	 * been saved, it will retrieve related ArReportsRelatedByAboutArReportSetId from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArReportSet.
	 */
	public function getArReportsRelatedByAboutArReportSetIdJoinArTag($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArReportSetPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReportsRelatedByAboutArReportSetId === null) {
			if ($this->isNew()) {
				$this->collArReportsRelatedByAboutArReportSetId = array();
			} else {

				$criteria->add(ArReportPeer::ABOUT_AR_REPORT_SET_ID, $this->id);

				$this->collArReportsRelatedByAboutArReportSetId = ArReportPeer::doSelectJoinArTag($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArReportPeer::ABOUT_AR_REPORT_SET_ID, $this->id);

			if (!isset($this->lastArReportRelatedByAboutArReportSetIdCriteria) || !$this->lastArReportRelatedByAboutArReportSetIdCriteria->equals($criteria)) {
				$this->collArReportsRelatedByAboutArReportSetId = ArReportPeer::doSelectJoinArTag($criteria, $con, $join_behavior);
			}
		}
		$this->lastArReportRelatedByAboutArReportSetIdCriteria = $criteria;

		return $this->collArReportsRelatedByAboutArReportSetId;
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArReportSet is new, it will return
	 * an empty collection; or if this ArReportSet has previously
	 * been saved, it will retrieve related ArReportsRelatedByAboutArReportSetId from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArReportSet.
	 */
	public function getArReportsRelatedByAboutArReportSetIdJoinArReportOrderOfChildren($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArReportSetPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReportsRelatedByAboutArReportSetId === null) {
			if ($this->isNew()) {
				$this->collArReportsRelatedByAboutArReportSetId = array();
			} else {

				$criteria->add(ArReportPeer::ABOUT_AR_REPORT_SET_ID, $this->id);

				$this->collArReportsRelatedByAboutArReportSetId = ArReportPeer::doSelectJoinArReportOrderOfChildren($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArReportPeer::ABOUT_AR_REPORT_SET_ID, $this->id);

			if (!isset($this->lastArReportRelatedByAboutArReportSetIdCriteria) || !$this->lastArReportRelatedByAboutArReportSetIdCriteria->equals($criteria)) {
				$this->collArReportsRelatedByAboutArReportSetId = ArReportPeer::doSelectJoinArReportOrderOfChildren($criteria, $con, $join_behavior);
			}
		}
		$this->lastArReportRelatedByAboutArReportSetIdCriteria = $criteria;

		return $this->collArReportsRelatedByAboutArReportSetId;
	}

	/**
	 * Clears out the collArPostponedReports collection (array).
	 *
	 * This does not modify the database; however, it will remove any associated objects, causing
	 * them to be refetched by subsequent calls to accessor method.
	 *
	 * @return     void
	 * @see        addArPostponedReports()
	 */
	public function clearArPostponedReports()
	{
		$this->collArPostponedReports = null; // important to set this to NULL since that means it is uninitialized
	}

	/**
	 * Initializes the collArPostponedReports collection (array).
	 *
	 * By default this just sets the collArPostponedReports collection to an empty array (like clearcollArPostponedReports());
	 * however, you may wish to override this method in your stub class to provide setting appropriate
	 * to your application -- for example, setting the initial array to the values stored in database.
	 *
	 * @return     void
	 */
	public function initArPostponedReports()
	{
		$this->collArPostponedReports = array();
	}

	/**
	 * Gets an array of ArPostponedReport objects which contain a foreign key that references this object.
	 *
	 * If this collection has already been initialized with an identical Criteria, it returns the collection.
	 * Otherwise if this ArReportSet has previously been saved, it will retrieve
	 * related ArPostponedReports from storage. If this ArReportSet is new, it will return
	 * an empty collection or the current collection, the criteria is ignored on a new object.
	 *
	 * @param      PropelPDO $con
	 * @param      Criteria $criteria
	 * @return     array ArPostponedReport[]
	 * @throws     PropelException
	 */
	public function getArPostponedReports($criteria = null, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArReportSetPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArPostponedReports === null) {
			if ($this->isNew()) {
			   $this->collArPostponedReports = array();
			} else {

				$criteria->add(ArPostponedReportPeer::AR_REPORT_SET_ID, $this->id);

				ArPostponedReportPeer::addSelectColumns($criteria);
				$this->collArPostponedReports = ArPostponedReportPeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArPostponedReportPeer::AR_REPORT_SET_ID, $this->id);

				ArPostponedReportPeer::addSelectColumns($criteria);
				if (!isset($this->lastArPostponedReportCriteria) || !$this->lastArPostponedReportCriteria->equals($criteria)) {
					$this->collArPostponedReports = ArPostponedReportPeer::doSelect($criteria, $con);
				}
			}
		}
		$this->lastArPostponedReportCriteria = $criteria;
		return $this->collArPostponedReports;
	}

	/**
	 * Returns the number of related ArPostponedReport objects.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct
	 * @param      PropelPDO $con
	 * @return     int Count of related ArPostponedReport objects.
	 * @throws     PropelException
	 */
	public function countArPostponedReports(Criteria $criteria = null, $distinct = false, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArReportSetPeer::DATABASE_NAME);
		} else {
			$criteria = clone $criteria;
		}

		if ($distinct) {
			$criteria->setDistinct();
		}

		$count = null;

		if ($this->collArPostponedReports === null) {
			if ($this->isNew()) {
				$count = 0;
			} else {

				$criteria->add(ArPostponedReportPeer::AR_REPORT_SET_ID, $this->id);

				$count = ArPostponedReportPeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArPostponedReportPeer::AR_REPORT_SET_ID, $this->id);

				if (!isset($this->lastArPostponedReportCriteria) || !$this->lastArPostponedReportCriteria->equals($criteria)) {
					$count = ArPostponedReportPeer::doCount($criteria, false, $con);
				} else {
					$count = count($this->collArPostponedReports);
				}
			} else {
				$count = count($this->collArPostponedReports);
			}
		}
		return $count;
	}

	/**
	 * Method called to associate a ArPostponedReport object to this object
	 * through the ArPostponedReport foreign key attribute.
	 *
	 * @param      ArPostponedReport $l ArPostponedReport
	 * @return     void
	 * @throws     PropelException
	 */
	public function addArPostponedReport(ArPostponedReport $l)
	{
		if ($this->collArPostponedReports === null) {
			$this->initArPostponedReports();
		}
		if (!in_array($l, $this->collArPostponedReports, true)) { // only add it if the **same** object is not already associated
			array_push($this->collArPostponedReports, $l);
			$l->setArReportSet($this);
		}
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArReportSet is new, it will return
	 * an empty collection; or if this ArReportSet has previously
	 * been saved, it will retrieve related ArPostponedReports from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArReportSet.
	 */
	public function getArPostponedReportsJoinArOrganizationUnit($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArReportSetPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArPostponedReports === null) {
			if ($this->isNew()) {
				$this->collArPostponedReports = array();
			} else {

				$criteria->add(ArPostponedReportPeer::AR_REPORT_SET_ID, $this->id);

				$this->collArPostponedReports = ArPostponedReportPeer::doSelectJoinArOrganizationUnit($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArPostponedReportPeer::AR_REPORT_SET_ID, $this->id);

			if (!isset($this->lastArPostponedReportCriteria) || !$this->lastArPostponedReportCriteria->equals($criteria)) {
				$this->collArPostponedReports = ArPostponedReportPeer::doSelectJoinArOrganizationUnit($criteria, $con, $join_behavior);
			}
		}
		$this->lastArPostponedReportCriteria = $criteria;

		return $this->collArPostponedReports;
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
			if ($this->collArReportsRelatedByArReportSetId) {
				foreach ((array) $this->collArReportsRelatedByArReportSetId as $o) {
					$o->clearAllReferences($deep);
				}
			}
			if ($this->collArReportsRelatedByAboutArReportSetId) {
				foreach ((array) $this->collArReportsRelatedByAboutArReportSetId as $o) {
					$o->clearAllReferences($deep);
				}
			}
			if ($this->collArPostponedReports) {
				foreach ((array) $this->collArPostponedReports as $o) {
					$o->clearAllReferences($deep);
				}
			}
		} // if ($deep)

		$this->collArReportsRelatedByArReportSetId = null;
		$this->collArReportsRelatedByAboutArReportSetId = null;
		$this->collArPostponedReports = null;
			$this->aArReportScheduler = null;
	}

} // BaseArReportSet
