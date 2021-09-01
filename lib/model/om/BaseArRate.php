<?php

/**
 * Base class that represents a row from the 'ar_rate' table.
 *
 * 
 *
 * @package    lib.model.om
 */
abstract class BaseArRate extends BaseObject  implements Persistent {


	/**
	 * The Peer class.
	 * Instance provides a convenient way of calling static methods on a class
	 * that calling code may not be able to identify.
	 * @var        ArRatePeer
	 */
	protected static $peer;

	/**
	 * The value for the id field.
	 * @var        int
	 */
	protected $id;

	/**
	 * The value for the ar_vendor_id field.
	 * @var        int
	 */
	protected $ar_vendor_id;

	/**
	 * The value for the ar_rate_format_id field.
	 * @var        int
	 */
	protected $ar_rate_format_id;

	/**
	 * The value for the from_time field.
	 * @var        string
	 */
	protected $from_time;

	/**
	 * The value for the internal_name field.
	 * @var        string
	 */
	protected $internal_name;

	/**
	 * The value for the ar_rate_id field.
	 * @var        int
	 */
	protected $ar_rate_id;

	/**
	 * The value for the short_description field.
	 * @var        string
	 */
	protected $short_description;

	/**
	 * The value for the note field.
	 * @var        string
	 */
	protected $note;

	/**
	 * The value for the is_exported_to_resellers field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $is_exported_to_resellers;

	/**
	 * The value for the was_compiled field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $was_compiled;

	/**
	 * The value for the source_data_file field.
	 * @var        resource
	 */
	protected $source_data_file;

	/**
	 * The value for the backup_source_data_file field.
	 * @var        resource
	 */
	protected $backup_source_data_file;

	/**
	 * The value for the html_description field.
	 * @var        resource
	 */
	protected $html_description;

	/**
	 * @var        ArVendor
	 */
	protected $aArVendor;

	/**
	 * @var        ArRateFormat
	 */
	protected $aArRateFormat;

	/**
	 * @var        ArRate
	 */
	protected $aArRateRelatedByArRateId;

	/**
	 * @var        array ArRate[] Collection to store aggregation of ArRate objects.
	 */
	protected $collArRatesRelatedByArRateId;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArRatesRelatedByArRateId.
	 */
	private $lastArRateRelatedByArRateIdCriteria = null;

	/**
	 * @var        array ArRateSharedWithReseller[] Collection to store aggregation of ArRateSharedWithReseller objects.
	 */
	protected $collArRateSharedWithResellers;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArRateSharedWithResellers.
	 */
	private $lastArRateSharedWithResellerCriteria = null;

	/**
	 * @var        array ArSpecificRateCalc[] Collection to store aggregation of ArSpecificRateCalc objects.
	 */
	protected $collArSpecificRateCalcs;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArSpecificRateCalcs.
	 */
	private $lastArSpecificRateCalcCriteria = null;

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
	
	const PEER = 'ArRatePeer';

	/**
	 * Applies default values to this object.
	 * This method should be called from the object's constructor (or
	 * equivalent initialization method).
	 * @see        __construct()
	 */
	public function applyDefaultValues()
	{
		$this->is_exported_to_resellers = false;
		$this->was_compiled = false;
	}

	/**
	 * Initializes internal state of BaseArRate object.
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
	 * Get the [ar_vendor_id] column value.
	 * 
	 * @return     int
	 */
	public function getArVendorId()
	{
		return $this->ar_vendor_id;
	}

	/**
	 * Get the [ar_rate_format_id] column value.
	 * 
	 * @return     int
	 */
	public function getArRateFormatId()
	{
		return $this->ar_rate_format_id;
	}

	/**
	 * Get the [optionally formatted] temporal [from_time] column value.
	 * 
	 *
	 * @param      string $format The date/time format string (either date()-style or strftime()-style).
	 *							If format is NULL, then the raw DateTime object will be returned.
	 * @return     mixed Formatted date/time value as string or DateTime object (if format is NULL), NULL if column is NULL, and 0 if column value is 0000-00-00 00:00:00
	 * @throws     PropelException - if unable to parse/validate the date/time value.
	 */
	public function getFromTime($format = 'Y-m-d H:i:s')
	{
		if ($this->from_time === null) {
			return null;
		}


		if ($this->from_time === '0000-00-00 00:00:00') {
			// while technically this is not a default value of NULL,
			// this seems to be closest in meaning.
			return null;
		} else {
			try {
				$dt = new DateTime($this->from_time);
			} catch (Exception $x) {
				throw new PropelException("Internally stored date/time/timestamp value could not be converted to DateTime: " . var_export($this->from_time, true), $x);
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
	 * Get the [internal_name] column value.
	 * 
	 * @return     string
	 */
	public function getInternalName()
	{
		return $this->internal_name;
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
	 * Get the [short_description] column value.
	 * 
	 * @return     string
	 */
	public function getShortDescription()
	{
		return $this->short_description;
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
	 * Get the [is_exported_to_resellers] column value.
	 * 
	 * @return     boolean
	 */
	public function getIsExportedToResellers()
	{
		return $this->is_exported_to_resellers;
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
	 * Get the [source_data_file] column value.
	 * 
	 * @return     resource
	 */
	public function getSourceDataFile()
	{
		return $this->source_data_file;
	}

	/**
	 * Get the [backup_source_data_file] column value.
	 * 
	 * @return     resource
	 */
	public function getBackupSourceDataFile()
	{
		return $this->backup_source_data_file;
	}

	/**
	 * Get the [html_description] column value.
	 * 
	 * @return     resource
	 */
	public function getHtmlDescription()
	{
		return $this->html_description;
	}

	/**
	 * Set the value of [id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArRate The current object (for fluent API support)
	 */
	public function setId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->id !== $v) {
			$this->id = $v;
			$this->modifiedColumns[] = ArRatePeer::ID;
		}

		return $this;
	} // setId()

	/**
	 * Set the value of [ar_vendor_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArRate The current object (for fluent API support)
	 */
	public function setArVendorId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->ar_vendor_id !== $v) {
			$this->ar_vendor_id = $v;
			$this->modifiedColumns[] = ArRatePeer::AR_VENDOR_ID;
		}

		if ($this->aArVendor !== null && $this->aArVendor->getId() !== $v) {
			$this->aArVendor = null;
		}

		return $this;
	} // setArVendorId()

	/**
	 * Set the value of [ar_rate_format_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArRate The current object (for fluent API support)
	 */
	public function setArRateFormatId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->ar_rate_format_id !== $v) {
			$this->ar_rate_format_id = $v;
			$this->modifiedColumns[] = ArRatePeer::AR_RATE_FORMAT_ID;
		}

		if ($this->aArRateFormat !== null && $this->aArRateFormat->getId() !== $v) {
			$this->aArRateFormat = null;
		}

		return $this;
	} // setArRateFormatId()

	/**
	 * Sets the value of [from_time] column to a normalized version of the date/time value specified.
	 * 
	 * @param      mixed $v string, integer (timestamp), or DateTime value.  Empty string will
	 *						be treated as NULL for temporal objects.
	 * @return     ArRate The current object (for fluent API support)
	 */
	public function setFromTime($v)
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

		if ( $this->from_time !== null || $dt !== null ) {
			// (nested ifs are a little easier to read in this case)

			$currNorm = ($this->from_time !== null && $tmpDt = new DateTime($this->from_time)) ? $tmpDt->format('Y-m-d H:i:s') : null;
			$newNorm = ($dt !== null) ? $dt->format('Y-m-d H:i:s') : null;

			if ( ($currNorm !== $newNorm) // normalized values don't match 
					)
			{
				$this->from_time = ($dt ? $dt->format('Y-m-d H:i:s') : null);
				$this->modifiedColumns[] = ArRatePeer::FROM_TIME;
			}
		} // if either are not null

		return $this;
	} // setFromTime()

	/**
	 * Set the value of [internal_name] column.
	 * 
	 * @param      string $v new value
	 * @return     ArRate The current object (for fluent API support)
	 */
	public function setInternalName($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->internal_name !== $v) {
			$this->internal_name = $v;
			$this->modifiedColumns[] = ArRatePeer::INTERNAL_NAME;
		}

		return $this;
	} // setInternalName()

	/**
	 * Set the value of [ar_rate_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArRate The current object (for fluent API support)
	 */
	public function setArRateId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->ar_rate_id !== $v) {
			$this->ar_rate_id = $v;
			$this->modifiedColumns[] = ArRatePeer::AR_RATE_ID;
		}

		if ($this->aArRateRelatedByArRateId !== null && $this->aArRateRelatedByArRateId->getId() !== $v) {
			$this->aArRateRelatedByArRateId = null;
		}

		return $this;
	} // setArRateId()

	/**
	 * Set the value of [short_description] column.
	 * 
	 * @param      string $v new value
	 * @return     ArRate The current object (for fluent API support)
	 */
	public function setShortDescription($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->short_description !== $v) {
			$this->short_description = $v;
			$this->modifiedColumns[] = ArRatePeer::SHORT_DESCRIPTION;
		}

		return $this;
	} // setShortDescription()

	/**
	 * Set the value of [note] column.
	 * 
	 * @param      string $v new value
	 * @return     ArRate The current object (for fluent API support)
	 */
	public function setNote($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->note !== $v) {
			$this->note = $v;
			$this->modifiedColumns[] = ArRatePeer::NOTE;
		}

		return $this;
	} // setNote()

	/**
	 * Set the value of [is_exported_to_resellers] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArRate The current object (for fluent API support)
	 */
	public function setIsExportedToResellers($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->is_exported_to_resellers !== $v || $this->isNew()) {
			$this->is_exported_to_resellers = $v;
			$this->modifiedColumns[] = ArRatePeer::IS_EXPORTED_TO_RESELLERS;
		}

		return $this;
	} // setIsExportedToResellers()

	/**
	 * Set the value of [was_compiled] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArRate The current object (for fluent API support)
	 */
	public function setWasCompiled($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->was_compiled !== $v || $this->isNew()) {
			$this->was_compiled = $v;
			$this->modifiedColumns[] = ArRatePeer::WAS_COMPILED;
		}

		return $this;
	} // setWasCompiled()

	/**
	 * Set the value of [source_data_file] column.
	 * 
	 * @param      resource $v new value
	 * @return     ArRate The current object (for fluent API support)
	 */
	public function setSourceDataFile($v)
	{
		// Because BLOB columns are streams in PDO we have to assume that they are
		// always modified when a new value is passed in.  For example, the contents
		// of the stream itself may have changed externally.
		if (!is_resource($v) && $v !== null) {
			$this->source_data_file = fopen('php://memory', 'r+');
			fwrite($this->source_data_file, $v);
			rewind($this->source_data_file);
		} else { // it's already a stream
			$this->source_data_file = $v;
		}
		$this->modifiedColumns[] = ArRatePeer::SOURCE_DATA_FILE;

		return $this;
	} // setSourceDataFile()

	/**
	 * Set the value of [backup_source_data_file] column.
	 * 
	 * @param      resource $v new value
	 * @return     ArRate The current object (for fluent API support)
	 */
	public function setBackupSourceDataFile($v)
	{
		// Because BLOB columns are streams in PDO we have to assume that they are
		// always modified when a new value is passed in.  For example, the contents
		// of the stream itself may have changed externally.
		if (!is_resource($v) && $v !== null) {
			$this->backup_source_data_file = fopen('php://memory', 'r+');
			fwrite($this->backup_source_data_file, $v);
			rewind($this->backup_source_data_file);
		} else { // it's already a stream
			$this->backup_source_data_file = $v;
		}
		$this->modifiedColumns[] = ArRatePeer::BACKUP_SOURCE_DATA_FILE;

		return $this;
	} // setBackupSourceDataFile()

	/**
	 * Set the value of [html_description] column.
	 * 
	 * @param      resource $v new value
	 * @return     ArRate The current object (for fluent API support)
	 */
	public function setHtmlDescription($v)
	{
		// Because BLOB columns are streams in PDO we have to assume that they are
		// always modified when a new value is passed in.  For example, the contents
		// of the stream itself may have changed externally.
		if (!is_resource($v) && $v !== null) {
			$this->html_description = fopen('php://memory', 'r+');
			fwrite($this->html_description, $v);
			rewind($this->html_description);
		} else { // it's already a stream
			$this->html_description = $v;
		}
		$this->modifiedColumns[] = ArRatePeer::HTML_DESCRIPTION;

		return $this;
	} // setHtmlDescription()

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
			if ($this->is_exported_to_resellers !== false) {
				return false;
			}

			if ($this->was_compiled !== false) {
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
			$this->ar_vendor_id = ($row[$startcol + 1] !== null) ? (int) $row[$startcol + 1] : null;
			$this->ar_rate_format_id = ($row[$startcol + 2] !== null) ? (int) $row[$startcol + 2] : null;
			$this->from_time = ($row[$startcol + 3] !== null) ? (string) $row[$startcol + 3] : null;
			$this->internal_name = ($row[$startcol + 4] !== null) ? (string) $row[$startcol + 4] : null;
			$this->ar_rate_id = ($row[$startcol + 5] !== null) ? (int) $row[$startcol + 5] : null;
			$this->short_description = ($row[$startcol + 6] !== null) ? (string) $row[$startcol + 6] : null;
			$this->note = ($row[$startcol + 7] !== null) ? (string) $row[$startcol + 7] : null;
			$this->is_exported_to_resellers = ($row[$startcol + 8] !== null) ? (boolean) $row[$startcol + 8] : null;
			$this->was_compiled = ($row[$startcol + 9] !== null) ? (boolean) $row[$startcol + 9] : null;
			if ($row[$startcol + 10] !== null) {
				$this->source_data_file = fopen('php://memory', 'r+');
				fwrite($this->source_data_file, $row[$startcol + 10]);
				rewind($this->source_data_file);
			} else {
				$this->source_data_file = null;
			}
			if ($row[$startcol + 11] !== null) {
				$this->backup_source_data_file = fopen('php://memory', 'r+');
				fwrite($this->backup_source_data_file, $row[$startcol + 11]);
				rewind($this->backup_source_data_file);
			} else {
				$this->backup_source_data_file = null;
			}
			if ($row[$startcol + 12] !== null) {
				$this->html_description = fopen('php://memory', 'r+');
				fwrite($this->html_description, $row[$startcol + 12]);
				rewind($this->html_description);
			} else {
				$this->html_description = null;
			}
			$this->resetModified();

			$this->setNew(false);

			if ($rehydrate) {
				$this->ensureConsistency();
			}

			// FIXME - using NUM_COLUMNS may be clearer.
			return $startcol + 13; // 13 = ArRatePeer::NUM_COLUMNS - ArRatePeer::NUM_LAZY_LOAD_COLUMNS).

		} catch (Exception $e) {
			throw new PropelException("Error populating ArRate object", $e);
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
		if ($this->aArRateFormat !== null && $this->ar_rate_format_id !== $this->aArRateFormat->getId()) {
			$this->aArRateFormat = null;
		}
		if ($this->aArRateRelatedByArRateId !== null && $this->ar_rate_id !== $this->aArRateRelatedByArRateId->getId()) {
			$this->aArRateRelatedByArRateId = null;
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
			$con = Propel::getConnection(ArRatePeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		// We don't need to alter the object instance pool; we're just modifying this instance
		// already in the pool.

		$stmt = ArRatePeer::doSelectStmt($this->buildPkeyCriteria(), $con);
		$row = $stmt->fetch(PDO::FETCH_NUM);
		$stmt->closeCursor();
		if (!$row) {
			throw new PropelException('Cannot find matching row in the database to reload object values.');
		}
		$this->hydrate($row, 0, true); // rehydrate

		if ($deep) {  // also de-associate any related objects?

			$this->aArVendor = null;
			$this->aArRateFormat = null;
			$this->aArRateRelatedByArRateId = null;
			$this->collArRatesRelatedByArRateId = null;
			$this->lastArRateRelatedByArRateIdCriteria = null;

			$this->collArRateSharedWithResellers = null;
			$this->lastArRateSharedWithResellerCriteria = null;

			$this->collArSpecificRateCalcs = null;
			$this->lastArSpecificRateCalcCriteria = null;

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
			$con = Propel::getConnection(ArRatePeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}
		
		$con->beginTransaction();
		try {
			$ret = $this->preDelete($con);
			if ($ret) {
				ArRatePeer::doDelete($this, $con);
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
			$con = Propel::getConnection(ArRatePeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
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
				ArRatePeer::addInstanceToPool($this);
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

			if ($this->aArRateFormat !== null) {
				if ($this->aArRateFormat->isModified() || $this->aArRateFormat->isNew()) {
					$affectedRows += $this->aArRateFormat->save($con);
				}
				$this->setArRateFormat($this->aArRateFormat);
			}

			if ($this->aArRateRelatedByArRateId !== null) {
				if ($this->aArRateRelatedByArRateId->isModified() || $this->aArRateRelatedByArRateId->isNew()) {
					$affectedRows += $this->aArRateRelatedByArRateId->save($con);
				}
				$this->setArRateRelatedByArRateId($this->aArRateRelatedByArRateId);
			}

			if ($this->isNew() ) {
				$this->modifiedColumns[] = ArRatePeer::ID;
			}

			// If this object has been modified, then save it to the database.
			if ($this->isModified()) {
				if ($this->isNew()) {
					$pk = ArRatePeer::doInsert($this, $con);
					$affectedRows += 1; // we are assuming that there is only 1 row per doInsert() which
										 // should always be true here (even though technically
										 // BasePeer::doInsert() can insert multiple rows).

					$this->setId($pk);  //[IMV] update autoincrement primary key

					$this->setNew(false);
				} else {
					$affectedRows += ArRatePeer::doUpdate($this, $con);
				}

				// Rewind the source_data_file LOB column, since PDO does not rewind after inserting value.
				if ($this->source_data_file !== null && is_resource($this->source_data_file)) {
					rewind($this->source_data_file);
				}

				// Rewind the backup_source_data_file LOB column, since PDO does not rewind after inserting value.
				if ($this->backup_source_data_file !== null && is_resource($this->backup_source_data_file)) {
					rewind($this->backup_source_data_file);
				}

				// Rewind the html_description LOB column, since PDO does not rewind after inserting value.
				if ($this->html_description !== null && is_resource($this->html_description)) {
					rewind($this->html_description);
				}

				$this->resetModified(); // [HL] After being saved an object is no longer 'modified'
			}

			if ($this->collArRatesRelatedByArRateId !== null) {
				foreach ($this->collArRatesRelatedByArRateId as $referrerFK) {
					if (!$referrerFK->isDeleted()) {
						$affectedRows += $referrerFK->save($con);
					}
				}
			}

			if ($this->collArRateSharedWithResellers !== null) {
				foreach ($this->collArRateSharedWithResellers as $referrerFK) {
					if (!$referrerFK->isDeleted()) {
						$affectedRows += $referrerFK->save($con);
					}
				}
			}

			if ($this->collArSpecificRateCalcs !== null) {
				foreach ($this->collArSpecificRateCalcs as $referrerFK) {
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

			if ($this->aArVendor !== null) {
				if (!$this->aArVendor->validate($columns)) {
					$failureMap = array_merge($failureMap, $this->aArVendor->getValidationFailures());
				}
			}

			if ($this->aArRateFormat !== null) {
				if (!$this->aArRateFormat->validate($columns)) {
					$failureMap = array_merge($failureMap, $this->aArRateFormat->getValidationFailures());
				}
			}

			if ($this->aArRateRelatedByArRateId !== null) {
				if (!$this->aArRateRelatedByArRateId->validate($columns)) {
					$failureMap = array_merge($failureMap, $this->aArRateRelatedByArRateId->getValidationFailures());
				}
			}


			if (($retval = ArRatePeer::doValidate($this, $columns)) !== true) {
				$failureMap = array_merge($failureMap, $retval);
			}


				if ($this->collArRatesRelatedByArRateId !== null) {
					foreach ($this->collArRatesRelatedByArRateId as $referrerFK) {
						if (!$referrerFK->validate($columns)) {
							$failureMap = array_merge($failureMap, $referrerFK->getValidationFailures());
						}
					}
				}

				if ($this->collArRateSharedWithResellers !== null) {
					foreach ($this->collArRateSharedWithResellers as $referrerFK) {
						if (!$referrerFK->validate($columns)) {
							$failureMap = array_merge($failureMap, $referrerFK->getValidationFailures());
						}
					}
				}

				if ($this->collArSpecificRateCalcs !== null) {
					foreach ($this->collArSpecificRateCalcs as $referrerFK) {
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
		$pos = ArRatePeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
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
				return $this->getArVendorId();
				break;
			case 2:
				return $this->getArRateFormatId();
				break;
			case 3:
				return $this->getFromTime();
				break;
			case 4:
				return $this->getInternalName();
				break;
			case 5:
				return $this->getArRateId();
				break;
			case 6:
				return $this->getShortDescription();
				break;
			case 7:
				return $this->getNote();
				break;
			case 8:
				return $this->getIsExportedToResellers();
				break;
			case 9:
				return $this->getWasCompiled();
				break;
			case 10:
				return $this->getSourceDataFile();
				break;
			case 11:
				return $this->getBackupSourceDataFile();
				break;
			case 12:
				return $this->getHtmlDescription();
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
		$keys = ArRatePeer::getFieldNames($keyType);
		$result = array(
			$keys[0] => $this->getId(),
			$keys[1] => $this->getArVendorId(),
			$keys[2] => $this->getArRateFormatId(),
			$keys[3] => $this->getFromTime(),
			$keys[4] => $this->getInternalName(),
			$keys[5] => $this->getArRateId(),
			$keys[6] => $this->getShortDescription(),
			$keys[7] => $this->getNote(),
			$keys[8] => $this->getIsExportedToResellers(),
			$keys[9] => $this->getWasCompiled(),
			$keys[10] => $this->getSourceDataFile(),
			$keys[11] => $this->getBackupSourceDataFile(),
			$keys[12] => $this->getHtmlDescription(),
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
		$pos = ArRatePeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
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
				$this->setArVendorId($value);
				break;
			case 2:
				$this->setArRateFormatId($value);
				break;
			case 3:
				$this->setFromTime($value);
				break;
			case 4:
				$this->setInternalName($value);
				break;
			case 5:
				$this->setArRateId($value);
				break;
			case 6:
				$this->setShortDescription($value);
				break;
			case 7:
				$this->setNote($value);
				break;
			case 8:
				$this->setIsExportedToResellers($value);
				break;
			case 9:
				$this->setWasCompiled($value);
				break;
			case 10:
				$this->setSourceDataFile($value);
				break;
			case 11:
				$this->setBackupSourceDataFile($value);
				break;
			case 12:
				$this->setHtmlDescription($value);
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
		$keys = ArRatePeer::getFieldNames($keyType);

		if (array_key_exists($keys[0], $arr)) $this->setId($arr[$keys[0]]);
		if (array_key_exists($keys[1], $arr)) $this->setArVendorId($arr[$keys[1]]);
		if (array_key_exists($keys[2], $arr)) $this->setArRateFormatId($arr[$keys[2]]);
		if (array_key_exists($keys[3], $arr)) $this->setFromTime($arr[$keys[3]]);
		if (array_key_exists($keys[4], $arr)) $this->setInternalName($arr[$keys[4]]);
		if (array_key_exists($keys[5], $arr)) $this->setArRateId($arr[$keys[5]]);
		if (array_key_exists($keys[6], $arr)) $this->setShortDescription($arr[$keys[6]]);
		if (array_key_exists($keys[7], $arr)) $this->setNote($arr[$keys[7]]);
		if (array_key_exists($keys[8], $arr)) $this->setIsExportedToResellers($arr[$keys[8]]);
		if (array_key_exists($keys[9], $arr)) $this->setWasCompiled($arr[$keys[9]]);
		if (array_key_exists($keys[10], $arr)) $this->setSourceDataFile($arr[$keys[10]]);
		if (array_key_exists($keys[11], $arr)) $this->setBackupSourceDataFile($arr[$keys[11]]);
		if (array_key_exists($keys[12], $arr)) $this->setHtmlDescription($arr[$keys[12]]);
	}

	/**
	 * Build a Criteria object containing the values of all modified columns in this object.
	 *
	 * @return     Criteria The Criteria object containing all modified values.
	 */
	public function buildCriteria()
	{
		$criteria = new Criteria(ArRatePeer::DATABASE_NAME);

		if ($this->isColumnModified(ArRatePeer::ID)) $criteria->add(ArRatePeer::ID, $this->id);
		if ($this->isColumnModified(ArRatePeer::AR_VENDOR_ID)) $criteria->add(ArRatePeer::AR_VENDOR_ID, $this->ar_vendor_id);
		if ($this->isColumnModified(ArRatePeer::AR_RATE_FORMAT_ID)) $criteria->add(ArRatePeer::AR_RATE_FORMAT_ID, $this->ar_rate_format_id);
		if ($this->isColumnModified(ArRatePeer::FROM_TIME)) $criteria->add(ArRatePeer::FROM_TIME, $this->from_time);
		if ($this->isColumnModified(ArRatePeer::INTERNAL_NAME)) $criteria->add(ArRatePeer::INTERNAL_NAME, $this->internal_name);
		if ($this->isColumnModified(ArRatePeer::AR_RATE_ID)) $criteria->add(ArRatePeer::AR_RATE_ID, $this->ar_rate_id);
		if ($this->isColumnModified(ArRatePeer::SHORT_DESCRIPTION)) $criteria->add(ArRatePeer::SHORT_DESCRIPTION, $this->short_description);
		if ($this->isColumnModified(ArRatePeer::NOTE)) $criteria->add(ArRatePeer::NOTE, $this->note);
		if ($this->isColumnModified(ArRatePeer::IS_EXPORTED_TO_RESELLERS)) $criteria->add(ArRatePeer::IS_EXPORTED_TO_RESELLERS, $this->is_exported_to_resellers);
		if ($this->isColumnModified(ArRatePeer::WAS_COMPILED)) $criteria->add(ArRatePeer::WAS_COMPILED, $this->was_compiled);
		if ($this->isColumnModified(ArRatePeer::SOURCE_DATA_FILE)) $criteria->add(ArRatePeer::SOURCE_DATA_FILE, $this->source_data_file);
		if ($this->isColumnModified(ArRatePeer::BACKUP_SOURCE_DATA_FILE)) $criteria->add(ArRatePeer::BACKUP_SOURCE_DATA_FILE, $this->backup_source_data_file);
		if ($this->isColumnModified(ArRatePeer::HTML_DESCRIPTION)) $criteria->add(ArRatePeer::HTML_DESCRIPTION, $this->html_description);

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
		$criteria = new Criteria(ArRatePeer::DATABASE_NAME);

		$criteria->add(ArRatePeer::ID, $this->id);

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
	 * @param      object $copyObj An object of ArRate (or compatible) type.
	 * @param      boolean $deepCopy Whether to also copy all rows that refer (by fkey) to the current row.
	 * @throws     PropelException
	 */
	public function copyInto($copyObj, $deepCopy = false)
	{

		$copyObj->setArVendorId($this->ar_vendor_id);

		$copyObj->setArRateFormatId($this->ar_rate_format_id);

		$copyObj->setFromTime($this->from_time);

		$copyObj->setInternalName($this->internal_name);

		$copyObj->setArRateId($this->ar_rate_id);

		$copyObj->setShortDescription($this->short_description);

		$copyObj->setNote($this->note);

		$copyObj->setIsExportedToResellers($this->is_exported_to_resellers);

		$copyObj->setWasCompiled($this->was_compiled);

		$copyObj->setSourceDataFile($this->source_data_file);

		$copyObj->setBackupSourceDataFile($this->backup_source_data_file);

		$copyObj->setHtmlDescription($this->html_description);


		if ($deepCopy) {
			// important: temporarily setNew(false) because this affects the behavior of
			// the getter/setter methods for fkey referrer objects.
			$copyObj->setNew(false);

			foreach ($this->getArRatesRelatedByArRateId() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArRateRelatedByArRateId($relObj->copy($deepCopy));
				}
			}

			foreach ($this->getArRateSharedWithResellers() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArRateSharedWithReseller($relObj->copy($deepCopy));
				}
			}

			foreach ($this->getArSpecificRateCalcs() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArSpecificRateCalc($relObj->copy($deepCopy));
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
	 * @return     ArRate Clone of current object.
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
	 * @return     ArRatePeer
	 */
	public function getPeer()
	{
		if (self::$peer === null) {
			self::$peer = new ArRatePeer();
		}
		return self::$peer;
	}

	/**
	 * Declares an association between this object and a ArVendor object.
	 *
	 * @param      ArVendor $v
	 * @return     ArRate The current object (for fluent API support)
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
			$v->addArRate($this);
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
			   $this->aArVendor->addArRates($this);
			 */
		}
		return $this->aArVendor;
	}

	/**
	 * Declares an association between this object and a ArRateFormat object.
	 *
	 * @param      ArRateFormat $v
	 * @return     ArRate The current object (for fluent API support)
	 * @throws     PropelException
	 */
	public function setArRateFormat(ArRateFormat $v = null)
	{
		if ($v === null) {
			$this->setArRateFormatId(NULL);
		} else {
			$this->setArRateFormatId($v->getId());
		}

		$this->aArRateFormat = $v;

		// Add binding for other direction of this n:n relationship.
		// If this object has already been added to the ArRateFormat object, it will not be re-added.
		if ($v !== null) {
			$v->addArRate($this);
		}

		return $this;
	}


	/**
	 * Get the associated ArRateFormat object
	 *
	 * @param      PropelPDO Optional Connection object.
	 * @return     ArRateFormat The associated ArRateFormat object.
	 * @throws     PropelException
	 */
	public function getArRateFormat(PropelPDO $con = null)
	{
		if ($this->aArRateFormat === null && ($this->ar_rate_format_id !== null)) {
			$this->aArRateFormat = ArRateFormatPeer::retrieveByPk($this->ar_rate_format_id);
			/* The following can be used additionally to
			   guarantee the related object contains a reference
			   to this object.  This level of coupling may, however, be
			   undesirable since it could result in an only partially populated collection
			   in the referenced object.
			   $this->aArRateFormat->addArRates($this);
			 */
		}
		return $this->aArRateFormat;
	}

	/**
	 * Declares an association between this object and a ArRate object.
	 *
	 * @param      ArRate $v
	 * @return     ArRate The current object (for fluent API support)
	 * @throws     PropelException
	 */
	public function setArRateRelatedByArRateId(ArRate $v = null)
	{
		if ($v === null) {
			$this->setArRateId(NULL);
		} else {
			$this->setArRateId($v->getId());
		}

		$this->aArRateRelatedByArRateId = $v;

		// Add binding for other direction of this n:n relationship.
		// If this object has already been added to the ArRate object, it will not be re-added.
		if ($v !== null) {
			$v->addArRateRelatedByArRateId($this);
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
	public function getArRateRelatedByArRateId(PropelPDO $con = null)
	{
		if ($this->aArRateRelatedByArRateId === null && ($this->ar_rate_id !== null)) {
			$this->aArRateRelatedByArRateId = ArRatePeer::retrieveByPk($this->ar_rate_id);
			/* The following can be used additionally to
			   guarantee the related object contains a reference
			   to this object.  This level of coupling may, however, be
			   undesirable since it could result in an only partially populated collection
			   in the referenced object.
			   $this->aArRateRelatedByArRateId->addArRatesRelatedByArRateId($this);
			 */
		}
		return $this->aArRateRelatedByArRateId;
	}

	/**
	 * Clears out the collArRatesRelatedByArRateId collection (array).
	 *
	 * This does not modify the database; however, it will remove any associated objects, causing
	 * them to be refetched by subsequent calls to accessor method.
	 *
	 * @return     void
	 * @see        addArRatesRelatedByArRateId()
	 */
	public function clearArRatesRelatedByArRateId()
	{
		$this->collArRatesRelatedByArRateId = null; // important to set this to NULL since that means it is uninitialized
	}

	/**
	 * Initializes the collArRatesRelatedByArRateId collection (array).
	 *
	 * By default this just sets the collArRatesRelatedByArRateId collection to an empty array (like clearcollArRatesRelatedByArRateId());
	 * however, you may wish to override this method in your stub class to provide setting appropriate
	 * to your application -- for example, setting the initial array to the values stored in database.
	 *
	 * @return     void
	 */
	public function initArRatesRelatedByArRateId()
	{
		$this->collArRatesRelatedByArRateId = array();
	}

	/**
	 * Gets an array of ArRate objects which contain a foreign key that references this object.
	 *
	 * If this collection has already been initialized with an identical Criteria, it returns the collection.
	 * Otherwise if this ArRate has previously been saved, it will retrieve
	 * related ArRatesRelatedByArRateId from storage. If this ArRate is new, it will return
	 * an empty collection or the current collection, the criteria is ignored on a new object.
	 *
	 * @param      PropelPDO $con
	 * @param      Criteria $criteria
	 * @return     array ArRate[]
	 * @throws     PropelException
	 */
	public function getArRatesRelatedByArRateId($criteria = null, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArRatePeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArRatesRelatedByArRateId === null) {
			if ($this->isNew()) {
			   $this->collArRatesRelatedByArRateId = array();
			} else {

				$criteria->add(ArRatePeer::AR_RATE_ID, $this->id);

				ArRatePeer::addSelectColumns($criteria);
				$this->collArRatesRelatedByArRateId = ArRatePeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArRatePeer::AR_RATE_ID, $this->id);

				ArRatePeer::addSelectColumns($criteria);
				if (!isset($this->lastArRateRelatedByArRateIdCriteria) || !$this->lastArRateRelatedByArRateIdCriteria->equals($criteria)) {
					$this->collArRatesRelatedByArRateId = ArRatePeer::doSelect($criteria, $con);
				}
			}
		}
		$this->lastArRateRelatedByArRateIdCriteria = $criteria;
		return $this->collArRatesRelatedByArRateId;
	}

	/**
	 * Returns the number of related ArRate objects.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct
	 * @param      PropelPDO $con
	 * @return     int Count of related ArRate objects.
	 * @throws     PropelException
	 */
	public function countArRatesRelatedByArRateId(Criteria $criteria = null, $distinct = false, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArRatePeer::DATABASE_NAME);
		} else {
			$criteria = clone $criteria;
		}

		if ($distinct) {
			$criteria->setDistinct();
		}

		$count = null;

		if ($this->collArRatesRelatedByArRateId === null) {
			if ($this->isNew()) {
				$count = 0;
			} else {

				$criteria->add(ArRatePeer::AR_RATE_ID, $this->id);

				$count = ArRatePeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArRatePeer::AR_RATE_ID, $this->id);

				if (!isset($this->lastArRateRelatedByArRateIdCriteria) || !$this->lastArRateRelatedByArRateIdCriteria->equals($criteria)) {
					$count = ArRatePeer::doCount($criteria, false, $con);
				} else {
					$count = count($this->collArRatesRelatedByArRateId);
				}
			} else {
				$count = count($this->collArRatesRelatedByArRateId);
			}
		}
		return $count;
	}

	/**
	 * Method called to associate a ArRate object to this object
	 * through the ArRate foreign key attribute.
	 *
	 * @param      ArRate $l ArRate
	 * @return     void
	 * @throws     PropelException
	 */
	public function addArRateRelatedByArRateId(ArRate $l)
	{
		if ($this->collArRatesRelatedByArRateId === null) {
			$this->initArRatesRelatedByArRateId();
		}
		if (!in_array($l, $this->collArRatesRelatedByArRateId, true)) { // only add it if the **same** object is not already associated
			array_push($this->collArRatesRelatedByArRateId, $l);
			$l->setArRateRelatedByArRateId($this);
		}
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArRate is new, it will return
	 * an empty collection; or if this ArRate has previously
	 * been saved, it will retrieve related ArRatesRelatedByArRateId from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArRate.
	 */
	public function getArRatesRelatedByArRateIdJoinArVendor($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArRatePeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArRatesRelatedByArRateId === null) {
			if ($this->isNew()) {
				$this->collArRatesRelatedByArRateId = array();
			} else {

				$criteria->add(ArRatePeer::AR_RATE_ID, $this->id);

				$this->collArRatesRelatedByArRateId = ArRatePeer::doSelectJoinArVendor($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArRatePeer::AR_RATE_ID, $this->id);

			if (!isset($this->lastArRateRelatedByArRateIdCriteria) || !$this->lastArRateRelatedByArRateIdCriteria->equals($criteria)) {
				$this->collArRatesRelatedByArRateId = ArRatePeer::doSelectJoinArVendor($criteria, $con, $join_behavior);
			}
		}
		$this->lastArRateRelatedByArRateIdCriteria = $criteria;

		return $this->collArRatesRelatedByArRateId;
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArRate is new, it will return
	 * an empty collection; or if this ArRate has previously
	 * been saved, it will retrieve related ArRatesRelatedByArRateId from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArRate.
	 */
	public function getArRatesRelatedByArRateIdJoinArRateFormat($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArRatePeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArRatesRelatedByArRateId === null) {
			if ($this->isNew()) {
				$this->collArRatesRelatedByArRateId = array();
			} else {

				$criteria->add(ArRatePeer::AR_RATE_ID, $this->id);

				$this->collArRatesRelatedByArRateId = ArRatePeer::doSelectJoinArRateFormat($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArRatePeer::AR_RATE_ID, $this->id);

			if (!isset($this->lastArRateRelatedByArRateIdCriteria) || !$this->lastArRateRelatedByArRateIdCriteria->equals($criteria)) {
				$this->collArRatesRelatedByArRateId = ArRatePeer::doSelectJoinArRateFormat($criteria, $con, $join_behavior);
			}
		}
		$this->lastArRateRelatedByArRateIdCriteria = $criteria;

		return $this->collArRatesRelatedByArRateId;
	}

	/**
	 * Clears out the collArRateSharedWithResellers collection (array).
	 *
	 * This does not modify the database; however, it will remove any associated objects, causing
	 * them to be refetched by subsequent calls to accessor method.
	 *
	 * @return     void
	 * @see        addArRateSharedWithResellers()
	 */
	public function clearArRateSharedWithResellers()
	{
		$this->collArRateSharedWithResellers = null; // important to set this to NULL since that means it is uninitialized
	}

	/**
	 * Initializes the collArRateSharedWithResellers collection (array).
	 *
	 * By default this just sets the collArRateSharedWithResellers collection to an empty array (like clearcollArRateSharedWithResellers());
	 * however, you may wish to override this method in your stub class to provide setting appropriate
	 * to your application -- for example, setting the initial array to the values stored in database.
	 *
	 * @return     void
	 */
	public function initArRateSharedWithResellers()
	{
		$this->collArRateSharedWithResellers = array();
	}

	/**
	 * Gets an array of ArRateSharedWithReseller objects which contain a foreign key that references this object.
	 *
	 * If this collection has already been initialized with an identical Criteria, it returns the collection.
	 * Otherwise if this ArRate has previously been saved, it will retrieve
	 * related ArRateSharedWithResellers from storage. If this ArRate is new, it will return
	 * an empty collection or the current collection, the criteria is ignored on a new object.
	 *
	 * @param      PropelPDO $con
	 * @param      Criteria $criteria
	 * @return     array ArRateSharedWithReseller[]
	 * @throws     PropelException
	 */
	public function getArRateSharedWithResellers($criteria = null, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArRatePeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArRateSharedWithResellers === null) {
			if ($this->isNew()) {
			   $this->collArRateSharedWithResellers = array();
			} else {

				$criteria->add(ArRateSharedWithResellerPeer::AR_RATE_ID, $this->id);

				ArRateSharedWithResellerPeer::addSelectColumns($criteria);
				$this->collArRateSharedWithResellers = ArRateSharedWithResellerPeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArRateSharedWithResellerPeer::AR_RATE_ID, $this->id);

				ArRateSharedWithResellerPeer::addSelectColumns($criteria);
				if (!isset($this->lastArRateSharedWithResellerCriteria) || !$this->lastArRateSharedWithResellerCriteria->equals($criteria)) {
					$this->collArRateSharedWithResellers = ArRateSharedWithResellerPeer::doSelect($criteria, $con);
				}
			}
		}
		$this->lastArRateSharedWithResellerCriteria = $criteria;
		return $this->collArRateSharedWithResellers;
	}

	/**
	 * Returns the number of related ArRateSharedWithReseller objects.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct
	 * @param      PropelPDO $con
	 * @return     int Count of related ArRateSharedWithReseller objects.
	 * @throws     PropelException
	 */
	public function countArRateSharedWithResellers(Criteria $criteria = null, $distinct = false, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArRatePeer::DATABASE_NAME);
		} else {
			$criteria = clone $criteria;
		}

		if ($distinct) {
			$criteria->setDistinct();
		}

		$count = null;

		if ($this->collArRateSharedWithResellers === null) {
			if ($this->isNew()) {
				$count = 0;
			} else {

				$criteria->add(ArRateSharedWithResellerPeer::AR_RATE_ID, $this->id);

				$count = ArRateSharedWithResellerPeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArRateSharedWithResellerPeer::AR_RATE_ID, $this->id);

				if (!isset($this->lastArRateSharedWithResellerCriteria) || !$this->lastArRateSharedWithResellerCriteria->equals($criteria)) {
					$count = ArRateSharedWithResellerPeer::doCount($criteria, false, $con);
				} else {
					$count = count($this->collArRateSharedWithResellers);
				}
			} else {
				$count = count($this->collArRateSharedWithResellers);
			}
		}
		return $count;
	}

	/**
	 * Method called to associate a ArRateSharedWithReseller object to this object
	 * through the ArRateSharedWithReseller foreign key attribute.
	 *
	 * @param      ArRateSharedWithReseller $l ArRateSharedWithReseller
	 * @return     void
	 * @throws     PropelException
	 */
	public function addArRateSharedWithReseller(ArRateSharedWithReseller $l)
	{
		if ($this->collArRateSharedWithResellers === null) {
			$this->initArRateSharedWithResellers();
		}
		if (!in_array($l, $this->collArRateSharedWithResellers, true)) { // only add it if the **same** object is not already associated
			array_push($this->collArRateSharedWithResellers, $l);
			$l->setArRate($this);
		}
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArRate is new, it will return
	 * an empty collection; or if this ArRate has previously
	 * been saved, it will retrieve related ArRateSharedWithResellers from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArRate.
	 */
	public function getArRateSharedWithResellersJoinArReseller($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArRatePeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArRateSharedWithResellers === null) {
			if ($this->isNew()) {
				$this->collArRateSharedWithResellers = array();
			} else {

				$criteria->add(ArRateSharedWithResellerPeer::AR_RATE_ID, $this->id);

				$this->collArRateSharedWithResellers = ArRateSharedWithResellerPeer::doSelectJoinArReseller($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArRateSharedWithResellerPeer::AR_RATE_ID, $this->id);

			if (!isset($this->lastArRateSharedWithResellerCriteria) || !$this->lastArRateSharedWithResellerCriteria->equals($criteria)) {
				$this->collArRateSharedWithResellers = ArRateSharedWithResellerPeer::doSelectJoinArReseller($criteria, $con, $join_behavior);
			}
		}
		$this->lastArRateSharedWithResellerCriteria = $criteria;

		return $this->collArRateSharedWithResellers;
	}

	/**
	 * Clears out the collArSpecificRateCalcs collection (array).
	 *
	 * This does not modify the database; however, it will remove any associated objects, causing
	 * them to be refetched by subsequent calls to accessor method.
	 *
	 * @return     void
	 * @see        addArSpecificRateCalcs()
	 */
	public function clearArSpecificRateCalcs()
	{
		$this->collArSpecificRateCalcs = null; // important to set this to NULL since that means it is uninitialized
	}

	/**
	 * Initializes the collArSpecificRateCalcs collection (array).
	 *
	 * By default this just sets the collArSpecificRateCalcs collection to an empty array (like clearcollArSpecificRateCalcs());
	 * however, you may wish to override this method in your stub class to provide setting appropriate
	 * to your application -- for example, setting the initial array to the values stored in database.
	 *
	 * @return     void
	 */
	public function initArSpecificRateCalcs()
	{
		$this->collArSpecificRateCalcs = array();
	}

	/**
	 * Gets an array of ArSpecificRateCalc objects which contain a foreign key that references this object.
	 *
	 * If this collection has already been initialized with an identical Criteria, it returns the collection.
	 * Otherwise if this ArRate has previously been saved, it will retrieve
	 * related ArSpecificRateCalcs from storage. If this ArRate is new, it will return
	 * an empty collection or the current collection, the criteria is ignored on a new object.
	 *
	 * @param      PropelPDO $con
	 * @param      Criteria $criteria
	 * @return     array ArSpecificRateCalc[]
	 * @throws     PropelException
	 */
	public function getArSpecificRateCalcs($criteria = null, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArRatePeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArSpecificRateCalcs === null) {
			if ($this->isNew()) {
			   $this->collArSpecificRateCalcs = array();
			} else {

				$criteria->add(ArSpecificRateCalcPeer::AR_RATE_ID, $this->id);

				ArSpecificRateCalcPeer::addSelectColumns($criteria);
				$this->collArSpecificRateCalcs = ArSpecificRateCalcPeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArSpecificRateCalcPeer::AR_RATE_ID, $this->id);

				ArSpecificRateCalcPeer::addSelectColumns($criteria);
				if (!isset($this->lastArSpecificRateCalcCriteria) || !$this->lastArSpecificRateCalcCriteria->equals($criteria)) {
					$this->collArSpecificRateCalcs = ArSpecificRateCalcPeer::doSelect($criteria, $con);
				}
			}
		}
		$this->lastArSpecificRateCalcCriteria = $criteria;
		return $this->collArSpecificRateCalcs;
	}

	/**
	 * Returns the number of related ArSpecificRateCalc objects.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct
	 * @param      PropelPDO $con
	 * @return     int Count of related ArSpecificRateCalc objects.
	 * @throws     PropelException
	 */
	public function countArSpecificRateCalcs(Criteria $criteria = null, $distinct = false, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArRatePeer::DATABASE_NAME);
		} else {
			$criteria = clone $criteria;
		}

		if ($distinct) {
			$criteria->setDistinct();
		}

		$count = null;

		if ($this->collArSpecificRateCalcs === null) {
			if ($this->isNew()) {
				$count = 0;
			} else {

				$criteria->add(ArSpecificRateCalcPeer::AR_RATE_ID, $this->id);

				$count = ArSpecificRateCalcPeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArSpecificRateCalcPeer::AR_RATE_ID, $this->id);

				if (!isset($this->lastArSpecificRateCalcCriteria) || !$this->lastArSpecificRateCalcCriteria->equals($criteria)) {
					$count = ArSpecificRateCalcPeer::doCount($criteria, false, $con);
				} else {
					$count = count($this->collArSpecificRateCalcs);
				}
			} else {
				$count = count($this->collArSpecificRateCalcs);
			}
		}
		return $count;
	}

	/**
	 * Method called to associate a ArSpecificRateCalc object to this object
	 * through the ArSpecificRateCalc foreign key attribute.
	 *
	 * @param      ArSpecificRateCalc $l ArSpecificRateCalc
	 * @return     void
	 * @throws     PropelException
	 */
	public function addArSpecificRateCalc(ArSpecificRateCalc $l)
	{
		if ($this->collArSpecificRateCalcs === null) {
			$this->initArSpecificRateCalcs();
		}
		if (!in_array($l, $this->collArSpecificRateCalcs, true)) { // only add it if the **same** object is not already associated
			array_push($this->collArSpecificRateCalcs, $l);
			$l->setArRate($this);
		}
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
			if ($this->collArRatesRelatedByArRateId) {
				foreach ((array) $this->collArRatesRelatedByArRateId as $o) {
					$o->clearAllReferences($deep);
				}
			}
			if ($this->collArRateSharedWithResellers) {
				foreach ((array) $this->collArRateSharedWithResellers as $o) {
					$o->clearAllReferences($deep);
				}
			}
			if ($this->collArSpecificRateCalcs) {
				foreach ((array) $this->collArSpecificRateCalcs as $o) {
					$o->clearAllReferences($deep);
				}
			}
		} // if ($deep)

		$this->collArRatesRelatedByArRateId = null;
		$this->collArRateSharedWithResellers = null;
		$this->collArSpecificRateCalcs = null;
			$this->aArVendor = null;
			$this->aArRateFormat = null;
			$this->aArRateRelatedByArRateId = null;
	}

} // BaseArRate
