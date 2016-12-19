<?php

/**
 * Base class that represents a row from the 'ar_report_to_read_user_view' table.
 *
 * 
 *
 * @package    lib.model.om
 */
abstract class BaseArReportToReadUserView extends BaseObject  implements Persistent {


	/**
	 * The Peer class.
	 * Instance provides a convenient way of calling static methods on a class
	 * that calling code may not be able to identify.
	 * @var        ArReportToReadUserViewPeer
	 */
	protected static $peer;

	/**
	 * The value for the id field.
	 * @var        int
	 */
	protected $id;

	/**
	 * The value for the ar_report_to_read_id field.
	 * @var        int
	 */
	protected $ar_report_to_read_id;

	/**
	 * The value for the ar_report_id field.
	 * @var        int
	 */
	protected $ar_report_id;

	/**
	 * The value for the ar_user_id field.
	 * @var        int
	 */
	protected $ar_user_id;

	/**
	 * The value for the seen_or_received_from_user field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $seen_or_received_from_user;

	/**
	 * The value for the ar_organization_unit_id field.
	 * @var        int
	 */
	protected $ar_organization_unit_id;

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
	 * The value for the produced_report_generation_date field.
	 * @var        string
	 */
	protected $produced_report_generation_date;

	/**
	 * The value for the produced_report_short_description field.
	 * @var        string
	 */
	protected $produced_report_short_description;

	/**
	 * The value for the produced_report_additional_description field.
	 * @var        string
	 */
	protected $produced_report_additional_description;

	/**
	 * The value for the produced_report_already_reviewed field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $produced_report_already_reviewed;

	/**
	 * The value for the produced_report_is_draft field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $produced_report_is_draft;

	/**
	 * @var        ArReportToRead
	 */
	protected $aArReportToRead;

	/**
	 * @var        ArReport
	 */
	protected $aArReport;

	/**
	 * @var        ArUser
	 */
	protected $aArUser;

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
	
	const PEER = 'ArReportToReadUserViewPeer';

	/**
	 * Applies default values to this object.
	 * This method should be called from the object's constructor (or
	 * equivalent initialization method).
	 * @see        __construct()
	 */
	public function applyDefaultValues()
	{
		$this->seen_or_received_from_user = false;
		$this->produced_report_already_reviewed = false;
		$this->produced_report_is_draft = false;
	}

	/**
	 * Initializes internal state of BaseArReportToReadUserView object.
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
	 * Get the [ar_report_to_read_id] column value.
	 * 
	 * @return     int
	 */
	public function getArReportToReadId()
	{
		return $this->ar_report_to_read_id;
	}

	/**
	 * Get the [ar_report_id] column value.
	 * 
	 * @return     int
	 */
	public function getArReportId()
	{
		return $this->ar_report_id;
	}

	/**
	 * Get the [ar_user_id] column value.
	 * 
	 * @return     int
	 */
	public function getArUserId()
	{
		return $this->ar_user_id;
	}

	/**
	 * Get the [seen_or_received_from_user] column value.
	 * 
	 * @return     boolean
	 */
	public function getSeenOrReceivedFromUser()
	{
		return $this->seen_or_received_from_user;
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
	 * Get the [optionally formatted] temporal [produced_report_generation_date] column value.
	 * 
	 *
	 * @param      string $format The date/time format string (either date()-style or strftime()-style).
	 *							If format is NULL, then the raw DateTime object will be returned.
	 * @return     mixed Formatted date/time value as string or DateTime object (if format is NULL), NULL if column is NULL, and 0 if column value is 0000-00-00 00:00:00
	 * @throws     PropelException - if unable to parse/validate the date/time value.
	 */
	public function getProducedReportGenerationDate($format = 'Y-m-d H:i:s')
	{
		if ($this->produced_report_generation_date === null) {
			return null;
		}


		if ($this->produced_report_generation_date === '0000-00-00 00:00:00') {
			// while technically this is not a default value of NULL,
			// this seems to be closest in meaning.
			return null;
		} else {
			try {
				$dt = new DateTime($this->produced_report_generation_date);
			} catch (Exception $x) {
				throw new PropelException("Internally stored date/time/timestamp value could not be converted to DateTime: " . var_export($this->produced_report_generation_date, true), $x);
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
	 * Get the [produced_report_short_description] column value.
	 * 
	 * @return     string
	 */
	public function getProducedReportShortDescription()
	{
		return $this->produced_report_short_description;
	}

	/**
	 * Get the [produced_report_additional_description] column value.
	 * 
	 * @return     string
	 */
	public function getProducedReportAdditionalDescription()
	{
		return $this->produced_report_additional_description;
	}

	/**
	 * Get the [produced_report_already_reviewed] column value.
	 * 
	 * @return     boolean
	 */
	public function getProducedReportAlreadyReviewed()
	{
		return $this->produced_report_already_reviewed;
	}

	/**
	 * Get the [produced_report_is_draft] column value.
	 * 
	 * @return     boolean
	 */
	public function getProducedReportIsDraft()
	{
		return $this->produced_report_is_draft;
	}

	/**
	 * Set the value of [id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArReportToReadUserView The current object (for fluent API support)
	 */
	public function setId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->id !== $v) {
			$this->id = $v;
			$this->modifiedColumns[] = ArReportToReadUserViewPeer::ID;
		}

		return $this;
	} // setId()

	/**
	 * Set the value of [ar_report_to_read_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArReportToReadUserView The current object (for fluent API support)
	 */
	public function setArReportToReadId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->ar_report_to_read_id !== $v) {
			$this->ar_report_to_read_id = $v;
			$this->modifiedColumns[] = ArReportToReadUserViewPeer::AR_REPORT_TO_READ_ID;
		}

		if ($this->aArReportToRead !== null && $this->aArReportToRead->getId() !== $v) {
			$this->aArReportToRead = null;
		}

		return $this;
	} // setArReportToReadId()

	/**
	 * Set the value of [ar_report_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArReportToReadUserView The current object (for fluent API support)
	 */
	public function setArReportId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->ar_report_id !== $v) {
			$this->ar_report_id = $v;
			$this->modifiedColumns[] = ArReportToReadUserViewPeer::AR_REPORT_ID;
		}

		if ($this->aArReport !== null && $this->aArReport->getId() !== $v) {
			$this->aArReport = null;
		}

		return $this;
	} // setArReportId()

	/**
	 * Set the value of [ar_user_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArReportToReadUserView The current object (for fluent API support)
	 */
	public function setArUserId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->ar_user_id !== $v) {
			$this->ar_user_id = $v;
			$this->modifiedColumns[] = ArReportToReadUserViewPeer::AR_USER_ID;
		}

		if ($this->aArUser !== null && $this->aArUser->getId() !== $v) {
			$this->aArUser = null;
		}

		return $this;
	} // setArUserId()

	/**
	 * Set the value of [seen_or_received_from_user] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArReportToReadUserView The current object (for fluent API support)
	 */
	public function setSeenOrReceivedFromUser($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->seen_or_received_from_user !== $v || $this->isNew()) {
			$this->seen_or_received_from_user = $v;
			$this->modifiedColumns[] = ArReportToReadUserViewPeer::SEEN_OR_RECEIVED_FROM_USER;
		}

		return $this;
	} // setSeenOrReceivedFromUser()

	/**
	 * Set the value of [ar_organization_unit_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArReportToReadUserView The current object (for fluent API support)
	 */
	public function setArOrganizationUnitId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->ar_organization_unit_id !== $v) {
			$this->ar_organization_unit_id = $v;
			$this->modifiedColumns[] = ArReportToReadUserViewPeer::AR_ORGANIZATION_UNIT_ID;
		}

		if ($this->aArOrganizationUnit !== null && $this->aArOrganizationUnit->getId() !== $v) {
			$this->aArOrganizationUnit = null;
		}

		return $this;
	} // setArOrganizationUnitId()

	/**
	 * Sets the value of [from_date] column to a normalized version of the date/time value specified.
	 * 
	 * @param      mixed $v string, integer (timestamp), or DateTime value.  Empty string will
	 *						be treated as NULL for temporal objects.
	 * @return     ArReportToReadUserView The current object (for fluent API support)
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
				$this->modifiedColumns[] = ArReportToReadUserViewPeer::FROM_DATE;
			}
		} // if either are not null

		return $this;
	} // setFromDate()

	/**
	 * Sets the value of [to_date] column to a normalized version of the date/time value specified.
	 * 
	 * @param      mixed $v string, integer (timestamp), or DateTime value.  Empty string will
	 *						be treated as NULL for temporal objects.
	 * @return     ArReportToReadUserView The current object (for fluent API support)
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
				$this->modifiedColumns[] = ArReportToReadUserViewPeer::TO_DATE;
			}
		} // if either are not null

		return $this;
	} // setToDate()

	/**
	 * Sets the value of [produced_report_generation_date] column to a normalized version of the date/time value specified.
	 * 
	 * @param      mixed $v string, integer (timestamp), or DateTime value.  Empty string will
	 *						be treated as NULL for temporal objects.
	 * @return     ArReportToReadUserView The current object (for fluent API support)
	 */
	public function setProducedReportGenerationDate($v)
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

		if ( $this->produced_report_generation_date !== null || $dt !== null ) {
			// (nested ifs are a little easier to read in this case)

			$currNorm = ($this->produced_report_generation_date !== null && $tmpDt = new DateTime($this->produced_report_generation_date)) ? $tmpDt->format('Y-m-d H:i:s') : null;
			$newNorm = ($dt !== null) ? $dt->format('Y-m-d H:i:s') : null;

			if ( ($currNorm !== $newNorm) // normalized values don't match 
					)
			{
				$this->produced_report_generation_date = ($dt ? $dt->format('Y-m-d H:i:s') : null);
				$this->modifiedColumns[] = ArReportToReadUserViewPeer::PRODUCED_REPORT_GENERATION_DATE;
			}
		} // if either are not null

		return $this;
	} // setProducedReportGenerationDate()

	/**
	 * Set the value of [produced_report_short_description] column.
	 * 
	 * @param      string $v new value
	 * @return     ArReportToReadUserView The current object (for fluent API support)
	 */
	public function setProducedReportShortDescription($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->produced_report_short_description !== $v) {
			$this->produced_report_short_description = $v;
			$this->modifiedColumns[] = ArReportToReadUserViewPeer::PRODUCED_REPORT_SHORT_DESCRIPTION;
		}

		return $this;
	} // setProducedReportShortDescription()

	/**
	 * Set the value of [produced_report_additional_description] column.
	 * 
	 * @param      string $v new value
	 * @return     ArReportToReadUserView The current object (for fluent API support)
	 */
	public function setProducedReportAdditionalDescription($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->produced_report_additional_description !== $v) {
			$this->produced_report_additional_description = $v;
			$this->modifiedColumns[] = ArReportToReadUserViewPeer::PRODUCED_REPORT_ADDITIONAL_DESCRIPTION;
		}

		return $this;
	} // setProducedReportAdditionalDescription()

	/**
	 * Set the value of [produced_report_already_reviewed] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArReportToReadUserView The current object (for fluent API support)
	 */
	public function setProducedReportAlreadyReviewed($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->produced_report_already_reviewed !== $v || $this->isNew()) {
			$this->produced_report_already_reviewed = $v;
			$this->modifiedColumns[] = ArReportToReadUserViewPeer::PRODUCED_REPORT_ALREADY_REVIEWED;
		}

		return $this;
	} // setProducedReportAlreadyReviewed()

	/**
	 * Set the value of [produced_report_is_draft] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArReportToReadUserView The current object (for fluent API support)
	 */
	public function setProducedReportIsDraft($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->produced_report_is_draft !== $v || $this->isNew()) {
			$this->produced_report_is_draft = $v;
			$this->modifiedColumns[] = ArReportToReadUserViewPeer::PRODUCED_REPORT_IS_DRAFT;
		}

		return $this;
	} // setProducedReportIsDraft()

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
			if ($this->seen_or_received_from_user !== false) {
				return false;
			}

			if ($this->produced_report_already_reviewed !== false) {
				return false;
			}

			if ($this->produced_report_is_draft !== false) {
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
			$this->ar_report_to_read_id = ($row[$startcol + 1] !== null) ? (int) $row[$startcol + 1] : null;
			$this->ar_report_id = ($row[$startcol + 2] !== null) ? (int) $row[$startcol + 2] : null;
			$this->ar_user_id = ($row[$startcol + 3] !== null) ? (int) $row[$startcol + 3] : null;
			$this->seen_or_received_from_user = ($row[$startcol + 4] !== null) ? (boolean) $row[$startcol + 4] : null;
			$this->ar_organization_unit_id = ($row[$startcol + 5] !== null) ? (int) $row[$startcol + 5] : null;
			$this->from_date = ($row[$startcol + 6] !== null) ? (string) $row[$startcol + 6] : null;
			$this->to_date = ($row[$startcol + 7] !== null) ? (string) $row[$startcol + 7] : null;
			$this->produced_report_generation_date = ($row[$startcol + 8] !== null) ? (string) $row[$startcol + 8] : null;
			$this->produced_report_short_description = ($row[$startcol + 9] !== null) ? (string) $row[$startcol + 9] : null;
			$this->produced_report_additional_description = ($row[$startcol + 10] !== null) ? (string) $row[$startcol + 10] : null;
			$this->produced_report_already_reviewed = ($row[$startcol + 11] !== null) ? (boolean) $row[$startcol + 11] : null;
			$this->produced_report_is_draft = ($row[$startcol + 12] !== null) ? (boolean) $row[$startcol + 12] : null;
			$this->resetModified();

			$this->setNew(false);

			if ($rehydrate) {
				$this->ensureConsistency();
			}

			// FIXME - using NUM_COLUMNS may be clearer.
			return $startcol + 13; // 13 = ArReportToReadUserViewPeer::NUM_COLUMNS - ArReportToReadUserViewPeer::NUM_LAZY_LOAD_COLUMNS).

		} catch (Exception $e) {
			throw new PropelException("Error populating ArReportToReadUserView object", $e);
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

		if ($this->aArReportToRead !== null && $this->ar_report_to_read_id !== $this->aArReportToRead->getId()) {
			$this->aArReportToRead = null;
		}
		if ($this->aArReport !== null && $this->ar_report_id !== $this->aArReport->getId()) {
			$this->aArReport = null;
		}
		if ($this->aArUser !== null && $this->ar_user_id !== $this->aArUser->getId()) {
			$this->aArUser = null;
		}
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
			$con = Propel::getConnection(ArReportToReadUserViewPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		// We don't need to alter the object instance pool; we're just modifying this instance
		// already in the pool.

		$stmt = ArReportToReadUserViewPeer::doSelectStmt($this->buildPkeyCriteria(), $con);
		$row = $stmt->fetch(PDO::FETCH_NUM);
		$stmt->closeCursor();
		if (!$row) {
			throw new PropelException('Cannot find matching row in the database to reload object values.');
		}
		$this->hydrate($row, 0, true); // rehydrate

		if ($deep) {  // also de-associate any related objects?

			$this->aArReportToRead = null;
			$this->aArReport = null;
			$this->aArUser = null;
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
			$con = Propel::getConnection(ArReportToReadUserViewPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}
		
		$con->beginTransaction();
		try {
			$ret = $this->preDelete($con);
			if ($ret) {
				ArReportToReadUserViewPeer::doDelete($this, $con);
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
			$con = Propel::getConnection(ArReportToReadUserViewPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
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
				ArReportToReadUserViewPeer::addInstanceToPool($this);
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

			if ($this->aArReportToRead !== null) {
				if ($this->aArReportToRead->isModified() || $this->aArReportToRead->isNew()) {
					$affectedRows += $this->aArReportToRead->save($con);
				}
				$this->setArReportToRead($this->aArReportToRead);
			}

			if ($this->aArReport !== null) {
				if ($this->aArReport->isModified() || $this->aArReport->isNew()) {
					$affectedRows += $this->aArReport->save($con);
				}
				$this->setArReport($this->aArReport);
			}

			if ($this->aArUser !== null) {
				if ($this->aArUser->isModified() || $this->aArUser->isNew()) {
					$affectedRows += $this->aArUser->save($con);
				}
				$this->setArUser($this->aArUser);
			}

			if ($this->aArOrganizationUnit !== null) {
				if ($this->aArOrganizationUnit->isModified() || $this->aArOrganizationUnit->isNew()) {
					$affectedRows += $this->aArOrganizationUnit->save($con);
				}
				$this->setArOrganizationUnit($this->aArOrganizationUnit);
			}

			if ($this->isNew() ) {
				$this->modifiedColumns[] = ArReportToReadUserViewPeer::ID;
			}

			// If this object has been modified, then save it to the database.
			if ($this->isModified()) {
				if ($this->isNew()) {
					$pk = ArReportToReadUserViewPeer::doInsert($this, $con);
					$affectedRows += 1; // we are assuming that there is only 1 row per doInsert() which
										 // should always be true here (even though technically
										 // BasePeer::doInsert() can insert multiple rows).

					$this->setId($pk);  //[IMV] update autoincrement primary key

					$this->setNew(false);
				} else {
					$affectedRows += ArReportToReadUserViewPeer::doUpdate($this, $con);
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

			if ($this->aArReportToRead !== null) {
				if (!$this->aArReportToRead->validate($columns)) {
					$failureMap = array_merge($failureMap, $this->aArReportToRead->getValidationFailures());
				}
			}

			if ($this->aArReport !== null) {
				if (!$this->aArReport->validate($columns)) {
					$failureMap = array_merge($failureMap, $this->aArReport->getValidationFailures());
				}
			}

			if ($this->aArUser !== null) {
				if (!$this->aArUser->validate($columns)) {
					$failureMap = array_merge($failureMap, $this->aArUser->getValidationFailures());
				}
			}

			if ($this->aArOrganizationUnit !== null) {
				if (!$this->aArOrganizationUnit->validate($columns)) {
					$failureMap = array_merge($failureMap, $this->aArOrganizationUnit->getValidationFailures());
				}
			}


			if (($retval = ArReportToReadUserViewPeer::doValidate($this, $columns)) !== true) {
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
		$pos = ArReportToReadUserViewPeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
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
				return $this->getArReportToReadId();
				break;
			case 2:
				return $this->getArReportId();
				break;
			case 3:
				return $this->getArUserId();
				break;
			case 4:
				return $this->getSeenOrReceivedFromUser();
				break;
			case 5:
				return $this->getArOrganizationUnitId();
				break;
			case 6:
				return $this->getFromDate();
				break;
			case 7:
				return $this->getToDate();
				break;
			case 8:
				return $this->getProducedReportGenerationDate();
				break;
			case 9:
				return $this->getProducedReportShortDescription();
				break;
			case 10:
				return $this->getProducedReportAdditionalDescription();
				break;
			case 11:
				return $this->getProducedReportAlreadyReviewed();
				break;
			case 12:
				return $this->getProducedReportIsDraft();
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
		$keys = ArReportToReadUserViewPeer::getFieldNames($keyType);
		$result = array(
			$keys[0] => $this->getId(),
			$keys[1] => $this->getArReportToReadId(),
			$keys[2] => $this->getArReportId(),
			$keys[3] => $this->getArUserId(),
			$keys[4] => $this->getSeenOrReceivedFromUser(),
			$keys[5] => $this->getArOrganizationUnitId(),
			$keys[6] => $this->getFromDate(),
			$keys[7] => $this->getToDate(),
			$keys[8] => $this->getProducedReportGenerationDate(),
			$keys[9] => $this->getProducedReportShortDescription(),
			$keys[10] => $this->getProducedReportAdditionalDescription(),
			$keys[11] => $this->getProducedReportAlreadyReviewed(),
			$keys[12] => $this->getProducedReportIsDraft(),
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
		$pos = ArReportToReadUserViewPeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
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
				$this->setArReportToReadId($value);
				break;
			case 2:
				$this->setArReportId($value);
				break;
			case 3:
				$this->setArUserId($value);
				break;
			case 4:
				$this->setSeenOrReceivedFromUser($value);
				break;
			case 5:
				$this->setArOrganizationUnitId($value);
				break;
			case 6:
				$this->setFromDate($value);
				break;
			case 7:
				$this->setToDate($value);
				break;
			case 8:
				$this->setProducedReportGenerationDate($value);
				break;
			case 9:
				$this->setProducedReportShortDescription($value);
				break;
			case 10:
				$this->setProducedReportAdditionalDescription($value);
				break;
			case 11:
				$this->setProducedReportAlreadyReviewed($value);
				break;
			case 12:
				$this->setProducedReportIsDraft($value);
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
		$keys = ArReportToReadUserViewPeer::getFieldNames($keyType);

		if (array_key_exists($keys[0], $arr)) $this->setId($arr[$keys[0]]);
		if (array_key_exists($keys[1], $arr)) $this->setArReportToReadId($arr[$keys[1]]);
		if (array_key_exists($keys[2], $arr)) $this->setArReportId($arr[$keys[2]]);
		if (array_key_exists($keys[3], $arr)) $this->setArUserId($arr[$keys[3]]);
		if (array_key_exists($keys[4], $arr)) $this->setSeenOrReceivedFromUser($arr[$keys[4]]);
		if (array_key_exists($keys[5], $arr)) $this->setArOrganizationUnitId($arr[$keys[5]]);
		if (array_key_exists($keys[6], $arr)) $this->setFromDate($arr[$keys[6]]);
		if (array_key_exists($keys[7], $arr)) $this->setToDate($arr[$keys[7]]);
		if (array_key_exists($keys[8], $arr)) $this->setProducedReportGenerationDate($arr[$keys[8]]);
		if (array_key_exists($keys[9], $arr)) $this->setProducedReportShortDescription($arr[$keys[9]]);
		if (array_key_exists($keys[10], $arr)) $this->setProducedReportAdditionalDescription($arr[$keys[10]]);
		if (array_key_exists($keys[11], $arr)) $this->setProducedReportAlreadyReviewed($arr[$keys[11]]);
		if (array_key_exists($keys[12], $arr)) $this->setProducedReportIsDraft($arr[$keys[12]]);
	}

	/**
	 * Build a Criteria object containing the values of all modified columns in this object.
	 *
	 * @return     Criteria The Criteria object containing all modified values.
	 */
	public function buildCriteria()
	{
		$criteria = new Criteria(ArReportToReadUserViewPeer::DATABASE_NAME);

		if ($this->isColumnModified(ArReportToReadUserViewPeer::ID)) $criteria->add(ArReportToReadUserViewPeer::ID, $this->id);
		if ($this->isColumnModified(ArReportToReadUserViewPeer::AR_REPORT_TO_READ_ID)) $criteria->add(ArReportToReadUserViewPeer::AR_REPORT_TO_READ_ID, $this->ar_report_to_read_id);
		if ($this->isColumnModified(ArReportToReadUserViewPeer::AR_REPORT_ID)) $criteria->add(ArReportToReadUserViewPeer::AR_REPORT_ID, $this->ar_report_id);
		if ($this->isColumnModified(ArReportToReadUserViewPeer::AR_USER_ID)) $criteria->add(ArReportToReadUserViewPeer::AR_USER_ID, $this->ar_user_id);
		if ($this->isColumnModified(ArReportToReadUserViewPeer::SEEN_OR_RECEIVED_FROM_USER)) $criteria->add(ArReportToReadUserViewPeer::SEEN_OR_RECEIVED_FROM_USER, $this->seen_or_received_from_user);
		if ($this->isColumnModified(ArReportToReadUserViewPeer::AR_ORGANIZATION_UNIT_ID)) $criteria->add(ArReportToReadUserViewPeer::AR_ORGANIZATION_UNIT_ID, $this->ar_organization_unit_id);
		if ($this->isColumnModified(ArReportToReadUserViewPeer::FROM_DATE)) $criteria->add(ArReportToReadUserViewPeer::FROM_DATE, $this->from_date);
		if ($this->isColumnModified(ArReportToReadUserViewPeer::TO_DATE)) $criteria->add(ArReportToReadUserViewPeer::TO_DATE, $this->to_date);
		if ($this->isColumnModified(ArReportToReadUserViewPeer::PRODUCED_REPORT_GENERATION_DATE)) $criteria->add(ArReportToReadUserViewPeer::PRODUCED_REPORT_GENERATION_DATE, $this->produced_report_generation_date);
		if ($this->isColumnModified(ArReportToReadUserViewPeer::PRODUCED_REPORT_SHORT_DESCRIPTION)) $criteria->add(ArReportToReadUserViewPeer::PRODUCED_REPORT_SHORT_DESCRIPTION, $this->produced_report_short_description);
		if ($this->isColumnModified(ArReportToReadUserViewPeer::PRODUCED_REPORT_ADDITIONAL_DESCRIPTION)) $criteria->add(ArReportToReadUserViewPeer::PRODUCED_REPORT_ADDITIONAL_DESCRIPTION, $this->produced_report_additional_description);
		if ($this->isColumnModified(ArReportToReadUserViewPeer::PRODUCED_REPORT_ALREADY_REVIEWED)) $criteria->add(ArReportToReadUserViewPeer::PRODUCED_REPORT_ALREADY_REVIEWED, $this->produced_report_already_reviewed);
		if ($this->isColumnModified(ArReportToReadUserViewPeer::PRODUCED_REPORT_IS_DRAFT)) $criteria->add(ArReportToReadUserViewPeer::PRODUCED_REPORT_IS_DRAFT, $this->produced_report_is_draft);

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
		$criteria = new Criteria(ArReportToReadUserViewPeer::DATABASE_NAME);

		$criteria->add(ArReportToReadUserViewPeer::ID, $this->id);

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
	 * @param      object $copyObj An object of ArReportToReadUserView (or compatible) type.
	 * @param      boolean $deepCopy Whether to also copy all rows that refer (by fkey) to the current row.
	 * @throws     PropelException
	 */
	public function copyInto($copyObj, $deepCopy = false)
	{

		$copyObj->setArReportToReadId($this->ar_report_to_read_id);

		$copyObj->setArReportId($this->ar_report_id);

		$copyObj->setArUserId($this->ar_user_id);

		$copyObj->setSeenOrReceivedFromUser($this->seen_or_received_from_user);

		$copyObj->setArOrganizationUnitId($this->ar_organization_unit_id);

		$copyObj->setFromDate($this->from_date);

		$copyObj->setToDate($this->to_date);

		$copyObj->setProducedReportGenerationDate($this->produced_report_generation_date);

		$copyObj->setProducedReportShortDescription($this->produced_report_short_description);

		$copyObj->setProducedReportAdditionalDescription($this->produced_report_additional_description);

		$copyObj->setProducedReportAlreadyReviewed($this->produced_report_already_reviewed);

		$copyObj->setProducedReportIsDraft($this->produced_report_is_draft);


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
	 * @return     ArReportToReadUserView Clone of current object.
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
	 * @return     ArReportToReadUserViewPeer
	 */
	public function getPeer()
	{
		if (self::$peer === null) {
			self::$peer = new ArReportToReadUserViewPeer();
		}
		return self::$peer;
	}

	/**
	 * Declares an association between this object and a ArReportToRead object.
	 *
	 * @param      ArReportToRead $v
	 * @return     ArReportToReadUserView The current object (for fluent API support)
	 * @throws     PropelException
	 */
	public function setArReportToRead(ArReportToRead $v = null)
	{
		if ($v === null) {
			$this->setArReportToReadId(NULL);
		} else {
			$this->setArReportToReadId($v->getId());
		}

		$this->aArReportToRead = $v;

		// Add binding for other direction of this n:n relationship.
		// If this object has already been added to the ArReportToRead object, it will not be re-added.
		if ($v !== null) {
			$v->addArReportToReadUserView($this);
		}

		return $this;
	}


	/**
	 * Get the associated ArReportToRead object
	 *
	 * @param      PropelPDO Optional Connection object.
	 * @return     ArReportToRead The associated ArReportToRead object.
	 * @throws     PropelException
	 */
	public function getArReportToRead(PropelPDO $con = null)
	{
		if ($this->aArReportToRead === null && ($this->ar_report_to_read_id !== null)) {
			$this->aArReportToRead = ArReportToReadPeer::retrieveByPk($this->ar_report_to_read_id);
			/* The following can be used additionally to
			   guarantee the related object contains a reference
			   to this object.  This level of coupling may, however, be
			   undesirable since it could result in an only partially populated collection
			   in the referenced object.
			   $this->aArReportToRead->addArReportToReadUserViews($this);
			 */
		}
		return $this->aArReportToRead;
	}

	/**
	 * Declares an association between this object and a ArReport object.
	 *
	 * @param      ArReport $v
	 * @return     ArReportToReadUserView The current object (for fluent API support)
	 * @throws     PropelException
	 */
	public function setArReport(ArReport $v = null)
	{
		if ($v === null) {
			$this->setArReportId(NULL);
		} else {
			$this->setArReportId($v->getId());
		}

		$this->aArReport = $v;

		// Add binding for other direction of this n:n relationship.
		// If this object has already been added to the ArReport object, it will not be re-added.
		if ($v !== null) {
			$v->addArReportToReadUserView($this);
		}

		return $this;
	}


	/**
	 * Get the associated ArReport object
	 *
	 * @param      PropelPDO Optional Connection object.
	 * @return     ArReport The associated ArReport object.
	 * @throws     PropelException
	 */
	public function getArReport(PropelPDO $con = null)
	{
		if ($this->aArReport === null && ($this->ar_report_id !== null)) {
			$this->aArReport = ArReportPeer::retrieveByPk($this->ar_report_id);
			/* The following can be used additionally to
			   guarantee the related object contains a reference
			   to this object.  This level of coupling may, however, be
			   undesirable since it could result in an only partially populated collection
			   in the referenced object.
			   $this->aArReport->addArReportToReadUserViews($this);
			 */
		}
		return $this->aArReport;
	}

	/**
	 * Declares an association between this object and a ArUser object.
	 *
	 * @param      ArUser $v
	 * @return     ArReportToReadUserView The current object (for fluent API support)
	 * @throws     PropelException
	 */
	public function setArUser(ArUser $v = null)
	{
		if ($v === null) {
			$this->setArUserId(NULL);
		} else {
			$this->setArUserId($v->getId());
		}

		$this->aArUser = $v;

		// Add binding for other direction of this n:n relationship.
		// If this object has already been added to the ArUser object, it will not be re-added.
		if ($v !== null) {
			$v->addArReportToReadUserView($this);
		}

		return $this;
	}


	/**
	 * Get the associated ArUser object
	 *
	 * @param      PropelPDO Optional Connection object.
	 * @return     ArUser The associated ArUser object.
	 * @throws     PropelException
	 */
	public function getArUser(PropelPDO $con = null)
	{
		if ($this->aArUser === null && ($this->ar_user_id !== null)) {
			$this->aArUser = ArUserPeer::retrieveByPk($this->ar_user_id);
			/* The following can be used additionally to
			   guarantee the related object contains a reference
			   to this object.  This level of coupling may, however, be
			   undesirable since it could result in an only partially populated collection
			   in the referenced object.
			   $this->aArUser->addArReportToReadUserViews($this);
			 */
		}
		return $this->aArUser;
	}

	/**
	 * Declares an association between this object and a ArOrganizationUnit object.
	 *
	 * @param      ArOrganizationUnit $v
	 * @return     ArReportToReadUserView The current object (for fluent API support)
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
			$v->addArReportToReadUserView($this);
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
			   $this->aArOrganizationUnit->addArReportToReadUserViews($this);
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

			$this->aArReportToRead = null;
			$this->aArReport = null;
			$this->aArUser = null;
			$this->aArOrganizationUnit = null;
	}

} // BaseArReportToReadUserView
