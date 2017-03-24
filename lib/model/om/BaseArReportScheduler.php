<?php

/**
 * Base class that represents a row from the 'ar_report_scheduler' table.
 *
 * 
 *
 * @package    lib.model.om
 */
abstract class BaseArReportScheduler extends BaseObject  implements Persistent {


	/**
	 * The Peer class.
	 * Instance provides a convenient way of calling static methods on a class
	 * that calling code may not be able to identify.
	 * @var        ArReportSchedulerPeer
	 */
	protected static $peer;

	/**
	 * The value for the id field.
	 * @var        int
	 */
	protected $id;

	/**
	 * The value for the is_active field.
	 * @var        boolean
	 */
	protected $is_active;

	/**
	 * The value for the last_execution_date field.
	 * @var        string
	 */
	protected $last_execution_date;

	/**
	 * The value for the last_from_date field.
	 * @var        string
	 */
	protected $last_from_date;

	/**
	 * The value for the last_to_date field.
	 * @var        string
	 */
	protected $last_to_date;

	/**
	 * The value for the ar_report_id field.
	 * @var        int
	 */
	protected $ar_report_id;

	/**
	 * The value for the ar_organization_unit_id field.
	 * @var        int
	 */
	protected $ar_organization_unit_id;

	/**
	 * The value for the short_description field.
	 * @var        string
	 */
	protected $short_description;

	/**
	 * The value for the additional_description field.
	 * @var        string
	 */
	protected $additional_description;

	/**
	 * The value for the note field.
	 * @var        string
	 */
	protected $note;

	/**
	 * The value for the produced_report_must_be_reviewed field.
	 * Note: this column has a database default value of: true
	 * @var        boolean
	 */
	protected $produced_report_must_be_reviewed;

	/**
	 * The value for the ar_report_generation_id field.
	 * @var        int
	 */
	protected $ar_report_generation_id;

	/**
	 * The value for the schedule_every_x_days field.
	 * @var        int
	 */
	protected $schedule_every_x_days;

	/**
	 * The value for the schedule_every_x_months field.
	 * @var        int
	 */
	protected $schedule_every_x_months;

	/**
	 * The value for the start_generation_after_x_hours field.
	 * Note: this column has a database default value of: 2
	 * @var        int
	 */
	protected $start_generation_after_x_hours;

	/**
	 * The value for the internal_name field.
	 * @var        string
	 */
	protected $internal_name;

	/**
	 * The value for the ar_legal_date_generation_method_id field.
	 * @var        int
	 */
	protected $ar_legal_date_generation_method_id;

	/**
	 * The value for the days_to_add_to_legal_date_generation_method field.
	 * @var        int
	 */
	protected $days_to_add_to_legal_date_generation_method;

	/**
	 * The value for the is_yearly_legal_numeration field.
	 * @var        boolean
	 */
	protected $is_yearly_legal_numeration;

	/**
	 * The value for the generate_only_if_there_is_cost field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $generate_only_if_there_is_cost;

	/**
	 * The value for the minimum_cost field.
	 * @var        string
	 */
	protected $minimum_cost;

	/**
	 * The value for the send_compact_report_list_to_accountant field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $send_compact_report_list_to_accountant;

	/**
	 * @var        ArReport
	 */
	protected $aArReport;

	/**
	 * @var        ArOrganizationUnit
	 */
	protected $aArOrganizationUnit;

	/**
	 * @var        ArReportGeneration
	 */
	protected $aArReportGeneration;

	/**
	 * @var        array ArReportSet[] Collection to store aggregation of ArReportSet objects.
	 */
	protected $collArReportSets;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArReportSets.
	 */
	private $lastArReportSetCriteria = null;

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
	
	const PEER = 'ArReportSchedulerPeer';

	/**
	 * Applies default values to this object.
	 * This method should be called from the object's constructor (or
	 * equivalent initialization method).
	 * @see        __construct()
	 */
	public function applyDefaultValues()
	{
		$this->produced_report_must_be_reviewed = true;
		$this->start_generation_after_x_hours = 2;
		$this->generate_only_if_there_is_cost = false;
		$this->send_compact_report_list_to_accountant = false;
	}

	/**
	 * Initializes internal state of BaseArReportScheduler object.
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
	 * Get the [is_active] column value.
	 * 
	 * @return     boolean
	 */
	public function getIsActive()
	{
		return $this->is_active;
	}

	/**
	 * Get the [optionally formatted] temporal [last_execution_date] column value.
	 * 
	 *
	 * @param      string $format The date/time format string (either date()-style or strftime()-style).
	 *							If format is NULL, then the raw DateTime object will be returned.
	 * @return     mixed Formatted date/time value as string or DateTime object (if format is NULL), NULL if column is NULL, and 0 if column value is 0000-00-00 00:00:00
	 * @throws     PropelException - if unable to parse/validate the date/time value.
	 */
	public function getLastExecutionDate($format = 'Y-m-d H:i:s')
	{
		if ($this->last_execution_date === null) {
			return null;
		}


		if ($this->last_execution_date === '0000-00-00 00:00:00') {
			// while technically this is not a default value of NULL,
			// this seems to be closest in meaning.
			return null;
		} else {
			try {
				$dt = new DateTime($this->last_execution_date);
			} catch (Exception $x) {
				throw new PropelException("Internally stored date/time/timestamp value could not be converted to DateTime: " . var_export($this->last_execution_date, true), $x);
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
	 * Get the [optionally formatted] temporal [last_from_date] column value.
	 * 
	 *
	 * @param      string $format The date/time format string (either date()-style or strftime()-style).
	 *							If format is NULL, then the raw DateTime object will be returned.
	 * @return     mixed Formatted date/time value as string or DateTime object (if format is NULL), NULL if column is NULL, and 0 if column value is 0000-00-00 00:00:00
	 * @throws     PropelException - if unable to parse/validate the date/time value.
	 */
	public function getLastFromDate($format = 'Y-m-d H:i:s')
	{
		if ($this->last_from_date === null) {
			return null;
		}


		if ($this->last_from_date === '0000-00-00 00:00:00') {
			// while technically this is not a default value of NULL,
			// this seems to be closest in meaning.
			return null;
		} else {
			try {
				$dt = new DateTime($this->last_from_date);
			} catch (Exception $x) {
				throw new PropelException("Internally stored date/time/timestamp value could not be converted to DateTime: " . var_export($this->last_from_date, true), $x);
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
	 * Get the [optionally formatted] temporal [last_to_date] column value.
	 * 
	 *
	 * @param      string $format The date/time format string (either date()-style or strftime()-style).
	 *							If format is NULL, then the raw DateTime object will be returned.
	 * @return     mixed Formatted date/time value as string or DateTime object (if format is NULL), NULL if column is NULL, and 0 if column value is 0000-00-00 00:00:00
	 * @throws     PropelException - if unable to parse/validate the date/time value.
	 */
	public function getLastToDate($format = 'Y-m-d H:i:s')
	{
		if ($this->last_to_date === null) {
			return null;
		}


		if ($this->last_to_date === '0000-00-00 00:00:00') {
			// while technically this is not a default value of NULL,
			// this seems to be closest in meaning.
			return null;
		} else {
			try {
				$dt = new DateTime($this->last_to_date);
			} catch (Exception $x) {
				throw new PropelException("Internally stored date/time/timestamp value could not be converted to DateTime: " . var_export($this->last_to_date, true), $x);
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
	 * Get the [ar_report_id] column value.
	 * 
	 * @return     int
	 */
	public function getArReportId()
	{
		return $this->ar_report_id;
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
	 * Get the [short_description] column value.
	 * 
	 * @return     string
	 */
	public function getShortDescription()
	{
		return $this->short_description;
	}

	/**
	 * Get the [additional_description] column value.
	 * 
	 * @return     string
	 */
	public function getAdditionalDescription()
	{
		return $this->additional_description;
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
	 * Get the [produced_report_must_be_reviewed] column value.
	 * 
	 * @return     boolean
	 */
	public function getProducedReportMustBeReviewed()
	{
		return $this->produced_report_must_be_reviewed;
	}

	/**
	 * Get the [ar_report_generation_id] column value.
	 * 
	 * @return     int
	 */
	public function getArReportGenerationId()
	{
		return $this->ar_report_generation_id;
	}

	/**
	 * Get the [schedule_every_x_days] column value.
	 * 
	 * @return     int
	 */
	public function getScheduleEveryXDays()
	{
		return $this->schedule_every_x_days;
	}

	/**
	 * Get the [schedule_every_x_months] column value.
	 * 
	 * @return     int
	 */
	public function getScheduleEveryXMonths()
	{
		return $this->schedule_every_x_months;
	}

	/**
	 * Get the [start_generation_after_x_hours] column value.
	 * 
	 * @return     int
	 */
	public function getStartGenerationAfterXHours()
	{
		return $this->start_generation_after_x_hours;
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
	 * Get the [ar_legal_date_generation_method_id] column value.
	 * 
	 * @return     int
	 */
	public function getArLegalDateGenerationMethodId()
	{
		return $this->ar_legal_date_generation_method_id;
	}

	/**
	 * Get the [days_to_add_to_legal_date_generation_method] column value.
	 * 
	 * @return     int
	 */
	public function getDaysToAddToLegalDateGenerationMethod()
	{
		return $this->days_to_add_to_legal_date_generation_method;
	}

	/**
	 * Get the [is_yearly_legal_numeration] column value.
	 * 
	 * @return     boolean
	 */
	public function getIsYearlyLegalNumeration()
	{
		return $this->is_yearly_legal_numeration;
	}

	/**
	 * Get the [generate_only_if_there_is_cost] column value.
	 * 
	 * @return     boolean
	 */
	public function getGenerateOnlyIfThereIsCost()
	{
		return $this->generate_only_if_there_is_cost;
	}

	/**
	 * Get the [minimum_cost] column value.
	 * 
	 * @return     string
	 */
	public function getMinimumCost()
	{
		return $this->minimum_cost;
	}

	/**
	 * Get the [send_compact_report_list_to_accountant] column value.
	 * 
	 * @return     boolean
	 */
	public function getSendCompactReportListToAccountant()
	{
		return $this->send_compact_report_list_to_accountant;
	}

	/**
	 * Set the value of [id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArReportScheduler The current object (for fluent API support)
	 */
	public function setId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->id !== $v) {
			$this->id = $v;
			$this->modifiedColumns[] = ArReportSchedulerPeer::ID;
		}

		return $this;
	} // setId()

	/**
	 * Set the value of [is_active] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArReportScheduler The current object (for fluent API support)
	 */
	public function setIsActive($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->is_active !== $v) {
			$this->is_active = $v;
			$this->modifiedColumns[] = ArReportSchedulerPeer::IS_ACTIVE;
		}

		return $this;
	} // setIsActive()

	/**
	 * Sets the value of [last_execution_date] column to a normalized version of the date/time value specified.
	 * 
	 * @param      mixed $v string, integer (timestamp), or DateTime value.  Empty string will
	 *						be treated as NULL for temporal objects.
	 * @return     ArReportScheduler The current object (for fluent API support)
	 */
	public function setLastExecutionDate($v)
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

		if ( $this->last_execution_date !== null || $dt !== null ) {
			// (nested ifs are a little easier to read in this case)

			$currNorm = ($this->last_execution_date !== null && $tmpDt = new DateTime($this->last_execution_date)) ? $tmpDt->format('Y-m-d H:i:s') : null;
			$newNorm = ($dt !== null) ? $dt->format('Y-m-d H:i:s') : null;

			if ( ($currNorm !== $newNorm) // normalized values don't match 
					)
			{
				$this->last_execution_date = ($dt ? $dt->format('Y-m-d H:i:s') : null);
				$this->modifiedColumns[] = ArReportSchedulerPeer::LAST_EXECUTION_DATE;
			}
		} // if either are not null

		return $this;
	} // setLastExecutionDate()

	/**
	 * Sets the value of [last_from_date] column to a normalized version of the date/time value specified.
	 * 
	 * @param      mixed $v string, integer (timestamp), or DateTime value.  Empty string will
	 *						be treated as NULL for temporal objects.
	 * @return     ArReportScheduler The current object (for fluent API support)
	 */
	public function setLastFromDate($v)
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

		if ( $this->last_from_date !== null || $dt !== null ) {
			// (nested ifs are a little easier to read in this case)

			$currNorm = ($this->last_from_date !== null && $tmpDt = new DateTime($this->last_from_date)) ? $tmpDt->format('Y-m-d H:i:s') : null;
			$newNorm = ($dt !== null) ? $dt->format('Y-m-d H:i:s') : null;

			if ( ($currNorm !== $newNorm) // normalized values don't match 
					)
			{
				$this->last_from_date = ($dt ? $dt->format('Y-m-d H:i:s') : null);
				$this->modifiedColumns[] = ArReportSchedulerPeer::LAST_FROM_DATE;
			}
		} // if either are not null

		return $this;
	} // setLastFromDate()

	/**
	 * Sets the value of [last_to_date] column to a normalized version of the date/time value specified.
	 * 
	 * @param      mixed $v string, integer (timestamp), or DateTime value.  Empty string will
	 *						be treated as NULL for temporal objects.
	 * @return     ArReportScheduler The current object (for fluent API support)
	 */
	public function setLastToDate($v)
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

		if ( $this->last_to_date !== null || $dt !== null ) {
			// (nested ifs are a little easier to read in this case)

			$currNorm = ($this->last_to_date !== null && $tmpDt = new DateTime($this->last_to_date)) ? $tmpDt->format('Y-m-d H:i:s') : null;
			$newNorm = ($dt !== null) ? $dt->format('Y-m-d H:i:s') : null;

			if ( ($currNorm !== $newNorm) // normalized values don't match 
					)
			{
				$this->last_to_date = ($dt ? $dt->format('Y-m-d H:i:s') : null);
				$this->modifiedColumns[] = ArReportSchedulerPeer::LAST_TO_DATE;
			}
		} // if either are not null

		return $this;
	} // setLastToDate()

	/**
	 * Set the value of [ar_report_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArReportScheduler The current object (for fluent API support)
	 */
	public function setArReportId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->ar_report_id !== $v) {
			$this->ar_report_id = $v;
			$this->modifiedColumns[] = ArReportSchedulerPeer::AR_REPORT_ID;
		}

		if ($this->aArReport !== null && $this->aArReport->getId() !== $v) {
			$this->aArReport = null;
		}

		return $this;
	} // setArReportId()

	/**
	 * Set the value of [ar_organization_unit_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArReportScheduler The current object (for fluent API support)
	 */
	public function setArOrganizationUnitId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->ar_organization_unit_id !== $v) {
			$this->ar_organization_unit_id = $v;
			$this->modifiedColumns[] = ArReportSchedulerPeer::AR_ORGANIZATION_UNIT_ID;
		}

		if ($this->aArOrganizationUnit !== null && $this->aArOrganizationUnit->getId() !== $v) {
			$this->aArOrganizationUnit = null;
		}

		return $this;
	} // setArOrganizationUnitId()

	/**
	 * Set the value of [short_description] column.
	 * 
	 * @param      string $v new value
	 * @return     ArReportScheduler The current object (for fluent API support)
	 */
	public function setShortDescription($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->short_description !== $v) {
			$this->short_description = $v;
			$this->modifiedColumns[] = ArReportSchedulerPeer::SHORT_DESCRIPTION;
		}

		return $this;
	} // setShortDescription()

	/**
	 * Set the value of [additional_description] column.
	 * 
	 * @param      string $v new value
	 * @return     ArReportScheduler The current object (for fluent API support)
	 */
	public function setAdditionalDescription($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->additional_description !== $v) {
			$this->additional_description = $v;
			$this->modifiedColumns[] = ArReportSchedulerPeer::ADDITIONAL_DESCRIPTION;
		}

		return $this;
	} // setAdditionalDescription()

	/**
	 * Set the value of [note] column.
	 * 
	 * @param      string $v new value
	 * @return     ArReportScheduler The current object (for fluent API support)
	 */
	public function setNote($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->note !== $v) {
			$this->note = $v;
			$this->modifiedColumns[] = ArReportSchedulerPeer::NOTE;
		}

		return $this;
	} // setNote()

	/**
	 * Set the value of [produced_report_must_be_reviewed] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArReportScheduler The current object (for fluent API support)
	 */
	public function setProducedReportMustBeReviewed($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->produced_report_must_be_reviewed !== $v || $this->isNew()) {
			$this->produced_report_must_be_reviewed = $v;
			$this->modifiedColumns[] = ArReportSchedulerPeer::PRODUCED_REPORT_MUST_BE_REVIEWED;
		}

		return $this;
	} // setProducedReportMustBeReviewed()

	/**
	 * Set the value of [ar_report_generation_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArReportScheduler The current object (for fluent API support)
	 */
	public function setArReportGenerationId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->ar_report_generation_id !== $v) {
			$this->ar_report_generation_id = $v;
			$this->modifiedColumns[] = ArReportSchedulerPeer::AR_REPORT_GENERATION_ID;
		}

		if ($this->aArReportGeneration !== null && $this->aArReportGeneration->getId() !== $v) {
			$this->aArReportGeneration = null;
		}

		return $this;
	} // setArReportGenerationId()

	/**
	 * Set the value of [schedule_every_x_days] column.
	 * 
	 * @param      int $v new value
	 * @return     ArReportScheduler The current object (for fluent API support)
	 */
	public function setScheduleEveryXDays($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->schedule_every_x_days !== $v) {
			$this->schedule_every_x_days = $v;
			$this->modifiedColumns[] = ArReportSchedulerPeer::SCHEDULE_EVERY_X_DAYS;
		}

		return $this;
	} // setScheduleEveryXDays()

	/**
	 * Set the value of [schedule_every_x_months] column.
	 * 
	 * @param      int $v new value
	 * @return     ArReportScheduler The current object (for fluent API support)
	 */
	public function setScheduleEveryXMonths($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->schedule_every_x_months !== $v) {
			$this->schedule_every_x_months = $v;
			$this->modifiedColumns[] = ArReportSchedulerPeer::SCHEDULE_EVERY_X_MONTHS;
		}

		return $this;
	} // setScheduleEveryXMonths()

	/**
	 * Set the value of [start_generation_after_x_hours] column.
	 * 
	 * @param      int $v new value
	 * @return     ArReportScheduler The current object (for fluent API support)
	 */
	public function setStartGenerationAfterXHours($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->start_generation_after_x_hours !== $v || $this->isNew()) {
			$this->start_generation_after_x_hours = $v;
			$this->modifiedColumns[] = ArReportSchedulerPeer::START_GENERATION_AFTER_X_HOURS;
		}

		return $this;
	} // setStartGenerationAfterXHours()

	/**
	 * Set the value of [internal_name] column.
	 * 
	 * @param      string $v new value
	 * @return     ArReportScheduler The current object (for fluent API support)
	 */
	public function setInternalName($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->internal_name !== $v) {
			$this->internal_name = $v;
			$this->modifiedColumns[] = ArReportSchedulerPeer::INTERNAL_NAME;
		}

		return $this;
	} // setInternalName()

	/**
	 * Set the value of [ar_legal_date_generation_method_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArReportScheduler The current object (for fluent API support)
	 */
	public function setArLegalDateGenerationMethodId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->ar_legal_date_generation_method_id !== $v) {
			$this->ar_legal_date_generation_method_id = $v;
			$this->modifiedColumns[] = ArReportSchedulerPeer::AR_LEGAL_DATE_GENERATION_METHOD_ID;
		}

		return $this;
	} // setArLegalDateGenerationMethodId()

	/**
	 * Set the value of [days_to_add_to_legal_date_generation_method] column.
	 * 
	 * @param      int $v new value
	 * @return     ArReportScheduler The current object (for fluent API support)
	 */
	public function setDaysToAddToLegalDateGenerationMethod($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->days_to_add_to_legal_date_generation_method !== $v) {
			$this->days_to_add_to_legal_date_generation_method = $v;
			$this->modifiedColumns[] = ArReportSchedulerPeer::DAYS_TO_ADD_TO_LEGAL_DATE_GENERATION_METHOD;
		}

		return $this;
	} // setDaysToAddToLegalDateGenerationMethod()

	/**
	 * Set the value of [is_yearly_legal_numeration] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArReportScheduler The current object (for fluent API support)
	 */
	public function setIsYearlyLegalNumeration($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->is_yearly_legal_numeration !== $v) {
			$this->is_yearly_legal_numeration = $v;
			$this->modifiedColumns[] = ArReportSchedulerPeer::IS_YEARLY_LEGAL_NUMERATION;
		}

		return $this;
	} // setIsYearlyLegalNumeration()

	/**
	 * Set the value of [generate_only_if_there_is_cost] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArReportScheduler The current object (for fluent API support)
	 */
	public function setGenerateOnlyIfThereIsCost($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->generate_only_if_there_is_cost !== $v || $this->isNew()) {
			$this->generate_only_if_there_is_cost = $v;
			$this->modifiedColumns[] = ArReportSchedulerPeer::GENERATE_ONLY_IF_THERE_IS_COST;
		}

		return $this;
	} // setGenerateOnlyIfThereIsCost()

	/**
	 * Set the value of [minimum_cost] column.
	 * 
	 * @param      string $v new value
	 * @return     ArReportScheduler The current object (for fluent API support)
	 */
	public function setMinimumCost($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->minimum_cost !== $v) {
			$this->minimum_cost = $v;
			$this->modifiedColumns[] = ArReportSchedulerPeer::MINIMUM_COST;
		}

		return $this;
	} // setMinimumCost()

	/**
	 * Set the value of [send_compact_report_list_to_accountant] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArReportScheduler The current object (for fluent API support)
	 */
	public function setSendCompactReportListToAccountant($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->send_compact_report_list_to_accountant !== $v || $this->isNew()) {
			$this->send_compact_report_list_to_accountant = $v;
			$this->modifiedColumns[] = ArReportSchedulerPeer::SEND_COMPACT_REPORT_LIST_TO_ACCOUNTANT;
		}

		return $this;
	} // setSendCompactReportListToAccountant()

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
			if ($this->produced_report_must_be_reviewed !== true) {
				return false;
			}

			if ($this->start_generation_after_x_hours !== 2) {
				return false;
			}

			if ($this->generate_only_if_there_is_cost !== false) {
				return false;
			}

			if ($this->send_compact_report_list_to_accountant !== false) {
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
			$this->is_active = ($row[$startcol + 1] !== null) ? (boolean) $row[$startcol + 1] : null;
			$this->last_execution_date = ($row[$startcol + 2] !== null) ? (string) $row[$startcol + 2] : null;
			$this->last_from_date = ($row[$startcol + 3] !== null) ? (string) $row[$startcol + 3] : null;
			$this->last_to_date = ($row[$startcol + 4] !== null) ? (string) $row[$startcol + 4] : null;
			$this->ar_report_id = ($row[$startcol + 5] !== null) ? (int) $row[$startcol + 5] : null;
			$this->ar_organization_unit_id = ($row[$startcol + 6] !== null) ? (int) $row[$startcol + 6] : null;
			$this->short_description = ($row[$startcol + 7] !== null) ? (string) $row[$startcol + 7] : null;
			$this->additional_description = ($row[$startcol + 8] !== null) ? (string) $row[$startcol + 8] : null;
			$this->note = ($row[$startcol + 9] !== null) ? (string) $row[$startcol + 9] : null;
			$this->produced_report_must_be_reviewed = ($row[$startcol + 10] !== null) ? (boolean) $row[$startcol + 10] : null;
			$this->ar_report_generation_id = ($row[$startcol + 11] !== null) ? (int) $row[$startcol + 11] : null;
			$this->schedule_every_x_days = ($row[$startcol + 12] !== null) ? (int) $row[$startcol + 12] : null;
			$this->schedule_every_x_months = ($row[$startcol + 13] !== null) ? (int) $row[$startcol + 13] : null;
			$this->start_generation_after_x_hours = ($row[$startcol + 14] !== null) ? (int) $row[$startcol + 14] : null;
			$this->internal_name = ($row[$startcol + 15] !== null) ? (string) $row[$startcol + 15] : null;
			$this->ar_legal_date_generation_method_id = ($row[$startcol + 16] !== null) ? (int) $row[$startcol + 16] : null;
			$this->days_to_add_to_legal_date_generation_method = ($row[$startcol + 17] !== null) ? (int) $row[$startcol + 17] : null;
			$this->is_yearly_legal_numeration = ($row[$startcol + 18] !== null) ? (boolean) $row[$startcol + 18] : null;
			$this->generate_only_if_there_is_cost = ($row[$startcol + 19] !== null) ? (boolean) $row[$startcol + 19] : null;
			$this->minimum_cost = ($row[$startcol + 20] !== null) ? (string) $row[$startcol + 20] : null;
			$this->send_compact_report_list_to_accountant = ($row[$startcol + 21] !== null) ? (boolean) $row[$startcol + 21] : null;
			$this->resetModified();

			$this->setNew(false);

			if ($rehydrate) {
				$this->ensureConsistency();
			}

			// FIXME - using NUM_COLUMNS may be clearer.
			return $startcol + 22; // 22 = ArReportSchedulerPeer::NUM_COLUMNS - ArReportSchedulerPeer::NUM_LAZY_LOAD_COLUMNS).

		} catch (Exception $e) {
			throw new PropelException("Error populating ArReportScheduler object", $e);
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

		if ($this->aArReport !== null && $this->ar_report_id !== $this->aArReport->getId()) {
			$this->aArReport = null;
		}
		if ($this->aArOrganizationUnit !== null && $this->ar_organization_unit_id !== $this->aArOrganizationUnit->getId()) {
			$this->aArOrganizationUnit = null;
		}
		if ($this->aArReportGeneration !== null && $this->ar_report_generation_id !== $this->aArReportGeneration->getId()) {
			$this->aArReportGeneration = null;
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
			$con = Propel::getConnection(ArReportSchedulerPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		// We don't need to alter the object instance pool; we're just modifying this instance
		// already in the pool.

		$stmt = ArReportSchedulerPeer::doSelectStmt($this->buildPkeyCriteria(), $con);
		$row = $stmt->fetch(PDO::FETCH_NUM);
		$stmt->closeCursor();
		if (!$row) {
			throw new PropelException('Cannot find matching row in the database to reload object values.');
		}
		$this->hydrate($row, 0, true); // rehydrate

		if ($deep) {  // also de-associate any related objects?

			$this->aArReport = null;
			$this->aArOrganizationUnit = null;
			$this->aArReportGeneration = null;
			$this->collArReportSets = null;
			$this->lastArReportSetCriteria = null;

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
			$con = Propel::getConnection(ArReportSchedulerPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}
		
		$con->beginTransaction();
		try {
			$ret = $this->preDelete($con);
			if ($ret) {
				ArReportSchedulerPeer::doDelete($this, $con);
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
			$con = Propel::getConnection(ArReportSchedulerPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
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
				ArReportSchedulerPeer::addInstanceToPool($this);
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

			if ($this->aArReport !== null) {
				if ($this->aArReport->isModified() || $this->aArReport->isNew()) {
					$affectedRows += $this->aArReport->save($con);
				}
				$this->setArReport($this->aArReport);
			}

			if ($this->aArOrganizationUnit !== null) {
				if ($this->aArOrganizationUnit->isModified() || $this->aArOrganizationUnit->isNew()) {
					$affectedRows += $this->aArOrganizationUnit->save($con);
				}
				$this->setArOrganizationUnit($this->aArOrganizationUnit);
			}

			if ($this->aArReportGeneration !== null) {
				if ($this->aArReportGeneration->isModified() || $this->aArReportGeneration->isNew()) {
					$affectedRows += $this->aArReportGeneration->save($con);
				}
				$this->setArReportGeneration($this->aArReportGeneration);
			}

			if ($this->isNew() ) {
				$this->modifiedColumns[] = ArReportSchedulerPeer::ID;
			}

			// If this object has been modified, then save it to the database.
			if ($this->isModified()) {
				if ($this->isNew()) {
					$pk = ArReportSchedulerPeer::doInsert($this, $con);
					$affectedRows += 1; // we are assuming that there is only 1 row per doInsert() which
										 // should always be true here (even though technically
										 // BasePeer::doInsert() can insert multiple rows).

					$this->setId($pk);  //[IMV] update autoincrement primary key

					$this->setNew(false);
				} else {
					$affectedRows += ArReportSchedulerPeer::doUpdate($this, $con);
				}

				$this->resetModified(); // [HL] After being saved an object is no longer 'modified'
			}

			if ($this->collArReportSets !== null) {
				foreach ($this->collArReportSets as $referrerFK) {
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

			if ($this->aArReport !== null) {
				if (!$this->aArReport->validate($columns)) {
					$failureMap = array_merge($failureMap, $this->aArReport->getValidationFailures());
				}
			}

			if ($this->aArOrganizationUnit !== null) {
				if (!$this->aArOrganizationUnit->validate($columns)) {
					$failureMap = array_merge($failureMap, $this->aArOrganizationUnit->getValidationFailures());
				}
			}

			if ($this->aArReportGeneration !== null) {
				if (!$this->aArReportGeneration->validate($columns)) {
					$failureMap = array_merge($failureMap, $this->aArReportGeneration->getValidationFailures());
				}
			}


			if (($retval = ArReportSchedulerPeer::doValidate($this, $columns)) !== true) {
				$failureMap = array_merge($failureMap, $retval);
			}


				if ($this->collArReportSets !== null) {
					foreach ($this->collArReportSets as $referrerFK) {
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
		$pos = ArReportSchedulerPeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
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
				return $this->getIsActive();
				break;
			case 2:
				return $this->getLastExecutionDate();
				break;
			case 3:
				return $this->getLastFromDate();
				break;
			case 4:
				return $this->getLastToDate();
				break;
			case 5:
				return $this->getArReportId();
				break;
			case 6:
				return $this->getArOrganizationUnitId();
				break;
			case 7:
				return $this->getShortDescription();
				break;
			case 8:
				return $this->getAdditionalDescription();
				break;
			case 9:
				return $this->getNote();
				break;
			case 10:
				return $this->getProducedReportMustBeReviewed();
				break;
			case 11:
				return $this->getArReportGenerationId();
				break;
			case 12:
				return $this->getScheduleEveryXDays();
				break;
			case 13:
				return $this->getScheduleEveryXMonths();
				break;
			case 14:
				return $this->getStartGenerationAfterXHours();
				break;
			case 15:
				return $this->getInternalName();
				break;
			case 16:
				return $this->getArLegalDateGenerationMethodId();
				break;
			case 17:
				return $this->getDaysToAddToLegalDateGenerationMethod();
				break;
			case 18:
				return $this->getIsYearlyLegalNumeration();
				break;
			case 19:
				return $this->getGenerateOnlyIfThereIsCost();
				break;
			case 20:
				return $this->getMinimumCost();
				break;
			case 21:
				return $this->getSendCompactReportListToAccountant();
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
		$keys = ArReportSchedulerPeer::getFieldNames($keyType);
		$result = array(
			$keys[0] => $this->getId(),
			$keys[1] => $this->getIsActive(),
			$keys[2] => $this->getLastExecutionDate(),
			$keys[3] => $this->getLastFromDate(),
			$keys[4] => $this->getLastToDate(),
			$keys[5] => $this->getArReportId(),
			$keys[6] => $this->getArOrganizationUnitId(),
			$keys[7] => $this->getShortDescription(),
			$keys[8] => $this->getAdditionalDescription(),
			$keys[9] => $this->getNote(),
			$keys[10] => $this->getProducedReportMustBeReviewed(),
			$keys[11] => $this->getArReportGenerationId(),
			$keys[12] => $this->getScheduleEveryXDays(),
			$keys[13] => $this->getScheduleEveryXMonths(),
			$keys[14] => $this->getStartGenerationAfterXHours(),
			$keys[15] => $this->getInternalName(),
			$keys[16] => $this->getArLegalDateGenerationMethodId(),
			$keys[17] => $this->getDaysToAddToLegalDateGenerationMethod(),
			$keys[18] => $this->getIsYearlyLegalNumeration(),
			$keys[19] => $this->getGenerateOnlyIfThereIsCost(),
			$keys[20] => $this->getMinimumCost(),
			$keys[21] => $this->getSendCompactReportListToAccountant(),
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
		$pos = ArReportSchedulerPeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
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
				$this->setIsActive($value);
				break;
			case 2:
				$this->setLastExecutionDate($value);
				break;
			case 3:
				$this->setLastFromDate($value);
				break;
			case 4:
				$this->setLastToDate($value);
				break;
			case 5:
				$this->setArReportId($value);
				break;
			case 6:
				$this->setArOrganizationUnitId($value);
				break;
			case 7:
				$this->setShortDescription($value);
				break;
			case 8:
				$this->setAdditionalDescription($value);
				break;
			case 9:
				$this->setNote($value);
				break;
			case 10:
				$this->setProducedReportMustBeReviewed($value);
				break;
			case 11:
				$this->setArReportGenerationId($value);
				break;
			case 12:
				$this->setScheduleEveryXDays($value);
				break;
			case 13:
				$this->setScheduleEveryXMonths($value);
				break;
			case 14:
				$this->setStartGenerationAfterXHours($value);
				break;
			case 15:
				$this->setInternalName($value);
				break;
			case 16:
				$this->setArLegalDateGenerationMethodId($value);
				break;
			case 17:
				$this->setDaysToAddToLegalDateGenerationMethod($value);
				break;
			case 18:
				$this->setIsYearlyLegalNumeration($value);
				break;
			case 19:
				$this->setGenerateOnlyIfThereIsCost($value);
				break;
			case 20:
				$this->setMinimumCost($value);
				break;
			case 21:
				$this->setSendCompactReportListToAccountant($value);
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
		$keys = ArReportSchedulerPeer::getFieldNames($keyType);

		if (array_key_exists($keys[0], $arr)) $this->setId($arr[$keys[0]]);
		if (array_key_exists($keys[1], $arr)) $this->setIsActive($arr[$keys[1]]);
		if (array_key_exists($keys[2], $arr)) $this->setLastExecutionDate($arr[$keys[2]]);
		if (array_key_exists($keys[3], $arr)) $this->setLastFromDate($arr[$keys[3]]);
		if (array_key_exists($keys[4], $arr)) $this->setLastToDate($arr[$keys[4]]);
		if (array_key_exists($keys[5], $arr)) $this->setArReportId($arr[$keys[5]]);
		if (array_key_exists($keys[6], $arr)) $this->setArOrganizationUnitId($arr[$keys[6]]);
		if (array_key_exists($keys[7], $arr)) $this->setShortDescription($arr[$keys[7]]);
		if (array_key_exists($keys[8], $arr)) $this->setAdditionalDescription($arr[$keys[8]]);
		if (array_key_exists($keys[9], $arr)) $this->setNote($arr[$keys[9]]);
		if (array_key_exists($keys[10], $arr)) $this->setProducedReportMustBeReviewed($arr[$keys[10]]);
		if (array_key_exists($keys[11], $arr)) $this->setArReportGenerationId($arr[$keys[11]]);
		if (array_key_exists($keys[12], $arr)) $this->setScheduleEveryXDays($arr[$keys[12]]);
		if (array_key_exists($keys[13], $arr)) $this->setScheduleEveryXMonths($arr[$keys[13]]);
		if (array_key_exists($keys[14], $arr)) $this->setStartGenerationAfterXHours($arr[$keys[14]]);
		if (array_key_exists($keys[15], $arr)) $this->setInternalName($arr[$keys[15]]);
		if (array_key_exists($keys[16], $arr)) $this->setArLegalDateGenerationMethodId($arr[$keys[16]]);
		if (array_key_exists($keys[17], $arr)) $this->setDaysToAddToLegalDateGenerationMethod($arr[$keys[17]]);
		if (array_key_exists($keys[18], $arr)) $this->setIsYearlyLegalNumeration($arr[$keys[18]]);
		if (array_key_exists($keys[19], $arr)) $this->setGenerateOnlyIfThereIsCost($arr[$keys[19]]);
		if (array_key_exists($keys[20], $arr)) $this->setMinimumCost($arr[$keys[20]]);
		if (array_key_exists($keys[21], $arr)) $this->setSendCompactReportListToAccountant($arr[$keys[21]]);
	}

	/**
	 * Build a Criteria object containing the values of all modified columns in this object.
	 *
	 * @return     Criteria The Criteria object containing all modified values.
	 */
	public function buildCriteria()
	{
		$criteria = new Criteria(ArReportSchedulerPeer::DATABASE_NAME);

		if ($this->isColumnModified(ArReportSchedulerPeer::ID)) $criteria->add(ArReportSchedulerPeer::ID, $this->id);
		if ($this->isColumnModified(ArReportSchedulerPeer::IS_ACTIVE)) $criteria->add(ArReportSchedulerPeer::IS_ACTIVE, $this->is_active);
		if ($this->isColumnModified(ArReportSchedulerPeer::LAST_EXECUTION_DATE)) $criteria->add(ArReportSchedulerPeer::LAST_EXECUTION_DATE, $this->last_execution_date);
		if ($this->isColumnModified(ArReportSchedulerPeer::LAST_FROM_DATE)) $criteria->add(ArReportSchedulerPeer::LAST_FROM_DATE, $this->last_from_date);
		if ($this->isColumnModified(ArReportSchedulerPeer::LAST_TO_DATE)) $criteria->add(ArReportSchedulerPeer::LAST_TO_DATE, $this->last_to_date);
		if ($this->isColumnModified(ArReportSchedulerPeer::AR_REPORT_ID)) $criteria->add(ArReportSchedulerPeer::AR_REPORT_ID, $this->ar_report_id);
		if ($this->isColumnModified(ArReportSchedulerPeer::AR_ORGANIZATION_UNIT_ID)) $criteria->add(ArReportSchedulerPeer::AR_ORGANIZATION_UNIT_ID, $this->ar_organization_unit_id);
		if ($this->isColumnModified(ArReportSchedulerPeer::SHORT_DESCRIPTION)) $criteria->add(ArReportSchedulerPeer::SHORT_DESCRIPTION, $this->short_description);
		if ($this->isColumnModified(ArReportSchedulerPeer::ADDITIONAL_DESCRIPTION)) $criteria->add(ArReportSchedulerPeer::ADDITIONAL_DESCRIPTION, $this->additional_description);
		if ($this->isColumnModified(ArReportSchedulerPeer::NOTE)) $criteria->add(ArReportSchedulerPeer::NOTE, $this->note);
		if ($this->isColumnModified(ArReportSchedulerPeer::PRODUCED_REPORT_MUST_BE_REVIEWED)) $criteria->add(ArReportSchedulerPeer::PRODUCED_REPORT_MUST_BE_REVIEWED, $this->produced_report_must_be_reviewed);
		if ($this->isColumnModified(ArReportSchedulerPeer::AR_REPORT_GENERATION_ID)) $criteria->add(ArReportSchedulerPeer::AR_REPORT_GENERATION_ID, $this->ar_report_generation_id);
		if ($this->isColumnModified(ArReportSchedulerPeer::SCHEDULE_EVERY_X_DAYS)) $criteria->add(ArReportSchedulerPeer::SCHEDULE_EVERY_X_DAYS, $this->schedule_every_x_days);
		if ($this->isColumnModified(ArReportSchedulerPeer::SCHEDULE_EVERY_X_MONTHS)) $criteria->add(ArReportSchedulerPeer::SCHEDULE_EVERY_X_MONTHS, $this->schedule_every_x_months);
		if ($this->isColumnModified(ArReportSchedulerPeer::START_GENERATION_AFTER_X_HOURS)) $criteria->add(ArReportSchedulerPeer::START_GENERATION_AFTER_X_HOURS, $this->start_generation_after_x_hours);
		if ($this->isColumnModified(ArReportSchedulerPeer::INTERNAL_NAME)) $criteria->add(ArReportSchedulerPeer::INTERNAL_NAME, $this->internal_name);
		if ($this->isColumnModified(ArReportSchedulerPeer::AR_LEGAL_DATE_GENERATION_METHOD_ID)) $criteria->add(ArReportSchedulerPeer::AR_LEGAL_DATE_GENERATION_METHOD_ID, $this->ar_legal_date_generation_method_id);
		if ($this->isColumnModified(ArReportSchedulerPeer::DAYS_TO_ADD_TO_LEGAL_DATE_GENERATION_METHOD)) $criteria->add(ArReportSchedulerPeer::DAYS_TO_ADD_TO_LEGAL_DATE_GENERATION_METHOD, $this->days_to_add_to_legal_date_generation_method);
		if ($this->isColumnModified(ArReportSchedulerPeer::IS_YEARLY_LEGAL_NUMERATION)) $criteria->add(ArReportSchedulerPeer::IS_YEARLY_LEGAL_NUMERATION, $this->is_yearly_legal_numeration);
		if ($this->isColumnModified(ArReportSchedulerPeer::GENERATE_ONLY_IF_THERE_IS_COST)) $criteria->add(ArReportSchedulerPeer::GENERATE_ONLY_IF_THERE_IS_COST, $this->generate_only_if_there_is_cost);
		if ($this->isColumnModified(ArReportSchedulerPeer::MINIMUM_COST)) $criteria->add(ArReportSchedulerPeer::MINIMUM_COST, $this->minimum_cost);
		if ($this->isColumnModified(ArReportSchedulerPeer::SEND_COMPACT_REPORT_LIST_TO_ACCOUNTANT)) $criteria->add(ArReportSchedulerPeer::SEND_COMPACT_REPORT_LIST_TO_ACCOUNTANT, $this->send_compact_report_list_to_accountant);

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
		$criteria = new Criteria(ArReportSchedulerPeer::DATABASE_NAME);

		$criteria->add(ArReportSchedulerPeer::ID, $this->id);

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
	 * @param      object $copyObj An object of ArReportScheduler (or compatible) type.
	 * @param      boolean $deepCopy Whether to also copy all rows that refer (by fkey) to the current row.
	 * @throws     PropelException
	 */
	public function copyInto($copyObj, $deepCopy = false)
	{

		$copyObj->setIsActive($this->is_active);

		$copyObj->setLastExecutionDate($this->last_execution_date);

		$copyObj->setLastFromDate($this->last_from_date);

		$copyObj->setLastToDate($this->last_to_date);

		$copyObj->setArReportId($this->ar_report_id);

		$copyObj->setArOrganizationUnitId($this->ar_organization_unit_id);

		$copyObj->setShortDescription($this->short_description);

		$copyObj->setAdditionalDescription($this->additional_description);

		$copyObj->setNote($this->note);

		$copyObj->setProducedReportMustBeReviewed($this->produced_report_must_be_reviewed);

		$copyObj->setArReportGenerationId($this->ar_report_generation_id);

		$copyObj->setScheduleEveryXDays($this->schedule_every_x_days);

		$copyObj->setScheduleEveryXMonths($this->schedule_every_x_months);

		$copyObj->setStartGenerationAfterXHours($this->start_generation_after_x_hours);

		$copyObj->setInternalName($this->internal_name);

		$copyObj->setArLegalDateGenerationMethodId($this->ar_legal_date_generation_method_id);

		$copyObj->setDaysToAddToLegalDateGenerationMethod($this->days_to_add_to_legal_date_generation_method);

		$copyObj->setIsYearlyLegalNumeration($this->is_yearly_legal_numeration);

		$copyObj->setGenerateOnlyIfThereIsCost($this->generate_only_if_there_is_cost);

		$copyObj->setMinimumCost($this->minimum_cost);

		$copyObj->setSendCompactReportListToAccountant($this->send_compact_report_list_to_accountant);


		if ($deepCopy) {
			// important: temporarily setNew(false) because this affects the behavior of
			// the getter/setter methods for fkey referrer objects.
			$copyObj->setNew(false);

			foreach ($this->getArReportSets() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArReportSet($relObj->copy($deepCopy));
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
	 * @return     ArReportScheduler Clone of current object.
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
	 * @return     ArReportSchedulerPeer
	 */
	public function getPeer()
	{
		if (self::$peer === null) {
			self::$peer = new ArReportSchedulerPeer();
		}
		return self::$peer;
	}

	/**
	 * Declares an association between this object and a ArReport object.
	 *
	 * @param      ArReport $v
	 * @return     ArReportScheduler The current object (for fluent API support)
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
			$v->addArReportScheduler($this);
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
			   $this->aArReport->addArReportSchedulers($this);
			 */
		}
		return $this->aArReport;
	}

	/**
	 * Declares an association between this object and a ArOrganizationUnit object.
	 *
	 * @param      ArOrganizationUnit $v
	 * @return     ArReportScheduler The current object (for fluent API support)
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
			$v->addArReportScheduler($this);
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
			   $this->aArOrganizationUnit->addArReportSchedulers($this);
			 */
		}
		return $this->aArOrganizationUnit;
	}

	/**
	 * Declares an association between this object and a ArReportGeneration object.
	 *
	 * @param      ArReportGeneration $v
	 * @return     ArReportScheduler The current object (for fluent API support)
	 * @throws     PropelException
	 */
	public function setArReportGeneration(ArReportGeneration $v = null)
	{
		if ($v === null) {
			$this->setArReportGenerationId(NULL);
		} else {
			$this->setArReportGenerationId($v->getId());
		}

		$this->aArReportGeneration = $v;

		// Add binding for other direction of this n:n relationship.
		// If this object has already been added to the ArReportGeneration object, it will not be re-added.
		if ($v !== null) {
			$v->addArReportScheduler($this);
		}

		return $this;
	}


	/**
	 * Get the associated ArReportGeneration object
	 *
	 * @param      PropelPDO Optional Connection object.
	 * @return     ArReportGeneration The associated ArReportGeneration object.
	 * @throws     PropelException
	 */
	public function getArReportGeneration(PropelPDO $con = null)
	{
		if ($this->aArReportGeneration === null && ($this->ar_report_generation_id !== null)) {
			$this->aArReportGeneration = ArReportGenerationPeer::retrieveByPk($this->ar_report_generation_id);
			/* The following can be used additionally to
			   guarantee the related object contains a reference
			   to this object.  This level of coupling may, however, be
			   undesirable since it could result in an only partially populated collection
			   in the referenced object.
			   $this->aArReportGeneration->addArReportSchedulers($this);
			 */
		}
		return $this->aArReportGeneration;
	}

	/**
	 * Clears out the collArReportSets collection (array).
	 *
	 * This does not modify the database; however, it will remove any associated objects, causing
	 * them to be refetched by subsequent calls to accessor method.
	 *
	 * @return     void
	 * @see        addArReportSets()
	 */
	public function clearArReportSets()
	{
		$this->collArReportSets = null; // important to set this to NULL since that means it is uninitialized
	}

	/**
	 * Initializes the collArReportSets collection (array).
	 *
	 * By default this just sets the collArReportSets collection to an empty array (like clearcollArReportSets());
	 * however, you may wish to override this method in your stub class to provide setting appropriate
	 * to your application -- for example, setting the initial array to the values stored in database.
	 *
	 * @return     void
	 */
	public function initArReportSets()
	{
		$this->collArReportSets = array();
	}

	/**
	 * Gets an array of ArReportSet objects which contain a foreign key that references this object.
	 *
	 * If this collection has already been initialized with an identical Criteria, it returns the collection.
	 * Otherwise if this ArReportScheduler has previously been saved, it will retrieve
	 * related ArReportSets from storage. If this ArReportScheduler is new, it will return
	 * an empty collection or the current collection, the criteria is ignored on a new object.
	 *
	 * @param      PropelPDO $con
	 * @param      Criteria $criteria
	 * @return     array ArReportSet[]
	 * @throws     PropelException
	 */
	public function getArReportSets($criteria = null, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArReportSchedulerPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReportSets === null) {
			if ($this->isNew()) {
			   $this->collArReportSets = array();
			} else {

				$criteria->add(ArReportSetPeer::AR_REPORT_SCHEDULER_ID, $this->id);

				ArReportSetPeer::addSelectColumns($criteria);
				$this->collArReportSets = ArReportSetPeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArReportSetPeer::AR_REPORT_SCHEDULER_ID, $this->id);

				ArReportSetPeer::addSelectColumns($criteria);
				if (!isset($this->lastArReportSetCriteria) || !$this->lastArReportSetCriteria->equals($criteria)) {
					$this->collArReportSets = ArReportSetPeer::doSelect($criteria, $con);
				}
			}
		}
		$this->lastArReportSetCriteria = $criteria;
		return $this->collArReportSets;
	}

	/**
	 * Returns the number of related ArReportSet objects.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct
	 * @param      PropelPDO $con
	 * @return     int Count of related ArReportSet objects.
	 * @throws     PropelException
	 */
	public function countArReportSets(Criteria $criteria = null, $distinct = false, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArReportSchedulerPeer::DATABASE_NAME);
		} else {
			$criteria = clone $criteria;
		}

		if ($distinct) {
			$criteria->setDistinct();
		}

		$count = null;

		if ($this->collArReportSets === null) {
			if ($this->isNew()) {
				$count = 0;
			} else {

				$criteria->add(ArReportSetPeer::AR_REPORT_SCHEDULER_ID, $this->id);

				$count = ArReportSetPeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArReportSetPeer::AR_REPORT_SCHEDULER_ID, $this->id);

				if (!isset($this->lastArReportSetCriteria) || !$this->lastArReportSetCriteria->equals($criteria)) {
					$count = ArReportSetPeer::doCount($criteria, false, $con);
				} else {
					$count = count($this->collArReportSets);
				}
			} else {
				$count = count($this->collArReportSets);
			}
		}
		return $count;
	}

	/**
	 * Method called to associate a ArReportSet object to this object
	 * through the ArReportSet foreign key attribute.
	 *
	 * @param      ArReportSet $l ArReportSet
	 * @return     void
	 * @throws     PropelException
	 */
	public function addArReportSet(ArReportSet $l)
	{
		if ($this->collArReportSets === null) {
			$this->initArReportSets();
		}
		if (!in_array($l, $this->collArReportSets, true)) { // only add it if the **same** object is not already associated
			array_push($this->collArReportSets, $l);
			$l->setArReportScheduler($this);
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
			if ($this->collArReportSets) {
				foreach ((array) $this->collArReportSets as $o) {
					$o->clearAllReferences($deep);
				}
			}
		} // if ($deep)

		$this->collArReportSets = null;
			$this->aArReport = null;
			$this->aArOrganizationUnit = null;
			$this->aArReportGeneration = null;
	}

} // BaseArReportScheduler
