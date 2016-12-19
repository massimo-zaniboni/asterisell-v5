<?php

/**
 * Base class that represents a row from the 'ar_instance_status' table.
 *
 * 
 *
 * @package    lib.model.om
 */
abstract class BaseArInstanceStatus extends BaseObject  implements Persistent {


	/**
	 * The Peer class.
	 * Instance provides a convenient way of calling static methods on a class
	 * that calling code may not be able to identify.
	 * @var        ArInstanceStatusPeer
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
	 * The value for the instance_code field.
	 * @var        string
	 */
	protected $instance_code;

	/**
	 * The value for the ar_organization_unit_id field.
	 * @var        int
	 */
	protected $ar_organization_unit_id;

	/**
	 * The value for the application_version field.
	 * @var        string
	 */
	protected $application_version;

	/**
	 * The value for the nr_of_critical_errors field.
	 * @var        int
	 */
	protected $nr_of_critical_errors;

	/**
	 * The value for the nr_of_important_errors field.
	 * @var        int
	 */
	protected $nr_of_important_errors;

	/**
	 * The value for the nr_of_warning_errors field.
	 * @var        int
	 */
	protected $nr_of_warning_errors;

	/**
	 * The value for the nr_of_extensions field.
	 * @var        int
	 */
	protected $nr_of_extensions;

	/**
	 * The value for the nr_of_unspecified_extensions field.
	 * @var        int
	 */
	protected $nr_of_unspecified_extensions;

	/**
	 * The value for the property_nr_errors_outgoing_previous_month field.
	 * @var        int
	 */
	protected $property_nr_errors_outgoing_previous_month;

	/**
	 * The value for the property_nr_errors_incoming_previous_month field.
	 * @var        int
	 */
	protected $property_nr_errors_incoming_previous_month;

	/**
	 * The value for the property_nr_errors_internal_previous_month field.
	 * @var        int
	 */
	protected $property_nr_errors_internal_previous_month;

	/**
	 * The value for the property_nr_errors_outgoing_last_30_days field.
	 * @var        int
	 */
	protected $property_nr_errors_outgoing_last_30_days;

	/**
	 * The value for the property_nr_errors_incoming_last_30_days field.
	 * @var        int
	 */
	protected $property_nr_errors_incoming_last_30_days;

	/**
	 * The value for the property_nr_errors_internal_last_30_days field.
	 * @var        int
	 */
	protected $property_nr_errors_internal_last_30_days;

	/**
	 * The value for the property_nr_outgoing_previous_month field.
	 * @var        int
	 */
	protected $property_nr_outgoing_previous_month;

	/**
	 * The value for the property_nr_incoming_previous_month field.
	 * @var        int
	 */
	protected $property_nr_incoming_previous_month;

	/**
	 * The value for the property_nr_internal_previous_month field.
	 * @var        int
	 */
	protected $property_nr_internal_previous_month;

	/**
	 * The value for the property_nr_outgoing_last_30_days field.
	 * @var        int
	 */
	protected $property_nr_outgoing_last_30_days;

	/**
	 * The value for the property_nr_incoming_last_30_days field.
	 * @var        int
	 */
	protected $property_nr_incoming_last_30_days;

	/**
	 * The value for the property_nr_internal_last_30_days field.
	 * @var        int
	 */
	protected $property_nr_internal_last_30_days;

	/**
	 * The value for the last_processed_cdr_timestamp field.
	 * @var        string
	 */
	protected $last_processed_cdr_timestamp;

	/**
	 * The value for the info_timestamp field.
	 * @var        string
	 */
	protected $info_timestamp;

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
	
	const PEER = 'ArInstanceStatusPeer';

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
	 * Get the [instance_code] column value.
	 * 
	 * @return     string
	 */
	public function getInstanceCode()
	{
		return $this->instance_code;
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
	 * Get the [application_version] column value.
	 * 
	 * @return     string
	 */
	public function getApplicationVersion()
	{
		return $this->application_version;
	}

	/**
	 * Get the [nr_of_critical_errors] column value.
	 * 
	 * @return     int
	 */
	public function getNrOfCriticalErrors()
	{
		return $this->nr_of_critical_errors;
	}

	/**
	 * Get the [nr_of_important_errors] column value.
	 * 
	 * @return     int
	 */
	public function getNrOfImportantErrors()
	{
		return $this->nr_of_important_errors;
	}

	/**
	 * Get the [nr_of_warning_errors] column value.
	 * 
	 * @return     int
	 */
	public function getNrOfWarningErrors()
	{
		return $this->nr_of_warning_errors;
	}

	/**
	 * Get the [nr_of_extensions] column value.
	 * 
	 * @return     int
	 */
	public function getNrOfExtensions()
	{
		return $this->nr_of_extensions;
	}

	/**
	 * Get the [nr_of_unspecified_extensions] column value.
	 * 
	 * @return     int
	 */
	public function getNrOfUnspecifiedExtensions()
	{
		return $this->nr_of_unspecified_extensions;
	}

	/**
	 * Get the [property_nr_errors_outgoing_previous_month] column value.
	 * 
	 * @return     int
	 */
	public function getPropertyNrErrorsOutgoingPreviousMonth()
	{
		return $this->property_nr_errors_outgoing_previous_month;
	}

	/**
	 * Get the [property_nr_errors_incoming_previous_month] column value.
	 * 
	 * @return     int
	 */
	public function getPropertyNrErrorsIncomingPreviousMonth()
	{
		return $this->property_nr_errors_incoming_previous_month;
	}

	/**
	 * Get the [property_nr_errors_internal_previous_month] column value.
	 * 
	 * @return     int
	 */
	public function getPropertyNrErrorsInternalPreviousMonth()
	{
		return $this->property_nr_errors_internal_previous_month;
	}

	/**
	 * Get the [property_nr_errors_outgoing_last_30_days] column value.
	 * 
	 * @return     int
	 */
	public function getPropertyNrErrorsOutgoingLast30Days()
	{
		return $this->property_nr_errors_outgoing_last_30_days;
	}

	/**
	 * Get the [property_nr_errors_incoming_last_30_days] column value.
	 * 
	 * @return     int
	 */
	public function getPropertyNrErrorsIncomingLast30Days()
	{
		return $this->property_nr_errors_incoming_last_30_days;
	}

	/**
	 * Get the [property_nr_errors_internal_last_30_days] column value.
	 * 
	 * @return     int
	 */
	public function getPropertyNrErrorsInternalLast30Days()
	{
		return $this->property_nr_errors_internal_last_30_days;
	}

	/**
	 * Get the [property_nr_outgoing_previous_month] column value.
	 * 
	 * @return     int
	 */
	public function getPropertyNrOutgoingPreviousMonth()
	{
		return $this->property_nr_outgoing_previous_month;
	}

	/**
	 * Get the [property_nr_incoming_previous_month] column value.
	 * 
	 * @return     int
	 */
	public function getPropertyNrIncomingPreviousMonth()
	{
		return $this->property_nr_incoming_previous_month;
	}

	/**
	 * Get the [property_nr_internal_previous_month] column value.
	 * 
	 * @return     int
	 */
	public function getPropertyNrInternalPreviousMonth()
	{
		return $this->property_nr_internal_previous_month;
	}

	/**
	 * Get the [property_nr_outgoing_last_30_days] column value.
	 * 
	 * @return     int
	 */
	public function getPropertyNrOutgoingLast30Days()
	{
		return $this->property_nr_outgoing_last_30_days;
	}

	/**
	 * Get the [property_nr_incoming_last_30_days] column value.
	 * 
	 * @return     int
	 */
	public function getPropertyNrIncomingLast30Days()
	{
		return $this->property_nr_incoming_last_30_days;
	}

	/**
	 * Get the [property_nr_internal_last_30_days] column value.
	 * 
	 * @return     int
	 */
	public function getPropertyNrInternalLast30Days()
	{
		return $this->property_nr_internal_last_30_days;
	}

	/**
	 * Get the [optionally formatted] temporal [last_processed_cdr_timestamp] column value.
	 * 
	 *
	 * @param      string $format The date/time format string (either date()-style or strftime()-style).
	 *							If format is NULL, then the raw DateTime object will be returned.
	 * @return     mixed Formatted date/time value as string or DateTime object (if format is NULL), NULL if column is NULL, and 0 if column value is 0000-00-00 00:00:00
	 * @throws     PropelException - if unable to parse/validate the date/time value.
	 */
	public function getLastProcessedCdrTimestamp($format = 'Y-m-d H:i:s')
	{
		if ($this->last_processed_cdr_timestamp === null) {
			return null;
		}


		if ($this->last_processed_cdr_timestamp === '0000-00-00 00:00:00') {
			// while technically this is not a default value of NULL,
			// this seems to be closest in meaning.
			return null;
		} else {
			try {
				$dt = new DateTime($this->last_processed_cdr_timestamp);
			} catch (Exception $x) {
				throw new PropelException("Internally stored date/time/timestamp value could not be converted to DateTime: " . var_export($this->last_processed_cdr_timestamp, true), $x);
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
	 * Get the [optionally formatted] temporal [info_timestamp] column value.
	 * 
	 *
	 * @param      string $format The date/time format string (either date()-style or strftime()-style).
	 *							If format is NULL, then the raw DateTime object will be returned.
	 * @return     mixed Formatted date/time value as string or DateTime object (if format is NULL), NULL if column is NULL, and 0 if column value is 0000-00-00 00:00:00
	 * @throws     PropelException - if unable to parse/validate the date/time value.
	 */
	public function getInfoTimestamp($format = 'Y-m-d H:i:s')
	{
		if ($this->info_timestamp === null) {
			return null;
		}


		if ($this->info_timestamp === '0000-00-00 00:00:00') {
			// while technically this is not a default value of NULL,
			// this seems to be closest in meaning.
			return null;
		} else {
			try {
				$dt = new DateTime($this->info_timestamp);
			} catch (Exception $x) {
				throw new PropelException("Internally stored date/time/timestamp value could not be converted to DateTime: " . var_export($this->info_timestamp, true), $x);
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
	 * Set the value of [id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArInstanceStatus The current object (for fluent API support)
	 */
	public function setId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->id !== $v) {
			$this->id = $v;
			$this->modifiedColumns[] = ArInstanceStatusPeer::ID;
		}

		return $this;
	} // setId()

	/**
	 * Set the value of [internal_name] column.
	 * 
	 * @param      string $v new value
	 * @return     ArInstanceStatus The current object (for fluent API support)
	 */
	public function setInternalName($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->internal_name !== $v) {
			$this->internal_name = $v;
			$this->modifiedColumns[] = ArInstanceStatusPeer::INTERNAL_NAME;
		}

		return $this;
	} // setInternalName()

	/**
	 * Set the value of [instance_code] column.
	 * 
	 * @param      string $v new value
	 * @return     ArInstanceStatus The current object (for fluent API support)
	 */
	public function setInstanceCode($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->instance_code !== $v) {
			$this->instance_code = $v;
			$this->modifiedColumns[] = ArInstanceStatusPeer::INSTANCE_CODE;
		}

		return $this;
	} // setInstanceCode()

	/**
	 * Set the value of [ar_organization_unit_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArInstanceStatus The current object (for fluent API support)
	 */
	public function setArOrganizationUnitId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->ar_organization_unit_id !== $v) {
			$this->ar_organization_unit_id = $v;
			$this->modifiedColumns[] = ArInstanceStatusPeer::AR_ORGANIZATION_UNIT_ID;
		}

		if ($this->aArOrganizationUnit !== null && $this->aArOrganizationUnit->getId() !== $v) {
			$this->aArOrganizationUnit = null;
		}

		return $this;
	} // setArOrganizationUnitId()

	/**
	 * Set the value of [application_version] column.
	 * 
	 * @param      string $v new value
	 * @return     ArInstanceStatus The current object (for fluent API support)
	 */
	public function setApplicationVersion($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->application_version !== $v) {
			$this->application_version = $v;
			$this->modifiedColumns[] = ArInstanceStatusPeer::APPLICATION_VERSION;
		}

		return $this;
	} // setApplicationVersion()

	/**
	 * Set the value of [nr_of_critical_errors] column.
	 * 
	 * @param      int $v new value
	 * @return     ArInstanceStatus The current object (for fluent API support)
	 */
	public function setNrOfCriticalErrors($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->nr_of_critical_errors !== $v) {
			$this->nr_of_critical_errors = $v;
			$this->modifiedColumns[] = ArInstanceStatusPeer::NR_OF_CRITICAL_ERRORS;
		}

		return $this;
	} // setNrOfCriticalErrors()

	/**
	 * Set the value of [nr_of_important_errors] column.
	 * 
	 * @param      int $v new value
	 * @return     ArInstanceStatus The current object (for fluent API support)
	 */
	public function setNrOfImportantErrors($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->nr_of_important_errors !== $v) {
			$this->nr_of_important_errors = $v;
			$this->modifiedColumns[] = ArInstanceStatusPeer::NR_OF_IMPORTANT_ERRORS;
		}

		return $this;
	} // setNrOfImportantErrors()

	/**
	 * Set the value of [nr_of_warning_errors] column.
	 * 
	 * @param      int $v new value
	 * @return     ArInstanceStatus The current object (for fluent API support)
	 */
	public function setNrOfWarningErrors($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->nr_of_warning_errors !== $v) {
			$this->nr_of_warning_errors = $v;
			$this->modifiedColumns[] = ArInstanceStatusPeer::NR_OF_WARNING_ERRORS;
		}

		return $this;
	} // setNrOfWarningErrors()

	/**
	 * Set the value of [nr_of_extensions] column.
	 * 
	 * @param      int $v new value
	 * @return     ArInstanceStatus The current object (for fluent API support)
	 */
	public function setNrOfExtensions($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->nr_of_extensions !== $v) {
			$this->nr_of_extensions = $v;
			$this->modifiedColumns[] = ArInstanceStatusPeer::NR_OF_EXTENSIONS;
		}

		return $this;
	} // setNrOfExtensions()

	/**
	 * Set the value of [nr_of_unspecified_extensions] column.
	 * 
	 * @param      int $v new value
	 * @return     ArInstanceStatus The current object (for fluent API support)
	 */
	public function setNrOfUnspecifiedExtensions($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->nr_of_unspecified_extensions !== $v) {
			$this->nr_of_unspecified_extensions = $v;
			$this->modifiedColumns[] = ArInstanceStatusPeer::NR_OF_UNSPECIFIED_EXTENSIONS;
		}

		return $this;
	} // setNrOfUnspecifiedExtensions()

	/**
	 * Set the value of [property_nr_errors_outgoing_previous_month] column.
	 * 
	 * @param      int $v new value
	 * @return     ArInstanceStatus The current object (for fluent API support)
	 */
	public function setPropertyNrErrorsOutgoingPreviousMonth($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->property_nr_errors_outgoing_previous_month !== $v) {
			$this->property_nr_errors_outgoing_previous_month = $v;
			$this->modifiedColumns[] = ArInstanceStatusPeer::PROPERTY_NR_ERRORS_OUTGOING_PREVIOUS_MONTH;
		}

		return $this;
	} // setPropertyNrErrorsOutgoingPreviousMonth()

	/**
	 * Set the value of [property_nr_errors_incoming_previous_month] column.
	 * 
	 * @param      int $v new value
	 * @return     ArInstanceStatus The current object (for fluent API support)
	 */
	public function setPropertyNrErrorsIncomingPreviousMonth($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->property_nr_errors_incoming_previous_month !== $v) {
			$this->property_nr_errors_incoming_previous_month = $v;
			$this->modifiedColumns[] = ArInstanceStatusPeer::PROPERTY_NR_ERRORS_INCOMING_PREVIOUS_MONTH;
		}

		return $this;
	} // setPropertyNrErrorsIncomingPreviousMonth()

	/**
	 * Set the value of [property_nr_errors_internal_previous_month] column.
	 * 
	 * @param      int $v new value
	 * @return     ArInstanceStatus The current object (for fluent API support)
	 */
	public function setPropertyNrErrorsInternalPreviousMonth($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->property_nr_errors_internal_previous_month !== $v) {
			$this->property_nr_errors_internal_previous_month = $v;
			$this->modifiedColumns[] = ArInstanceStatusPeer::PROPERTY_NR_ERRORS_INTERNAL_PREVIOUS_MONTH;
		}

		return $this;
	} // setPropertyNrErrorsInternalPreviousMonth()

	/**
	 * Set the value of [property_nr_errors_outgoing_last_30_days] column.
	 * 
	 * @param      int $v new value
	 * @return     ArInstanceStatus The current object (for fluent API support)
	 */
	public function setPropertyNrErrorsOutgoingLast30Days($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->property_nr_errors_outgoing_last_30_days !== $v) {
			$this->property_nr_errors_outgoing_last_30_days = $v;
			$this->modifiedColumns[] = ArInstanceStatusPeer::PROPERTY_NR_ERRORS_OUTGOING_LAST_30_DAYS;
		}

		return $this;
	} // setPropertyNrErrorsOutgoingLast30Days()

	/**
	 * Set the value of [property_nr_errors_incoming_last_30_days] column.
	 * 
	 * @param      int $v new value
	 * @return     ArInstanceStatus The current object (for fluent API support)
	 */
	public function setPropertyNrErrorsIncomingLast30Days($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->property_nr_errors_incoming_last_30_days !== $v) {
			$this->property_nr_errors_incoming_last_30_days = $v;
			$this->modifiedColumns[] = ArInstanceStatusPeer::PROPERTY_NR_ERRORS_INCOMING_LAST_30_DAYS;
		}

		return $this;
	} // setPropertyNrErrorsIncomingLast30Days()

	/**
	 * Set the value of [property_nr_errors_internal_last_30_days] column.
	 * 
	 * @param      int $v new value
	 * @return     ArInstanceStatus The current object (for fluent API support)
	 */
	public function setPropertyNrErrorsInternalLast30Days($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->property_nr_errors_internal_last_30_days !== $v) {
			$this->property_nr_errors_internal_last_30_days = $v;
			$this->modifiedColumns[] = ArInstanceStatusPeer::PROPERTY_NR_ERRORS_INTERNAL_LAST_30_DAYS;
		}

		return $this;
	} // setPropertyNrErrorsInternalLast30Days()

	/**
	 * Set the value of [property_nr_outgoing_previous_month] column.
	 * 
	 * @param      int $v new value
	 * @return     ArInstanceStatus The current object (for fluent API support)
	 */
	public function setPropertyNrOutgoingPreviousMonth($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->property_nr_outgoing_previous_month !== $v) {
			$this->property_nr_outgoing_previous_month = $v;
			$this->modifiedColumns[] = ArInstanceStatusPeer::PROPERTY_NR_OUTGOING_PREVIOUS_MONTH;
		}

		return $this;
	} // setPropertyNrOutgoingPreviousMonth()

	/**
	 * Set the value of [property_nr_incoming_previous_month] column.
	 * 
	 * @param      int $v new value
	 * @return     ArInstanceStatus The current object (for fluent API support)
	 */
	public function setPropertyNrIncomingPreviousMonth($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->property_nr_incoming_previous_month !== $v) {
			$this->property_nr_incoming_previous_month = $v;
			$this->modifiedColumns[] = ArInstanceStatusPeer::PROPERTY_NR_INCOMING_PREVIOUS_MONTH;
		}

		return $this;
	} // setPropertyNrIncomingPreviousMonth()

	/**
	 * Set the value of [property_nr_internal_previous_month] column.
	 * 
	 * @param      int $v new value
	 * @return     ArInstanceStatus The current object (for fluent API support)
	 */
	public function setPropertyNrInternalPreviousMonth($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->property_nr_internal_previous_month !== $v) {
			$this->property_nr_internal_previous_month = $v;
			$this->modifiedColumns[] = ArInstanceStatusPeer::PROPERTY_NR_INTERNAL_PREVIOUS_MONTH;
		}

		return $this;
	} // setPropertyNrInternalPreviousMonth()

	/**
	 * Set the value of [property_nr_outgoing_last_30_days] column.
	 * 
	 * @param      int $v new value
	 * @return     ArInstanceStatus The current object (for fluent API support)
	 */
	public function setPropertyNrOutgoingLast30Days($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->property_nr_outgoing_last_30_days !== $v) {
			$this->property_nr_outgoing_last_30_days = $v;
			$this->modifiedColumns[] = ArInstanceStatusPeer::PROPERTY_NR_OUTGOING_LAST_30_DAYS;
		}

		return $this;
	} // setPropertyNrOutgoingLast30Days()

	/**
	 * Set the value of [property_nr_incoming_last_30_days] column.
	 * 
	 * @param      int $v new value
	 * @return     ArInstanceStatus The current object (for fluent API support)
	 */
	public function setPropertyNrIncomingLast30Days($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->property_nr_incoming_last_30_days !== $v) {
			$this->property_nr_incoming_last_30_days = $v;
			$this->modifiedColumns[] = ArInstanceStatusPeer::PROPERTY_NR_INCOMING_LAST_30_DAYS;
		}

		return $this;
	} // setPropertyNrIncomingLast30Days()

	/**
	 * Set the value of [property_nr_internal_last_30_days] column.
	 * 
	 * @param      int $v new value
	 * @return     ArInstanceStatus The current object (for fluent API support)
	 */
	public function setPropertyNrInternalLast30Days($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->property_nr_internal_last_30_days !== $v) {
			$this->property_nr_internal_last_30_days = $v;
			$this->modifiedColumns[] = ArInstanceStatusPeer::PROPERTY_NR_INTERNAL_LAST_30_DAYS;
		}

		return $this;
	} // setPropertyNrInternalLast30Days()

	/**
	 * Sets the value of [last_processed_cdr_timestamp] column to a normalized version of the date/time value specified.
	 * 
	 * @param      mixed $v string, integer (timestamp), or DateTime value.  Empty string will
	 *						be treated as NULL for temporal objects.
	 * @return     ArInstanceStatus The current object (for fluent API support)
	 */
	public function setLastProcessedCdrTimestamp($v)
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

		if ( $this->last_processed_cdr_timestamp !== null || $dt !== null ) {
			// (nested ifs are a little easier to read in this case)

			$currNorm = ($this->last_processed_cdr_timestamp !== null && $tmpDt = new DateTime($this->last_processed_cdr_timestamp)) ? $tmpDt->format('Y-m-d H:i:s') : null;
			$newNorm = ($dt !== null) ? $dt->format('Y-m-d H:i:s') : null;

			if ( ($currNorm !== $newNorm) // normalized values don't match 
					)
			{
				$this->last_processed_cdr_timestamp = ($dt ? $dt->format('Y-m-d H:i:s') : null);
				$this->modifiedColumns[] = ArInstanceStatusPeer::LAST_PROCESSED_CDR_TIMESTAMP;
			}
		} // if either are not null

		return $this;
	} // setLastProcessedCdrTimestamp()

	/**
	 * Sets the value of [info_timestamp] column to a normalized version of the date/time value specified.
	 * 
	 * @param      mixed $v string, integer (timestamp), or DateTime value.  Empty string will
	 *						be treated as NULL for temporal objects.
	 * @return     ArInstanceStatus The current object (for fluent API support)
	 */
	public function setInfoTimestamp($v)
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

		if ( $this->info_timestamp !== null || $dt !== null ) {
			// (nested ifs are a little easier to read in this case)

			$currNorm = ($this->info_timestamp !== null && $tmpDt = new DateTime($this->info_timestamp)) ? $tmpDt->format('Y-m-d H:i:s') : null;
			$newNorm = ($dt !== null) ? $dt->format('Y-m-d H:i:s') : null;

			if ( ($currNorm !== $newNorm) // normalized values don't match 
					)
			{
				$this->info_timestamp = ($dt ? $dt->format('Y-m-d H:i:s') : null);
				$this->modifiedColumns[] = ArInstanceStatusPeer::INFO_TIMESTAMP;
			}
		} // if either are not null

		return $this;
	} // setInfoTimestamp()

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
			$this->internal_name = ($row[$startcol + 1] !== null) ? (string) $row[$startcol + 1] : null;
			$this->instance_code = ($row[$startcol + 2] !== null) ? (string) $row[$startcol + 2] : null;
			$this->ar_organization_unit_id = ($row[$startcol + 3] !== null) ? (int) $row[$startcol + 3] : null;
			$this->application_version = ($row[$startcol + 4] !== null) ? (string) $row[$startcol + 4] : null;
			$this->nr_of_critical_errors = ($row[$startcol + 5] !== null) ? (int) $row[$startcol + 5] : null;
			$this->nr_of_important_errors = ($row[$startcol + 6] !== null) ? (int) $row[$startcol + 6] : null;
			$this->nr_of_warning_errors = ($row[$startcol + 7] !== null) ? (int) $row[$startcol + 7] : null;
			$this->nr_of_extensions = ($row[$startcol + 8] !== null) ? (int) $row[$startcol + 8] : null;
			$this->nr_of_unspecified_extensions = ($row[$startcol + 9] !== null) ? (int) $row[$startcol + 9] : null;
			$this->property_nr_errors_outgoing_previous_month = ($row[$startcol + 10] !== null) ? (int) $row[$startcol + 10] : null;
			$this->property_nr_errors_incoming_previous_month = ($row[$startcol + 11] !== null) ? (int) $row[$startcol + 11] : null;
			$this->property_nr_errors_internal_previous_month = ($row[$startcol + 12] !== null) ? (int) $row[$startcol + 12] : null;
			$this->property_nr_errors_outgoing_last_30_days = ($row[$startcol + 13] !== null) ? (int) $row[$startcol + 13] : null;
			$this->property_nr_errors_incoming_last_30_days = ($row[$startcol + 14] !== null) ? (int) $row[$startcol + 14] : null;
			$this->property_nr_errors_internal_last_30_days = ($row[$startcol + 15] !== null) ? (int) $row[$startcol + 15] : null;
			$this->property_nr_outgoing_previous_month = ($row[$startcol + 16] !== null) ? (int) $row[$startcol + 16] : null;
			$this->property_nr_incoming_previous_month = ($row[$startcol + 17] !== null) ? (int) $row[$startcol + 17] : null;
			$this->property_nr_internal_previous_month = ($row[$startcol + 18] !== null) ? (int) $row[$startcol + 18] : null;
			$this->property_nr_outgoing_last_30_days = ($row[$startcol + 19] !== null) ? (int) $row[$startcol + 19] : null;
			$this->property_nr_incoming_last_30_days = ($row[$startcol + 20] !== null) ? (int) $row[$startcol + 20] : null;
			$this->property_nr_internal_last_30_days = ($row[$startcol + 21] !== null) ? (int) $row[$startcol + 21] : null;
			$this->last_processed_cdr_timestamp = ($row[$startcol + 22] !== null) ? (string) $row[$startcol + 22] : null;
			$this->info_timestamp = ($row[$startcol + 23] !== null) ? (string) $row[$startcol + 23] : null;
			$this->resetModified();

			$this->setNew(false);

			if ($rehydrate) {
				$this->ensureConsistency();
			}

			// FIXME - using NUM_COLUMNS may be clearer.
			return $startcol + 24; // 24 = ArInstanceStatusPeer::NUM_COLUMNS - ArInstanceStatusPeer::NUM_LAZY_LOAD_COLUMNS).

		} catch (Exception $e) {
			throw new PropelException("Error populating ArInstanceStatus object", $e);
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
			$con = Propel::getConnection(ArInstanceStatusPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		// We don't need to alter the object instance pool; we're just modifying this instance
		// already in the pool.

		$stmt = ArInstanceStatusPeer::doSelectStmt($this->buildPkeyCriteria(), $con);
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
			$con = Propel::getConnection(ArInstanceStatusPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}
		
		$con->beginTransaction();
		try {
			$ret = $this->preDelete($con);
			if ($ret) {
				ArInstanceStatusPeer::doDelete($this, $con);
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
			$con = Propel::getConnection(ArInstanceStatusPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
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
				ArInstanceStatusPeer::addInstanceToPool($this);
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
				$this->modifiedColumns[] = ArInstanceStatusPeer::ID;
			}

			// If this object has been modified, then save it to the database.
			if ($this->isModified()) {
				if ($this->isNew()) {
					$pk = ArInstanceStatusPeer::doInsert($this, $con);
					$affectedRows += 1; // we are assuming that there is only 1 row per doInsert() which
										 // should always be true here (even though technically
										 // BasePeer::doInsert() can insert multiple rows).

					$this->setId($pk);  //[IMV] update autoincrement primary key

					$this->setNew(false);
				} else {
					$affectedRows += ArInstanceStatusPeer::doUpdate($this, $con);
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


			if (($retval = ArInstanceStatusPeer::doValidate($this, $columns)) !== true) {
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
		$pos = ArInstanceStatusPeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
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
				return $this->getInstanceCode();
				break;
			case 3:
				return $this->getArOrganizationUnitId();
				break;
			case 4:
				return $this->getApplicationVersion();
				break;
			case 5:
				return $this->getNrOfCriticalErrors();
				break;
			case 6:
				return $this->getNrOfImportantErrors();
				break;
			case 7:
				return $this->getNrOfWarningErrors();
				break;
			case 8:
				return $this->getNrOfExtensions();
				break;
			case 9:
				return $this->getNrOfUnspecifiedExtensions();
				break;
			case 10:
				return $this->getPropertyNrErrorsOutgoingPreviousMonth();
				break;
			case 11:
				return $this->getPropertyNrErrorsIncomingPreviousMonth();
				break;
			case 12:
				return $this->getPropertyNrErrorsInternalPreviousMonth();
				break;
			case 13:
				return $this->getPropertyNrErrorsOutgoingLast30Days();
				break;
			case 14:
				return $this->getPropertyNrErrorsIncomingLast30Days();
				break;
			case 15:
				return $this->getPropertyNrErrorsInternalLast30Days();
				break;
			case 16:
				return $this->getPropertyNrOutgoingPreviousMonth();
				break;
			case 17:
				return $this->getPropertyNrIncomingPreviousMonth();
				break;
			case 18:
				return $this->getPropertyNrInternalPreviousMonth();
				break;
			case 19:
				return $this->getPropertyNrOutgoingLast30Days();
				break;
			case 20:
				return $this->getPropertyNrIncomingLast30Days();
				break;
			case 21:
				return $this->getPropertyNrInternalLast30Days();
				break;
			case 22:
				return $this->getLastProcessedCdrTimestamp();
				break;
			case 23:
				return $this->getInfoTimestamp();
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
		$keys = ArInstanceStatusPeer::getFieldNames($keyType);
		$result = array(
			$keys[0] => $this->getId(),
			$keys[1] => $this->getInternalName(),
			$keys[2] => $this->getInstanceCode(),
			$keys[3] => $this->getArOrganizationUnitId(),
			$keys[4] => $this->getApplicationVersion(),
			$keys[5] => $this->getNrOfCriticalErrors(),
			$keys[6] => $this->getNrOfImportantErrors(),
			$keys[7] => $this->getNrOfWarningErrors(),
			$keys[8] => $this->getNrOfExtensions(),
			$keys[9] => $this->getNrOfUnspecifiedExtensions(),
			$keys[10] => $this->getPropertyNrErrorsOutgoingPreviousMonth(),
			$keys[11] => $this->getPropertyNrErrorsIncomingPreviousMonth(),
			$keys[12] => $this->getPropertyNrErrorsInternalPreviousMonth(),
			$keys[13] => $this->getPropertyNrErrorsOutgoingLast30Days(),
			$keys[14] => $this->getPropertyNrErrorsIncomingLast30Days(),
			$keys[15] => $this->getPropertyNrErrorsInternalLast30Days(),
			$keys[16] => $this->getPropertyNrOutgoingPreviousMonth(),
			$keys[17] => $this->getPropertyNrIncomingPreviousMonth(),
			$keys[18] => $this->getPropertyNrInternalPreviousMonth(),
			$keys[19] => $this->getPropertyNrOutgoingLast30Days(),
			$keys[20] => $this->getPropertyNrIncomingLast30Days(),
			$keys[21] => $this->getPropertyNrInternalLast30Days(),
			$keys[22] => $this->getLastProcessedCdrTimestamp(),
			$keys[23] => $this->getInfoTimestamp(),
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
		$pos = ArInstanceStatusPeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
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
				$this->setInstanceCode($value);
				break;
			case 3:
				$this->setArOrganizationUnitId($value);
				break;
			case 4:
				$this->setApplicationVersion($value);
				break;
			case 5:
				$this->setNrOfCriticalErrors($value);
				break;
			case 6:
				$this->setNrOfImportantErrors($value);
				break;
			case 7:
				$this->setNrOfWarningErrors($value);
				break;
			case 8:
				$this->setNrOfExtensions($value);
				break;
			case 9:
				$this->setNrOfUnspecifiedExtensions($value);
				break;
			case 10:
				$this->setPropertyNrErrorsOutgoingPreviousMonth($value);
				break;
			case 11:
				$this->setPropertyNrErrorsIncomingPreviousMonth($value);
				break;
			case 12:
				$this->setPropertyNrErrorsInternalPreviousMonth($value);
				break;
			case 13:
				$this->setPropertyNrErrorsOutgoingLast30Days($value);
				break;
			case 14:
				$this->setPropertyNrErrorsIncomingLast30Days($value);
				break;
			case 15:
				$this->setPropertyNrErrorsInternalLast30Days($value);
				break;
			case 16:
				$this->setPropertyNrOutgoingPreviousMonth($value);
				break;
			case 17:
				$this->setPropertyNrIncomingPreviousMonth($value);
				break;
			case 18:
				$this->setPropertyNrInternalPreviousMonth($value);
				break;
			case 19:
				$this->setPropertyNrOutgoingLast30Days($value);
				break;
			case 20:
				$this->setPropertyNrIncomingLast30Days($value);
				break;
			case 21:
				$this->setPropertyNrInternalLast30Days($value);
				break;
			case 22:
				$this->setLastProcessedCdrTimestamp($value);
				break;
			case 23:
				$this->setInfoTimestamp($value);
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
		$keys = ArInstanceStatusPeer::getFieldNames($keyType);

		if (array_key_exists($keys[0], $arr)) $this->setId($arr[$keys[0]]);
		if (array_key_exists($keys[1], $arr)) $this->setInternalName($arr[$keys[1]]);
		if (array_key_exists($keys[2], $arr)) $this->setInstanceCode($arr[$keys[2]]);
		if (array_key_exists($keys[3], $arr)) $this->setArOrganizationUnitId($arr[$keys[3]]);
		if (array_key_exists($keys[4], $arr)) $this->setApplicationVersion($arr[$keys[4]]);
		if (array_key_exists($keys[5], $arr)) $this->setNrOfCriticalErrors($arr[$keys[5]]);
		if (array_key_exists($keys[6], $arr)) $this->setNrOfImportantErrors($arr[$keys[6]]);
		if (array_key_exists($keys[7], $arr)) $this->setNrOfWarningErrors($arr[$keys[7]]);
		if (array_key_exists($keys[8], $arr)) $this->setNrOfExtensions($arr[$keys[8]]);
		if (array_key_exists($keys[9], $arr)) $this->setNrOfUnspecifiedExtensions($arr[$keys[9]]);
		if (array_key_exists($keys[10], $arr)) $this->setPropertyNrErrorsOutgoingPreviousMonth($arr[$keys[10]]);
		if (array_key_exists($keys[11], $arr)) $this->setPropertyNrErrorsIncomingPreviousMonth($arr[$keys[11]]);
		if (array_key_exists($keys[12], $arr)) $this->setPropertyNrErrorsInternalPreviousMonth($arr[$keys[12]]);
		if (array_key_exists($keys[13], $arr)) $this->setPropertyNrErrorsOutgoingLast30Days($arr[$keys[13]]);
		if (array_key_exists($keys[14], $arr)) $this->setPropertyNrErrorsIncomingLast30Days($arr[$keys[14]]);
		if (array_key_exists($keys[15], $arr)) $this->setPropertyNrErrorsInternalLast30Days($arr[$keys[15]]);
		if (array_key_exists($keys[16], $arr)) $this->setPropertyNrOutgoingPreviousMonth($arr[$keys[16]]);
		if (array_key_exists($keys[17], $arr)) $this->setPropertyNrIncomingPreviousMonth($arr[$keys[17]]);
		if (array_key_exists($keys[18], $arr)) $this->setPropertyNrInternalPreviousMonth($arr[$keys[18]]);
		if (array_key_exists($keys[19], $arr)) $this->setPropertyNrOutgoingLast30Days($arr[$keys[19]]);
		if (array_key_exists($keys[20], $arr)) $this->setPropertyNrIncomingLast30Days($arr[$keys[20]]);
		if (array_key_exists($keys[21], $arr)) $this->setPropertyNrInternalLast30Days($arr[$keys[21]]);
		if (array_key_exists($keys[22], $arr)) $this->setLastProcessedCdrTimestamp($arr[$keys[22]]);
		if (array_key_exists($keys[23], $arr)) $this->setInfoTimestamp($arr[$keys[23]]);
	}

	/**
	 * Build a Criteria object containing the values of all modified columns in this object.
	 *
	 * @return     Criteria The Criteria object containing all modified values.
	 */
	public function buildCriteria()
	{
		$criteria = new Criteria(ArInstanceStatusPeer::DATABASE_NAME);

		if ($this->isColumnModified(ArInstanceStatusPeer::ID)) $criteria->add(ArInstanceStatusPeer::ID, $this->id);
		if ($this->isColumnModified(ArInstanceStatusPeer::INTERNAL_NAME)) $criteria->add(ArInstanceStatusPeer::INTERNAL_NAME, $this->internal_name);
		if ($this->isColumnModified(ArInstanceStatusPeer::INSTANCE_CODE)) $criteria->add(ArInstanceStatusPeer::INSTANCE_CODE, $this->instance_code);
		if ($this->isColumnModified(ArInstanceStatusPeer::AR_ORGANIZATION_UNIT_ID)) $criteria->add(ArInstanceStatusPeer::AR_ORGANIZATION_UNIT_ID, $this->ar_organization_unit_id);
		if ($this->isColumnModified(ArInstanceStatusPeer::APPLICATION_VERSION)) $criteria->add(ArInstanceStatusPeer::APPLICATION_VERSION, $this->application_version);
		if ($this->isColumnModified(ArInstanceStatusPeer::NR_OF_CRITICAL_ERRORS)) $criteria->add(ArInstanceStatusPeer::NR_OF_CRITICAL_ERRORS, $this->nr_of_critical_errors);
		if ($this->isColumnModified(ArInstanceStatusPeer::NR_OF_IMPORTANT_ERRORS)) $criteria->add(ArInstanceStatusPeer::NR_OF_IMPORTANT_ERRORS, $this->nr_of_important_errors);
		if ($this->isColumnModified(ArInstanceStatusPeer::NR_OF_WARNING_ERRORS)) $criteria->add(ArInstanceStatusPeer::NR_OF_WARNING_ERRORS, $this->nr_of_warning_errors);
		if ($this->isColumnModified(ArInstanceStatusPeer::NR_OF_EXTENSIONS)) $criteria->add(ArInstanceStatusPeer::NR_OF_EXTENSIONS, $this->nr_of_extensions);
		if ($this->isColumnModified(ArInstanceStatusPeer::NR_OF_UNSPECIFIED_EXTENSIONS)) $criteria->add(ArInstanceStatusPeer::NR_OF_UNSPECIFIED_EXTENSIONS, $this->nr_of_unspecified_extensions);
		if ($this->isColumnModified(ArInstanceStatusPeer::PROPERTY_NR_ERRORS_OUTGOING_PREVIOUS_MONTH)) $criteria->add(ArInstanceStatusPeer::PROPERTY_NR_ERRORS_OUTGOING_PREVIOUS_MONTH, $this->property_nr_errors_outgoing_previous_month);
		if ($this->isColumnModified(ArInstanceStatusPeer::PROPERTY_NR_ERRORS_INCOMING_PREVIOUS_MONTH)) $criteria->add(ArInstanceStatusPeer::PROPERTY_NR_ERRORS_INCOMING_PREVIOUS_MONTH, $this->property_nr_errors_incoming_previous_month);
		if ($this->isColumnModified(ArInstanceStatusPeer::PROPERTY_NR_ERRORS_INTERNAL_PREVIOUS_MONTH)) $criteria->add(ArInstanceStatusPeer::PROPERTY_NR_ERRORS_INTERNAL_PREVIOUS_MONTH, $this->property_nr_errors_internal_previous_month);
		if ($this->isColumnModified(ArInstanceStatusPeer::PROPERTY_NR_ERRORS_OUTGOING_LAST_30_DAYS)) $criteria->add(ArInstanceStatusPeer::PROPERTY_NR_ERRORS_OUTGOING_LAST_30_DAYS, $this->property_nr_errors_outgoing_last_30_days);
		if ($this->isColumnModified(ArInstanceStatusPeer::PROPERTY_NR_ERRORS_INCOMING_LAST_30_DAYS)) $criteria->add(ArInstanceStatusPeer::PROPERTY_NR_ERRORS_INCOMING_LAST_30_DAYS, $this->property_nr_errors_incoming_last_30_days);
		if ($this->isColumnModified(ArInstanceStatusPeer::PROPERTY_NR_ERRORS_INTERNAL_LAST_30_DAYS)) $criteria->add(ArInstanceStatusPeer::PROPERTY_NR_ERRORS_INTERNAL_LAST_30_DAYS, $this->property_nr_errors_internal_last_30_days);
		if ($this->isColumnModified(ArInstanceStatusPeer::PROPERTY_NR_OUTGOING_PREVIOUS_MONTH)) $criteria->add(ArInstanceStatusPeer::PROPERTY_NR_OUTGOING_PREVIOUS_MONTH, $this->property_nr_outgoing_previous_month);
		if ($this->isColumnModified(ArInstanceStatusPeer::PROPERTY_NR_INCOMING_PREVIOUS_MONTH)) $criteria->add(ArInstanceStatusPeer::PROPERTY_NR_INCOMING_PREVIOUS_MONTH, $this->property_nr_incoming_previous_month);
		if ($this->isColumnModified(ArInstanceStatusPeer::PROPERTY_NR_INTERNAL_PREVIOUS_MONTH)) $criteria->add(ArInstanceStatusPeer::PROPERTY_NR_INTERNAL_PREVIOUS_MONTH, $this->property_nr_internal_previous_month);
		if ($this->isColumnModified(ArInstanceStatusPeer::PROPERTY_NR_OUTGOING_LAST_30_DAYS)) $criteria->add(ArInstanceStatusPeer::PROPERTY_NR_OUTGOING_LAST_30_DAYS, $this->property_nr_outgoing_last_30_days);
		if ($this->isColumnModified(ArInstanceStatusPeer::PROPERTY_NR_INCOMING_LAST_30_DAYS)) $criteria->add(ArInstanceStatusPeer::PROPERTY_NR_INCOMING_LAST_30_DAYS, $this->property_nr_incoming_last_30_days);
		if ($this->isColumnModified(ArInstanceStatusPeer::PROPERTY_NR_INTERNAL_LAST_30_DAYS)) $criteria->add(ArInstanceStatusPeer::PROPERTY_NR_INTERNAL_LAST_30_DAYS, $this->property_nr_internal_last_30_days);
		if ($this->isColumnModified(ArInstanceStatusPeer::LAST_PROCESSED_CDR_TIMESTAMP)) $criteria->add(ArInstanceStatusPeer::LAST_PROCESSED_CDR_TIMESTAMP, $this->last_processed_cdr_timestamp);
		if ($this->isColumnModified(ArInstanceStatusPeer::INFO_TIMESTAMP)) $criteria->add(ArInstanceStatusPeer::INFO_TIMESTAMP, $this->info_timestamp);

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
		$criteria = new Criteria(ArInstanceStatusPeer::DATABASE_NAME);

		$criteria->add(ArInstanceStatusPeer::ID, $this->id);

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
	 * @param      object $copyObj An object of ArInstanceStatus (or compatible) type.
	 * @param      boolean $deepCopy Whether to also copy all rows that refer (by fkey) to the current row.
	 * @throws     PropelException
	 */
	public function copyInto($copyObj, $deepCopy = false)
	{

		$copyObj->setInternalName($this->internal_name);

		$copyObj->setInstanceCode($this->instance_code);

		$copyObj->setArOrganizationUnitId($this->ar_organization_unit_id);

		$copyObj->setApplicationVersion($this->application_version);

		$copyObj->setNrOfCriticalErrors($this->nr_of_critical_errors);

		$copyObj->setNrOfImportantErrors($this->nr_of_important_errors);

		$copyObj->setNrOfWarningErrors($this->nr_of_warning_errors);

		$copyObj->setNrOfExtensions($this->nr_of_extensions);

		$copyObj->setNrOfUnspecifiedExtensions($this->nr_of_unspecified_extensions);

		$copyObj->setPropertyNrErrorsOutgoingPreviousMonth($this->property_nr_errors_outgoing_previous_month);

		$copyObj->setPropertyNrErrorsIncomingPreviousMonth($this->property_nr_errors_incoming_previous_month);

		$copyObj->setPropertyNrErrorsInternalPreviousMonth($this->property_nr_errors_internal_previous_month);

		$copyObj->setPropertyNrErrorsOutgoingLast30Days($this->property_nr_errors_outgoing_last_30_days);

		$copyObj->setPropertyNrErrorsIncomingLast30Days($this->property_nr_errors_incoming_last_30_days);

		$copyObj->setPropertyNrErrorsInternalLast30Days($this->property_nr_errors_internal_last_30_days);

		$copyObj->setPropertyNrOutgoingPreviousMonth($this->property_nr_outgoing_previous_month);

		$copyObj->setPropertyNrIncomingPreviousMonth($this->property_nr_incoming_previous_month);

		$copyObj->setPropertyNrInternalPreviousMonth($this->property_nr_internal_previous_month);

		$copyObj->setPropertyNrOutgoingLast30Days($this->property_nr_outgoing_last_30_days);

		$copyObj->setPropertyNrIncomingLast30Days($this->property_nr_incoming_last_30_days);

		$copyObj->setPropertyNrInternalLast30Days($this->property_nr_internal_last_30_days);

		$copyObj->setLastProcessedCdrTimestamp($this->last_processed_cdr_timestamp);

		$copyObj->setInfoTimestamp($this->info_timestamp);


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
	 * @return     ArInstanceStatus Clone of current object.
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
	 * @return     ArInstanceStatusPeer
	 */
	public function getPeer()
	{
		if (self::$peer === null) {
			self::$peer = new ArInstanceStatusPeer();
		}
		return self::$peer;
	}

	/**
	 * Declares an association between this object and a ArOrganizationUnit object.
	 *
	 * @param      ArOrganizationUnit $v
	 * @return     ArInstanceStatus The current object (for fluent API support)
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
			$v->addArInstanceStatus($this);
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
			   $this->aArOrganizationUnit->addArInstanceStatuss($this);
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

} // BaseArInstanceStatus
