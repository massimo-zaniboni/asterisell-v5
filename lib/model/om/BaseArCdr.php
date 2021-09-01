<?php

/**
 * Base class that represents a row from the 'ar_cdr' table.
 *
 * 
 *
 * @package    lib.model.om
 */
abstract class BaseArCdr extends BaseObject  implements Persistent {


	/**
	 * The Peer class.
	 * Instance provides a convenient way of calling static methods on a class
	 * that calling code may not be able to identify.
	 * @var        ArCdrPeer
	 */
	protected static $peer;

	/**
	 * The value for the calldate field.
	 * @var        string
	 */
	protected $calldate;

	/**
	 * The value for the id field.
	 * Note: this column has a database default value of: 0
	 * @var        int
	 */
	protected $id;

	/**
	 * The value for the is_service_cdr field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $is_service_cdr;

	/**
	 * The value for the to_calldate field.
	 * @var        string
	 */
	protected $to_calldate;

	/**
	 * The value for the count_of_calls field.
	 * Note: this column has a database default value of: 1
	 * @var        int
	 */
	protected $count_of_calls;

	/**
	 * The value for the destination_type field.
	 * Note: this column has a database default value of: 0
	 * @var        int
	 */
	protected $destination_type;

	/**
	 * The value for the is_redirect field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $is_redirect;

	/**
	 * The value for the duration field.
	 * Note: this column has a database default value of: 0
	 * @var        int
	 */
	protected $duration;

	/**
	 * The value for the billsec field.
	 * Note: this column has a database default value of: 0
	 * @var        int
	 */
	protected $billsec;

	/**
	 * The value for the ar_organization_unit_id field.
	 * @var        int
	 */
	protected $ar_organization_unit_id;

	/**
	 * The value for the cached_parent_id_hierarchy field.
	 * @var        string
	 */
	protected $cached_parent_id_hierarchy;

	/**
	 * The value for the billable_ar_organization_unit_id field.
	 * @var        int
	 */
	protected $billable_ar_organization_unit_id;

	/**
	 * The value for the bundle_ar_organization_unit_id field.
	 * @var        int
	 */
	protected $bundle_ar_organization_unit_id;

	/**
	 * The value for the income field.
	 * @var        string
	 */
	protected $income;

	/**
	 * The value for the cost_saving field.
	 * @var        string
	 */
	protected $cost_saving;

	/**
	 * The value for the ar_vendor_id field.
	 * @var        int
	 */
	protected $ar_vendor_id;

	/**
	 * The value for the ar_communication_channel_type_id field.
	 * @var        int
	 */
	protected $ar_communication_channel_type_id;

	/**
	 * The value for the cost field.
	 * @var        string
	 */
	protected $cost;

	/**
	 * The value for the expected_cost field.
	 * @var        string
	 */
	protected $expected_cost;

	/**
	 * The value for the ar_telephone_prefix_id field.
	 * @var        int
	 */
	protected $ar_telephone_prefix_id;

	/**
	 * The value for the cached_external_telephone_number field.
	 * @var        string
	 */
	protected $cached_external_telephone_number;

	/**
	 * The value for the external_telephone_number_with_applied_portability field.
	 * @var        string
	 */
	protected $external_telephone_number_with_applied_portability;

	/**
	 * The value for the cached_masked_external_telephone_number field.
	 * @var        string
	 */
	protected $cached_masked_external_telephone_number;

	/**
	 * The value for the error_destination_type field.
	 * Note: this column has a database default value of: 0
	 * @var        int
	 */
	protected $error_destination_type;

	/**
	 * The value for the ar_problem_duplication_key field.
	 * @var        string
	 */
	protected $ar_problem_duplication_key;

	/**
	 * The value for the debug_cost_rate field.
	 * @var        string
	 */
	protected $debug_cost_rate;

	/**
	 * The value for the debug_income_rate field.
	 * @var        string
	 */
	protected $debug_income_rate;

	/**
	 * The value for the imported_info field.
	 * @var        string
	 */
	protected $imported_info;

	/**
	 * The value for the exported_internal_telephone_number field.
	 * @var        string
	 */
	protected $exported_internal_telephone_number;

	/**
	 * The value for the exported_billable_customer_ar_party_id field.
	 * @var        int
	 */
	protected $exported_billable_customer_ar_party_id;

	/**
	 * The value for the from_source_cdr_id field.
	 * @var        int
	 */
	protected $from_source_cdr_id;

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
	
	const PEER = 'ArCdrPeer';

	/**
	 * Applies default values to this object.
	 * This method should be called from the object's constructor (or
	 * equivalent initialization method).
	 * @see        __construct()
	 */
	public function applyDefaultValues()
	{
		$this->id = 0;
		$this->is_service_cdr = false;
		$this->count_of_calls = 1;
		$this->destination_type = 0;
		$this->is_redirect = false;
		$this->duration = 0;
		$this->billsec = 0;
		$this->error_destination_type = 0;
	}

	/**
	 * Initializes internal state of BaseArCdr object.
	 * @see        applyDefaults()
	 */
	public function __construct()
	{
		parent::__construct();
		$this->applyDefaultValues();
	}

	/**
	 * Get the [optionally formatted] temporal [calldate] column value.
	 * 
	 *
	 * @param      string $format The date/time format string (either date()-style or strftime()-style).
	 *							If format is NULL, then the raw DateTime object will be returned.
	 * @return     mixed Formatted date/time value as string or DateTime object (if format is NULL), NULL if column is NULL, and 0 if column value is 0000-00-00 00:00:00
	 * @throws     PropelException - if unable to parse/validate the date/time value.
	 */
	public function getCalldate($format = 'Y-m-d H:i:s')
	{
		if ($this->calldate === null) {
			return null;
		}


		if ($this->calldate === '0000-00-00 00:00:00') {
			// while technically this is not a default value of NULL,
			// this seems to be closest in meaning.
			return null;
		} else {
			try {
				$dt = new DateTime($this->calldate);
			} catch (Exception $x) {
				throw new PropelException("Internally stored date/time/timestamp value could not be converted to DateTime: " . var_export($this->calldate, true), $x);
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
	 * Get the [id] column value.
	 * 
	 * @return     int
	 */
	public function getId()
	{
		return $this->id;
	}

	/**
	 * Get the [is_service_cdr] column value.
	 * 
	 * @return     boolean
	 */
	public function getIsServiceCdr()
	{
		return $this->is_service_cdr;
	}

	/**
	 * Get the [optionally formatted] temporal [to_calldate] column value.
	 * 
	 *
	 * @param      string $format The date/time format string (either date()-style or strftime()-style).
	 *							If format is NULL, then the raw DateTime object will be returned.
	 * @return     mixed Formatted date/time value as string or DateTime object (if format is NULL), NULL if column is NULL, and 0 if column value is 0000-00-00 00:00:00
	 * @throws     PropelException - if unable to parse/validate the date/time value.
	 */
	public function getToCalldate($format = 'Y-m-d H:i:s')
	{
		if ($this->to_calldate === null) {
			return null;
		}


		if ($this->to_calldate === '0000-00-00 00:00:00') {
			// while technically this is not a default value of NULL,
			// this seems to be closest in meaning.
			return null;
		} else {
			try {
				$dt = new DateTime($this->to_calldate);
			} catch (Exception $x) {
				throw new PropelException("Internally stored date/time/timestamp value could not be converted to DateTime: " . var_export($this->to_calldate, true), $x);
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
	 * Get the [count_of_calls] column value.
	 * 
	 * @return     int
	 */
	public function getCountOfCalls()
	{
		return $this->count_of_calls;
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
	 * Get the [is_redirect] column value.
	 * 
	 * @return     boolean
	 */
	public function getIsRedirect()
	{
		return $this->is_redirect;
	}

	/**
	 * Get the [duration] column value.
	 * 
	 * @return     int
	 */
	public function getDuration()
	{
		return $this->duration;
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
	 * Get the [ar_organization_unit_id] column value.
	 * 
	 * @return     int
	 */
	public function getArOrganizationUnitId()
	{
		return $this->ar_organization_unit_id;
	}

	/**
	 * Get the [cached_parent_id_hierarchy] column value.
	 * 
	 * @return     string
	 */
	public function getCachedParentIdHierarchy()
	{
		return $this->cached_parent_id_hierarchy;
	}

	/**
	 * Get the [billable_ar_organization_unit_id] column value.
	 * 
	 * @return     int
	 */
	public function getBillableArOrganizationUnitId()
	{
		return $this->billable_ar_organization_unit_id;
	}

	/**
	 * Get the [bundle_ar_organization_unit_id] column value.
	 * 
	 * @return     int
	 */
	public function getBundleArOrganizationUnitId()
	{
		return $this->bundle_ar_organization_unit_id;
	}

	/**
	 * Get the [income] column value.
	 * 
	 * @return     string
	 */
	public function getIncome()
	{
		return $this->income;
	}

	/**
	 * Get the [cost_saving] column value.
	 * 
	 * @return     string
	 */
	public function getCostSaving()
	{
		return $this->cost_saving;
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
	 * Get the [ar_communication_channel_type_id] column value.
	 * 
	 * @return     int
	 */
	public function getArCommunicationChannelTypeId()
	{
		return $this->ar_communication_channel_type_id;
	}

	/**
	 * Get the [cost] column value.
	 * 
	 * @return     string
	 */
	public function getCost()
	{
		return $this->cost;
	}

	/**
	 * Get the [expected_cost] column value.
	 * 
	 * @return     string
	 */
	public function getExpectedCost()
	{
		return $this->expected_cost;
	}

	/**
	 * Get the [ar_telephone_prefix_id] column value.
	 * 
	 * @return     int
	 */
	public function getArTelephonePrefixId()
	{
		return $this->ar_telephone_prefix_id;
	}

	/**
	 * Get the [cached_external_telephone_number] column value.
	 * 
	 * @return     string
	 */
	public function getCachedExternalTelephoneNumber()
	{
		return $this->cached_external_telephone_number;
	}

	/**
	 * Get the [external_telephone_number_with_applied_portability] column value.
	 * 
	 * @return     string
	 */
	public function getExternalTelephoneNumberWithAppliedPortability()
	{
		return $this->external_telephone_number_with_applied_portability;
	}

	/**
	 * Get the [cached_masked_external_telephone_number] column value.
	 * 
	 * @return     string
	 */
	public function getCachedMaskedExternalTelephoneNumber()
	{
		return $this->cached_masked_external_telephone_number;
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
	 * Get the [ar_problem_duplication_key] column value.
	 * 
	 * @return     string
	 */
	public function getArProblemDuplicationKey()
	{
		return $this->ar_problem_duplication_key;
	}

	/**
	 * Get the [debug_cost_rate] column value.
	 * 
	 * @return     string
	 */
	public function getDebugCostRate()
	{
		return $this->debug_cost_rate;
	}

	/**
	 * Get the [debug_income_rate] column value.
	 * 
	 * @return     string
	 */
	public function getDebugIncomeRate()
	{
		return $this->debug_income_rate;
	}

	/**
	 * Get the [imported_info] column value.
	 * 
	 * @return     string
	 */
	public function getImportedInfo()
	{
		return $this->imported_info;
	}

	/**
	 * Get the [exported_internal_telephone_number] column value.
	 * 
	 * @return     string
	 */
	public function getExportedInternalTelephoneNumber()
	{
		return $this->exported_internal_telephone_number;
	}

	/**
	 * Get the [exported_billable_customer_ar_party_id] column value.
	 * 
	 * @return     int
	 */
	public function getExportedBillableCustomerArPartyId()
	{
		return $this->exported_billable_customer_ar_party_id;
	}

	/**
	 * Get the [from_source_cdr_id] column value.
	 * 
	 * @return     int
	 */
	public function getFromSourceCdrId()
	{
		return $this->from_source_cdr_id;
	}

	/**
	 * Sets the value of [calldate] column to a normalized version of the date/time value specified.
	 * 
	 * @param      mixed $v string, integer (timestamp), or DateTime value.  Empty string will
	 *						be treated as NULL for temporal objects.
	 * @return     ArCdr The current object (for fluent API support)
	 */
	public function setCalldate($v)
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

		if ( $this->calldate !== null || $dt !== null ) {
			// (nested ifs are a little easier to read in this case)

			$currNorm = ($this->calldate !== null && $tmpDt = new DateTime($this->calldate)) ? $tmpDt->format('Y-m-d H:i:s') : null;
			$newNorm = ($dt !== null) ? $dt->format('Y-m-d H:i:s') : null;

			if ( ($currNorm !== $newNorm) // normalized values don't match 
					)
			{
				$this->calldate = ($dt ? $dt->format('Y-m-d H:i:s') : null);
				$this->modifiedColumns[] = ArCdrPeer::CALLDATE;
			}
		} // if either are not null

		return $this;
	} // setCalldate()

	/**
	 * Set the value of [id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArCdr The current object (for fluent API support)
	 */
	public function setId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->id !== $v || $this->isNew()) {
			$this->id = $v;
			$this->modifiedColumns[] = ArCdrPeer::ID;
		}

		return $this;
	} // setId()

	/**
	 * Set the value of [is_service_cdr] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArCdr The current object (for fluent API support)
	 */
	public function setIsServiceCdr($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->is_service_cdr !== $v || $this->isNew()) {
			$this->is_service_cdr = $v;
			$this->modifiedColumns[] = ArCdrPeer::IS_SERVICE_CDR;
		}

		return $this;
	} // setIsServiceCdr()

	/**
	 * Sets the value of [to_calldate] column to a normalized version of the date/time value specified.
	 * 
	 * @param      mixed $v string, integer (timestamp), or DateTime value.  Empty string will
	 *						be treated as NULL for temporal objects.
	 * @return     ArCdr The current object (for fluent API support)
	 */
	public function setToCalldate($v)
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

		if ( $this->to_calldate !== null || $dt !== null ) {
			// (nested ifs are a little easier to read in this case)

			$currNorm = ($this->to_calldate !== null && $tmpDt = new DateTime($this->to_calldate)) ? $tmpDt->format('Y-m-d H:i:s') : null;
			$newNorm = ($dt !== null) ? $dt->format('Y-m-d H:i:s') : null;

			if ( ($currNorm !== $newNorm) // normalized values don't match 
					)
			{
				$this->to_calldate = ($dt ? $dt->format('Y-m-d H:i:s') : null);
				$this->modifiedColumns[] = ArCdrPeer::TO_CALLDATE;
			}
		} // if either are not null

		return $this;
	} // setToCalldate()

	/**
	 * Set the value of [count_of_calls] column.
	 * 
	 * @param      int $v new value
	 * @return     ArCdr The current object (for fluent API support)
	 */
	public function setCountOfCalls($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->count_of_calls !== $v || $this->isNew()) {
			$this->count_of_calls = $v;
			$this->modifiedColumns[] = ArCdrPeer::COUNT_OF_CALLS;
		}

		return $this;
	} // setCountOfCalls()

	/**
	 * Set the value of [destination_type] column.
	 * 
	 * @param      int $v new value
	 * @return     ArCdr The current object (for fluent API support)
	 */
	public function setDestinationType($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->destination_type !== $v || $this->isNew()) {
			$this->destination_type = $v;
			$this->modifiedColumns[] = ArCdrPeer::DESTINATION_TYPE;
		}

		return $this;
	} // setDestinationType()

	/**
	 * Set the value of [is_redirect] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArCdr The current object (for fluent API support)
	 */
	public function setIsRedirect($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->is_redirect !== $v || $this->isNew()) {
			$this->is_redirect = $v;
			$this->modifiedColumns[] = ArCdrPeer::IS_REDIRECT;
		}

		return $this;
	} // setIsRedirect()

	/**
	 * Set the value of [duration] column.
	 * 
	 * @param      int $v new value
	 * @return     ArCdr The current object (for fluent API support)
	 */
	public function setDuration($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->duration !== $v || $this->isNew()) {
			$this->duration = $v;
			$this->modifiedColumns[] = ArCdrPeer::DURATION;
		}

		return $this;
	} // setDuration()

	/**
	 * Set the value of [billsec] column.
	 * 
	 * @param      int $v new value
	 * @return     ArCdr The current object (for fluent API support)
	 */
	public function setBillsec($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->billsec !== $v || $this->isNew()) {
			$this->billsec = $v;
			$this->modifiedColumns[] = ArCdrPeer::BILLSEC;
		}

		return $this;
	} // setBillsec()

	/**
	 * Set the value of [ar_organization_unit_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArCdr The current object (for fluent API support)
	 */
	public function setArOrganizationUnitId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->ar_organization_unit_id !== $v) {
			$this->ar_organization_unit_id = $v;
			$this->modifiedColumns[] = ArCdrPeer::AR_ORGANIZATION_UNIT_ID;
		}

		return $this;
	} // setArOrganizationUnitId()

	/**
	 * Set the value of [cached_parent_id_hierarchy] column.
	 * 
	 * @param      string $v new value
	 * @return     ArCdr The current object (for fluent API support)
	 */
	public function setCachedParentIdHierarchy($v)
	{
		// Because BLOB columns are streams in PDO we have to assume that they are
		// always modified when a new value is passed in.  For example, the contents
		// of the stream itself may have changed externally.
		if (!is_resource($v) && $v !== null) {
			$this->cached_parent_id_hierarchy = fopen('php://memory', 'r+');
			fwrite($this->cached_parent_id_hierarchy, $v);
			rewind($this->cached_parent_id_hierarchy);
		} else { // it's already a stream
			$this->cached_parent_id_hierarchy = $v;
		}
		$this->modifiedColumns[] = ArCdrPeer::CACHED_PARENT_ID_HIERARCHY;

		return $this;
	} // setCachedParentIdHierarchy()

	/**
	 * Set the value of [billable_ar_organization_unit_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArCdr The current object (for fluent API support)
	 */
	public function setBillableArOrganizationUnitId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->billable_ar_organization_unit_id !== $v) {
			$this->billable_ar_organization_unit_id = $v;
			$this->modifiedColumns[] = ArCdrPeer::BILLABLE_AR_ORGANIZATION_UNIT_ID;
		}

		return $this;
	} // setBillableArOrganizationUnitId()

	/**
	 * Set the value of [bundle_ar_organization_unit_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArCdr The current object (for fluent API support)
	 */
	public function setBundleArOrganizationUnitId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->bundle_ar_organization_unit_id !== $v) {
			$this->bundle_ar_organization_unit_id = $v;
			$this->modifiedColumns[] = ArCdrPeer::BUNDLE_AR_ORGANIZATION_UNIT_ID;
		}

		return $this;
	} // setBundleArOrganizationUnitId()

	/**
	 * Set the value of [income] column.
	 * 
	 * @param      string $v new value
	 * @return     ArCdr The current object (for fluent API support)
	 */
	public function setIncome($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->income !== $v) {
			$this->income = $v;
			$this->modifiedColumns[] = ArCdrPeer::INCOME;
		}

		return $this;
	} // setIncome()

	/**
	 * Set the value of [cost_saving] column.
	 * 
	 * @param      string $v new value
	 * @return     ArCdr The current object (for fluent API support)
	 */
	public function setCostSaving($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->cost_saving !== $v) {
			$this->cost_saving = $v;
			$this->modifiedColumns[] = ArCdrPeer::COST_SAVING;
		}

		return $this;
	} // setCostSaving()

	/**
	 * Set the value of [ar_vendor_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArCdr The current object (for fluent API support)
	 */
	public function setArVendorId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->ar_vendor_id !== $v) {
			$this->ar_vendor_id = $v;
			$this->modifiedColumns[] = ArCdrPeer::AR_VENDOR_ID;
		}

		return $this;
	} // setArVendorId()

	/**
	 * Set the value of [ar_communication_channel_type_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArCdr The current object (for fluent API support)
	 */
	public function setArCommunicationChannelTypeId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->ar_communication_channel_type_id !== $v) {
			$this->ar_communication_channel_type_id = $v;
			$this->modifiedColumns[] = ArCdrPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID;
		}

		return $this;
	} // setArCommunicationChannelTypeId()

	/**
	 * Set the value of [cost] column.
	 * 
	 * @param      string $v new value
	 * @return     ArCdr The current object (for fluent API support)
	 */
	public function setCost($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->cost !== $v) {
			$this->cost = $v;
			$this->modifiedColumns[] = ArCdrPeer::COST;
		}

		return $this;
	} // setCost()

	/**
	 * Set the value of [expected_cost] column.
	 * 
	 * @param      string $v new value
	 * @return     ArCdr The current object (for fluent API support)
	 */
	public function setExpectedCost($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->expected_cost !== $v) {
			$this->expected_cost = $v;
			$this->modifiedColumns[] = ArCdrPeer::EXPECTED_COST;
		}

		return $this;
	} // setExpectedCost()

	/**
	 * Set the value of [ar_telephone_prefix_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArCdr The current object (for fluent API support)
	 */
	public function setArTelephonePrefixId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->ar_telephone_prefix_id !== $v) {
			$this->ar_telephone_prefix_id = $v;
			$this->modifiedColumns[] = ArCdrPeer::AR_TELEPHONE_PREFIX_ID;
		}

		return $this;
	} // setArTelephonePrefixId()

	/**
	 * Set the value of [cached_external_telephone_number] column.
	 * 
	 * @param      string $v new value
	 * @return     ArCdr The current object (for fluent API support)
	 */
	public function setCachedExternalTelephoneNumber($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->cached_external_telephone_number !== $v) {
			$this->cached_external_telephone_number = $v;
			$this->modifiedColumns[] = ArCdrPeer::CACHED_EXTERNAL_TELEPHONE_NUMBER;
		}

		return $this;
	} // setCachedExternalTelephoneNumber()

	/**
	 * Set the value of [external_telephone_number_with_applied_portability] column.
	 * 
	 * @param      string $v new value
	 * @return     ArCdr The current object (for fluent API support)
	 */
	public function setExternalTelephoneNumberWithAppliedPortability($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->external_telephone_number_with_applied_portability !== $v) {
			$this->external_telephone_number_with_applied_portability = $v;
			$this->modifiedColumns[] = ArCdrPeer::EXTERNAL_TELEPHONE_NUMBER_WITH_APPLIED_PORTABILITY;
		}

		return $this;
	} // setExternalTelephoneNumberWithAppliedPortability()

	/**
	 * Set the value of [cached_masked_external_telephone_number] column.
	 * 
	 * @param      string $v new value
	 * @return     ArCdr The current object (for fluent API support)
	 */
	public function setCachedMaskedExternalTelephoneNumber($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->cached_masked_external_telephone_number !== $v) {
			$this->cached_masked_external_telephone_number = $v;
			$this->modifiedColumns[] = ArCdrPeer::CACHED_MASKED_EXTERNAL_TELEPHONE_NUMBER;
		}

		return $this;
	} // setCachedMaskedExternalTelephoneNumber()

	/**
	 * Set the value of [error_destination_type] column.
	 * 
	 * @param      int $v new value
	 * @return     ArCdr The current object (for fluent API support)
	 */
	public function setErrorDestinationType($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->error_destination_type !== $v || $this->isNew()) {
			$this->error_destination_type = $v;
			$this->modifiedColumns[] = ArCdrPeer::ERROR_DESTINATION_TYPE;
		}

		return $this;
	} // setErrorDestinationType()

	/**
	 * Set the value of [ar_problem_duplication_key] column.
	 * 
	 * @param      string $v new value
	 * @return     ArCdr The current object (for fluent API support)
	 */
	public function setArProblemDuplicationKey($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->ar_problem_duplication_key !== $v) {
			$this->ar_problem_duplication_key = $v;
			$this->modifiedColumns[] = ArCdrPeer::AR_PROBLEM_DUPLICATION_KEY;
		}

		return $this;
	} // setArProblemDuplicationKey()

	/**
	 * Set the value of [debug_cost_rate] column.
	 * 
	 * @param      string $v new value
	 * @return     ArCdr The current object (for fluent API support)
	 */
	public function setDebugCostRate($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->debug_cost_rate !== $v) {
			$this->debug_cost_rate = $v;
			$this->modifiedColumns[] = ArCdrPeer::DEBUG_COST_RATE;
		}

		return $this;
	} // setDebugCostRate()

	/**
	 * Set the value of [debug_income_rate] column.
	 * 
	 * @param      string $v new value
	 * @return     ArCdr The current object (for fluent API support)
	 */
	public function setDebugIncomeRate($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->debug_income_rate !== $v) {
			$this->debug_income_rate = $v;
			$this->modifiedColumns[] = ArCdrPeer::DEBUG_INCOME_RATE;
		}

		return $this;
	} // setDebugIncomeRate()

	/**
	 * Set the value of [imported_info] column.
	 * 
	 * @param      string $v new value
	 * @return     ArCdr The current object (for fluent API support)
	 */
	public function setImportedInfo($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->imported_info !== $v) {
			$this->imported_info = $v;
			$this->modifiedColumns[] = ArCdrPeer::IMPORTED_INFO;
		}

		return $this;
	} // setImportedInfo()

	/**
	 * Set the value of [exported_internal_telephone_number] column.
	 * 
	 * @param      string $v new value
	 * @return     ArCdr The current object (for fluent API support)
	 */
	public function setExportedInternalTelephoneNumber($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->exported_internal_telephone_number !== $v) {
			$this->exported_internal_telephone_number = $v;
			$this->modifiedColumns[] = ArCdrPeer::EXPORTED_INTERNAL_TELEPHONE_NUMBER;
		}

		return $this;
	} // setExportedInternalTelephoneNumber()

	/**
	 * Set the value of [exported_billable_customer_ar_party_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArCdr The current object (for fluent API support)
	 */
	public function setExportedBillableCustomerArPartyId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->exported_billable_customer_ar_party_id !== $v) {
			$this->exported_billable_customer_ar_party_id = $v;
			$this->modifiedColumns[] = ArCdrPeer::EXPORTED_BILLABLE_CUSTOMER_AR_PARTY_ID;
		}

		return $this;
	} // setExportedBillableCustomerArPartyId()

	/**
	 * Set the value of [from_source_cdr_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArCdr The current object (for fluent API support)
	 */
	public function setFromSourceCdrId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->from_source_cdr_id !== $v) {
			$this->from_source_cdr_id = $v;
			$this->modifiedColumns[] = ArCdrPeer::FROM_SOURCE_CDR_ID;
		}

		return $this;
	} // setFromSourceCdrId()

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
			if ($this->id !== 0) {
				return false;
			}

			if ($this->is_service_cdr !== false) {
				return false;
			}

			if ($this->count_of_calls !== 1) {
				return false;
			}

			if ($this->destination_type !== 0) {
				return false;
			}

			if ($this->is_redirect !== false) {
				return false;
			}

			if ($this->duration !== 0) {
				return false;
			}

			if ($this->billsec !== 0) {
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

			$this->calldate = ($row[$startcol + 0] !== null) ? (string) $row[$startcol + 0] : null;
			$this->id = ($row[$startcol + 1] !== null) ? (int) $row[$startcol + 1] : null;
			$this->is_service_cdr = ($row[$startcol + 2] !== null) ? (boolean) $row[$startcol + 2] : null;
			$this->to_calldate = ($row[$startcol + 3] !== null) ? (string) $row[$startcol + 3] : null;
			$this->count_of_calls = ($row[$startcol + 4] !== null) ? (int) $row[$startcol + 4] : null;
			$this->destination_type = ($row[$startcol + 5] !== null) ? (int) $row[$startcol + 5] : null;
			$this->is_redirect = ($row[$startcol + 6] !== null) ? (boolean) $row[$startcol + 6] : null;
			$this->duration = ($row[$startcol + 7] !== null) ? (int) $row[$startcol + 7] : null;
			$this->billsec = ($row[$startcol + 8] !== null) ? (int) $row[$startcol + 8] : null;
			$this->ar_organization_unit_id = ($row[$startcol + 9] !== null) ? (int) $row[$startcol + 9] : null;
			if ($row[$startcol + 10] !== null) {
				$this->cached_parent_id_hierarchy = fopen('php://memory', 'r+');
				fwrite($this->cached_parent_id_hierarchy, $row[$startcol + 10]);
				rewind($this->cached_parent_id_hierarchy);
			} else {
				$this->cached_parent_id_hierarchy = null;
			}
			$this->billable_ar_organization_unit_id = ($row[$startcol + 11] !== null) ? (int) $row[$startcol + 11] : null;
			$this->bundle_ar_organization_unit_id = ($row[$startcol + 12] !== null) ? (int) $row[$startcol + 12] : null;
			$this->income = ($row[$startcol + 13] !== null) ? (string) $row[$startcol + 13] : null;
			$this->cost_saving = ($row[$startcol + 14] !== null) ? (string) $row[$startcol + 14] : null;
			$this->ar_vendor_id = ($row[$startcol + 15] !== null) ? (int) $row[$startcol + 15] : null;
			$this->ar_communication_channel_type_id = ($row[$startcol + 16] !== null) ? (int) $row[$startcol + 16] : null;
			$this->cost = ($row[$startcol + 17] !== null) ? (string) $row[$startcol + 17] : null;
			$this->expected_cost = ($row[$startcol + 18] !== null) ? (string) $row[$startcol + 18] : null;
			$this->ar_telephone_prefix_id = ($row[$startcol + 19] !== null) ? (int) $row[$startcol + 19] : null;
			$this->cached_external_telephone_number = ($row[$startcol + 20] !== null) ? (string) $row[$startcol + 20] : null;
			$this->external_telephone_number_with_applied_portability = ($row[$startcol + 21] !== null) ? (string) $row[$startcol + 21] : null;
			$this->cached_masked_external_telephone_number = ($row[$startcol + 22] !== null) ? (string) $row[$startcol + 22] : null;
			$this->error_destination_type = ($row[$startcol + 23] !== null) ? (int) $row[$startcol + 23] : null;
			$this->ar_problem_duplication_key = ($row[$startcol + 24] !== null) ? (string) $row[$startcol + 24] : null;
			$this->debug_cost_rate = ($row[$startcol + 25] !== null) ? (string) $row[$startcol + 25] : null;
			$this->debug_income_rate = ($row[$startcol + 26] !== null) ? (string) $row[$startcol + 26] : null;
			$this->imported_info = ($row[$startcol + 27] !== null) ? (string) $row[$startcol + 27] : null;
			$this->exported_internal_telephone_number = ($row[$startcol + 28] !== null) ? (string) $row[$startcol + 28] : null;
			$this->exported_billable_customer_ar_party_id = ($row[$startcol + 29] !== null) ? (int) $row[$startcol + 29] : null;
			$this->from_source_cdr_id = ($row[$startcol + 30] !== null) ? (int) $row[$startcol + 30] : null;
			$this->resetModified();

			$this->setNew(false);

			if ($rehydrate) {
				$this->ensureConsistency();
			}

			// FIXME - using NUM_COLUMNS may be clearer.
			return $startcol + 31; // 31 = ArCdrPeer::NUM_COLUMNS - ArCdrPeer::NUM_LAZY_LOAD_COLUMNS).

		} catch (Exception $e) {
			throw new PropelException("Error populating ArCdr object", $e);
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
			$con = Propel::getConnection(ArCdrPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		// We don't need to alter the object instance pool; we're just modifying this instance
		// already in the pool.

		$stmt = ArCdrPeer::doSelectStmt($this->buildPkeyCriteria(), $con);
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
			$con = Propel::getConnection(ArCdrPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}
		
		$con->beginTransaction();
		try {
			$ret = $this->preDelete($con);
			if ($ret) {
				ArCdrPeer::doDelete($this, $con);
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
			$con = Propel::getConnection(ArCdrPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
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
				ArCdrPeer::addInstanceToPool($this);
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


			// If this object has been modified, then save it to the database.
			if ($this->isModified()) {
				if ($this->isNew()) {
					$pk = ArCdrPeer::doInsert($this, $con);
					$affectedRows += 1; // we are assuming that there is only 1 row per doInsert() which
										 // should always be true here (even though technically
										 // BasePeer::doInsert() can insert multiple rows).

					$this->setNew(false);
				} else {
					$affectedRows += ArCdrPeer::doUpdate($this, $con);
				}

				// Rewind the cached_parent_id_hierarchy LOB column, since PDO does not rewind after inserting value.
				if ($this->cached_parent_id_hierarchy !== null && is_resource($this->cached_parent_id_hierarchy)) {
					rewind($this->cached_parent_id_hierarchy);
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


			if (($retval = ArCdrPeer::doValidate($this, $columns)) !== true) {
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
		$pos = ArCdrPeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
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
				return $this->getCalldate();
				break;
			case 1:
				return $this->getId();
				break;
			case 2:
				return $this->getIsServiceCdr();
				break;
			case 3:
				return $this->getToCalldate();
				break;
			case 4:
				return $this->getCountOfCalls();
				break;
			case 5:
				return $this->getDestinationType();
				break;
			case 6:
				return $this->getIsRedirect();
				break;
			case 7:
				return $this->getDuration();
				break;
			case 8:
				return $this->getBillsec();
				break;
			case 9:
				return $this->getArOrganizationUnitId();
				break;
			case 10:
				return $this->getCachedParentIdHierarchy();
				break;
			case 11:
				return $this->getBillableArOrganizationUnitId();
				break;
			case 12:
				return $this->getBundleArOrganizationUnitId();
				break;
			case 13:
				return $this->getIncome();
				break;
			case 14:
				return $this->getCostSaving();
				break;
			case 15:
				return $this->getArVendorId();
				break;
			case 16:
				return $this->getArCommunicationChannelTypeId();
				break;
			case 17:
				return $this->getCost();
				break;
			case 18:
				return $this->getExpectedCost();
				break;
			case 19:
				return $this->getArTelephonePrefixId();
				break;
			case 20:
				return $this->getCachedExternalTelephoneNumber();
				break;
			case 21:
				return $this->getExternalTelephoneNumberWithAppliedPortability();
				break;
			case 22:
				return $this->getCachedMaskedExternalTelephoneNumber();
				break;
			case 23:
				return $this->getErrorDestinationType();
				break;
			case 24:
				return $this->getArProblemDuplicationKey();
				break;
			case 25:
				return $this->getDebugCostRate();
				break;
			case 26:
				return $this->getDebugIncomeRate();
				break;
			case 27:
				return $this->getImportedInfo();
				break;
			case 28:
				return $this->getExportedInternalTelephoneNumber();
				break;
			case 29:
				return $this->getExportedBillableCustomerArPartyId();
				break;
			case 30:
				return $this->getFromSourceCdrId();
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
		$keys = ArCdrPeer::getFieldNames($keyType);
		$result = array(
			$keys[0] => $this->getCalldate(),
			$keys[1] => $this->getId(),
			$keys[2] => $this->getIsServiceCdr(),
			$keys[3] => $this->getToCalldate(),
			$keys[4] => $this->getCountOfCalls(),
			$keys[5] => $this->getDestinationType(),
			$keys[6] => $this->getIsRedirect(),
			$keys[7] => $this->getDuration(),
			$keys[8] => $this->getBillsec(),
			$keys[9] => $this->getArOrganizationUnitId(),
			$keys[10] => $this->getCachedParentIdHierarchy(),
			$keys[11] => $this->getBillableArOrganizationUnitId(),
			$keys[12] => $this->getBundleArOrganizationUnitId(),
			$keys[13] => $this->getIncome(),
			$keys[14] => $this->getCostSaving(),
			$keys[15] => $this->getArVendorId(),
			$keys[16] => $this->getArCommunicationChannelTypeId(),
			$keys[17] => $this->getCost(),
			$keys[18] => $this->getExpectedCost(),
			$keys[19] => $this->getArTelephonePrefixId(),
			$keys[20] => $this->getCachedExternalTelephoneNumber(),
			$keys[21] => $this->getExternalTelephoneNumberWithAppliedPortability(),
			$keys[22] => $this->getCachedMaskedExternalTelephoneNumber(),
			$keys[23] => $this->getErrorDestinationType(),
			$keys[24] => $this->getArProblemDuplicationKey(),
			$keys[25] => $this->getDebugCostRate(),
			$keys[26] => $this->getDebugIncomeRate(),
			$keys[27] => $this->getImportedInfo(),
			$keys[28] => $this->getExportedInternalTelephoneNumber(),
			$keys[29] => $this->getExportedBillableCustomerArPartyId(),
			$keys[30] => $this->getFromSourceCdrId(),
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
		$pos = ArCdrPeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
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
				$this->setCalldate($value);
				break;
			case 1:
				$this->setId($value);
				break;
			case 2:
				$this->setIsServiceCdr($value);
				break;
			case 3:
				$this->setToCalldate($value);
				break;
			case 4:
				$this->setCountOfCalls($value);
				break;
			case 5:
				$this->setDestinationType($value);
				break;
			case 6:
				$this->setIsRedirect($value);
				break;
			case 7:
				$this->setDuration($value);
				break;
			case 8:
				$this->setBillsec($value);
				break;
			case 9:
				$this->setArOrganizationUnitId($value);
				break;
			case 10:
				$this->setCachedParentIdHierarchy($value);
				break;
			case 11:
				$this->setBillableArOrganizationUnitId($value);
				break;
			case 12:
				$this->setBundleArOrganizationUnitId($value);
				break;
			case 13:
				$this->setIncome($value);
				break;
			case 14:
				$this->setCostSaving($value);
				break;
			case 15:
				$this->setArVendorId($value);
				break;
			case 16:
				$this->setArCommunicationChannelTypeId($value);
				break;
			case 17:
				$this->setCost($value);
				break;
			case 18:
				$this->setExpectedCost($value);
				break;
			case 19:
				$this->setArTelephonePrefixId($value);
				break;
			case 20:
				$this->setCachedExternalTelephoneNumber($value);
				break;
			case 21:
				$this->setExternalTelephoneNumberWithAppliedPortability($value);
				break;
			case 22:
				$this->setCachedMaskedExternalTelephoneNumber($value);
				break;
			case 23:
				$this->setErrorDestinationType($value);
				break;
			case 24:
				$this->setArProblemDuplicationKey($value);
				break;
			case 25:
				$this->setDebugCostRate($value);
				break;
			case 26:
				$this->setDebugIncomeRate($value);
				break;
			case 27:
				$this->setImportedInfo($value);
				break;
			case 28:
				$this->setExportedInternalTelephoneNumber($value);
				break;
			case 29:
				$this->setExportedBillableCustomerArPartyId($value);
				break;
			case 30:
				$this->setFromSourceCdrId($value);
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
		$keys = ArCdrPeer::getFieldNames($keyType);

		if (array_key_exists($keys[0], $arr)) $this->setCalldate($arr[$keys[0]]);
		if (array_key_exists($keys[1], $arr)) $this->setId($arr[$keys[1]]);
		if (array_key_exists($keys[2], $arr)) $this->setIsServiceCdr($arr[$keys[2]]);
		if (array_key_exists($keys[3], $arr)) $this->setToCalldate($arr[$keys[3]]);
		if (array_key_exists($keys[4], $arr)) $this->setCountOfCalls($arr[$keys[4]]);
		if (array_key_exists($keys[5], $arr)) $this->setDestinationType($arr[$keys[5]]);
		if (array_key_exists($keys[6], $arr)) $this->setIsRedirect($arr[$keys[6]]);
		if (array_key_exists($keys[7], $arr)) $this->setDuration($arr[$keys[7]]);
		if (array_key_exists($keys[8], $arr)) $this->setBillsec($arr[$keys[8]]);
		if (array_key_exists($keys[9], $arr)) $this->setArOrganizationUnitId($arr[$keys[9]]);
		if (array_key_exists($keys[10], $arr)) $this->setCachedParentIdHierarchy($arr[$keys[10]]);
		if (array_key_exists($keys[11], $arr)) $this->setBillableArOrganizationUnitId($arr[$keys[11]]);
		if (array_key_exists($keys[12], $arr)) $this->setBundleArOrganizationUnitId($arr[$keys[12]]);
		if (array_key_exists($keys[13], $arr)) $this->setIncome($arr[$keys[13]]);
		if (array_key_exists($keys[14], $arr)) $this->setCostSaving($arr[$keys[14]]);
		if (array_key_exists($keys[15], $arr)) $this->setArVendorId($arr[$keys[15]]);
		if (array_key_exists($keys[16], $arr)) $this->setArCommunicationChannelTypeId($arr[$keys[16]]);
		if (array_key_exists($keys[17], $arr)) $this->setCost($arr[$keys[17]]);
		if (array_key_exists($keys[18], $arr)) $this->setExpectedCost($arr[$keys[18]]);
		if (array_key_exists($keys[19], $arr)) $this->setArTelephonePrefixId($arr[$keys[19]]);
		if (array_key_exists($keys[20], $arr)) $this->setCachedExternalTelephoneNumber($arr[$keys[20]]);
		if (array_key_exists($keys[21], $arr)) $this->setExternalTelephoneNumberWithAppliedPortability($arr[$keys[21]]);
		if (array_key_exists($keys[22], $arr)) $this->setCachedMaskedExternalTelephoneNumber($arr[$keys[22]]);
		if (array_key_exists($keys[23], $arr)) $this->setErrorDestinationType($arr[$keys[23]]);
		if (array_key_exists($keys[24], $arr)) $this->setArProblemDuplicationKey($arr[$keys[24]]);
		if (array_key_exists($keys[25], $arr)) $this->setDebugCostRate($arr[$keys[25]]);
		if (array_key_exists($keys[26], $arr)) $this->setDebugIncomeRate($arr[$keys[26]]);
		if (array_key_exists($keys[27], $arr)) $this->setImportedInfo($arr[$keys[27]]);
		if (array_key_exists($keys[28], $arr)) $this->setExportedInternalTelephoneNumber($arr[$keys[28]]);
		if (array_key_exists($keys[29], $arr)) $this->setExportedBillableCustomerArPartyId($arr[$keys[29]]);
		if (array_key_exists($keys[30], $arr)) $this->setFromSourceCdrId($arr[$keys[30]]);
	}

	/**
	 * Build a Criteria object containing the values of all modified columns in this object.
	 *
	 * @return     Criteria The Criteria object containing all modified values.
	 */
	public function buildCriteria()
	{
		$criteria = new Criteria(ArCdrPeer::DATABASE_NAME);

		if ($this->isColumnModified(ArCdrPeer::CALLDATE)) $criteria->add(ArCdrPeer::CALLDATE, $this->calldate);
		if ($this->isColumnModified(ArCdrPeer::ID)) $criteria->add(ArCdrPeer::ID, $this->id);
		if ($this->isColumnModified(ArCdrPeer::IS_SERVICE_CDR)) $criteria->add(ArCdrPeer::IS_SERVICE_CDR, $this->is_service_cdr);
		if ($this->isColumnModified(ArCdrPeer::TO_CALLDATE)) $criteria->add(ArCdrPeer::TO_CALLDATE, $this->to_calldate);
		if ($this->isColumnModified(ArCdrPeer::COUNT_OF_CALLS)) $criteria->add(ArCdrPeer::COUNT_OF_CALLS, $this->count_of_calls);
		if ($this->isColumnModified(ArCdrPeer::DESTINATION_TYPE)) $criteria->add(ArCdrPeer::DESTINATION_TYPE, $this->destination_type);
		if ($this->isColumnModified(ArCdrPeer::IS_REDIRECT)) $criteria->add(ArCdrPeer::IS_REDIRECT, $this->is_redirect);
		if ($this->isColumnModified(ArCdrPeer::DURATION)) $criteria->add(ArCdrPeer::DURATION, $this->duration);
		if ($this->isColumnModified(ArCdrPeer::BILLSEC)) $criteria->add(ArCdrPeer::BILLSEC, $this->billsec);
		if ($this->isColumnModified(ArCdrPeer::AR_ORGANIZATION_UNIT_ID)) $criteria->add(ArCdrPeer::AR_ORGANIZATION_UNIT_ID, $this->ar_organization_unit_id);
		if ($this->isColumnModified(ArCdrPeer::CACHED_PARENT_ID_HIERARCHY)) $criteria->add(ArCdrPeer::CACHED_PARENT_ID_HIERARCHY, $this->cached_parent_id_hierarchy);
		if ($this->isColumnModified(ArCdrPeer::BILLABLE_AR_ORGANIZATION_UNIT_ID)) $criteria->add(ArCdrPeer::BILLABLE_AR_ORGANIZATION_UNIT_ID, $this->billable_ar_organization_unit_id);
		if ($this->isColumnModified(ArCdrPeer::BUNDLE_AR_ORGANIZATION_UNIT_ID)) $criteria->add(ArCdrPeer::BUNDLE_AR_ORGANIZATION_UNIT_ID, $this->bundle_ar_organization_unit_id);
		if ($this->isColumnModified(ArCdrPeer::INCOME)) $criteria->add(ArCdrPeer::INCOME, $this->income);
		if ($this->isColumnModified(ArCdrPeer::COST_SAVING)) $criteria->add(ArCdrPeer::COST_SAVING, $this->cost_saving);
		if ($this->isColumnModified(ArCdrPeer::AR_VENDOR_ID)) $criteria->add(ArCdrPeer::AR_VENDOR_ID, $this->ar_vendor_id);
		if ($this->isColumnModified(ArCdrPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID)) $criteria->add(ArCdrPeer::AR_COMMUNICATION_CHANNEL_TYPE_ID, $this->ar_communication_channel_type_id);
		if ($this->isColumnModified(ArCdrPeer::COST)) $criteria->add(ArCdrPeer::COST, $this->cost);
		if ($this->isColumnModified(ArCdrPeer::EXPECTED_COST)) $criteria->add(ArCdrPeer::EXPECTED_COST, $this->expected_cost);
		if ($this->isColumnModified(ArCdrPeer::AR_TELEPHONE_PREFIX_ID)) $criteria->add(ArCdrPeer::AR_TELEPHONE_PREFIX_ID, $this->ar_telephone_prefix_id);
		if ($this->isColumnModified(ArCdrPeer::CACHED_EXTERNAL_TELEPHONE_NUMBER)) $criteria->add(ArCdrPeer::CACHED_EXTERNAL_TELEPHONE_NUMBER, $this->cached_external_telephone_number);
		if ($this->isColumnModified(ArCdrPeer::EXTERNAL_TELEPHONE_NUMBER_WITH_APPLIED_PORTABILITY)) $criteria->add(ArCdrPeer::EXTERNAL_TELEPHONE_NUMBER_WITH_APPLIED_PORTABILITY, $this->external_telephone_number_with_applied_portability);
		if ($this->isColumnModified(ArCdrPeer::CACHED_MASKED_EXTERNAL_TELEPHONE_NUMBER)) $criteria->add(ArCdrPeer::CACHED_MASKED_EXTERNAL_TELEPHONE_NUMBER, $this->cached_masked_external_telephone_number);
		if ($this->isColumnModified(ArCdrPeer::ERROR_DESTINATION_TYPE)) $criteria->add(ArCdrPeer::ERROR_DESTINATION_TYPE, $this->error_destination_type);
		if ($this->isColumnModified(ArCdrPeer::AR_PROBLEM_DUPLICATION_KEY)) $criteria->add(ArCdrPeer::AR_PROBLEM_DUPLICATION_KEY, $this->ar_problem_duplication_key);
		if ($this->isColumnModified(ArCdrPeer::DEBUG_COST_RATE)) $criteria->add(ArCdrPeer::DEBUG_COST_RATE, $this->debug_cost_rate);
		if ($this->isColumnModified(ArCdrPeer::DEBUG_INCOME_RATE)) $criteria->add(ArCdrPeer::DEBUG_INCOME_RATE, $this->debug_income_rate);
		if ($this->isColumnModified(ArCdrPeer::IMPORTED_INFO)) $criteria->add(ArCdrPeer::IMPORTED_INFO, $this->imported_info);
		if ($this->isColumnModified(ArCdrPeer::EXPORTED_INTERNAL_TELEPHONE_NUMBER)) $criteria->add(ArCdrPeer::EXPORTED_INTERNAL_TELEPHONE_NUMBER, $this->exported_internal_telephone_number);
		if ($this->isColumnModified(ArCdrPeer::EXPORTED_BILLABLE_CUSTOMER_AR_PARTY_ID)) $criteria->add(ArCdrPeer::EXPORTED_BILLABLE_CUSTOMER_AR_PARTY_ID, $this->exported_billable_customer_ar_party_id);
		if ($this->isColumnModified(ArCdrPeer::FROM_SOURCE_CDR_ID)) $criteria->add(ArCdrPeer::FROM_SOURCE_CDR_ID, $this->from_source_cdr_id);

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
		$criteria = new Criteria(ArCdrPeer::DATABASE_NAME);

		$criteria->add(ArCdrPeer::CALLDATE, $this->calldate);
		$criteria->add(ArCdrPeer::ID, $this->id);
		$criteria->add(ArCdrPeer::IS_SERVICE_CDR, $this->is_service_cdr);

		return $criteria;
	}

	/**
	 * Returns the composite primary key for this object.
	 * The array elements will be in same order as specified in XML.
	 * @return     array
	 */
	public function getPrimaryKey()
	{
		$pks = array();

		$pks[0] = $this->getCalldate();

		$pks[1] = $this->getId();

		$pks[2] = $this->getIsServiceCdr();

		return $pks;
	}

	/**
	 * Set the [composite] primary key.
	 *
	 * @param      array $keys The elements of the composite key (order must match the order in XML file).
	 * @return     void
	 */
	public function setPrimaryKey($keys)
	{

		$this->setCalldate($keys[0]);

		$this->setId($keys[1]);

		$this->setIsServiceCdr($keys[2]);

	}

	/**
	 * Sets contents of passed object to values from current object.
	 *
	 * If desired, this method can also make copies of all associated (fkey referrers)
	 * objects.
	 *
	 * @param      object $copyObj An object of ArCdr (or compatible) type.
	 * @param      boolean $deepCopy Whether to also copy all rows that refer (by fkey) to the current row.
	 * @throws     PropelException
	 */
	public function copyInto($copyObj, $deepCopy = false)
	{

		$copyObj->setCalldate($this->calldate);

		$copyObj->setId($this->id);

		$copyObj->setIsServiceCdr($this->is_service_cdr);

		$copyObj->setToCalldate($this->to_calldate);

		$copyObj->setCountOfCalls($this->count_of_calls);

		$copyObj->setDestinationType($this->destination_type);

		$copyObj->setIsRedirect($this->is_redirect);

		$copyObj->setDuration($this->duration);

		$copyObj->setBillsec($this->billsec);

		$copyObj->setArOrganizationUnitId($this->ar_organization_unit_id);

		$copyObj->setCachedParentIdHierarchy($this->cached_parent_id_hierarchy);

		$copyObj->setBillableArOrganizationUnitId($this->billable_ar_organization_unit_id);

		$copyObj->setBundleArOrganizationUnitId($this->bundle_ar_organization_unit_id);

		$copyObj->setIncome($this->income);

		$copyObj->setCostSaving($this->cost_saving);

		$copyObj->setArVendorId($this->ar_vendor_id);

		$copyObj->setArCommunicationChannelTypeId($this->ar_communication_channel_type_id);

		$copyObj->setCost($this->cost);

		$copyObj->setExpectedCost($this->expected_cost);

		$copyObj->setArTelephonePrefixId($this->ar_telephone_prefix_id);

		$copyObj->setCachedExternalTelephoneNumber($this->cached_external_telephone_number);

		$copyObj->setExternalTelephoneNumberWithAppliedPortability($this->external_telephone_number_with_applied_portability);

		$copyObj->setCachedMaskedExternalTelephoneNumber($this->cached_masked_external_telephone_number);

		$copyObj->setErrorDestinationType($this->error_destination_type);

		$copyObj->setArProblemDuplicationKey($this->ar_problem_duplication_key);

		$copyObj->setDebugCostRate($this->debug_cost_rate);

		$copyObj->setDebugIncomeRate($this->debug_income_rate);

		$copyObj->setImportedInfo($this->imported_info);

		$copyObj->setExportedInternalTelephoneNumber($this->exported_internal_telephone_number);

		$copyObj->setExportedBillableCustomerArPartyId($this->exported_billable_customer_ar_party_id);

		$copyObj->setFromSourceCdrId($this->from_source_cdr_id);


		$copyObj->setNew(true);

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
	 * @return     ArCdr Clone of current object.
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
	 * @return     ArCdrPeer
	 */
	public function getPeer()
	{
		if (self::$peer === null) {
			self::$peer = new ArCdrPeer();
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

} // BaseArCdr
