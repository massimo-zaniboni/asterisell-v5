<?php

/**
 * Base class that represents a row from the 'ar_party' table.
 *
 * 
 *
 * @package    lib.model.om
 */
abstract class BaseArParty extends BaseObject  implements Persistent {


	/**
	 * The Peer class.
	 * Instance provides a convenient way of calling static methods on a class
	 * that calling code may not be able to identify.
	 * @var        ArPartyPeer
	 */
	protected static $peer;

	/**
	 * The value for the id field.
	 * @var        int
	 */
	protected $id;

	/**
	 * The value for the name field.
	 * @var        string
	 */
	protected $name;

	/**
	 * The value for the compact_name field.
	 * @var        string
	 */
	protected $compact_name;

	/**
	 * The value for the external_crm_code field.
	 * @var        string
	 */
	protected $external_crm_code;

	/**
	 * The value for the vat field.
	 * @var        string
	 */
	protected $vat;

	/**
	 * The value for the is_billable field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $is_billable;

	/**
	 * The value for the legal_address field.
	 * @var        string
	 */
	protected $legal_address;

	/**
	 * The value for the legal_city field.
	 * @var        string
	 */
	protected $legal_city;

	/**
	 * The value for the legal_zipcode field.
	 * @var        string
	 */
	protected $legal_zipcode;

	/**
	 * The value for the legal_state_province field.
	 * @var        string
	 */
	protected $legal_state_province;

	/**
	 * The value for the legal_country field.
	 * @var        string
	 */
	protected $legal_country;

	/**
	 * The value for the email field.
	 * @var        string
	 */
	protected $email;

	/**
	 * The value for the phone field.
	 * @var        string
	 */
	protected $phone;

	/**
	 * The value for the phone2 field.
	 * @var        string
	 */
	protected $phone2;

	/**
	 * The value for the fax field.
	 * @var        string
	 */
	protected $fax;

	/**
	 * The value for the max_limit_30 field.
	 * @var        int
	 */
	protected $max_limit_30;

	/**
	 * The value for the last_email_advise_for_max_limit_30 field.
	 * @var        string
	 */
	protected $last_email_advise_for_max_limit_30;

	/**
	 * The value for the is_active field.
	 * Note: this column has a database default value of: true
	 * @var        boolean
	 */
	protected $is_active;

	/**
	 * The value for the ar_reseller_id field.
	 * @var        int
	 */
	protected $ar_reseller_id;

	/**
	 * The value for the migration_field_for_telephone field.
	 * @var        string
	 */
	protected $migration_field_for_telephone;

	/**
	 * The value for the migration_field_for_adsl field.
	 * @var        string
	 */
	protected $migration_field_for_adsl;

	/**
	 * The value for the payment_iban field.
	 * @var        string
	 */
	protected $payment_iban;

	/**
	 * The value for the payment_bic field.
	 * @var        string
	 */
	protected $payment_bic;

	/**
	 * The value for the payment_sepa field.
	 * @var        string
	 */
	protected $payment_sepa;

	/**
	 * The value for the payment_info field.
	 * @var        string
	 */
	protected $payment_info;

	/**
	 * @var        ArReseller
	 */
	protected $aArReseller;

	/**
	 * @var        array ArPartyHasTag[] Collection to store aggregation of ArPartyHasTag objects.
	 */
	protected $collArPartyHasTags;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArPartyHasTags.
	 */
	private $lastArPartyHasTagCriteria = null;

	/**
	 * @var        array ArOrganizationUnitHasStructure[] Collection to store aggregation of ArOrganizationUnitHasStructure objects.
	 */
	protected $collArOrganizationUnitHasStructures;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArOrganizationUnitHasStructures.
	 */
	private $lastArOrganizationUnitHasStructureCriteria = null;

	/**
	 * @var        array ArVendor[] Collection to store aggregation of ArVendor objects.
	 */
	protected $collArVendors;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArVendors.
	 */
	private $lastArVendorCriteria = null;

	/**
	 * @var        array ArUser[] Collection to store aggregation of ArUser objects.
	 */
	protected $collArUsers;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArUsers.
	 */
	private $lastArUserCriteria = null;

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
	
	const PEER = 'ArPartyPeer';

	/**
	 * Applies default values to this object.
	 * This method should be called from the object's constructor (or
	 * equivalent initialization method).
	 * @see        __construct()
	 */
	public function applyDefaultValues()
	{
		$this->is_billable = false;
		$this->is_active = true;
	}

	/**
	 * Initializes internal state of BaseArParty object.
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
	 * Get the [name] column value.
	 * 
	 * @return     string
	 */
	public function getName()
	{
		return $this->name;
	}

	/**
	 * Get the [compact_name] column value.
	 * 
	 * @return     string
	 */
	public function getCompactName()
	{
		return $this->compact_name;
	}

	/**
	 * Get the [external_crm_code] column value.
	 * 
	 * @return     string
	 */
	public function getExternalCrmCode()
	{
		return $this->external_crm_code;
	}

	/**
	 * Get the [vat] column value.
	 * 
	 * @return     string
	 */
	public function getVat()
	{
		return $this->vat;
	}

	/**
	 * Get the [is_billable] column value.
	 * 
	 * @return     boolean
	 */
	public function getIsBillable()
	{
		return $this->is_billable;
	}

	/**
	 * Get the [legal_address] column value.
	 * 
	 * @return     string
	 */
	public function getLegalAddress()
	{
		return $this->legal_address;
	}

	/**
	 * Get the [legal_city] column value.
	 * 
	 * @return     string
	 */
	public function getLegalCity()
	{
		return $this->legal_city;
	}

	/**
	 * Get the [legal_zipcode] column value.
	 * 
	 * @return     string
	 */
	public function getLegalZipcode()
	{
		return $this->legal_zipcode;
	}

	/**
	 * Get the [legal_state_province] column value.
	 * 
	 * @return     string
	 */
	public function getLegalStateProvince()
	{
		return $this->legal_state_province;
	}

	/**
	 * Get the [legal_country] column value.
	 * 
	 * @return     string
	 */
	public function getLegalCountry()
	{
		return $this->legal_country;
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
	 * Get the [phone] column value.
	 * 
	 * @return     string
	 */
	public function getPhone()
	{
		return $this->phone;
	}

	/**
	 * Get the [phone2] column value.
	 * 
	 * @return     string
	 */
	public function getPhone2()
	{
		return $this->phone2;
	}

	/**
	 * Get the [fax] column value.
	 * 
	 * @return     string
	 */
	public function getFax()
	{
		return $this->fax;
	}

	/**
	 * Get the [max_limit_30] column value.
	 * 
	 * @return     int
	 */
	public function getMaxLimit30()
	{
		return $this->max_limit_30;
	}

	/**
	 * Get the [optionally formatted] temporal [last_email_advise_for_max_limit_30] column value.
	 * 
	 *
	 * @param      string $format The date/time format string (either date()-style or strftime()-style).
	 *							If format is NULL, then the raw DateTime object will be returned.
	 * @return     mixed Formatted date/time value as string or DateTime object (if format is NULL), NULL if column is NULL, and 0 if column value is 0000-00-00 00:00:00
	 * @throws     PropelException - if unable to parse/validate the date/time value.
	 */
	public function getLastEmailAdviseForMaxLimit30($format = 'Y-m-d H:i:s')
	{
		if ($this->last_email_advise_for_max_limit_30 === null) {
			return null;
		}


		if ($this->last_email_advise_for_max_limit_30 === '0000-00-00 00:00:00') {
			// while technically this is not a default value of NULL,
			// this seems to be closest in meaning.
			return null;
		} else {
			try {
				$dt = new DateTime($this->last_email_advise_for_max_limit_30);
			} catch (Exception $x) {
				throw new PropelException("Internally stored date/time/timestamp value could not be converted to DateTime: " . var_export($this->last_email_advise_for_max_limit_30, true), $x);
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
	 * Get the [is_active] column value.
	 * 
	 * @return     boolean
	 */
	public function getIsActive()
	{
		return $this->is_active;
	}

	/**
	 * Get the [ar_reseller_id] column value.
	 * 
	 * @return     int
	 */
	public function getArResellerId()
	{
		return $this->ar_reseller_id;
	}

	/**
	 * Get the [migration_field_for_telephone] column value.
	 * 
	 * @return     string
	 */
	public function getMigrationFieldForTelephone()
	{
		return $this->migration_field_for_telephone;
	}

	/**
	 * Get the [migration_field_for_adsl] column value.
	 * 
	 * @return     string
	 */
	public function getMigrationFieldForAdsl()
	{
		return $this->migration_field_for_adsl;
	}

	/**
	 * Get the [payment_iban] column value.
	 * 
	 * @return     string
	 */
	public function getPaymentIban()
	{
		return $this->payment_iban;
	}

	/**
	 * Get the [payment_bic] column value.
	 * 
	 * @return     string
	 */
	public function getPaymentBic()
	{
		return $this->payment_bic;
	}

	/**
	 * Get the [payment_sepa] column value.
	 * 
	 * @return     string
	 */
	public function getPaymentSepa()
	{
		return $this->payment_sepa;
	}

	/**
	 * Get the [payment_info] column value.
	 * 
	 * @return     string
	 */
	public function getPaymentInfo()
	{
		return $this->payment_info;
	}

	/**
	 * Set the value of [id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArParty The current object (for fluent API support)
	 */
	public function setId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->id !== $v) {
			$this->id = $v;
			$this->modifiedColumns[] = ArPartyPeer::ID;
		}

		return $this;
	} // setId()

	/**
	 * Set the value of [name] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParty The current object (for fluent API support)
	 */
	public function setName($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->name !== $v) {
			$this->name = $v;
			$this->modifiedColumns[] = ArPartyPeer::NAME;
		}

		return $this;
	} // setName()

	/**
	 * Set the value of [compact_name] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParty The current object (for fluent API support)
	 */
	public function setCompactName($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->compact_name !== $v) {
			$this->compact_name = $v;
			$this->modifiedColumns[] = ArPartyPeer::COMPACT_NAME;
		}

		return $this;
	} // setCompactName()

	/**
	 * Set the value of [external_crm_code] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParty The current object (for fluent API support)
	 */
	public function setExternalCrmCode($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->external_crm_code !== $v) {
			$this->external_crm_code = $v;
			$this->modifiedColumns[] = ArPartyPeer::EXTERNAL_CRM_CODE;
		}

		return $this;
	} // setExternalCrmCode()

	/**
	 * Set the value of [vat] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParty The current object (for fluent API support)
	 */
	public function setVat($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->vat !== $v) {
			$this->vat = $v;
			$this->modifiedColumns[] = ArPartyPeer::VAT;
		}

		return $this;
	} // setVat()

	/**
	 * Set the value of [is_billable] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArParty The current object (for fluent API support)
	 */
	public function setIsBillable($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->is_billable !== $v || $this->isNew()) {
			$this->is_billable = $v;
			$this->modifiedColumns[] = ArPartyPeer::IS_BILLABLE;
		}

		return $this;
	} // setIsBillable()

	/**
	 * Set the value of [legal_address] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParty The current object (for fluent API support)
	 */
	public function setLegalAddress($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->legal_address !== $v) {
			$this->legal_address = $v;
			$this->modifiedColumns[] = ArPartyPeer::LEGAL_ADDRESS;
		}

		return $this;
	} // setLegalAddress()

	/**
	 * Set the value of [legal_city] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParty The current object (for fluent API support)
	 */
	public function setLegalCity($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->legal_city !== $v) {
			$this->legal_city = $v;
			$this->modifiedColumns[] = ArPartyPeer::LEGAL_CITY;
		}

		return $this;
	} // setLegalCity()

	/**
	 * Set the value of [legal_zipcode] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParty The current object (for fluent API support)
	 */
	public function setLegalZipcode($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->legal_zipcode !== $v) {
			$this->legal_zipcode = $v;
			$this->modifiedColumns[] = ArPartyPeer::LEGAL_ZIPCODE;
		}

		return $this;
	} // setLegalZipcode()

	/**
	 * Set the value of [legal_state_province] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParty The current object (for fluent API support)
	 */
	public function setLegalStateProvince($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->legal_state_province !== $v) {
			$this->legal_state_province = $v;
			$this->modifiedColumns[] = ArPartyPeer::LEGAL_STATE_PROVINCE;
		}

		return $this;
	} // setLegalStateProvince()

	/**
	 * Set the value of [legal_country] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParty The current object (for fluent API support)
	 */
	public function setLegalCountry($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->legal_country !== $v) {
			$this->legal_country = $v;
			$this->modifiedColumns[] = ArPartyPeer::LEGAL_COUNTRY;
		}

		return $this;
	} // setLegalCountry()

	/**
	 * Set the value of [email] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParty The current object (for fluent API support)
	 */
	public function setEmail($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->email !== $v) {
			$this->email = $v;
			$this->modifiedColumns[] = ArPartyPeer::EMAIL;
		}

		return $this;
	} // setEmail()

	/**
	 * Set the value of [phone] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParty The current object (for fluent API support)
	 */
	public function setPhone($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->phone !== $v) {
			$this->phone = $v;
			$this->modifiedColumns[] = ArPartyPeer::PHONE;
		}

		return $this;
	} // setPhone()

	/**
	 * Set the value of [phone2] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParty The current object (for fluent API support)
	 */
	public function setPhone2($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->phone2 !== $v) {
			$this->phone2 = $v;
			$this->modifiedColumns[] = ArPartyPeer::PHONE2;
		}

		return $this;
	} // setPhone2()

	/**
	 * Set the value of [fax] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParty The current object (for fluent API support)
	 */
	public function setFax($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->fax !== $v) {
			$this->fax = $v;
			$this->modifiedColumns[] = ArPartyPeer::FAX;
		}

		return $this;
	} // setFax()

	/**
	 * Set the value of [max_limit_30] column.
	 * 
	 * @param      int $v new value
	 * @return     ArParty The current object (for fluent API support)
	 */
	public function setMaxLimit30($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->max_limit_30 !== $v) {
			$this->max_limit_30 = $v;
			$this->modifiedColumns[] = ArPartyPeer::MAX_LIMIT_30;
		}

		return $this;
	} // setMaxLimit30()

	/**
	 * Sets the value of [last_email_advise_for_max_limit_30] column to a normalized version of the date/time value specified.
	 * 
	 * @param      mixed $v string, integer (timestamp), or DateTime value.  Empty string will
	 *						be treated as NULL for temporal objects.
	 * @return     ArParty The current object (for fluent API support)
	 */
	public function setLastEmailAdviseForMaxLimit30($v)
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

		if ( $this->last_email_advise_for_max_limit_30 !== null || $dt !== null ) {
			// (nested ifs are a little easier to read in this case)

			$currNorm = ($this->last_email_advise_for_max_limit_30 !== null && $tmpDt = new DateTime($this->last_email_advise_for_max_limit_30)) ? $tmpDt->format('Y-m-d H:i:s') : null;
			$newNorm = ($dt !== null) ? $dt->format('Y-m-d H:i:s') : null;

			if ( ($currNorm !== $newNorm) // normalized values don't match 
					)
			{
				$this->last_email_advise_for_max_limit_30 = ($dt ? $dt->format('Y-m-d H:i:s') : null);
				$this->modifiedColumns[] = ArPartyPeer::LAST_EMAIL_ADVISE_FOR_MAX_LIMIT_30;
			}
		} // if either are not null

		return $this;
	} // setLastEmailAdviseForMaxLimit30()

	/**
	 * Set the value of [is_active] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArParty The current object (for fluent API support)
	 */
	public function setIsActive($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->is_active !== $v || $this->isNew()) {
			$this->is_active = $v;
			$this->modifiedColumns[] = ArPartyPeer::IS_ACTIVE;
		}

		return $this;
	} // setIsActive()

	/**
	 * Set the value of [ar_reseller_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArParty The current object (for fluent API support)
	 */
	public function setArResellerId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->ar_reseller_id !== $v) {
			$this->ar_reseller_id = $v;
			$this->modifiedColumns[] = ArPartyPeer::AR_RESELLER_ID;
		}

		if ($this->aArReseller !== null && $this->aArReseller->getId() !== $v) {
			$this->aArReseller = null;
		}

		return $this;
	} // setArResellerId()

	/**
	 * Set the value of [migration_field_for_telephone] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParty The current object (for fluent API support)
	 */
	public function setMigrationFieldForTelephone($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->migration_field_for_telephone !== $v) {
			$this->migration_field_for_telephone = $v;
			$this->modifiedColumns[] = ArPartyPeer::MIGRATION_FIELD_FOR_TELEPHONE;
		}

		return $this;
	} // setMigrationFieldForTelephone()

	/**
	 * Set the value of [migration_field_for_adsl] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParty The current object (for fluent API support)
	 */
	public function setMigrationFieldForAdsl($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->migration_field_for_adsl !== $v) {
			$this->migration_field_for_adsl = $v;
			$this->modifiedColumns[] = ArPartyPeer::MIGRATION_FIELD_FOR_ADSL;
		}

		return $this;
	} // setMigrationFieldForAdsl()

	/**
	 * Set the value of [payment_iban] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParty The current object (for fluent API support)
	 */
	public function setPaymentIban($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->payment_iban !== $v) {
			$this->payment_iban = $v;
			$this->modifiedColumns[] = ArPartyPeer::PAYMENT_IBAN;
		}

		return $this;
	} // setPaymentIban()

	/**
	 * Set the value of [payment_bic] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParty The current object (for fluent API support)
	 */
	public function setPaymentBic($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->payment_bic !== $v) {
			$this->payment_bic = $v;
			$this->modifiedColumns[] = ArPartyPeer::PAYMENT_BIC;
		}

		return $this;
	} // setPaymentBic()

	/**
	 * Set the value of [payment_sepa] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParty The current object (for fluent API support)
	 */
	public function setPaymentSepa($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->payment_sepa !== $v) {
			$this->payment_sepa = $v;
			$this->modifiedColumns[] = ArPartyPeer::PAYMENT_SEPA;
		}

		return $this;
	} // setPaymentSepa()

	/**
	 * Set the value of [payment_info] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParty The current object (for fluent API support)
	 */
	public function setPaymentInfo($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->payment_info !== $v) {
			$this->payment_info = $v;
			$this->modifiedColumns[] = ArPartyPeer::PAYMENT_INFO;
		}

		return $this;
	} // setPaymentInfo()

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
			if ($this->is_billable !== false) {
				return false;
			}

			if ($this->is_active !== true) {
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
			$this->name = ($row[$startcol + 1] !== null) ? (string) $row[$startcol + 1] : null;
			$this->compact_name = ($row[$startcol + 2] !== null) ? (string) $row[$startcol + 2] : null;
			$this->external_crm_code = ($row[$startcol + 3] !== null) ? (string) $row[$startcol + 3] : null;
			$this->vat = ($row[$startcol + 4] !== null) ? (string) $row[$startcol + 4] : null;
			$this->is_billable = ($row[$startcol + 5] !== null) ? (boolean) $row[$startcol + 5] : null;
			$this->legal_address = ($row[$startcol + 6] !== null) ? (string) $row[$startcol + 6] : null;
			$this->legal_city = ($row[$startcol + 7] !== null) ? (string) $row[$startcol + 7] : null;
			$this->legal_zipcode = ($row[$startcol + 8] !== null) ? (string) $row[$startcol + 8] : null;
			$this->legal_state_province = ($row[$startcol + 9] !== null) ? (string) $row[$startcol + 9] : null;
			$this->legal_country = ($row[$startcol + 10] !== null) ? (string) $row[$startcol + 10] : null;
			$this->email = ($row[$startcol + 11] !== null) ? (string) $row[$startcol + 11] : null;
			$this->phone = ($row[$startcol + 12] !== null) ? (string) $row[$startcol + 12] : null;
			$this->phone2 = ($row[$startcol + 13] !== null) ? (string) $row[$startcol + 13] : null;
			$this->fax = ($row[$startcol + 14] !== null) ? (string) $row[$startcol + 14] : null;
			$this->max_limit_30 = ($row[$startcol + 15] !== null) ? (int) $row[$startcol + 15] : null;
			$this->last_email_advise_for_max_limit_30 = ($row[$startcol + 16] !== null) ? (string) $row[$startcol + 16] : null;
			$this->is_active = ($row[$startcol + 17] !== null) ? (boolean) $row[$startcol + 17] : null;
			$this->ar_reseller_id = ($row[$startcol + 18] !== null) ? (int) $row[$startcol + 18] : null;
			$this->migration_field_for_telephone = ($row[$startcol + 19] !== null) ? (string) $row[$startcol + 19] : null;
			$this->migration_field_for_adsl = ($row[$startcol + 20] !== null) ? (string) $row[$startcol + 20] : null;
			$this->payment_iban = ($row[$startcol + 21] !== null) ? (string) $row[$startcol + 21] : null;
			$this->payment_bic = ($row[$startcol + 22] !== null) ? (string) $row[$startcol + 22] : null;
			$this->payment_sepa = ($row[$startcol + 23] !== null) ? (string) $row[$startcol + 23] : null;
			$this->payment_info = ($row[$startcol + 24] !== null) ? (string) $row[$startcol + 24] : null;
			$this->resetModified();

			$this->setNew(false);

			if ($rehydrate) {
				$this->ensureConsistency();
			}

			// FIXME - using NUM_COLUMNS may be clearer.
			return $startcol + 25; // 25 = ArPartyPeer::NUM_COLUMNS - ArPartyPeer::NUM_LAZY_LOAD_COLUMNS).

		} catch (Exception $e) {
			throw new PropelException("Error populating ArParty object", $e);
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

		if ($this->aArReseller !== null && $this->ar_reseller_id !== $this->aArReseller->getId()) {
			$this->aArReseller = null;
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
			$con = Propel::getConnection(ArPartyPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		// We don't need to alter the object instance pool; we're just modifying this instance
		// already in the pool.

		$stmt = ArPartyPeer::doSelectStmt($this->buildPkeyCriteria(), $con);
		$row = $stmt->fetch(PDO::FETCH_NUM);
		$stmt->closeCursor();
		if (!$row) {
			throw new PropelException('Cannot find matching row in the database to reload object values.');
		}
		$this->hydrate($row, 0, true); // rehydrate

		if ($deep) {  // also de-associate any related objects?

			$this->aArReseller = null;
			$this->collArPartyHasTags = null;
			$this->lastArPartyHasTagCriteria = null;

			$this->collArOrganizationUnitHasStructures = null;
			$this->lastArOrganizationUnitHasStructureCriteria = null;

			$this->collArVendors = null;
			$this->lastArVendorCriteria = null;

			$this->collArUsers = null;
			$this->lastArUserCriteria = null;

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
			$con = Propel::getConnection(ArPartyPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}
		
		$con->beginTransaction();
		try {
			$ret = $this->preDelete($con);
			if ($ret) {
				ArPartyPeer::doDelete($this, $con);
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
			$con = Propel::getConnection(ArPartyPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
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
				ArPartyPeer::addInstanceToPool($this);
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

			if ($this->aArReseller !== null) {
				if ($this->aArReseller->isModified() || $this->aArReseller->isNew()) {
					$affectedRows += $this->aArReseller->save($con);
				}
				$this->setArReseller($this->aArReseller);
			}

			if ($this->isNew() ) {
				$this->modifiedColumns[] = ArPartyPeer::ID;
			}

			// If this object has been modified, then save it to the database.
			if ($this->isModified()) {
				if ($this->isNew()) {
					$pk = ArPartyPeer::doInsert($this, $con);
					$affectedRows += 1; // we are assuming that there is only 1 row per doInsert() which
										 // should always be true here (even though technically
										 // BasePeer::doInsert() can insert multiple rows).

					$this->setId($pk);  //[IMV] update autoincrement primary key

					$this->setNew(false);
				} else {
					$affectedRows += ArPartyPeer::doUpdate($this, $con);
				}

				$this->resetModified(); // [HL] After being saved an object is no longer 'modified'
			}

			if ($this->collArPartyHasTags !== null) {
				foreach ($this->collArPartyHasTags as $referrerFK) {
					if (!$referrerFK->isDeleted()) {
						$affectedRows += $referrerFK->save($con);
					}
				}
			}

			if ($this->collArOrganizationUnitHasStructures !== null) {
				foreach ($this->collArOrganizationUnitHasStructures as $referrerFK) {
					if (!$referrerFK->isDeleted()) {
						$affectedRows += $referrerFK->save($con);
					}
				}
			}

			if ($this->collArVendors !== null) {
				foreach ($this->collArVendors as $referrerFK) {
					if (!$referrerFK->isDeleted()) {
						$affectedRows += $referrerFK->save($con);
					}
				}
			}

			if ($this->collArUsers !== null) {
				foreach ($this->collArUsers as $referrerFK) {
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

			if ($this->aArReseller !== null) {
				if (!$this->aArReseller->validate($columns)) {
					$failureMap = array_merge($failureMap, $this->aArReseller->getValidationFailures());
				}
			}


			if (($retval = ArPartyPeer::doValidate($this, $columns)) !== true) {
				$failureMap = array_merge($failureMap, $retval);
			}


				if ($this->collArPartyHasTags !== null) {
					foreach ($this->collArPartyHasTags as $referrerFK) {
						if (!$referrerFK->validate($columns)) {
							$failureMap = array_merge($failureMap, $referrerFK->getValidationFailures());
						}
					}
				}

				if ($this->collArOrganizationUnitHasStructures !== null) {
					foreach ($this->collArOrganizationUnitHasStructures as $referrerFK) {
						if (!$referrerFK->validate($columns)) {
							$failureMap = array_merge($failureMap, $referrerFK->getValidationFailures());
						}
					}
				}

				if ($this->collArVendors !== null) {
					foreach ($this->collArVendors as $referrerFK) {
						if (!$referrerFK->validate($columns)) {
							$failureMap = array_merge($failureMap, $referrerFK->getValidationFailures());
						}
					}
				}

				if ($this->collArUsers !== null) {
					foreach ($this->collArUsers as $referrerFK) {
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
		$pos = ArPartyPeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
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
				return $this->getName();
				break;
			case 2:
				return $this->getCompactName();
				break;
			case 3:
				return $this->getExternalCrmCode();
				break;
			case 4:
				return $this->getVat();
				break;
			case 5:
				return $this->getIsBillable();
				break;
			case 6:
				return $this->getLegalAddress();
				break;
			case 7:
				return $this->getLegalCity();
				break;
			case 8:
				return $this->getLegalZipcode();
				break;
			case 9:
				return $this->getLegalStateProvince();
				break;
			case 10:
				return $this->getLegalCountry();
				break;
			case 11:
				return $this->getEmail();
				break;
			case 12:
				return $this->getPhone();
				break;
			case 13:
				return $this->getPhone2();
				break;
			case 14:
				return $this->getFax();
				break;
			case 15:
				return $this->getMaxLimit30();
				break;
			case 16:
				return $this->getLastEmailAdviseForMaxLimit30();
				break;
			case 17:
				return $this->getIsActive();
				break;
			case 18:
				return $this->getArResellerId();
				break;
			case 19:
				return $this->getMigrationFieldForTelephone();
				break;
			case 20:
				return $this->getMigrationFieldForAdsl();
				break;
			case 21:
				return $this->getPaymentIban();
				break;
			case 22:
				return $this->getPaymentBic();
				break;
			case 23:
				return $this->getPaymentSepa();
				break;
			case 24:
				return $this->getPaymentInfo();
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
		$keys = ArPartyPeer::getFieldNames($keyType);
		$result = array(
			$keys[0] => $this->getId(),
			$keys[1] => $this->getName(),
			$keys[2] => $this->getCompactName(),
			$keys[3] => $this->getExternalCrmCode(),
			$keys[4] => $this->getVat(),
			$keys[5] => $this->getIsBillable(),
			$keys[6] => $this->getLegalAddress(),
			$keys[7] => $this->getLegalCity(),
			$keys[8] => $this->getLegalZipcode(),
			$keys[9] => $this->getLegalStateProvince(),
			$keys[10] => $this->getLegalCountry(),
			$keys[11] => $this->getEmail(),
			$keys[12] => $this->getPhone(),
			$keys[13] => $this->getPhone2(),
			$keys[14] => $this->getFax(),
			$keys[15] => $this->getMaxLimit30(),
			$keys[16] => $this->getLastEmailAdviseForMaxLimit30(),
			$keys[17] => $this->getIsActive(),
			$keys[18] => $this->getArResellerId(),
			$keys[19] => $this->getMigrationFieldForTelephone(),
			$keys[20] => $this->getMigrationFieldForAdsl(),
			$keys[21] => $this->getPaymentIban(),
			$keys[22] => $this->getPaymentBic(),
			$keys[23] => $this->getPaymentSepa(),
			$keys[24] => $this->getPaymentInfo(),
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
		$pos = ArPartyPeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
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
				$this->setName($value);
				break;
			case 2:
				$this->setCompactName($value);
				break;
			case 3:
				$this->setExternalCrmCode($value);
				break;
			case 4:
				$this->setVat($value);
				break;
			case 5:
				$this->setIsBillable($value);
				break;
			case 6:
				$this->setLegalAddress($value);
				break;
			case 7:
				$this->setLegalCity($value);
				break;
			case 8:
				$this->setLegalZipcode($value);
				break;
			case 9:
				$this->setLegalStateProvince($value);
				break;
			case 10:
				$this->setLegalCountry($value);
				break;
			case 11:
				$this->setEmail($value);
				break;
			case 12:
				$this->setPhone($value);
				break;
			case 13:
				$this->setPhone2($value);
				break;
			case 14:
				$this->setFax($value);
				break;
			case 15:
				$this->setMaxLimit30($value);
				break;
			case 16:
				$this->setLastEmailAdviseForMaxLimit30($value);
				break;
			case 17:
				$this->setIsActive($value);
				break;
			case 18:
				$this->setArResellerId($value);
				break;
			case 19:
				$this->setMigrationFieldForTelephone($value);
				break;
			case 20:
				$this->setMigrationFieldForAdsl($value);
				break;
			case 21:
				$this->setPaymentIban($value);
				break;
			case 22:
				$this->setPaymentBic($value);
				break;
			case 23:
				$this->setPaymentSepa($value);
				break;
			case 24:
				$this->setPaymentInfo($value);
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
		$keys = ArPartyPeer::getFieldNames($keyType);

		if (array_key_exists($keys[0], $arr)) $this->setId($arr[$keys[0]]);
		if (array_key_exists($keys[1], $arr)) $this->setName($arr[$keys[1]]);
		if (array_key_exists($keys[2], $arr)) $this->setCompactName($arr[$keys[2]]);
		if (array_key_exists($keys[3], $arr)) $this->setExternalCrmCode($arr[$keys[3]]);
		if (array_key_exists($keys[4], $arr)) $this->setVat($arr[$keys[4]]);
		if (array_key_exists($keys[5], $arr)) $this->setIsBillable($arr[$keys[5]]);
		if (array_key_exists($keys[6], $arr)) $this->setLegalAddress($arr[$keys[6]]);
		if (array_key_exists($keys[7], $arr)) $this->setLegalCity($arr[$keys[7]]);
		if (array_key_exists($keys[8], $arr)) $this->setLegalZipcode($arr[$keys[8]]);
		if (array_key_exists($keys[9], $arr)) $this->setLegalStateProvince($arr[$keys[9]]);
		if (array_key_exists($keys[10], $arr)) $this->setLegalCountry($arr[$keys[10]]);
		if (array_key_exists($keys[11], $arr)) $this->setEmail($arr[$keys[11]]);
		if (array_key_exists($keys[12], $arr)) $this->setPhone($arr[$keys[12]]);
		if (array_key_exists($keys[13], $arr)) $this->setPhone2($arr[$keys[13]]);
		if (array_key_exists($keys[14], $arr)) $this->setFax($arr[$keys[14]]);
		if (array_key_exists($keys[15], $arr)) $this->setMaxLimit30($arr[$keys[15]]);
		if (array_key_exists($keys[16], $arr)) $this->setLastEmailAdviseForMaxLimit30($arr[$keys[16]]);
		if (array_key_exists($keys[17], $arr)) $this->setIsActive($arr[$keys[17]]);
		if (array_key_exists($keys[18], $arr)) $this->setArResellerId($arr[$keys[18]]);
		if (array_key_exists($keys[19], $arr)) $this->setMigrationFieldForTelephone($arr[$keys[19]]);
		if (array_key_exists($keys[20], $arr)) $this->setMigrationFieldForAdsl($arr[$keys[20]]);
		if (array_key_exists($keys[21], $arr)) $this->setPaymentIban($arr[$keys[21]]);
		if (array_key_exists($keys[22], $arr)) $this->setPaymentBic($arr[$keys[22]]);
		if (array_key_exists($keys[23], $arr)) $this->setPaymentSepa($arr[$keys[23]]);
		if (array_key_exists($keys[24], $arr)) $this->setPaymentInfo($arr[$keys[24]]);
	}

	/**
	 * Build a Criteria object containing the values of all modified columns in this object.
	 *
	 * @return     Criteria The Criteria object containing all modified values.
	 */
	public function buildCriteria()
	{
		$criteria = new Criteria(ArPartyPeer::DATABASE_NAME);

		if ($this->isColumnModified(ArPartyPeer::ID)) $criteria->add(ArPartyPeer::ID, $this->id);
		if ($this->isColumnModified(ArPartyPeer::NAME)) $criteria->add(ArPartyPeer::NAME, $this->name);
		if ($this->isColumnModified(ArPartyPeer::COMPACT_NAME)) $criteria->add(ArPartyPeer::COMPACT_NAME, $this->compact_name);
		if ($this->isColumnModified(ArPartyPeer::EXTERNAL_CRM_CODE)) $criteria->add(ArPartyPeer::EXTERNAL_CRM_CODE, $this->external_crm_code);
		if ($this->isColumnModified(ArPartyPeer::VAT)) $criteria->add(ArPartyPeer::VAT, $this->vat);
		if ($this->isColumnModified(ArPartyPeer::IS_BILLABLE)) $criteria->add(ArPartyPeer::IS_BILLABLE, $this->is_billable);
		if ($this->isColumnModified(ArPartyPeer::LEGAL_ADDRESS)) $criteria->add(ArPartyPeer::LEGAL_ADDRESS, $this->legal_address);
		if ($this->isColumnModified(ArPartyPeer::LEGAL_CITY)) $criteria->add(ArPartyPeer::LEGAL_CITY, $this->legal_city);
		if ($this->isColumnModified(ArPartyPeer::LEGAL_ZIPCODE)) $criteria->add(ArPartyPeer::LEGAL_ZIPCODE, $this->legal_zipcode);
		if ($this->isColumnModified(ArPartyPeer::LEGAL_STATE_PROVINCE)) $criteria->add(ArPartyPeer::LEGAL_STATE_PROVINCE, $this->legal_state_province);
		if ($this->isColumnModified(ArPartyPeer::LEGAL_COUNTRY)) $criteria->add(ArPartyPeer::LEGAL_COUNTRY, $this->legal_country);
		if ($this->isColumnModified(ArPartyPeer::EMAIL)) $criteria->add(ArPartyPeer::EMAIL, $this->email);
		if ($this->isColumnModified(ArPartyPeer::PHONE)) $criteria->add(ArPartyPeer::PHONE, $this->phone);
		if ($this->isColumnModified(ArPartyPeer::PHONE2)) $criteria->add(ArPartyPeer::PHONE2, $this->phone2);
		if ($this->isColumnModified(ArPartyPeer::FAX)) $criteria->add(ArPartyPeer::FAX, $this->fax);
		if ($this->isColumnModified(ArPartyPeer::MAX_LIMIT_30)) $criteria->add(ArPartyPeer::MAX_LIMIT_30, $this->max_limit_30);
		if ($this->isColumnModified(ArPartyPeer::LAST_EMAIL_ADVISE_FOR_MAX_LIMIT_30)) $criteria->add(ArPartyPeer::LAST_EMAIL_ADVISE_FOR_MAX_LIMIT_30, $this->last_email_advise_for_max_limit_30);
		if ($this->isColumnModified(ArPartyPeer::IS_ACTIVE)) $criteria->add(ArPartyPeer::IS_ACTIVE, $this->is_active);
		if ($this->isColumnModified(ArPartyPeer::AR_RESELLER_ID)) $criteria->add(ArPartyPeer::AR_RESELLER_ID, $this->ar_reseller_id);
		if ($this->isColumnModified(ArPartyPeer::MIGRATION_FIELD_FOR_TELEPHONE)) $criteria->add(ArPartyPeer::MIGRATION_FIELD_FOR_TELEPHONE, $this->migration_field_for_telephone);
		if ($this->isColumnModified(ArPartyPeer::MIGRATION_FIELD_FOR_ADSL)) $criteria->add(ArPartyPeer::MIGRATION_FIELD_FOR_ADSL, $this->migration_field_for_adsl);
		if ($this->isColumnModified(ArPartyPeer::PAYMENT_IBAN)) $criteria->add(ArPartyPeer::PAYMENT_IBAN, $this->payment_iban);
		if ($this->isColumnModified(ArPartyPeer::PAYMENT_BIC)) $criteria->add(ArPartyPeer::PAYMENT_BIC, $this->payment_bic);
		if ($this->isColumnModified(ArPartyPeer::PAYMENT_SEPA)) $criteria->add(ArPartyPeer::PAYMENT_SEPA, $this->payment_sepa);
		if ($this->isColumnModified(ArPartyPeer::PAYMENT_INFO)) $criteria->add(ArPartyPeer::PAYMENT_INFO, $this->payment_info);

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
		$criteria = new Criteria(ArPartyPeer::DATABASE_NAME);

		$criteria->add(ArPartyPeer::ID, $this->id);

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
	 * @param      object $copyObj An object of ArParty (or compatible) type.
	 * @param      boolean $deepCopy Whether to also copy all rows that refer (by fkey) to the current row.
	 * @throws     PropelException
	 */
	public function copyInto($copyObj, $deepCopy = false)
	{

		$copyObj->setName($this->name);

		$copyObj->setCompactName($this->compact_name);

		$copyObj->setExternalCrmCode($this->external_crm_code);

		$copyObj->setVat($this->vat);

		$copyObj->setIsBillable($this->is_billable);

		$copyObj->setLegalAddress($this->legal_address);

		$copyObj->setLegalCity($this->legal_city);

		$copyObj->setLegalZipcode($this->legal_zipcode);

		$copyObj->setLegalStateProvince($this->legal_state_province);

		$copyObj->setLegalCountry($this->legal_country);

		$copyObj->setEmail($this->email);

		$copyObj->setPhone($this->phone);

		$copyObj->setPhone2($this->phone2);

		$copyObj->setFax($this->fax);

		$copyObj->setMaxLimit30($this->max_limit_30);

		$copyObj->setLastEmailAdviseForMaxLimit30($this->last_email_advise_for_max_limit_30);

		$copyObj->setIsActive($this->is_active);

		$copyObj->setArResellerId($this->ar_reseller_id);

		$copyObj->setMigrationFieldForTelephone($this->migration_field_for_telephone);

		$copyObj->setMigrationFieldForAdsl($this->migration_field_for_adsl);

		$copyObj->setPaymentIban($this->payment_iban);

		$copyObj->setPaymentBic($this->payment_bic);

		$copyObj->setPaymentSepa($this->payment_sepa);

		$copyObj->setPaymentInfo($this->payment_info);


		if ($deepCopy) {
			// important: temporarily setNew(false) because this affects the behavior of
			// the getter/setter methods for fkey referrer objects.
			$copyObj->setNew(false);

			foreach ($this->getArPartyHasTags() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArPartyHasTag($relObj->copy($deepCopy));
				}
			}

			foreach ($this->getArOrganizationUnitHasStructures() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArOrganizationUnitHasStructure($relObj->copy($deepCopy));
				}
			}

			foreach ($this->getArVendors() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArVendor($relObj->copy($deepCopy));
				}
			}

			foreach ($this->getArUsers() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArUser($relObj->copy($deepCopy));
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
	 * @return     ArParty Clone of current object.
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
	 * @return     ArPartyPeer
	 */
	public function getPeer()
	{
		if (self::$peer === null) {
			self::$peer = new ArPartyPeer();
		}
		return self::$peer;
	}

	/**
	 * Declares an association between this object and a ArReseller object.
	 *
	 * @param      ArReseller $v
	 * @return     ArParty The current object (for fluent API support)
	 * @throws     PropelException
	 */
	public function setArReseller(ArReseller $v = null)
	{
		if ($v === null) {
			$this->setArResellerId(NULL);
		} else {
			$this->setArResellerId($v->getId());
		}

		$this->aArReseller = $v;

		// Add binding for other direction of this n:n relationship.
		// If this object has already been added to the ArReseller object, it will not be re-added.
		if ($v !== null) {
			$v->addArParty($this);
		}

		return $this;
	}


	/**
	 * Get the associated ArReseller object
	 *
	 * @param      PropelPDO Optional Connection object.
	 * @return     ArReseller The associated ArReseller object.
	 * @throws     PropelException
	 */
	public function getArReseller(PropelPDO $con = null)
	{
		if ($this->aArReseller === null && ($this->ar_reseller_id !== null)) {
			$this->aArReseller = ArResellerPeer::retrieveByPk($this->ar_reseller_id);
			/* The following can be used additionally to
			   guarantee the related object contains a reference
			   to this object.  This level of coupling may, however, be
			   undesirable since it could result in an only partially populated collection
			   in the referenced object.
			   $this->aArReseller->addArPartys($this);
			 */
		}
		return $this->aArReseller;
	}

	/**
	 * Clears out the collArPartyHasTags collection (array).
	 *
	 * This does not modify the database; however, it will remove any associated objects, causing
	 * them to be refetched by subsequent calls to accessor method.
	 *
	 * @return     void
	 * @see        addArPartyHasTags()
	 */
	public function clearArPartyHasTags()
	{
		$this->collArPartyHasTags = null; // important to set this to NULL since that means it is uninitialized
	}

	/**
	 * Initializes the collArPartyHasTags collection (array).
	 *
	 * By default this just sets the collArPartyHasTags collection to an empty array (like clearcollArPartyHasTags());
	 * however, you may wish to override this method in your stub class to provide setting appropriate
	 * to your application -- for example, setting the initial array to the values stored in database.
	 *
	 * @return     void
	 */
	public function initArPartyHasTags()
	{
		$this->collArPartyHasTags = array();
	}

	/**
	 * Gets an array of ArPartyHasTag objects which contain a foreign key that references this object.
	 *
	 * If this collection has already been initialized with an identical Criteria, it returns the collection.
	 * Otherwise if this ArParty has previously been saved, it will retrieve
	 * related ArPartyHasTags from storage. If this ArParty is new, it will return
	 * an empty collection or the current collection, the criteria is ignored on a new object.
	 *
	 * @param      PropelPDO $con
	 * @param      Criteria $criteria
	 * @return     array ArPartyHasTag[]
	 * @throws     PropelException
	 */
	public function getArPartyHasTags($criteria = null, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArPartyPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArPartyHasTags === null) {
			if ($this->isNew()) {
			   $this->collArPartyHasTags = array();
			} else {

				$criteria->add(ArPartyHasTagPeer::AR_PARTY_ID, $this->id);

				ArPartyHasTagPeer::addSelectColumns($criteria);
				$this->collArPartyHasTags = ArPartyHasTagPeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArPartyHasTagPeer::AR_PARTY_ID, $this->id);

				ArPartyHasTagPeer::addSelectColumns($criteria);
				if (!isset($this->lastArPartyHasTagCriteria) || !$this->lastArPartyHasTagCriteria->equals($criteria)) {
					$this->collArPartyHasTags = ArPartyHasTagPeer::doSelect($criteria, $con);
				}
			}
		}
		$this->lastArPartyHasTagCriteria = $criteria;
		return $this->collArPartyHasTags;
	}

	/**
	 * Returns the number of related ArPartyHasTag objects.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct
	 * @param      PropelPDO $con
	 * @return     int Count of related ArPartyHasTag objects.
	 * @throws     PropelException
	 */
	public function countArPartyHasTags(Criteria $criteria = null, $distinct = false, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArPartyPeer::DATABASE_NAME);
		} else {
			$criteria = clone $criteria;
		}

		if ($distinct) {
			$criteria->setDistinct();
		}

		$count = null;

		if ($this->collArPartyHasTags === null) {
			if ($this->isNew()) {
				$count = 0;
			} else {

				$criteria->add(ArPartyHasTagPeer::AR_PARTY_ID, $this->id);

				$count = ArPartyHasTagPeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArPartyHasTagPeer::AR_PARTY_ID, $this->id);

				if (!isset($this->lastArPartyHasTagCriteria) || !$this->lastArPartyHasTagCriteria->equals($criteria)) {
					$count = ArPartyHasTagPeer::doCount($criteria, false, $con);
				} else {
					$count = count($this->collArPartyHasTags);
				}
			} else {
				$count = count($this->collArPartyHasTags);
			}
		}
		return $count;
	}

	/**
	 * Method called to associate a ArPartyHasTag object to this object
	 * through the ArPartyHasTag foreign key attribute.
	 *
	 * @param      ArPartyHasTag $l ArPartyHasTag
	 * @return     void
	 * @throws     PropelException
	 */
	public function addArPartyHasTag(ArPartyHasTag $l)
	{
		if ($this->collArPartyHasTags === null) {
			$this->initArPartyHasTags();
		}
		if (!in_array($l, $this->collArPartyHasTags, true)) { // only add it if the **same** object is not already associated
			array_push($this->collArPartyHasTags, $l);
			$l->setArParty($this);
		}
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArParty is new, it will return
	 * an empty collection; or if this ArParty has previously
	 * been saved, it will retrieve related ArPartyHasTags from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArParty.
	 */
	public function getArPartyHasTagsJoinArTag($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArPartyPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArPartyHasTags === null) {
			if ($this->isNew()) {
				$this->collArPartyHasTags = array();
			} else {

				$criteria->add(ArPartyHasTagPeer::AR_PARTY_ID, $this->id);

				$this->collArPartyHasTags = ArPartyHasTagPeer::doSelectJoinArTag($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArPartyHasTagPeer::AR_PARTY_ID, $this->id);

			if (!isset($this->lastArPartyHasTagCriteria) || !$this->lastArPartyHasTagCriteria->equals($criteria)) {
				$this->collArPartyHasTags = ArPartyHasTagPeer::doSelectJoinArTag($criteria, $con, $join_behavior);
			}
		}
		$this->lastArPartyHasTagCriteria = $criteria;

		return $this->collArPartyHasTags;
	}

	/**
	 * Clears out the collArOrganizationUnitHasStructures collection (array).
	 *
	 * This does not modify the database; however, it will remove any associated objects, causing
	 * them to be refetched by subsequent calls to accessor method.
	 *
	 * @return     void
	 * @see        addArOrganizationUnitHasStructures()
	 */
	public function clearArOrganizationUnitHasStructures()
	{
		$this->collArOrganizationUnitHasStructures = null; // important to set this to NULL since that means it is uninitialized
	}

	/**
	 * Initializes the collArOrganizationUnitHasStructures collection (array).
	 *
	 * By default this just sets the collArOrganizationUnitHasStructures collection to an empty array (like clearcollArOrganizationUnitHasStructures());
	 * however, you may wish to override this method in your stub class to provide setting appropriate
	 * to your application -- for example, setting the initial array to the values stored in database.
	 *
	 * @return     void
	 */
	public function initArOrganizationUnitHasStructures()
	{
		$this->collArOrganizationUnitHasStructures = array();
	}

	/**
	 * Gets an array of ArOrganizationUnitHasStructure objects which contain a foreign key that references this object.
	 *
	 * If this collection has already been initialized with an identical Criteria, it returns the collection.
	 * Otherwise if this ArParty has previously been saved, it will retrieve
	 * related ArOrganizationUnitHasStructures from storage. If this ArParty is new, it will return
	 * an empty collection or the current collection, the criteria is ignored on a new object.
	 *
	 * @param      PropelPDO $con
	 * @param      Criteria $criteria
	 * @return     array ArOrganizationUnitHasStructure[]
	 * @throws     PropelException
	 */
	public function getArOrganizationUnitHasStructures($criteria = null, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArPartyPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArOrganizationUnitHasStructures === null) {
			if ($this->isNew()) {
			   $this->collArOrganizationUnitHasStructures = array();
			} else {

				$criteria->add(ArOrganizationUnitHasStructurePeer::AR_PARTY_ID, $this->id);

				ArOrganizationUnitHasStructurePeer::addSelectColumns($criteria);
				$this->collArOrganizationUnitHasStructures = ArOrganizationUnitHasStructurePeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArOrganizationUnitHasStructurePeer::AR_PARTY_ID, $this->id);

				ArOrganizationUnitHasStructurePeer::addSelectColumns($criteria);
				if (!isset($this->lastArOrganizationUnitHasStructureCriteria) || !$this->lastArOrganizationUnitHasStructureCriteria->equals($criteria)) {
					$this->collArOrganizationUnitHasStructures = ArOrganizationUnitHasStructurePeer::doSelect($criteria, $con);
				}
			}
		}
		$this->lastArOrganizationUnitHasStructureCriteria = $criteria;
		return $this->collArOrganizationUnitHasStructures;
	}

	/**
	 * Returns the number of related ArOrganizationUnitHasStructure objects.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct
	 * @param      PropelPDO $con
	 * @return     int Count of related ArOrganizationUnitHasStructure objects.
	 * @throws     PropelException
	 */
	public function countArOrganizationUnitHasStructures(Criteria $criteria = null, $distinct = false, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArPartyPeer::DATABASE_NAME);
		} else {
			$criteria = clone $criteria;
		}

		if ($distinct) {
			$criteria->setDistinct();
		}

		$count = null;

		if ($this->collArOrganizationUnitHasStructures === null) {
			if ($this->isNew()) {
				$count = 0;
			} else {

				$criteria->add(ArOrganizationUnitHasStructurePeer::AR_PARTY_ID, $this->id);

				$count = ArOrganizationUnitHasStructurePeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArOrganizationUnitHasStructurePeer::AR_PARTY_ID, $this->id);

				if (!isset($this->lastArOrganizationUnitHasStructureCriteria) || !$this->lastArOrganizationUnitHasStructureCriteria->equals($criteria)) {
					$count = ArOrganizationUnitHasStructurePeer::doCount($criteria, false, $con);
				} else {
					$count = count($this->collArOrganizationUnitHasStructures);
				}
			} else {
				$count = count($this->collArOrganizationUnitHasStructures);
			}
		}
		return $count;
	}

	/**
	 * Method called to associate a ArOrganizationUnitHasStructure object to this object
	 * through the ArOrganizationUnitHasStructure foreign key attribute.
	 *
	 * @param      ArOrganizationUnitHasStructure $l ArOrganizationUnitHasStructure
	 * @return     void
	 * @throws     PropelException
	 */
	public function addArOrganizationUnitHasStructure(ArOrganizationUnitHasStructure $l)
	{
		if ($this->collArOrganizationUnitHasStructures === null) {
			$this->initArOrganizationUnitHasStructures();
		}
		if (!in_array($l, $this->collArOrganizationUnitHasStructures, true)) { // only add it if the **same** object is not already associated
			array_push($this->collArOrganizationUnitHasStructures, $l);
			$l->setArParty($this);
		}
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArParty is new, it will return
	 * an empty collection; or if this ArParty has previously
	 * been saved, it will retrieve related ArOrganizationUnitHasStructures from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArParty.
	 */
	public function getArOrganizationUnitHasStructuresJoinArOrganizationUnitRelatedByArOrganizationUnitId($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArPartyPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArOrganizationUnitHasStructures === null) {
			if ($this->isNew()) {
				$this->collArOrganizationUnitHasStructures = array();
			} else {

				$criteria->add(ArOrganizationUnitHasStructurePeer::AR_PARTY_ID, $this->id);

				$this->collArOrganizationUnitHasStructures = ArOrganizationUnitHasStructurePeer::doSelectJoinArOrganizationUnitRelatedByArOrganizationUnitId($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArOrganizationUnitHasStructurePeer::AR_PARTY_ID, $this->id);

			if (!isset($this->lastArOrganizationUnitHasStructureCriteria) || !$this->lastArOrganizationUnitHasStructureCriteria->equals($criteria)) {
				$this->collArOrganizationUnitHasStructures = ArOrganizationUnitHasStructurePeer::doSelectJoinArOrganizationUnitRelatedByArOrganizationUnitId($criteria, $con, $join_behavior);
			}
		}
		$this->lastArOrganizationUnitHasStructureCriteria = $criteria;

		return $this->collArOrganizationUnitHasStructures;
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArParty is new, it will return
	 * an empty collection; or if this ArParty has previously
	 * been saved, it will retrieve related ArOrganizationUnitHasStructures from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArParty.
	 */
	public function getArOrganizationUnitHasStructuresJoinArOrganizationUnitType($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArPartyPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArOrganizationUnitHasStructures === null) {
			if ($this->isNew()) {
				$this->collArOrganizationUnitHasStructures = array();
			} else {

				$criteria->add(ArOrganizationUnitHasStructurePeer::AR_PARTY_ID, $this->id);

				$this->collArOrganizationUnitHasStructures = ArOrganizationUnitHasStructurePeer::doSelectJoinArOrganizationUnitType($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArOrganizationUnitHasStructurePeer::AR_PARTY_ID, $this->id);

			if (!isset($this->lastArOrganizationUnitHasStructureCriteria) || !$this->lastArOrganizationUnitHasStructureCriteria->equals($criteria)) {
				$this->collArOrganizationUnitHasStructures = ArOrganizationUnitHasStructurePeer::doSelectJoinArOrganizationUnitType($criteria, $con, $join_behavior);
			}
		}
		$this->lastArOrganizationUnitHasStructureCriteria = $criteria;

		return $this->collArOrganizationUnitHasStructures;
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArParty is new, it will return
	 * an empty collection; or if this ArParty has previously
	 * been saved, it will retrieve related ArOrganizationUnitHasStructures from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArParty.
	 */
	public function getArOrganizationUnitHasStructuresJoinArOrganizationUnitRelatedByArParentOrganizationUnitId($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArPartyPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArOrganizationUnitHasStructures === null) {
			if ($this->isNew()) {
				$this->collArOrganizationUnitHasStructures = array();
			} else {

				$criteria->add(ArOrganizationUnitHasStructurePeer::AR_PARTY_ID, $this->id);

				$this->collArOrganizationUnitHasStructures = ArOrganizationUnitHasStructurePeer::doSelectJoinArOrganizationUnitRelatedByArParentOrganizationUnitId($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArOrganizationUnitHasStructurePeer::AR_PARTY_ID, $this->id);

			if (!isset($this->lastArOrganizationUnitHasStructureCriteria) || !$this->lastArOrganizationUnitHasStructureCriteria->equals($criteria)) {
				$this->collArOrganizationUnitHasStructures = ArOrganizationUnitHasStructurePeer::doSelectJoinArOrganizationUnitRelatedByArParentOrganizationUnitId($criteria, $con, $join_behavior);
			}
		}
		$this->lastArOrganizationUnitHasStructureCriteria = $criteria;

		return $this->collArOrganizationUnitHasStructures;
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArParty is new, it will return
	 * an empty collection; or if this ArParty has previously
	 * been saved, it will retrieve related ArOrganizationUnitHasStructures from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArParty.
	 */
	public function getArOrganizationUnitHasStructuresJoinArRateCategory($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArPartyPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArOrganizationUnitHasStructures === null) {
			if ($this->isNew()) {
				$this->collArOrganizationUnitHasStructures = array();
			} else {

				$criteria->add(ArOrganizationUnitHasStructurePeer::AR_PARTY_ID, $this->id);

				$this->collArOrganizationUnitHasStructures = ArOrganizationUnitHasStructurePeer::doSelectJoinArRateCategory($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArOrganizationUnitHasStructurePeer::AR_PARTY_ID, $this->id);

			if (!isset($this->lastArOrganizationUnitHasStructureCriteria) || !$this->lastArOrganizationUnitHasStructureCriteria->equals($criteria)) {
				$this->collArOrganizationUnitHasStructures = ArOrganizationUnitHasStructurePeer::doSelectJoinArRateCategory($criteria, $con, $join_behavior);
			}
		}
		$this->lastArOrganizationUnitHasStructureCriteria = $criteria;

		return $this->collArOrganizationUnitHasStructures;
	}

	/**
	 * Clears out the collArVendors collection (array).
	 *
	 * This does not modify the database; however, it will remove any associated objects, causing
	 * them to be refetched by subsequent calls to accessor method.
	 *
	 * @return     void
	 * @see        addArVendors()
	 */
	public function clearArVendors()
	{
		$this->collArVendors = null; // important to set this to NULL since that means it is uninitialized
	}

	/**
	 * Initializes the collArVendors collection (array).
	 *
	 * By default this just sets the collArVendors collection to an empty array (like clearcollArVendors());
	 * however, you may wish to override this method in your stub class to provide setting appropriate
	 * to your application -- for example, setting the initial array to the values stored in database.
	 *
	 * @return     void
	 */
	public function initArVendors()
	{
		$this->collArVendors = array();
	}

	/**
	 * Gets an array of ArVendor objects which contain a foreign key that references this object.
	 *
	 * If this collection has already been initialized with an identical Criteria, it returns the collection.
	 * Otherwise if this ArParty has previously been saved, it will retrieve
	 * related ArVendors from storage. If this ArParty is new, it will return
	 * an empty collection or the current collection, the criteria is ignored on a new object.
	 *
	 * @param      PropelPDO $con
	 * @param      Criteria $criteria
	 * @return     array ArVendor[]
	 * @throws     PropelException
	 */
	public function getArVendors($criteria = null, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArPartyPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArVendors === null) {
			if ($this->isNew()) {
			   $this->collArVendors = array();
			} else {

				$criteria->add(ArVendorPeer::AR_PARTY_ID, $this->id);

				ArVendorPeer::addSelectColumns($criteria);
				$this->collArVendors = ArVendorPeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArVendorPeer::AR_PARTY_ID, $this->id);

				ArVendorPeer::addSelectColumns($criteria);
				if (!isset($this->lastArVendorCriteria) || !$this->lastArVendorCriteria->equals($criteria)) {
					$this->collArVendors = ArVendorPeer::doSelect($criteria, $con);
				}
			}
		}
		$this->lastArVendorCriteria = $criteria;
		return $this->collArVendors;
	}

	/**
	 * Returns the number of related ArVendor objects.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct
	 * @param      PropelPDO $con
	 * @return     int Count of related ArVendor objects.
	 * @throws     PropelException
	 */
	public function countArVendors(Criteria $criteria = null, $distinct = false, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArPartyPeer::DATABASE_NAME);
		} else {
			$criteria = clone $criteria;
		}

		if ($distinct) {
			$criteria->setDistinct();
		}

		$count = null;

		if ($this->collArVendors === null) {
			if ($this->isNew()) {
				$count = 0;
			} else {

				$criteria->add(ArVendorPeer::AR_PARTY_ID, $this->id);

				$count = ArVendorPeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArVendorPeer::AR_PARTY_ID, $this->id);

				if (!isset($this->lastArVendorCriteria) || !$this->lastArVendorCriteria->equals($criteria)) {
					$count = ArVendorPeer::doCount($criteria, false, $con);
				} else {
					$count = count($this->collArVendors);
				}
			} else {
				$count = count($this->collArVendors);
			}
		}
		return $count;
	}

	/**
	 * Method called to associate a ArVendor object to this object
	 * through the ArVendor foreign key attribute.
	 *
	 * @param      ArVendor $l ArVendor
	 * @return     void
	 * @throws     PropelException
	 */
	public function addArVendor(ArVendor $l)
	{
		if ($this->collArVendors === null) {
			$this->initArVendors();
		}
		if (!in_array($l, $this->collArVendors, true)) { // only add it if the **same** object is not already associated
			array_push($this->collArVendors, $l);
			$l->setArParty($this);
		}
	}

	/**
	 * Clears out the collArUsers collection (array).
	 *
	 * This does not modify the database; however, it will remove any associated objects, causing
	 * them to be refetched by subsequent calls to accessor method.
	 *
	 * @return     void
	 * @see        addArUsers()
	 */
	public function clearArUsers()
	{
		$this->collArUsers = null; // important to set this to NULL since that means it is uninitialized
	}

	/**
	 * Initializes the collArUsers collection (array).
	 *
	 * By default this just sets the collArUsers collection to an empty array (like clearcollArUsers());
	 * however, you may wish to override this method in your stub class to provide setting appropriate
	 * to your application -- for example, setting the initial array to the values stored in database.
	 *
	 * @return     void
	 */
	public function initArUsers()
	{
		$this->collArUsers = array();
	}

	/**
	 * Gets an array of ArUser objects which contain a foreign key that references this object.
	 *
	 * If this collection has already been initialized with an identical Criteria, it returns the collection.
	 * Otherwise if this ArParty has previously been saved, it will retrieve
	 * related ArUsers from storage. If this ArParty is new, it will return
	 * an empty collection or the current collection, the criteria is ignored on a new object.
	 *
	 * @param      PropelPDO $con
	 * @param      Criteria $criteria
	 * @return     array ArUser[]
	 * @throws     PropelException
	 */
	public function getArUsers($criteria = null, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArPartyPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArUsers === null) {
			if ($this->isNew()) {
			   $this->collArUsers = array();
			} else {

				$criteria->add(ArUserPeer::AR_PARTY_ID, $this->id);

				ArUserPeer::addSelectColumns($criteria);
				$this->collArUsers = ArUserPeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArUserPeer::AR_PARTY_ID, $this->id);

				ArUserPeer::addSelectColumns($criteria);
				if (!isset($this->lastArUserCriteria) || !$this->lastArUserCriteria->equals($criteria)) {
					$this->collArUsers = ArUserPeer::doSelect($criteria, $con);
				}
			}
		}
		$this->lastArUserCriteria = $criteria;
		return $this->collArUsers;
	}

	/**
	 * Returns the number of related ArUser objects.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct
	 * @param      PropelPDO $con
	 * @return     int Count of related ArUser objects.
	 * @throws     PropelException
	 */
	public function countArUsers(Criteria $criteria = null, $distinct = false, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArPartyPeer::DATABASE_NAME);
		} else {
			$criteria = clone $criteria;
		}

		if ($distinct) {
			$criteria->setDistinct();
		}

		$count = null;

		if ($this->collArUsers === null) {
			if ($this->isNew()) {
				$count = 0;
			} else {

				$criteria->add(ArUserPeer::AR_PARTY_ID, $this->id);

				$count = ArUserPeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArUserPeer::AR_PARTY_ID, $this->id);

				if (!isset($this->lastArUserCriteria) || !$this->lastArUserCriteria->equals($criteria)) {
					$count = ArUserPeer::doCount($criteria, false, $con);
				} else {
					$count = count($this->collArUsers);
				}
			} else {
				$count = count($this->collArUsers);
			}
		}
		return $count;
	}

	/**
	 * Method called to associate a ArUser object to this object
	 * through the ArUser foreign key attribute.
	 *
	 * @param      ArUser $l ArUser
	 * @return     void
	 * @throws     PropelException
	 */
	public function addArUser(ArUser $l)
	{
		if ($this->collArUsers === null) {
			$this->initArUsers();
		}
		if (!in_array($l, $this->collArUsers, true)) { // only add it if the **same** object is not already associated
			array_push($this->collArUsers, $l);
			$l->setArParty($this);
		}
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArParty is new, it will return
	 * an empty collection; or if this ArParty has previously
	 * been saved, it will retrieve related ArUsers from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArParty.
	 */
	public function getArUsersJoinArOrganizationUnit($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArPartyPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArUsers === null) {
			if ($this->isNew()) {
				$this->collArUsers = array();
			} else {

				$criteria->add(ArUserPeer::AR_PARTY_ID, $this->id);

				$this->collArUsers = ArUserPeer::doSelectJoinArOrganizationUnit($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArUserPeer::AR_PARTY_ID, $this->id);

			if (!isset($this->lastArUserCriteria) || !$this->lastArUserCriteria->equals($criteria)) {
				$this->collArUsers = ArUserPeer::doSelectJoinArOrganizationUnit($criteria, $con, $join_behavior);
			}
		}
		$this->lastArUserCriteria = $criteria;

		return $this->collArUsers;
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
			if ($this->collArPartyHasTags) {
				foreach ((array) $this->collArPartyHasTags as $o) {
					$o->clearAllReferences($deep);
				}
			}
			if ($this->collArOrganizationUnitHasStructures) {
				foreach ((array) $this->collArOrganizationUnitHasStructures as $o) {
					$o->clearAllReferences($deep);
				}
			}
			if ($this->collArVendors) {
				foreach ((array) $this->collArVendors as $o) {
					$o->clearAllReferences($deep);
				}
			}
			if ($this->collArUsers) {
				foreach ((array) $this->collArUsers as $o) {
					$o->clearAllReferences($deep);
				}
			}
		} // if ($deep)

		$this->collArPartyHasTags = null;
		$this->collArOrganizationUnitHasStructures = null;
		$this->collArVendors = null;
		$this->collArUsers = null;
			$this->aArReseller = null;
	}

} // BaseArParty
