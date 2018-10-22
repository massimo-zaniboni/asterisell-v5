<?php

/**
 * Base class that represents a row from the 'ar_params' table.
 *
 * 
 *
 * @package    lib.model.om
 */
abstract class BaseArParams extends BaseObject  implements Persistent {


	/**
	 * The Peer class.
	 * Instance provides a convenient way of calling static methods on a class
	 * that calling code may not be able to identify.
	 * @var        ArParamsPeer
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
	 * The value for the is_default field.
	 * @var        boolean
	 */
	protected $is_default;

	/**
	 * The value for the service_name field.
	 * @var        string
	 */
	protected $service_name;

	/**
	 * The value for the service_provider_website field.
	 * @var        string
	 */
	protected $service_provider_website;

	/**
	 * The value for the service_provider_email field.
	 * @var        string
	 */
	protected $service_provider_email;

	/**
	 * The value for the vat_tax_perc field.
	 * Note: this column has a database default value of: 0
	 * @var        int
	 */
	protected $vat_tax_perc;

	/**
	 * The value for the logo_image field.
	 * @var        string
	 */
	protected $logo_image;

	/**
	 * The value for the slogan field.
	 * @var        string
	 */
	protected $slogan;

	/**
	 * The value for the logo_image_in_invoices field.
	 * @var        string
	 */
	protected $logo_image_in_invoices;

	/**
	 * The value for the footer field.
	 * @var        string
	 */
	protected $footer;

	/**
	 * The value for the user_message field.
	 * @var        string
	 */
	protected $user_message;

	/**
	 * The value for the legal_name field.
	 * @var        string
	 */
	protected $legal_name;

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
	 * The value for the legal_address field.
	 * @var        string
	 */
	protected $legal_address;

	/**
	 * The value for the legal_website field.
	 * @var        string
	 */
	protected $legal_website;

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
	 * The value for the legal_email field.
	 * @var        string
	 */
	protected $legal_email;

	/**
	 * The value for the legal_phone field.
	 * @var        string
	 */
	protected $legal_phone;

	/**
	 * The value for the phone2 field.
	 * @var        string
	 */
	protected $phone2;

	/**
	 * The value for the legal_fax field.
	 * @var        string
	 */
	protected $legal_fax;

	/**
	 * The value for the invoice_notes field.
	 * @var        string
	 */
	protected $invoice_notes;

	/**
	 * The value for the invoice_payment_terms field.
	 * @var        string
	 */
	protected $invoice_payment_terms;

	/**
	 * The value for the invoice_payment_due_in_xx_days field.
	 * @var        int
	 */
	protected $invoice_payment_due_in_xx_days;

	/**
	 * The value for the sender_name_on_invoicing_emails field.
	 * @var        string
	 */
	protected $sender_name_on_invoicing_emails;

	/**
	 * The value for the invoicing_email_address field.
	 * @var        string
	 */
	protected $invoicing_email_address;

	/**
	 * The value for the logo_html_color field.
	 * @var        string
	 */
	protected $logo_html_color;

	/**
	 * The value for the html_notes_on_the_login_form field.
	 * @var        string
	 */
	protected $html_notes_on_the_login_form;

	/**
	 * The value for the official_calldate field.
	 * @var        string
	 */
	protected $official_calldate;

	/**
	 * The value for the scheduled_rerate_from_official_calldate field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $scheduled_rerate_from_official_calldate;

	/**
	 * The value for the new_imported_cdrs_from_calldate field.
	 * @var        string
	 */
	protected $new_imported_cdrs_from_calldate;

	/**
	 * The value for the scheduled_rerate_from_specific_calldate field.
	 * @var        string
	 */
	protected $scheduled_rerate_from_specific_calldate;

	/**
	 * The value for the current_count_of_rerating_failed_attempts field.
	 * Note: this column has a database default value of: 0
	 * @var        int
	 */
	protected $current_count_of_rerating_failed_attempts;

	/**
	 * The value for the current_rerating_event_is_running field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $current_rerating_event_is_running;

	/**
	 * The value for the should_reschedule_rerate_from_official_calldate field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $should_reschedule_rerate_from_official_calldate;

	/**
	 * The value for the wait_for_scheduled_rerate field.
	 * Note: this column has a database default value of: true
	 * @var        boolean
	 */
	protected $wait_for_scheduled_rerate;

	/**
	 * The value for the clean_error_table field.
	 * Note: this column has a database default value of: 0
	 * @var        int
	 */
	protected $clean_error_table;

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
	
	const PEER = 'ArParamsPeer';

	/**
	 * Applies default values to this object.
	 * This method should be called from the object's constructor (or
	 * equivalent initialization method).
	 * @see        __construct()
	 */
	public function applyDefaultValues()
	{
		$this->vat_tax_perc = 0;
		$this->scheduled_rerate_from_official_calldate = false;
		$this->current_count_of_rerating_failed_attempts = 0;
		$this->current_rerating_event_is_running = false;
		$this->should_reschedule_rerate_from_official_calldate = false;
		$this->wait_for_scheduled_rerate = true;
		$this->clean_error_table = 0;
	}

	/**
	 * Initializes internal state of BaseArParams object.
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
	 * Get the [is_default] column value.
	 * 
	 * @return     boolean
	 */
	public function getIsDefault()
	{
		return $this->is_default;
	}

	/**
	 * Get the [service_name] column value.
	 * 
	 * @return     string
	 */
	public function getServiceName()
	{
		return $this->service_name;
	}

	/**
	 * Get the [service_provider_website] column value.
	 * 
	 * @return     string
	 */
	public function getServiceProviderWebsite()
	{
		return $this->service_provider_website;
	}

	/**
	 * Get the [service_provider_email] column value.
	 * 
	 * @return     string
	 */
	public function getServiceProviderEmail()
	{
		return $this->service_provider_email;
	}

	/**
	 * Get the [vat_tax_perc] column value.
	 * 
	 * @return     int
	 */
	public function getVatTaxPerc()
	{
		return $this->vat_tax_perc;
	}

	/**
	 * Get the [logo_image] column value.
	 * 
	 * @return     string
	 */
	public function getLogoImage()
	{
		return $this->logo_image;
	}

	/**
	 * Get the [slogan] column value.
	 * 
	 * @return     string
	 */
	public function getSlogan()
	{
		return $this->slogan;
	}

	/**
	 * Get the [logo_image_in_invoices] column value.
	 * 
	 * @return     string
	 */
	public function getLogoImageInInvoices()
	{
		return $this->logo_image_in_invoices;
	}

	/**
	 * Get the [footer] column value.
	 * 
	 * @return     string
	 */
	public function getFooter()
	{
		return $this->footer;
	}

	/**
	 * Get the [user_message] column value.
	 * 
	 * @return     string
	 */
	public function getUserMessage()
	{
		return $this->user_message;
	}

	/**
	 * Get the [legal_name] column value.
	 * 
	 * @return     string
	 */
	public function getLegalName()
	{
		return $this->legal_name;
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
	 * Get the [legal_address] column value.
	 * 
	 * @return     string
	 */
	public function getLegalAddress()
	{
		return $this->legal_address;
	}

	/**
	 * Get the [legal_website] column value.
	 * 
	 * @return     string
	 */
	public function getLegalWebsite()
	{
		return $this->legal_website;
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
	 * Get the [legal_email] column value.
	 * 
	 * @return     string
	 */
	public function getLegalEmail()
	{
		return $this->legal_email;
	}

	/**
	 * Get the [legal_phone] column value.
	 * 
	 * @return     string
	 */
	public function getLegalPhone()
	{
		return $this->legal_phone;
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
	 * Get the [legal_fax] column value.
	 * 
	 * @return     string
	 */
	public function getLegalFax()
	{
		return $this->legal_fax;
	}

	/**
	 * Get the [invoice_notes] column value.
	 * 
	 * @return     string
	 */
	public function getInvoiceNotes()
	{
		return $this->invoice_notes;
	}

	/**
	 * Get the [invoice_payment_terms] column value.
	 * 
	 * @return     string
	 */
	public function getInvoicePaymentTerms()
	{
		return $this->invoice_payment_terms;
	}

	/**
	 * Get the [invoice_payment_due_in_xx_days] column value.
	 * 
	 * @return     int
	 */
	public function getInvoicePaymentDueInXxDays()
	{
		return $this->invoice_payment_due_in_xx_days;
	}

	/**
	 * Get the [sender_name_on_invoicing_emails] column value.
	 * 
	 * @return     string
	 */
	public function getSenderNameOnInvoicingEmails()
	{
		return $this->sender_name_on_invoicing_emails;
	}

	/**
	 * Get the [invoicing_email_address] column value.
	 * 
	 * @return     string
	 */
	public function getInvoicingEmailAddress()
	{
		return $this->invoicing_email_address;
	}

	/**
	 * Get the [logo_html_color] column value.
	 * 
	 * @return     string
	 */
	public function getLogoHtmlColor()
	{
		return $this->logo_html_color;
	}

	/**
	 * Get the [html_notes_on_the_login_form] column value.
	 * 
	 * @return     string
	 */
	public function getHtmlNotesOnTheLoginForm()
	{
		return $this->html_notes_on_the_login_form;
	}

	/**
	 * Get the [optionally formatted] temporal [official_calldate] column value.
	 * 
	 *
	 * @param      string $format The date/time format string (either date()-style or strftime()-style).
	 *							If format is NULL, then the raw DateTime object will be returned.
	 * @return     mixed Formatted date/time value as string or DateTime object (if format is NULL), NULL if column is NULL, and 0 if column value is 0000-00-00 00:00:00
	 * @throws     PropelException - if unable to parse/validate the date/time value.
	 */
	public function getOfficialCalldate($format = 'Y-m-d H:i:s')
	{
		if ($this->official_calldate === null) {
			return null;
		}


		if ($this->official_calldate === '0000-00-00 00:00:00') {
			// while technically this is not a default value of NULL,
			// this seems to be closest in meaning.
			return null;
		} else {
			try {
				$dt = new DateTime($this->official_calldate);
			} catch (Exception $x) {
				throw new PropelException("Internally stored date/time/timestamp value could not be converted to DateTime: " . var_export($this->official_calldate, true), $x);
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
	 * Get the [scheduled_rerate_from_official_calldate] column value.
	 * 
	 * @return     boolean
	 */
	public function getScheduledRerateFromOfficialCalldate()
	{
		return $this->scheduled_rerate_from_official_calldate;
	}

	/**
	 * Get the [optionally formatted] temporal [new_imported_cdrs_from_calldate] column value.
	 * 
	 *
	 * @param      string $format The date/time format string (either date()-style or strftime()-style).
	 *							If format is NULL, then the raw DateTime object will be returned.
	 * @return     mixed Formatted date/time value as string or DateTime object (if format is NULL), NULL if column is NULL, and 0 if column value is 0000-00-00 00:00:00
	 * @throws     PropelException - if unable to parse/validate the date/time value.
	 */
	public function getNewImportedCdrsFromCalldate($format = 'Y-m-d H:i:s')
	{
		if ($this->new_imported_cdrs_from_calldate === null) {
			return null;
		}


		if ($this->new_imported_cdrs_from_calldate === '0000-00-00 00:00:00') {
			// while technically this is not a default value of NULL,
			// this seems to be closest in meaning.
			return null;
		} else {
			try {
				$dt = new DateTime($this->new_imported_cdrs_from_calldate);
			} catch (Exception $x) {
				throw new PropelException("Internally stored date/time/timestamp value could not be converted to DateTime: " . var_export($this->new_imported_cdrs_from_calldate, true), $x);
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
	 * Get the [optionally formatted] temporal [scheduled_rerate_from_specific_calldate] column value.
	 * 
	 *
	 * @param      string $format The date/time format string (either date()-style or strftime()-style).
	 *							If format is NULL, then the raw DateTime object will be returned.
	 * @return     mixed Formatted date/time value as string or DateTime object (if format is NULL), NULL if column is NULL, and 0 if column value is 0000-00-00 00:00:00
	 * @throws     PropelException - if unable to parse/validate the date/time value.
	 */
	public function getScheduledRerateFromSpecificCalldate($format = 'Y-m-d H:i:s')
	{
		if ($this->scheduled_rerate_from_specific_calldate === null) {
			return null;
		}


		if ($this->scheduled_rerate_from_specific_calldate === '0000-00-00 00:00:00') {
			// while technically this is not a default value of NULL,
			// this seems to be closest in meaning.
			return null;
		} else {
			try {
				$dt = new DateTime($this->scheduled_rerate_from_specific_calldate);
			} catch (Exception $x) {
				throw new PropelException("Internally stored date/time/timestamp value could not be converted to DateTime: " . var_export($this->scheduled_rerate_from_specific_calldate, true), $x);
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
	 * Get the [current_count_of_rerating_failed_attempts] column value.
	 * 
	 * @return     int
	 */
	public function getCurrentCountOfReratingFailedAttempts()
	{
		return $this->current_count_of_rerating_failed_attempts;
	}

	/**
	 * Get the [current_rerating_event_is_running] column value.
	 * 
	 * @return     boolean
	 */
	public function getCurrentReratingEventIsRunning()
	{
		return $this->current_rerating_event_is_running;
	}

	/**
	 * Get the [should_reschedule_rerate_from_official_calldate] column value.
	 * 
	 * @return     boolean
	 */
	public function getShouldRescheduleRerateFromOfficialCalldate()
	{
		return $this->should_reschedule_rerate_from_official_calldate;
	}

	/**
	 * Get the [wait_for_scheduled_rerate] column value.
	 * 
	 * @return     boolean
	 */
	public function getWaitForScheduledRerate()
	{
		return $this->wait_for_scheduled_rerate;
	}

	/**
	 * Get the [clean_error_table] column value.
	 * 
	 * @return     int
	 */
	public function getCleanErrorTable()
	{
		return $this->clean_error_table;
	}

	/**
	 * Set the value of [id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->id !== $v) {
			$this->id = $v;
			$this->modifiedColumns[] = ArParamsPeer::ID;
		}

		return $this;
	} // setId()

	/**
	 * Set the value of [name] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setName($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->name !== $v) {
			$this->name = $v;
			$this->modifiedColumns[] = ArParamsPeer::NAME;
		}

		return $this;
	} // setName()

	/**
	 * Set the value of [is_default] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setIsDefault($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->is_default !== $v) {
			$this->is_default = $v;
			$this->modifiedColumns[] = ArParamsPeer::IS_DEFAULT;
		}

		return $this;
	} // setIsDefault()

	/**
	 * Set the value of [service_name] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setServiceName($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->service_name !== $v) {
			$this->service_name = $v;
			$this->modifiedColumns[] = ArParamsPeer::SERVICE_NAME;
		}

		return $this;
	} // setServiceName()

	/**
	 * Set the value of [service_provider_website] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setServiceProviderWebsite($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->service_provider_website !== $v) {
			$this->service_provider_website = $v;
			$this->modifiedColumns[] = ArParamsPeer::SERVICE_PROVIDER_WEBSITE;
		}

		return $this;
	} // setServiceProviderWebsite()

	/**
	 * Set the value of [service_provider_email] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setServiceProviderEmail($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->service_provider_email !== $v) {
			$this->service_provider_email = $v;
			$this->modifiedColumns[] = ArParamsPeer::SERVICE_PROVIDER_EMAIL;
		}

		return $this;
	} // setServiceProviderEmail()

	/**
	 * Set the value of [vat_tax_perc] column.
	 * 
	 * @param      int $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setVatTaxPerc($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->vat_tax_perc !== $v || $this->isNew()) {
			$this->vat_tax_perc = $v;
			$this->modifiedColumns[] = ArParamsPeer::VAT_TAX_PERC;
		}

		return $this;
	} // setVatTaxPerc()

	/**
	 * Set the value of [logo_image] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setLogoImage($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->logo_image !== $v) {
			$this->logo_image = $v;
			$this->modifiedColumns[] = ArParamsPeer::LOGO_IMAGE;
		}

		return $this;
	} // setLogoImage()

	/**
	 * Set the value of [slogan] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setSlogan($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->slogan !== $v) {
			$this->slogan = $v;
			$this->modifiedColumns[] = ArParamsPeer::SLOGAN;
		}

		return $this;
	} // setSlogan()

	/**
	 * Set the value of [logo_image_in_invoices] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setLogoImageInInvoices($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->logo_image_in_invoices !== $v) {
			$this->logo_image_in_invoices = $v;
			$this->modifiedColumns[] = ArParamsPeer::LOGO_IMAGE_IN_INVOICES;
		}

		return $this;
	} // setLogoImageInInvoices()

	/**
	 * Set the value of [footer] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setFooter($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->footer !== $v) {
			$this->footer = $v;
			$this->modifiedColumns[] = ArParamsPeer::FOOTER;
		}

		return $this;
	} // setFooter()

	/**
	 * Set the value of [user_message] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setUserMessage($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->user_message !== $v) {
			$this->user_message = $v;
			$this->modifiedColumns[] = ArParamsPeer::USER_MESSAGE;
		}

		return $this;
	} // setUserMessage()

	/**
	 * Set the value of [legal_name] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setLegalName($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->legal_name !== $v) {
			$this->legal_name = $v;
			$this->modifiedColumns[] = ArParamsPeer::LEGAL_NAME;
		}

		return $this;
	} // setLegalName()

	/**
	 * Set the value of [external_crm_code] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setExternalCrmCode($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->external_crm_code !== $v) {
			$this->external_crm_code = $v;
			$this->modifiedColumns[] = ArParamsPeer::EXTERNAL_CRM_CODE;
		}

		return $this;
	} // setExternalCrmCode()

	/**
	 * Set the value of [vat] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setVat($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->vat !== $v) {
			$this->vat = $v;
			$this->modifiedColumns[] = ArParamsPeer::VAT;
		}

		return $this;
	} // setVat()

	/**
	 * Set the value of [legal_address] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setLegalAddress($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->legal_address !== $v) {
			$this->legal_address = $v;
			$this->modifiedColumns[] = ArParamsPeer::LEGAL_ADDRESS;
		}

		return $this;
	} // setLegalAddress()

	/**
	 * Set the value of [legal_website] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setLegalWebsite($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->legal_website !== $v) {
			$this->legal_website = $v;
			$this->modifiedColumns[] = ArParamsPeer::LEGAL_WEBSITE;
		}

		return $this;
	} // setLegalWebsite()

	/**
	 * Set the value of [legal_city] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setLegalCity($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->legal_city !== $v) {
			$this->legal_city = $v;
			$this->modifiedColumns[] = ArParamsPeer::LEGAL_CITY;
		}

		return $this;
	} // setLegalCity()

	/**
	 * Set the value of [legal_zipcode] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setLegalZipcode($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->legal_zipcode !== $v) {
			$this->legal_zipcode = $v;
			$this->modifiedColumns[] = ArParamsPeer::LEGAL_ZIPCODE;
		}

		return $this;
	} // setLegalZipcode()

	/**
	 * Set the value of [legal_state_province] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setLegalStateProvince($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->legal_state_province !== $v) {
			$this->legal_state_province = $v;
			$this->modifiedColumns[] = ArParamsPeer::LEGAL_STATE_PROVINCE;
		}

		return $this;
	} // setLegalStateProvince()

	/**
	 * Set the value of [legal_country] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setLegalCountry($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->legal_country !== $v) {
			$this->legal_country = $v;
			$this->modifiedColumns[] = ArParamsPeer::LEGAL_COUNTRY;
		}

		return $this;
	} // setLegalCountry()

	/**
	 * Set the value of [legal_email] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setLegalEmail($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->legal_email !== $v) {
			$this->legal_email = $v;
			$this->modifiedColumns[] = ArParamsPeer::LEGAL_EMAIL;
		}

		return $this;
	} // setLegalEmail()

	/**
	 * Set the value of [legal_phone] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setLegalPhone($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->legal_phone !== $v) {
			$this->legal_phone = $v;
			$this->modifiedColumns[] = ArParamsPeer::LEGAL_PHONE;
		}

		return $this;
	} // setLegalPhone()

	/**
	 * Set the value of [phone2] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setPhone2($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->phone2 !== $v) {
			$this->phone2 = $v;
			$this->modifiedColumns[] = ArParamsPeer::PHONE2;
		}

		return $this;
	} // setPhone2()

	/**
	 * Set the value of [legal_fax] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setLegalFax($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->legal_fax !== $v) {
			$this->legal_fax = $v;
			$this->modifiedColumns[] = ArParamsPeer::LEGAL_FAX;
		}

		return $this;
	} // setLegalFax()

	/**
	 * Set the value of [invoice_notes] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setInvoiceNotes($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->invoice_notes !== $v) {
			$this->invoice_notes = $v;
			$this->modifiedColumns[] = ArParamsPeer::INVOICE_NOTES;
		}

		return $this;
	} // setInvoiceNotes()

	/**
	 * Set the value of [invoice_payment_terms] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setInvoicePaymentTerms($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->invoice_payment_terms !== $v) {
			$this->invoice_payment_terms = $v;
			$this->modifiedColumns[] = ArParamsPeer::INVOICE_PAYMENT_TERMS;
		}

		return $this;
	} // setInvoicePaymentTerms()

	/**
	 * Set the value of [invoice_payment_due_in_xx_days] column.
	 * 
	 * @param      int $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setInvoicePaymentDueInXxDays($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->invoice_payment_due_in_xx_days !== $v) {
			$this->invoice_payment_due_in_xx_days = $v;
			$this->modifiedColumns[] = ArParamsPeer::INVOICE_PAYMENT_DUE_IN_XX_DAYS;
		}

		return $this;
	} // setInvoicePaymentDueInXxDays()

	/**
	 * Set the value of [sender_name_on_invoicing_emails] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setSenderNameOnInvoicingEmails($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->sender_name_on_invoicing_emails !== $v) {
			$this->sender_name_on_invoicing_emails = $v;
			$this->modifiedColumns[] = ArParamsPeer::SENDER_NAME_ON_INVOICING_EMAILS;
		}

		return $this;
	} // setSenderNameOnInvoicingEmails()

	/**
	 * Set the value of [invoicing_email_address] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setInvoicingEmailAddress($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->invoicing_email_address !== $v) {
			$this->invoicing_email_address = $v;
			$this->modifiedColumns[] = ArParamsPeer::INVOICING_EMAIL_ADDRESS;
		}

		return $this;
	} // setInvoicingEmailAddress()

	/**
	 * Set the value of [logo_html_color] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setLogoHtmlColor($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->logo_html_color !== $v) {
			$this->logo_html_color = $v;
			$this->modifiedColumns[] = ArParamsPeer::LOGO_HTML_COLOR;
		}

		return $this;
	} // setLogoHtmlColor()

	/**
	 * Set the value of [html_notes_on_the_login_form] column.
	 * 
	 * @param      string $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setHtmlNotesOnTheLoginForm($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->html_notes_on_the_login_form !== $v) {
			$this->html_notes_on_the_login_form = $v;
			$this->modifiedColumns[] = ArParamsPeer::HTML_NOTES_ON_THE_LOGIN_FORM;
		}

		return $this;
	} // setHtmlNotesOnTheLoginForm()

	/**
	 * Sets the value of [official_calldate] column to a normalized version of the date/time value specified.
	 * 
	 * @param      mixed $v string, integer (timestamp), or DateTime value.  Empty string will
	 *						be treated as NULL for temporal objects.
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setOfficialCalldate($v)
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

		if ( $this->official_calldate !== null || $dt !== null ) {
			// (nested ifs are a little easier to read in this case)

			$currNorm = ($this->official_calldate !== null && $tmpDt = new DateTime($this->official_calldate)) ? $tmpDt->format('Y-m-d H:i:s') : null;
			$newNorm = ($dt !== null) ? $dt->format('Y-m-d H:i:s') : null;

			if ( ($currNorm !== $newNorm) // normalized values don't match 
					)
			{
				$this->official_calldate = ($dt ? $dt->format('Y-m-d H:i:s') : null);
				$this->modifiedColumns[] = ArParamsPeer::OFFICIAL_CALLDATE;
			}
		} // if either are not null

		return $this;
	} // setOfficialCalldate()

	/**
	 * Set the value of [scheduled_rerate_from_official_calldate] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setScheduledRerateFromOfficialCalldate($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->scheduled_rerate_from_official_calldate !== $v || $this->isNew()) {
			$this->scheduled_rerate_from_official_calldate = $v;
			$this->modifiedColumns[] = ArParamsPeer::SCHEDULED_RERATE_FROM_OFFICIAL_CALLDATE;
		}

		return $this;
	} // setScheduledRerateFromOfficialCalldate()

	/**
	 * Sets the value of [new_imported_cdrs_from_calldate] column to a normalized version of the date/time value specified.
	 * 
	 * @param      mixed $v string, integer (timestamp), or DateTime value.  Empty string will
	 *						be treated as NULL for temporal objects.
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setNewImportedCdrsFromCalldate($v)
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

		if ( $this->new_imported_cdrs_from_calldate !== null || $dt !== null ) {
			// (nested ifs are a little easier to read in this case)

			$currNorm = ($this->new_imported_cdrs_from_calldate !== null && $tmpDt = new DateTime($this->new_imported_cdrs_from_calldate)) ? $tmpDt->format('Y-m-d H:i:s') : null;
			$newNorm = ($dt !== null) ? $dt->format('Y-m-d H:i:s') : null;

			if ( ($currNorm !== $newNorm) // normalized values don't match 
					)
			{
				$this->new_imported_cdrs_from_calldate = ($dt ? $dt->format('Y-m-d H:i:s') : null);
				$this->modifiedColumns[] = ArParamsPeer::NEW_IMPORTED_CDRS_FROM_CALLDATE;
			}
		} // if either are not null

		return $this;
	} // setNewImportedCdrsFromCalldate()

	/**
	 * Sets the value of [scheduled_rerate_from_specific_calldate] column to a normalized version of the date/time value specified.
	 * 
	 * @param      mixed $v string, integer (timestamp), or DateTime value.  Empty string will
	 *						be treated as NULL for temporal objects.
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setScheduledRerateFromSpecificCalldate($v)
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

		if ( $this->scheduled_rerate_from_specific_calldate !== null || $dt !== null ) {
			// (nested ifs are a little easier to read in this case)

			$currNorm = ($this->scheduled_rerate_from_specific_calldate !== null && $tmpDt = new DateTime($this->scheduled_rerate_from_specific_calldate)) ? $tmpDt->format('Y-m-d H:i:s') : null;
			$newNorm = ($dt !== null) ? $dt->format('Y-m-d H:i:s') : null;

			if ( ($currNorm !== $newNorm) // normalized values don't match 
					)
			{
				$this->scheduled_rerate_from_specific_calldate = ($dt ? $dt->format('Y-m-d H:i:s') : null);
				$this->modifiedColumns[] = ArParamsPeer::SCHEDULED_RERATE_FROM_SPECIFIC_CALLDATE;
			}
		} // if either are not null

		return $this;
	} // setScheduledRerateFromSpecificCalldate()

	/**
	 * Set the value of [current_count_of_rerating_failed_attempts] column.
	 * 
	 * @param      int $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setCurrentCountOfReratingFailedAttempts($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->current_count_of_rerating_failed_attempts !== $v || $this->isNew()) {
			$this->current_count_of_rerating_failed_attempts = $v;
			$this->modifiedColumns[] = ArParamsPeer::CURRENT_COUNT_OF_RERATING_FAILED_ATTEMPTS;
		}

		return $this;
	} // setCurrentCountOfReratingFailedAttempts()

	/**
	 * Set the value of [current_rerating_event_is_running] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setCurrentReratingEventIsRunning($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->current_rerating_event_is_running !== $v || $this->isNew()) {
			$this->current_rerating_event_is_running = $v;
			$this->modifiedColumns[] = ArParamsPeer::CURRENT_RERATING_EVENT_IS_RUNNING;
		}

		return $this;
	} // setCurrentReratingEventIsRunning()

	/**
	 * Set the value of [should_reschedule_rerate_from_official_calldate] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setShouldRescheduleRerateFromOfficialCalldate($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->should_reschedule_rerate_from_official_calldate !== $v || $this->isNew()) {
			$this->should_reschedule_rerate_from_official_calldate = $v;
			$this->modifiedColumns[] = ArParamsPeer::SHOULD_RESCHEDULE_RERATE_FROM_OFFICIAL_CALLDATE;
		}

		return $this;
	} // setShouldRescheduleRerateFromOfficialCalldate()

	/**
	 * Set the value of [wait_for_scheduled_rerate] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setWaitForScheduledRerate($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->wait_for_scheduled_rerate !== $v || $this->isNew()) {
			$this->wait_for_scheduled_rerate = $v;
			$this->modifiedColumns[] = ArParamsPeer::WAIT_FOR_SCHEDULED_RERATE;
		}

		return $this;
	} // setWaitForScheduledRerate()

	/**
	 * Set the value of [clean_error_table] column.
	 * 
	 * @param      int $v new value
	 * @return     ArParams The current object (for fluent API support)
	 */
	public function setCleanErrorTable($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->clean_error_table !== $v || $this->isNew()) {
			$this->clean_error_table = $v;
			$this->modifiedColumns[] = ArParamsPeer::CLEAN_ERROR_TABLE;
		}

		return $this;
	} // setCleanErrorTable()

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
			if ($this->vat_tax_perc !== 0) {
				return false;
			}

			if ($this->scheduled_rerate_from_official_calldate !== false) {
				return false;
			}

			if ($this->current_count_of_rerating_failed_attempts !== 0) {
				return false;
			}

			if ($this->current_rerating_event_is_running !== false) {
				return false;
			}

			if ($this->should_reschedule_rerate_from_official_calldate !== false) {
				return false;
			}

			if ($this->wait_for_scheduled_rerate !== true) {
				return false;
			}

			if ($this->clean_error_table !== 0) {
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
			$this->is_default = ($row[$startcol + 2] !== null) ? (boolean) $row[$startcol + 2] : null;
			$this->service_name = ($row[$startcol + 3] !== null) ? (string) $row[$startcol + 3] : null;
			$this->service_provider_website = ($row[$startcol + 4] !== null) ? (string) $row[$startcol + 4] : null;
			$this->service_provider_email = ($row[$startcol + 5] !== null) ? (string) $row[$startcol + 5] : null;
			$this->vat_tax_perc = ($row[$startcol + 6] !== null) ? (int) $row[$startcol + 6] : null;
			$this->logo_image = ($row[$startcol + 7] !== null) ? (string) $row[$startcol + 7] : null;
			$this->slogan = ($row[$startcol + 8] !== null) ? (string) $row[$startcol + 8] : null;
			$this->logo_image_in_invoices = ($row[$startcol + 9] !== null) ? (string) $row[$startcol + 9] : null;
			$this->footer = ($row[$startcol + 10] !== null) ? (string) $row[$startcol + 10] : null;
			$this->user_message = ($row[$startcol + 11] !== null) ? (string) $row[$startcol + 11] : null;
			$this->legal_name = ($row[$startcol + 12] !== null) ? (string) $row[$startcol + 12] : null;
			$this->external_crm_code = ($row[$startcol + 13] !== null) ? (string) $row[$startcol + 13] : null;
			$this->vat = ($row[$startcol + 14] !== null) ? (string) $row[$startcol + 14] : null;
			$this->legal_address = ($row[$startcol + 15] !== null) ? (string) $row[$startcol + 15] : null;
			$this->legal_website = ($row[$startcol + 16] !== null) ? (string) $row[$startcol + 16] : null;
			$this->legal_city = ($row[$startcol + 17] !== null) ? (string) $row[$startcol + 17] : null;
			$this->legal_zipcode = ($row[$startcol + 18] !== null) ? (string) $row[$startcol + 18] : null;
			$this->legal_state_province = ($row[$startcol + 19] !== null) ? (string) $row[$startcol + 19] : null;
			$this->legal_country = ($row[$startcol + 20] !== null) ? (string) $row[$startcol + 20] : null;
			$this->legal_email = ($row[$startcol + 21] !== null) ? (string) $row[$startcol + 21] : null;
			$this->legal_phone = ($row[$startcol + 22] !== null) ? (string) $row[$startcol + 22] : null;
			$this->phone2 = ($row[$startcol + 23] !== null) ? (string) $row[$startcol + 23] : null;
			$this->legal_fax = ($row[$startcol + 24] !== null) ? (string) $row[$startcol + 24] : null;
			$this->invoice_notes = ($row[$startcol + 25] !== null) ? (string) $row[$startcol + 25] : null;
			$this->invoice_payment_terms = ($row[$startcol + 26] !== null) ? (string) $row[$startcol + 26] : null;
			$this->invoice_payment_due_in_xx_days = ($row[$startcol + 27] !== null) ? (int) $row[$startcol + 27] : null;
			$this->sender_name_on_invoicing_emails = ($row[$startcol + 28] !== null) ? (string) $row[$startcol + 28] : null;
			$this->invoicing_email_address = ($row[$startcol + 29] !== null) ? (string) $row[$startcol + 29] : null;
			$this->logo_html_color = ($row[$startcol + 30] !== null) ? (string) $row[$startcol + 30] : null;
			$this->html_notes_on_the_login_form = ($row[$startcol + 31] !== null) ? (string) $row[$startcol + 31] : null;
			$this->official_calldate = ($row[$startcol + 32] !== null) ? (string) $row[$startcol + 32] : null;
			$this->scheduled_rerate_from_official_calldate = ($row[$startcol + 33] !== null) ? (boolean) $row[$startcol + 33] : null;
			$this->new_imported_cdrs_from_calldate = ($row[$startcol + 34] !== null) ? (string) $row[$startcol + 34] : null;
			$this->scheduled_rerate_from_specific_calldate = ($row[$startcol + 35] !== null) ? (string) $row[$startcol + 35] : null;
			$this->current_count_of_rerating_failed_attempts = ($row[$startcol + 36] !== null) ? (int) $row[$startcol + 36] : null;
			$this->current_rerating_event_is_running = ($row[$startcol + 37] !== null) ? (boolean) $row[$startcol + 37] : null;
			$this->should_reschedule_rerate_from_official_calldate = ($row[$startcol + 38] !== null) ? (boolean) $row[$startcol + 38] : null;
			$this->wait_for_scheduled_rerate = ($row[$startcol + 39] !== null) ? (boolean) $row[$startcol + 39] : null;
			$this->clean_error_table = ($row[$startcol + 40] !== null) ? (int) $row[$startcol + 40] : null;
			$this->resetModified();

			$this->setNew(false);

			if ($rehydrate) {
				$this->ensureConsistency();
			}

			// FIXME - using NUM_COLUMNS may be clearer.
			return $startcol + 41; // 41 = ArParamsPeer::NUM_COLUMNS - ArParamsPeer::NUM_LAZY_LOAD_COLUMNS).

		} catch (Exception $e) {
			throw new PropelException("Error populating ArParams object", $e);
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
			$con = Propel::getConnection(ArParamsPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		// We don't need to alter the object instance pool; we're just modifying this instance
		// already in the pool.

		$stmt = ArParamsPeer::doSelectStmt($this->buildPkeyCriteria(), $con);
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
			$con = Propel::getConnection(ArParamsPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}
		
		$con->beginTransaction();
		try {
			$ret = $this->preDelete($con);
			if ($ret) {
				ArParamsPeer::doDelete($this, $con);
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
			$con = Propel::getConnection(ArParamsPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
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
				ArParamsPeer::addInstanceToPool($this);
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
				$this->modifiedColumns[] = ArParamsPeer::ID;
			}

			// If this object has been modified, then save it to the database.
			if ($this->isModified()) {
				if ($this->isNew()) {
					$pk = ArParamsPeer::doInsert($this, $con);
					$affectedRows += 1; // we are assuming that there is only 1 row per doInsert() which
										 // should always be true here (even though technically
										 // BasePeer::doInsert() can insert multiple rows).

					$this->setId($pk);  //[IMV] update autoincrement primary key

					$this->setNew(false);
				} else {
					$affectedRows += ArParamsPeer::doUpdate($this, $con);
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


			if (($retval = ArParamsPeer::doValidate($this, $columns)) !== true) {
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
		$pos = ArParamsPeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
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
				return $this->getIsDefault();
				break;
			case 3:
				return $this->getServiceName();
				break;
			case 4:
				return $this->getServiceProviderWebsite();
				break;
			case 5:
				return $this->getServiceProviderEmail();
				break;
			case 6:
				return $this->getVatTaxPerc();
				break;
			case 7:
				return $this->getLogoImage();
				break;
			case 8:
				return $this->getSlogan();
				break;
			case 9:
				return $this->getLogoImageInInvoices();
				break;
			case 10:
				return $this->getFooter();
				break;
			case 11:
				return $this->getUserMessage();
				break;
			case 12:
				return $this->getLegalName();
				break;
			case 13:
				return $this->getExternalCrmCode();
				break;
			case 14:
				return $this->getVat();
				break;
			case 15:
				return $this->getLegalAddress();
				break;
			case 16:
				return $this->getLegalWebsite();
				break;
			case 17:
				return $this->getLegalCity();
				break;
			case 18:
				return $this->getLegalZipcode();
				break;
			case 19:
				return $this->getLegalStateProvince();
				break;
			case 20:
				return $this->getLegalCountry();
				break;
			case 21:
				return $this->getLegalEmail();
				break;
			case 22:
				return $this->getLegalPhone();
				break;
			case 23:
				return $this->getPhone2();
				break;
			case 24:
				return $this->getLegalFax();
				break;
			case 25:
				return $this->getInvoiceNotes();
				break;
			case 26:
				return $this->getInvoicePaymentTerms();
				break;
			case 27:
				return $this->getInvoicePaymentDueInXxDays();
				break;
			case 28:
				return $this->getSenderNameOnInvoicingEmails();
				break;
			case 29:
				return $this->getInvoicingEmailAddress();
				break;
			case 30:
				return $this->getLogoHtmlColor();
				break;
			case 31:
				return $this->getHtmlNotesOnTheLoginForm();
				break;
			case 32:
				return $this->getOfficialCalldate();
				break;
			case 33:
				return $this->getScheduledRerateFromOfficialCalldate();
				break;
			case 34:
				return $this->getNewImportedCdrsFromCalldate();
				break;
			case 35:
				return $this->getScheduledRerateFromSpecificCalldate();
				break;
			case 36:
				return $this->getCurrentCountOfReratingFailedAttempts();
				break;
			case 37:
				return $this->getCurrentReratingEventIsRunning();
				break;
			case 38:
				return $this->getShouldRescheduleRerateFromOfficialCalldate();
				break;
			case 39:
				return $this->getWaitForScheduledRerate();
				break;
			case 40:
				return $this->getCleanErrorTable();
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
		$keys = ArParamsPeer::getFieldNames($keyType);
		$result = array(
			$keys[0] => $this->getId(),
			$keys[1] => $this->getName(),
			$keys[2] => $this->getIsDefault(),
			$keys[3] => $this->getServiceName(),
			$keys[4] => $this->getServiceProviderWebsite(),
			$keys[5] => $this->getServiceProviderEmail(),
			$keys[6] => $this->getVatTaxPerc(),
			$keys[7] => $this->getLogoImage(),
			$keys[8] => $this->getSlogan(),
			$keys[9] => $this->getLogoImageInInvoices(),
			$keys[10] => $this->getFooter(),
			$keys[11] => $this->getUserMessage(),
			$keys[12] => $this->getLegalName(),
			$keys[13] => $this->getExternalCrmCode(),
			$keys[14] => $this->getVat(),
			$keys[15] => $this->getLegalAddress(),
			$keys[16] => $this->getLegalWebsite(),
			$keys[17] => $this->getLegalCity(),
			$keys[18] => $this->getLegalZipcode(),
			$keys[19] => $this->getLegalStateProvince(),
			$keys[20] => $this->getLegalCountry(),
			$keys[21] => $this->getLegalEmail(),
			$keys[22] => $this->getLegalPhone(),
			$keys[23] => $this->getPhone2(),
			$keys[24] => $this->getLegalFax(),
			$keys[25] => $this->getInvoiceNotes(),
			$keys[26] => $this->getInvoicePaymentTerms(),
			$keys[27] => $this->getInvoicePaymentDueInXxDays(),
			$keys[28] => $this->getSenderNameOnInvoicingEmails(),
			$keys[29] => $this->getInvoicingEmailAddress(),
			$keys[30] => $this->getLogoHtmlColor(),
			$keys[31] => $this->getHtmlNotesOnTheLoginForm(),
			$keys[32] => $this->getOfficialCalldate(),
			$keys[33] => $this->getScheduledRerateFromOfficialCalldate(),
			$keys[34] => $this->getNewImportedCdrsFromCalldate(),
			$keys[35] => $this->getScheduledRerateFromSpecificCalldate(),
			$keys[36] => $this->getCurrentCountOfReratingFailedAttempts(),
			$keys[37] => $this->getCurrentReratingEventIsRunning(),
			$keys[38] => $this->getShouldRescheduleRerateFromOfficialCalldate(),
			$keys[39] => $this->getWaitForScheduledRerate(),
			$keys[40] => $this->getCleanErrorTable(),
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
		$pos = ArParamsPeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
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
				$this->setIsDefault($value);
				break;
			case 3:
				$this->setServiceName($value);
				break;
			case 4:
				$this->setServiceProviderWebsite($value);
				break;
			case 5:
				$this->setServiceProviderEmail($value);
				break;
			case 6:
				$this->setVatTaxPerc($value);
				break;
			case 7:
				$this->setLogoImage($value);
				break;
			case 8:
				$this->setSlogan($value);
				break;
			case 9:
				$this->setLogoImageInInvoices($value);
				break;
			case 10:
				$this->setFooter($value);
				break;
			case 11:
				$this->setUserMessage($value);
				break;
			case 12:
				$this->setLegalName($value);
				break;
			case 13:
				$this->setExternalCrmCode($value);
				break;
			case 14:
				$this->setVat($value);
				break;
			case 15:
				$this->setLegalAddress($value);
				break;
			case 16:
				$this->setLegalWebsite($value);
				break;
			case 17:
				$this->setLegalCity($value);
				break;
			case 18:
				$this->setLegalZipcode($value);
				break;
			case 19:
				$this->setLegalStateProvince($value);
				break;
			case 20:
				$this->setLegalCountry($value);
				break;
			case 21:
				$this->setLegalEmail($value);
				break;
			case 22:
				$this->setLegalPhone($value);
				break;
			case 23:
				$this->setPhone2($value);
				break;
			case 24:
				$this->setLegalFax($value);
				break;
			case 25:
				$this->setInvoiceNotes($value);
				break;
			case 26:
				$this->setInvoicePaymentTerms($value);
				break;
			case 27:
				$this->setInvoicePaymentDueInXxDays($value);
				break;
			case 28:
				$this->setSenderNameOnInvoicingEmails($value);
				break;
			case 29:
				$this->setInvoicingEmailAddress($value);
				break;
			case 30:
				$this->setLogoHtmlColor($value);
				break;
			case 31:
				$this->setHtmlNotesOnTheLoginForm($value);
				break;
			case 32:
				$this->setOfficialCalldate($value);
				break;
			case 33:
				$this->setScheduledRerateFromOfficialCalldate($value);
				break;
			case 34:
				$this->setNewImportedCdrsFromCalldate($value);
				break;
			case 35:
				$this->setScheduledRerateFromSpecificCalldate($value);
				break;
			case 36:
				$this->setCurrentCountOfReratingFailedAttempts($value);
				break;
			case 37:
				$this->setCurrentReratingEventIsRunning($value);
				break;
			case 38:
				$this->setShouldRescheduleRerateFromOfficialCalldate($value);
				break;
			case 39:
				$this->setWaitForScheduledRerate($value);
				break;
			case 40:
				$this->setCleanErrorTable($value);
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
		$keys = ArParamsPeer::getFieldNames($keyType);

		if (array_key_exists($keys[0], $arr)) $this->setId($arr[$keys[0]]);
		if (array_key_exists($keys[1], $arr)) $this->setName($arr[$keys[1]]);
		if (array_key_exists($keys[2], $arr)) $this->setIsDefault($arr[$keys[2]]);
		if (array_key_exists($keys[3], $arr)) $this->setServiceName($arr[$keys[3]]);
		if (array_key_exists($keys[4], $arr)) $this->setServiceProviderWebsite($arr[$keys[4]]);
		if (array_key_exists($keys[5], $arr)) $this->setServiceProviderEmail($arr[$keys[5]]);
		if (array_key_exists($keys[6], $arr)) $this->setVatTaxPerc($arr[$keys[6]]);
		if (array_key_exists($keys[7], $arr)) $this->setLogoImage($arr[$keys[7]]);
		if (array_key_exists($keys[8], $arr)) $this->setSlogan($arr[$keys[8]]);
		if (array_key_exists($keys[9], $arr)) $this->setLogoImageInInvoices($arr[$keys[9]]);
		if (array_key_exists($keys[10], $arr)) $this->setFooter($arr[$keys[10]]);
		if (array_key_exists($keys[11], $arr)) $this->setUserMessage($arr[$keys[11]]);
		if (array_key_exists($keys[12], $arr)) $this->setLegalName($arr[$keys[12]]);
		if (array_key_exists($keys[13], $arr)) $this->setExternalCrmCode($arr[$keys[13]]);
		if (array_key_exists($keys[14], $arr)) $this->setVat($arr[$keys[14]]);
		if (array_key_exists($keys[15], $arr)) $this->setLegalAddress($arr[$keys[15]]);
		if (array_key_exists($keys[16], $arr)) $this->setLegalWebsite($arr[$keys[16]]);
		if (array_key_exists($keys[17], $arr)) $this->setLegalCity($arr[$keys[17]]);
		if (array_key_exists($keys[18], $arr)) $this->setLegalZipcode($arr[$keys[18]]);
		if (array_key_exists($keys[19], $arr)) $this->setLegalStateProvince($arr[$keys[19]]);
		if (array_key_exists($keys[20], $arr)) $this->setLegalCountry($arr[$keys[20]]);
		if (array_key_exists($keys[21], $arr)) $this->setLegalEmail($arr[$keys[21]]);
		if (array_key_exists($keys[22], $arr)) $this->setLegalPhone($arr[$keys[22]]);
		if (array_key_exists($keys[23], $arr)) $this->setPhone2($arr[$keys[23]]);
		if (array_key_exists($keys[24], $arr)) $this->setLegalFax($arr[$keys[24]]);
		if (array_key_exists($keys[25], $arr)) $this->setInvoiceNotes($arr[$keys[25]]);
		if (array_key_exists($keys[26], $arr)) $this->setInvoicePaymentTerms($arr[$keys[26]]);
		if (array_key_exists($keys[27], $arr)) $this->setInvoicePaymentDueInXxDays($arr[$keys[27]]);
		if (array_key_exists($keys[28], $arr)) $this->setSenderNameOnInvoicingEmails($arr[$keys[28]]);
		if (array_key_exists($keys[29], $arr)) $this->setInvoicingEmailAddress($arr[$keys[29]]);
		if (array_key_exists($keys[30], $arr)) $this->setLogoHtmlColor($arr[$keys[30]]);
		if (array_key_exists($keys[31], $arr)) $this->setHtmlNotesOnTheLoginForm($arr[$keys[31]]);
		if (array_key_exists($keys[32], $arr)) $this->setOfficialCalldate($arr[$keys[32]]);
		if (array_key_exists($keys[33], $arr)) $this->setScheduledRerateFromOfficialCalldate($arr[$keys[33]]);
		if (array_key_exists($keys[34], $arr)) $this->setNewImportedCdrsFromCalldate($arr[$keys[34]]);
		if (array_key_exists($keys[35], $arr)) $this->setScheduledRerateFromSpecificCalldate($arr[$keys[35]]);
		if (array_key_exists($keys[36], $arr)) $this->setCurrentCountOfReratingFailedAttempts($arr[$keys[36]]);
		if (array_key_exists($keys[37], $arr)) $this->setCurrentReratingEventIsRunning($arr[$keys[37]]);
		if (array_key_exists($keys[38], $arr)) $this->setShouldRescheduleRerateFromOfficialCalldate($arr[$keys[38]]);
		if (array_key_exists($keys[39], $arr)) $this->setWaitForScheduledRerate($arr[$keys[39]]);
		if (array_key_exists($keys[40], $arr)) $this->setCleanErrorTable($arr[$keys[40]]);
	}

	/**
	 * Build a Criteria object containing the values of all modified columns in this object.
	 *
	 * @return     Criteria The Criteria object containing all modified values.
	 */
	public function buildCriteria()
	{
		$criteria = new Criteria(ArParamsPeer::DATABASE_NAME);

		if ($this->isColumnModified(ArParamsPeer::ID)) $criteria->add(ArParamsPeer::ID, $this->id);
		if ($this->isColumnModified(ArParamsPeer::NAME)) $criteria->add(ArParamsPeer::NAME, $this->name);
		if ($this->isColumnModified(ArParamsPeer::IS_DEFAULT)) $criteria->add(ArParamsPeer::IS_DEFAULT, $this->is_default);
		if ($this->isColumnModified(ArParamsPeer::SERVICE_NAME)) $criteria->add(ArParamsPeer::SERVICE_NAME, $this->service_name);
		if ($this->isColumnModified(ArParamsPeer::SERVICE_PROVIDER_WEBSITE)) $criteria->add(ArParamsPeer::SERVICE_PROVIDER_WEBSITE, $this->service_provider_website);
		if ($this->isColumnModified(ArParamsPeer::SERVICE_PROVIDER_EMAIL)) $criteria->add(ArParamsPeer::SERVICE_PROVIDER_EMAIL, $this->service_provider_email);
		if ($this->isColumnModified(ArParamsPeer::VAT_TAX_PERC)) $criteria->add(ArParamsPeer::VAT_TAX_PERC, $this->vat_tax_perc);
		if ($this->isColumnModified(ArParamsPeer::LOGO_IMAGE)) $criteria->add(ArParamsPeer::LOGO_IMAGE, $this->logo_image);
		if ($this->isColumnModified(ArParamsPeer::SLOGAN)) $criteria->add(ArParamsPeer::SLOGAN, $this->slogan);
		if ($this->isColumnModified(ArParamsPeer::LOGO_IMAGE_IN_INVOICES)) $criteria->add(ArParamsPeer::LOGO_IMAGE_IN_INVOICES, $this->logo_image_in_invoices);
		if ($this->isColumnModified(ArParamsPeer::FOOTER)) $criteria->add(ArParamsPeer::FOOTER, $this->footer);
		if ($this->isColumnModified(ArParamsPeer::USER_MESSAGE)) $criteria->add(ArParamsPeer::USER_MESSAGE, $this->user_message);
		if ($this->isColumnModified(ArParamsPeer::LEGAL_NAME)) $criteria->add(ArParamsPeer::LEGAL_NAME, $this->legal_name);
		if ($this->isColumnModified(ArParamsPeer::EXTERNAL_CRM_CODE)) $criteria->add(ArParamsPeer::EXTERNAL_CRM_CODE, $this->external_crm_code);
		if ($this->isColumnModified(ArParamsPeer::VAT)) $criteria->add(ArParamsPeer::VAT, $this->vat);
		if ($this->isColumnModified(ArParamsPeer::LEGAL_ADDRESS)) $criteria->add(ArParamsPeer::LEGAL_ADDRESS, $this->legal_address);
		if ($this->isColumnModified(ArParamsPeer::LEGAL_WEBSITE)) $criteria->add(ArParamsPeer::LEGAL_WEBSITE, $this->legal_website);
		if ($this->isColumnModified(ArParamsPeer::LEGAL_CITY)) $criteria->add(ArParamsPeer::LEGAL_CITY, $this->legal_city);
		if ($this->isColumnModified(ArParamsPeer::LEGAL_ZIPCODE)) $criteria->add(ArParamsPeer::LEGAL_ZIPCODE, $this->legal_zipcode);
		if ($this->isColumnModified(ArParamsPeer::LEGAL_STATE_PROVINCE)) $criteria->add(ArParamsPeer::LEGAL_STATE_PROVINCE, $this->legal_state_province);
		if ($this->isColumnModified(ArParamsPeer::LEGAL_COUNTRY)) $criteria->add(ArParamsPeer::LEGAL_COUNTRY, $this->legal_country);
		if ($this->isColumnModified(ArParamsPeer::LEGAL_EMAIL)) $criteria->add(ArParamsPeer::LEGAL_EMAIL, $this->legal_email);
		if ($this->isColumnModified(ArParamsPeer::LEGAL_PHONE)) $criteria->add(ArParamsPeer::LEGAL_PHONE, $this->legal_phone);
		if ($this->isColumnModified(ArParamsPeer::PHONE2)) $criteria->add(ArParamsPeer::PHONE2, $this->phone2);
		if ($this->isColumnModified(ArParamsPeer::LEGAL_FAX)) $criteria->add(ArParamsPeer::LEGAL_FAX, $this->legal_fax);
		if ($this->isColumnModified(ArParamsPeer::INVOICE_NOTES)) $criteria->add(ArParamsPeer::INVOICE_NOTES, $this->invoice_notes);
		if ($this->isColumnModified(ArParamsPeer::INVOICE_PAYMENT_TERMS)) $criteria->add(ArParamsPeer::INVOICE_PAYMENT_TERMS, $this->invoice_payment_terms);
		if ($this->isColumnModified(ArParamsPeer::INVOICE_PAYMENT_DUE_IN_XX_DAYS)) $criteria->add(ArParamsPeer::INVOICE_PAYMENT_DUE_IN_XX_DAYS, $this->invoice_payment_due_in_xx_days);
		if ($this->isColumnModified(ArParamsPeer::SENDER_NAME_ON_INVOICING_EMAILS)) $criteria->add(ArParamsPeer::SENDER_NAME_ON_INVOICING_EMAILS, $this->sender_name_on_invoicing_emails);
		if ($this->isColumnModified(ArParamsPeer::INVOICING_EMAIL_ADDRESS)) $criteria->add(ArParamsPeer::INVOICING_EMAIL_ADDRESS, $this->invoicing_email_address);
		if ($this->isColumnModified(ArParamsPeer::LOGO_HTML_COLOR)) $criteria->add(ArParamsPeer::LOGO_HTML_COLOR, $this->logo_html_color);
		if ($this->isColumnModified(ArParamsPeer::HTML_NOTES_ON_THE_LOGIN_FORM)) $criteria->add(ArParamsPeer::HTML_NOTES_ON_THE_LOGIN_FORM, $this->html_notes_on_the_login_form);
		if ($this->isColumnModified(ArParamsPeer::OFFICIAL_CALLDATE)) $criteria->add(ArParamsPeer::OFFICIAL_CALLDATE, $this->official_calldate);
		if ($this->isColumnModified(ArParamsPeer::SCHEDULED_RERATE_FROM_OFFICIAL_CALLDATE)) $criteria->add(ArParamsPeer::SCHEDULED_RERATE_FROM_OFFICIAL_CALLDATE, $this->scheduled_rerate_from_official_calldate);
		if ($this->isColumnModified(ArParamsPeer::NEW_IMPORTED_CDRS_FROM_CALLDATE)) $criteria->add(ArParamsPeer::NEW_IMPORTED_CDRS_FROM_CALLDATE, $this->new_imported_cdrs_from_calldate);
		if ($this->isColumnModified(ArParamsPeer::SCHEDULED_RERATE_FROM_SPECIFIC_CALLDATE)) $criteria->add(ArParamsPeer::SCHEDULED_RERATE_FROM_SPECIFIC_CALLDATE, $this->scheduled_rerate_from_specific_calldate);
		if ($this->isColumnModified(ArParamsPeer::CURRENT_COUNT_OF_RERATING_FAILED_ATTEMPTS)) $criteria->add(ArParamsPeer::CURRENT_COUNT_OF_RERATING_FAILED_ATTEMPTS, $this->current_count_of_rerating_failed_attempts);
		if ($this->isColumnModified(ArParamsPeer::CURRENT_RERATING_EVENT_IS_RUNNING)) $criteria->add(ArParamsPeer::CURRENT_RERATING_EVENT_IS_RUNNING, $this->current_rerating_event_is_running);
		if ($this->isColumnModified(ArParamsPeer::SHOULD_RESCHEDULE_RERATE_FROM_OFFICIAL_CALLDATE)) $criteria->add(ArParamsPeer::SHOULD_RESCHEDULE_RERATE_FROM_OFFICIAL_CALLDATE, $this->should_reschedule_rerate_from_official_calldate);
		if ($this->isColumnModified(ArParamsPeer::WAIT_FOR_SCHEDULED_RERATE)) $criteria->add(ArParamsPeer::WAIT_FOR_SCHEDULED_RERATE, $this->wait_for_scheduled_rerate);
		if ($this->isColumnModified(ArParamsPeer::CLEAN_ERROR_TABLE)) $criteria->add(ArParamsPeer::CLEAN_ERROR_TABLE, $this->clean_error_table);

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
		$criteria = new Criteria(ArParamsPeer::DATABASE_NAME);

		$criteria->add(ArParamsPeer::ID, $this->id);

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
	 * @param      object $copyObj An object of ArParams (or compatible) type.
	 * @param      boolean $deepCopy Whether to also copy all rows that refer (by fkey) to the current row.
	 * @throws     PropelException
	 */
	public function copyInto($copyObj, $deepCopy = false)
	{

		$copyObj->setName($this->name);

		$copyObj->setIsDefault($this->is_default);

		$copyObj->setServiceName($this->service_name);

		$copyObj->setServiceProviderWebsite($this->service_provider_website);

		$copyObj->setServiceProviderEmail($this->service_provider_email);

		$copyObj->setVatTaxPerc($this->vat_tax_perc);

		$copyObj->setLogoImage($this->logo_image);

		$copyObj->setSlogan($this->slogan);

		$copyObj->setLogoImageInInvoices($this->logo_image_in_invoices);

		$copyObj->setFooter($this->footer);

		$copyObj->setUserMessage($this->user_message);

		$copyObj->setLegalName($this->legal_name);

		$copyObj->setExternalCrmCode($this->external_crm_code);

		$copyObj->setVat($this->vat);

		$copyObj->setLegalAddress($this->legal_address);

		$copyObj->setLegalWebsite($this->legal_website);

		$copyObj->setLegalCity($this->legal_city);

		$copyObj->setLegalZipcode($this->legal_zipcode);

		$copyObj->setLegalStateProvince($this->legal_state_province);

		$copyObj->setLegalCountry($this->legal_country);

		$copyObj->setLegalEmail($this->legal_email);

		$copyObj->setLegalPhone($this->legal_phone);

		$copyObj->setPhone2($this->phone2);

		$copyObj->setLegalFax($this->legal_fax);

		$copyObj->setInvoiceNotes($this->invoice_notes);

		$copyObj->setInvoicePaymentTerms($this->invoice_payment_terms);

		$copyObj->setInvoicePaymentDueInXxDays($this->invoice_payment_due_in_xx_days);

		$copyObj->setSenderNameOnInvoicingEmails($this->sender_name_on_invoicing_emails);

		$copyObj->setInvoicingEmailAddress($this->invoicing_email_address);

		$copyObj->setLogoHtmlColor($this->logo_html_color);

		$copyObj->setHtmlNotesOnTheLoginForm($this->html_notes_on_the_login_form);

		$copyObj->setOfficialCalldate($this->official_calldate);

		$copyObj->setScheduledRerateFromOfficialCalldate($this->scheduled_rerate_from_official_calldate);

		$copyObj->setNewImportedCdrsFromCalldate($this->new_imported_cdrs_from_calldate);

		$copyObj->setScheduledRerateFromSpecificCalldate($this->scheduled_rerate_from_specific_calldate);

		$copyObj->setCurrentCountOfReratingFailedAttempts($this->current_count_of_rerating_failed_attempts);

		$copyObj->setCurrentReratingEventIsRunning($this->current_rerating_event_is_running);

		$copyObj->setShouldRescheduleRerateFromOfficialCalldate($this->should_reschedule_rerate_from_official_calldate);

		$copyObj->setWaitForScheduledRerate($this->wait_for_scheduled_rerate);

		$copyObj->setCleanErrorTable($this->clean_error_table);


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
	 * @return     ArParams Clone of current object.
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
	 * @return     ArParamsPeer
	 */
	public function getPeer()
	{
		if (self::$peer === null) {
			self::$peer = new ArParamsPeer();
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

} // BaseArParams
