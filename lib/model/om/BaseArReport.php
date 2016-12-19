<?php

/**
 * Base class that represents a row from the 'ar_report' table.
 *
 * 
 *
 * @package    lib.model.om
 */
abstract class BaseArReport extends BaseObject  implements Persistent {


	/**
	 * The Peer class.
	 * Instance provides a convenient way of calling static methods on a class
	 * that calling code may not be able to identify.
	 * @var        ArReportPeer
	 */
	protected static $peer;

	/**
	 * The value for the id field.
	 * @var        int
	 */
	protected $id;

	/**
	 * The value for the is_template field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $is_template;

	/**
	 * The value for the ar_report_set_id field.
	 * @var        int
	 */
	protected $ar_report_set_id;

	/**
	 * The value for the ar_organization_unit_id field.
	 * @var        int
	 */
	protected $ar_organization_unit_id;

	/**
	 * The value for the ar_user_id field.
	 * @var        int
	 */
	protected $ar_user_id;

	/**
	 * The value for the ar_vendor_id field.
	 * @var        int
	 */
	protected $ar_vendor_id;

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
	 * The value for the param_show_masked_telephone_numbers field.
	 * Note: this column has a database default value of: true
	 * @var        boolean
	 */
	protected $param_show_masked_telephone_numbers;

	/**
	 * The value for the param_show_call_cost field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $param_show_call_cost;

	/**
	 * The value for the param_show_call_income field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $param_show_call_income;

	/**
	 * The value for the param_show_also_outgoing_calls field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $param_show_also_outgoing_calls;

	/**
	 * The value for the param_show_also_system_calls field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $param_show_also_system_calls;

	/**
	 * The value for the param_show_also_incoming_calls field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $param_show_also_incoming_calls;

	/**
	 * The value for the param_show_also_internal_calls field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $param_show_also_internal_calls;

	/**
	 * The value for the param_show_call_details field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $param_show_call_details;

	/**
	 * The value for the param_show_voip_provider field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $param_show_voip_provider;

	/**
	 * The value for the param_show_communication_channel field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $param_show_communication_channel;

	/**
	 * The value for the param_show_geographic_location field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $param_show_geographic_location;

	/**
	 * The value for the param_show_connection_type field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $param_show_connection_type;

	/**
	 * The value for the param_show_cost_saving field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $param_show_cost_saving;

	/**
	 * The value for the param_is_legal field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $param_is_legal;

	/**
	 * The value for the param_expand_to_level field.
	 * Note: this column has a database default value of: 0
	 * @var        int
	 */
	protected $param_expand_to_level;

	/**
	 * The value for the ar_report_order_of_children_id field.
	 * @var        int
	 */
	protected $ar_report_order_of_children_id;

	/**
	 * The value for the php_class_name field.
	 * @var        string
	 */
	protected $php_class_name;

	/**
	 * The value for the produced_report_generation_date field.
	 * @var        string
	 */
	protected $produced_report_generation_date;

	/**
	 * The value for the report_name field.
	 * @var        string
	 */
	protected $report_name;

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
	 * The value for the produced_report_must_be_regenerated field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $produced_report_must_be_regenerated;

	/**
	 * The value for the produced_report_mime_type field.
	 * Note: this column has a database default value of: 'application/pdf'
	 * @var        string
	 */
	protected $produced_report_mime_type;

	/**
	 * The value for the produced_report_file_type_suffix field.
	 * Note: this column has a database default value of: 'pdf'
	 * @var        string
	 */
	protected $produced_report_file_type_suffix;

	/**
	 * The value for the produced_report_document field.
	 * @var        resource
	 */
	protected $produced_report_document;

	/**
	 * The value for the produced_report_document_checksum field.
	 * @var        string
	 */
	protected $produced_report_document_checksum;

	/**
	 * The value for the report_mail_subject field.
	 * @var        string
	 */
	protected $report_mail_subject;

	/**
	 * The value for the report_mail_body field.
	 * @var        string
	 */
	protected $report_mail_body;

	/**
	 * The value for the report_attachment_file_name field.
	 * @var        string
	 */
	protected $report_attachment_file_name;

	/**
	 * The value for the report_attachment_file_name_add_report_date field.
	 * Note: this column has a database default value of: false
	 * @var        boolean
	 */
	protected $report_attachment_file_name_add_report_date;

	/**
	 * The value for the internal_name field.
	 * @var        string
	 */
	protected $internal_name;

	/**
	 * The value for the cached_parent_id_hierarchy field.
	 * @var        string
	 */
	protected $cached_parent_id_hierarchy;

	/**
	 * The value for the legal_nr_prefix field.
	 * @var        string
	 */
	protected $legal_nr_prefix;

	/**
	 * The value for the legal_consecutive_nr field.
	 * @var        int
	 */
	protected $legal_consecutive_nr;

	/**
	 * The value for the legal_date field.
	 * @var        string
	 */
	protected $legal_date;

	/**
	 * The value for the legal_sender_name field.
	 * @var        string
	 */
	protected $legal_sender_name;

	/**
	 * The value for the legal_sender_vat field.
	 * @var        string
	 */
	protected $legal_sender_vat;

	/**
	 * The value for the legal_sender_address field.
	 * @var        string
	 */
	protected $legal_sender_address;

	/**
	 * The value for the legal_receiver_name field.
	 * @var        string
	 */
	protected $legal_receiver_name;

	/**
	 * The value for the legal_receiver_vat field.
	 * @var        string
	 */
	protected $legal_receiver_vat;

	/**
	 * The value for the legal_receiver_address field.
	 * @var        string
	 */
	protected $legal_receiver_address;

	/**
	 * The value for the total_without_tax field.
	 * @var        string
	 */
	protected $total_without_tax;

	/**
	 * The value for the tax field.
	 * @var        string
	 */
	protected $tax;

	/**
	 * The value for the applied_vat field.
	 * @var        string
	 */
	protected $applied_vat;

	/**
	 * The value for the total_with_tax field.
	 * @var        string
	 */
	protected $total_with_tax;

	/**
	 * @var        ArReportSet
	 */
	protected $aArReportSet;

	/**
	 * @var        ArOrganizationUnit
	 */
	protected $aArOrganizationUnit;

	/**
	 * @var        ArUser
	 */
	protected $aArUser;

	/**
	 * @var        ArVendor
	 */
	protected $aArVendor;

	/**
	 * @var        ArReportOrderOfChildren
	 */
	protected $aArReportOrderOfChildren;

	/**
	 * @var        array ArReportAlsoFor[] Collection to store aggregation of ArReportAlsoFor objects.
	 */
	protected $collArReportAlsoFors;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArReportAlsoFors.
	 */
	private $lastArReportAlsoForCriteria = null;

	/**
	 * @var        array ArReportScheduler[] Collection to store aggregation of ArReportScheduler objects.
	 */
	protected $collArReportSchedulers;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArReportSchedulers.
	 */
	private $lastArReportSchedulerCriteria = null;

	/**
	 * @var        array ArReportToRead[] Collection to store aggregation of ArReportToRead objects.
	 */
	protected $collArReportToReads;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArReportToReads.
	 */
	private $lastArReportToReadCriteria = null;

	/**
	 * @var        array ArReportToReadUserView[] Collection to store aggregation of ArReportToReadUserView objects.
	 */
	protected $collArReportToReadUserViews;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArReportToReadUserViews.
	 */
	private $lastArReportToReadUserViewCriteria = null;

	/**
	 * @var        array ArUserCanViewReport[] Collection to store aggregation of ArUserCanViewReport objects.
	 */
	protected $collArUserCanViewReports;

	/**
	 * @var        Criteria The criteria used to select the current contents of collArUserCanViewReports.
	 */
	private $lastArUserCanViewReportCriteria = null;

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
	
	const PEER = 'ArReportPeer';

	/**
	 * Applies default values to this object.
	 * This method should be called from the object's constructor (or
	 * equivalent initialization method).
	 * @see        __construct()
	 */
	public function applyDefaultValues()
	{
		$this->is_template = false;
		$this->param_show_masked_telephone_numbers = true;
		$this->param_show_call_cost = false;
		$this->param_show_call_income = false;
		$this->param_show_also_outgoing_calls = false;
		$this->param_show_also_system_calls = false;
		$this->param_show_also_incoming_calls = false;
		$this->param_show_also_internal_calls = false;
		$this->param_show_call_details = false;
		$this->param_show_voip_provider = false;
		$this->param_show_communication_channel = false;
		$this->param_show_geographic_location = false;
		$this->param_show_connection_type = false;
		$this->param_show_cost_saving = false;
		$this->param_is_legal = false;
		$this->param_expand_to_level = 0;
		$this->produced_report_already_reviewed = false;
		$this->produced_report_is_draft = false;
		$this->produced_report_must_be_regenerated = false;
		$this->produced_report_mime_type = 'application/pdf';
		$this->produced_report_file_type_suffix = 'pdf';
		$this->report_attachment_file_name_add_report_date = false;
	}

	/**
	 * Initializes internal state of BaseArReport object.
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
	 * Get the [is_template] column value.
	 * 
	 * @return     boolean
	 */
	public function getIsTemplate()
	{
		return $this->is_template;
	}

	/**
	 * Get the [ar_report_set_id] column value.
	 * 
	 * @return     int
	 */
	public function getArReportSetId()
	{
		return $this->ar_report_set_id;
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
	 * Get the [ar_user_id] column value.
	 * 
	 * @return     int
	 */
	public function getArUserId()
	{
		return $this->ar_user_id;
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
	 * Get the [param_show_masked_telephone_numbers] column value.
	 * 
	 * @return     boolean
	 */
	public function getParamShowMaskedTelephoneNumbers()
	{
		return $this->param_show_masked_telephone_numbers;
	}

	/**
	 * Get the [param_show_call_cost] column value.
	 * 
	 * @return     boolean
	 */
	public function getParamShowCallCost()
	{
		return $this->param_show_call_cost;
	}

	/**
	 * Get the [param_show_call_income] column value.
	 * 
	 * @return     boolean
	 */
	public function getParamShowCallIncome()
	{
		return $this->param_show_call_income;
	}

	/**
	 * Get the [param_show_also_outgoing_calls] column value.
	 * 
	 * @return     boolean
	 */
	public function getParamShowAlsoOutgoingCalls()
	{
		return $this->param_show_also_outgoing_calls;
	}

	/**
	 * Get the [param_show_also_system_calls] column value.
	 * 
	 * @return     boolean
	 */
	public function getParamShowAlsoSystemCalls()
	{
		return $this->param_show_also_system_calls;
	}

	/**
	 * Get the [param_show_also_incoming_calls] column value.
	 * 
	 * @return     boolean
	 */
	public function getParamShowAlsoIncomingCalls()
	{
		return $this->param_show_also_incoming_calls;
	}

	/**
	 * Get the [param_show_also_internal_calls] column value.
	 * 
	 * @return     boolean
	 */
	public function getParamShowAlsoInternalCalls()
	{
		return $this->param_show_also_internal_calls;
	}

	/**
	 * Get the [param_show_call_details] column value.
	 * 
	 * @return     boolean
	 */
	public function getParamShowCallDetails()
	{
		return $this->param_show_call_details;
	}

	/**
	 * Get the [param_show_voip_provider] column value.
	 * 
	 * @return     boolean
	 */
	public function getParamShowVoipProvider()
	{
		return $this->param_show_voip_provider;
	}

	/**
	 * Get the [param_show_communication_channel] column value.
	 * 
	 * @return     boolean
	 */
	public function getParamShowCommunicationChannel()
	{
		return $this->param_show_communication_channel;
	}

	/**
	 * Get the [param_show_geographic_location] column value.
	 * 
	 * @return     boolean
	 */
	public function getParamShowGeographicLocation()
	{
		return $this->param_show_geographic_location;
	}

	/**
	 * Get the [param_show_connection_type] column value.
	 * 
	 * @return     boolean
	 */
	public function getParamShowConnectionType()
	{
		return $this->param_show_connection_type;
	}

	/**
	 * Get the [param_show_cost_saving] column value.
	 * 
	 * @return     boolean
	 */
	public function getParamShowCostSaving()
	{
		return $this->param_show_cost_saving;
	}

	/**
	 * Get the [param_is_legal] column value.
	 * 
	 * @return     boolean
	 */
	public function getParamIsLegal()
	{
		return $this->param_is_legal;
	}

	/**
	 * Get the [param_expand_to_level] column value.
	 * 
	 * @return     int
	 */
	public function getParamExpandToLevel()
	{
		return $this->param_expand_to_level;
	}

	/**
	 * Get the [ar_report_order_of_children_id] column value.
	 * 
	 * @return     int
	 */
	public function getArReportOrderOfChildrenId()
	{
		return $this->ar_report_order_of_children_id;
	}

	/**
	 * Get the [php_class_name] column value.
	 * 
	 * @return     string
	 */
	public function getPhpClassName()
	{
		return $this->php_class_name;
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
	 * Get the [report_name] column value.
	 * 
	 * @return     string
	 */
	public function getReportName()
	{
		return $this->report_name;
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
	 * Get the [produced_report_must_be_regenerated] column value.
	 * 
	 * @return     boolean
	 */
	public function getProducedReportMustBeRegenerated()
	{
		return $this->produced_report_must_be_regenerated;
	}

	/**
	 * Get the [produced_report_mime_type] column value.
	 * 
	 * @return     string
	 */
	public function getProducedReportMimeType()
	{
		return $this->produced_report_mime_type;
	}

	/**
	 * Get the [produced_report_file_type_suffix] column value.
	 * 
	 * @return     string
	 */
	public function getProducedReportFileTypeSuffix()
	{
		return $this->produced_report_file_type_suffix;
	}

	/**
	 * Get the [produced_report_document] column value.
	 * 
	 * @return     resource
	 */
	public function getProducedReportDocument()
	{
		return $this->produced_report_document;
	}

	/**
	 * Get the [produced_report_document_checksum] column value.
	 * 
	 * @return     string
	 */
	public function getProducedReportDocumentChecksum()
	{
		return $this->produced_report_document_checksum;
	}

	/**
	 * Get the [report_mail_subject] column value.
	 * 
	 * @return     string
	 */
	public function getReportMailSubject()
	{
		return $this->report_mail_subject;
	}

	/**
	 * Get the [report_mail_body] column value.
	 * 
	 * @return     string
	 */
	public function getReportMailBody()
	{
		return $this->report_mail_body;
	}

	/**
	 * Get the [report_attachment_file_name] column value.
	 * 
	 * @return     string
	 */
	public function getReportAttachmentFileName()
	{
		return $this->report_attachment_file_name;
	}

	/**
	 * Get the [report_attachment_file_name_add_report_date] column value.
	 * 
	 * @return     boolean
	 */
	public function getReportAttachmentFileNameAddReportDate()
	{
		return $this->report_attachment_file_name_add_report_date;
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
	 * Get the [cached_parent_id_hierarchy] column value.
	 * 
	 * @return     string
	 */
	public function getCachedParentIdHierarchy()
	{
		return $this->cached_parent_id_hierarchy;
	}

	/**
	 * Get the [legal_nr_prefix] column value.
	 * 
	 * @return     string
	 */
	public function getLegalNrPrefix()
	{
		return $this->legal_nr_prefix;
	}

	/**
	 * Get the [legal_consecutive_nr] column value.
	 * 
	 * @return     int
	 */
	public function getLegalConsecutiveNr()
	{
		return $this->legal_consecutive_nr;
	}

	/**
	 * Get the [optionally formatted] temporal [legal_date] column value.
	 * 
	 *
	 * @param      string $format The date/time format string (either date()-style or strftime()-style).
	 *							If format is NULL, then the raw DateTime object will be returned.
	 * @return     mixed Formatted date/time value as string or DateTime object (if format is NULL), NULL if column is NULL, and 0 if column value is 0000-00-00
	 * @throws     PropelException - if unable to parse/validate the date/time value.
	 */
	public function getLegalDate($format = 'Y-m-d')
	{
		if ($this->legal_date === null) {
			return null;
		}


		if ($this->legal_date === '0000-00-00') {
			// while technically this is not a default value of NULL,
			// this seems to be closest in meaning.
			return null;
		} else {
			try {
				$dt = new DateTime($this->legal_date);
			} catch (Exception $x) {
				throw new PropelException("Internally stored date/time/timestamp value could not be converted to DateTime: " . var_export($this->legal_date, true), $x);
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
	 * Get the [legal_sender_name] column value.
	 * 
	 * @return     string
	 */
	public function getLegalSenderName()
	{
		return $this->legal_sender_name;
	}

	/**
	 * Get the [legal_sender_vat] column value.
	 * 
	 * @return     string
	 */
	public function getLegalSenderVat()
	{
		return $this->legal_sender_vat;
	}

	/**
	 * Get the [legal_sender_address] column value.
	 * 
	 * @return     string
	 */
	public function getLegalSenderAddress()
	{
		return $this->legal_sender_address;
	}

	/**
	 * Get the [legal_receiver_name] column value.
	 * 
	 * @return     string
	 */
	public function getLegalReceiverName()
	{
		return $this->legal_receiver_name;
	}

	/**
	 * Get the [legal_receiver_vat] column value.
	 * 
	 * @return     string
	 */
	public function getLegalReceiverVat()
	{
		return $this->legal_receiver_vat;
	}

	/**
	 * Get the [legal_receiver_address] column value.
	 * 
	 * @return     string
	 */
	public function getLegalReceiverAddress()
	{
		return $this->legal_receiver_address;
	}

	/**
	 * Get the [total_without_tax] column value.
	 * 
	 * @return     string
	 */
	public function getTotalWithoutTax()
	{
		return $this->total_without_tax;
	}

	/**
	 * Get the [tax] column value.
	 * 
	 * @return     string
	 */
	public function getTax()
	{
		return $this->tax;
	}

	/**
	 * Get the [applied_vat] column value.
	 * 
	 * @return     string
	 */
	public function getAppliedVat()
	{
		return $this->applied_vat;
	}

	/**
	 * Get the [total_with_tax] column value.
	 * 
	 * @return     string
	 */
	public function getTotalWithTax()
	{
		return $this->total_with_tax;
	}

	/**
	 * Set the value of [id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->id !== $v) {
			$this->id = $v;
			$this->modifiedColumns[] = ArReportPeer::ID;
		}

		return $this;
	} // setId()

	/**
	 * Set the value of [is_template] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setIsTemplate($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->is_template !== $v || $this->isNew()) {
			$this->is_template = $v;
			$this->modifiedColumns[] = ArReportPeer::IS_TEMPLATE;
		}

		return $this;
	} // setIsTemplate()

	/**
	 * Set the value of [ar_report_set_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setArReportSetId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->ar_report_set_id !== $v) {
			$this->ar_report_set_id = $v;
			$this->modifiedColumns[] = ArReportPeer::AR_REPORT_SET_ID;
		}

		if ($this->aArReportSet !== null && $this->aArReportSet->getId() !== $v) {
			$this->aArReportSet = null;
		}

		return $this;
	} // setArReportSetId()

	/**
	 * Set the value of [ar_organization_unit_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setArOrganizationUnitId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->ar_organization_unit_id !== $v) {
			$this->ar_organization_unit_id = $v;
			$this->modifiedColumns[] = ArReportPeer::AR_ORGANIZATION_UNIT_ID;
		}

		if ($this->aArOrganizationUnit !== null && $this->aArOrganizationUnit->getId() !== $v) {
			$this->aArOrganizationUnit = null;
		}

		return $this;
	} // setArOrganizationUnitId()

	/**
	 * Set the value of [ar_user_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setArUserId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->ar_user_id !== $v) {
			$this->ar_user_id = $v;
			$this->modifiedColumns[] = ArReportPeer::AR_USER_ID;
		}

		if ($this->aArUser !== null && $this->aArUser->getId() !== $v) {
			$this->aArUser = null;
		}

		return $this;
	} // setArUserId()

	/**
	 * Set the value of [ar_vendor_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setArVendorId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->ar_vendor_id !== $v) {
			$this->ar_vendor_id = $v;
			$this->modifiedColumns[] = ArReportPeer::AR_VENDOR_ID;
		}

		if ($this->aArVendor !== null && $this->aArVendor->getId() !== $v) {
			$this->aArVendor = null;
		}

		return $this;
	} // setArVendorId()

	/**
	 * Sets the value of [from_date] column to a normalized version of the date/time value specified.
	 * 
	 * @param      mixed $v string, integer (timestamp), or DateTime value.  Empty string will
	 *						be treated as NULL for temporal objects.
	 * @return     ArReport The current object (for fluent API support)
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
				$this->modifiedColumns[] = ArReportPeer::FROM_DATE;
			}
		} // if either are not null

		return $this;
	} // setFromDate()

	/**
	 * Sets the value of [to_date] column to a normalized version of the date/time value specified.
	 * 
	 * @param      mixed $v string, integer (timestamp), or DateTime value.  Empty string will
	 *						be treated as NULL for temporal objects.
	 * @return     ArReport The current object (for fluent API support)
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
				$this->modifiedColumns[] = ArReportPeer::TO_DATE;
			}
		} // if either are not null

		return $this;
	} // setToDate()

	/**
	 * Set the value of [param_show_masked_telephone_numbers] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setParamShowMaskedTelephoneNumbers($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->param_show_masked_telephone_numbers !== $v || $this->isNew()) {
			$this->param_show_masked_telephone_numbers = $v;
			$this->modifiedColumns[] = ArReportPeer::PARAM_SHOW_MASKED_TELEPHONE_NUMBERS;
		}

		return $this;
	} // setParamShowMaskedTelephoneNumbers()

	/**
	 * Set the value of [param_show_call_cost] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setParamShowCallCost($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->param_show_call_cost !== $v || $this->isNew()) {
			$this->param_show_call_cost = $v;
			$this->modifiedColumns[] = ArReportPeer::PARAM_SHOW_CALL_COST;
		}

		return $this;
	} // setParamShowCallCost()

	/**
	 * Set the value of [param_show_call_income] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setParamShowCallIncome($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->param_show_call_income !== $v || $this->isNew()) {
			$this->param_show_call_income = $v;
			$this->modifiedColumns[] = ArReportPeer::PARAM_SHOW_CALL_INCOME;
		}

		return $this;
	} // setParamShowCallIncome()

	/**
	 * Set the value of [param_show_also_outgoing_calls] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setParamShowAlsoOutgoingCalls($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->param_show_also_outgoing_calls !== $v || $this->isNew()) {
			$this->param_show_also_outgoing_calls = $v;
			$this->modifiedColumns[] = ArReportPeer::PARAM_SHOW_ALSO_OUTGOING_CALLS;
		}

		return $this;
	} // setParamShowAlsoOutgoingCalls()

	/**
	 * Set the value of [param_show_also_system_calls] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setParamShowAlsoSystemCalls($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->param_show_also_system_calls !== $v || $this->isNew()) {
			$this->param_show_also_system_calls = $v;
			$this->modifiedColumns[] = ArReportPeer::PARAM_SHOW_ALSO_SYSTEM_CALLS;
		}

		return $this;
	} // setParamShowAlsoSystemCalls()

	/**
	 * Set the value of [param_show_also_incoming_calls] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setParamShowAlsoIncomingCalls($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->param_show_also_incoming_calls !== $v || $this->isNew()) {
			$this->param_show_also_incoming_calls = $v;
			$this->modifiedColumns[] = ArReportPeer::PARAM_SHOW_ALSO_INCOMING_CALLS;
		}

		return $this;
	} // setParamShowAlsoIncomingCalls()

	/**
	 * Set the value of [param_show_also_internal_calls] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setParamShowAlsoInternalCalls($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->param_show_also_internal_calls !== $v || $this->isNew()) {
			$this->param_show_also_internal_calls = $v;
			$this->modifiedColumns[] = ArReportPeer::PARAM_SHOW_ALSO_INTERNAL_CALLS;
		}

		return $this;
	} // setParamShowAlsoInternalCalls()

	/**
	 * Set the value of [param_show_call_details] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setParamShowCallDetails($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->param_show_call_details !== $v || $this->isNew()) {
			$this->param_show_call_details = $v;
			$this->modifiedColumns[] = ArReportPeer::PARAM_SHOW_CALL_DETAILS;
		}

		return $this;
	} // setParamShowCallDetails()

	/**
	 * Set the value of [param_show_voip_provider] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setParamShowVoipProvider($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->param_show_voip_provider !== $v || $this->isNew()) {
			$this->param_show_voip_provider = $v;
			$this->modifiedColumns[] = ArReportPeer::PARAM_SHOW_VOIP_PROVIDER;
		}

		return $this;
	} // setParamShowVoipProvider()

	/**
	 * Set the value of [param_show_communication_channel] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setParamShowCommunicationChannel($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->param_show_communication_channel !== $v || $this->isNew()) {
			$this->param_show_communication_channel = $v;
			$this->modifiedColumns[] = ArReportPeer::PARAM_SHOW_COMMUNICATION_CHANNEL;
		}

		return $this;
	} // setParamShowCommunicationChannel()

	/**
	 * Set the value of [param_show_geographic_location] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setParamShowGeographicLocation($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->param_show_geographic_location !== $v || $this->isNew()) {
			$this->param_show_geographic_location = $v;
			$this->modifiedColumns[] = ArReportPeer::PARAM_SHOW_GEOGRAPHIC_LOCATION;
		}

		return $this;
	} // setParamShowGeographicLocation()

	/**
	 * Set the value of [param_show_connection_type] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setParamShowConnectionType($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->param_show_connection_type !== $v || $this->isNew()) {
			$this->param_show_connection_type = $v;
			$this->modifiedColumns[] = ArReportPeer::PARAM_SHOW_CONNECTION_TYPE;
		}

		return $this;
	} // setParamShowConnectionType()

	/**
	 * Set the value of [param_show_cost_saving] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setParamShowCostSaving($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->param_show_cost_saving !== $v || $this->isNew()) {
			$this->param_show_cost_saving = $v;
			$this->modifiedColumns[] = ArReportPeer::PARAM_SHOW_COST_SAVING;
		}

		return $this;
	} // setParamShowCostSaving()

	/**
	 * Set the value of [param_is_legal] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setParamIsLegal($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->param_is_legal !== $v || $this->isNew()) {
			$this->param_is_legal = $v;
			$this->modifiedColumns[] = ArReportPeer::PARAM_IS_LEGAL;
		}

		return $this;
	} // setParamIsLegal()

	/**
	 * Set the value of [param_expand_to_level] column.
	 * 
	 * @param      int $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setParamExpandToLevel($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->param_expand_to_level !== $v || $this->isNew()) {
			$this->param_expand_to_level = $v;
			$this->modifiedColumns[] = ArReportPeer::PARAM_EXPAND_TO_LEVEL;
		}

		return $this;
	} // setParamExpandToLevel()

	/**
	 * Set the value of [ar_report_order_of_children_id] column.
	 * 
	 * @param      int $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setArReportOrderOfChildrenId($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->ar_report_order_of_children_id !== $v) {
			$this->ar_report_order_of_children_id = $v;
			$this->modifiedColumns[] = ArReportPeer::AR_REPORT_ORDER_OF_CHILDREN_ID;
		}

		if ($this->aArReportOrderOfChildren !== null && $this->aArReportOrderOfChildren->getId() !== $v) {
			$this->aArReportOrderOfChildren = null;
		}

		return $this;
	} // setArReportOrderOfChildrenId()

	/**
	 * Set the value of [php_class_name] column.
	 * 
	 * @param      string $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setPhpClassName($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->php_class_name !== $v) {
			$this->php_class_name = $v;
			$this->modifiedColumns[] = ArReportPeer::PHP_CLASS_NAME;
		}

		return $this;
	} // setPhpClassName()

	/**
	 * Sets the value of [produced_report_generation_date] column to a normalized version of the date/time value specified.
	 * 
	 * @param      mixed $v string, integer (timestamp), or DateTime value.  Empty string will
	 *						be treated as NULL for temporal objects.
	 * @return     ArReport The current object (for fluent API support)
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
				$this->modifiedColumns[] = ArReportPeer::PRODUCED_REPORT_GENERATION_DATE;
			}
		} // if either are not null

		return $this;
	} // setProducedReportGenerationDate()

	/**
	 * Set the value of [report_name] column.
	 * 
	 * @param      string $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setReportName($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->report_name !== $v) {
			$this->report_name = $v;
			$this->modifiedColumns[] = ArReportPeer::REPORT_NAME;
		}

		return $this;
	} // setReportName()

	/**
	 * Set the value of [produced_report_short_description] column.
	 * 
	 * @param      string $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setProducedReportShortDescription($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->produced_report_short_description !== $v) {
			$this->produced_report_short_description = $v;
			$this->modifiedColumns[] = ArReportPeer::PRODUCED_REPORT_SHORT_DESCRIPTION;
		}

		return $this;
	} // setProducedReportShortDescription()

	/**
	 * Set the value of [produced_report_additional_description] column.
	 * 
	 * @param      string $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setProducedReportAdditionalDescription($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->produced_report_additional_description !== $v) {
			$this->produced_report_additional_description = $v;
			$this->modifiedColumns[] = ArReportPeer::PRODUCED_REPORT_ADDITIONAL_DESCRIPTION;
		}

		return $this;
	} // setProducedReportAdditionalDescription()

	/**
	 * Set the value of [produced_report_already_reviewed] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setProducedReportAlreadyReviewed($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->produced_report_already_reviewed !== $v || $this->isNew()) {
			$this->produced_report_already_reviewed = $v;
			$this->modifiedColumns[] = ArReportPeer::PRODUCED_REPORT_ALREADY_REVIEWED;
		}

		return $this;
	} // setProducedReportAlreadyReviewed()

	/**
	 * Set the value of [produced_report_is_draft] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setProducedReportIsDraft($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->produced_report_is_draft !== $v || $this->isNew()) {
			$this->produced_report_is_draft = $v;
			$this->modifiedColumns[] = ArReportPeer::PRODUCED_REPORT_IS_DRAFT;
		}

		return $this;
	} // setProducedReportIsDraft()

	/**
	 * Set the value of [produced_report_must_be_regenerated] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setProducedReportMustBeRegenerated($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->produced_report_must_be_regenerated !== $v || $this->isNew()) {
			$this->produced_report_must_be_regenerated = $v;
			$this->modifiedColumns[] = ArReportPeer::PRODUCED_REPORT_MUST_BE_REGENERATED;
		}

		return $this;
	} // setProducedReportMustBeRegenerated()

	/**
	 * Set the value of [produced_report_mime_type] column.
	 * 
	 * @param      string $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setProducedReportMimeType($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->produced_report_mime_type !== $v || $this->isNew()) {
			$this->produced_report_mime_type = $v;
			$this->modifiedColumns[] = ArReportPeer::PRODUCED_REPORT_MIME_TYPE;
		}

		return $this;
	} // setProducedReportMimeType()

	/**
	 * Set the value of [produced_report_file_type_suffix] column.
	 * 
	 * @param      string $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setProducedReportFileTypeSuffix($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->produced_report_file_type_suffix !== $v || $this->isNew()) {
			$this->produced_report_file_type_suffix = $v;
			$this->modifiedColumns[] = ArReportPeer::PRODUCED_REPORT_FILE_TYPE_SUFFIX;
		}

		return $this;
	} // setProducedReportFileTypeSuffix()

	/**
	 * Set the value of [produced_report_document] column.
	 * 
	 * @param      resource $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setProducedReportDocument($v)
	{
		// Because BLOB columns are streams in PDO we have to assume that they are
		// always modified when a new value is passed in.  For example, the contents
		// of the stream itself may have changed externally.
		if (!is_resource($v) && $v !== null) {
			$this->produced_report_document = fopen('php://memory', 'r+');
			fwrite($this->produced_report_document, $v);
			rewind($this->produced_report_document);
		} else { // it's already a stream
			$this->produced_report_document = $v;
		}
		$this->modifiedColumns[] = ArReportPeer::PRODUCED_REPORT_DOCUMENT;

		return $this;
	} // setProducedReportDocument()

	/**
	 * Set the value of [produced_report_document_checksum] column.
	 * 
	 * @param      string $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setProducedReportDocumentChecksum($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->produced_report_document_checksum !== $v) {
			$this->produced_report_document_checksum = $v;
			$this->modifiedColumns[] = ArReportPeer::PRODUCED_REPORT_DOCUMENT_CHECKSUM;
		}

		return $this;
	} // setProducedReportDocumentChecksum()

	/**
	 * Set the value of [report_mail_subject] column.
	 * 
	 * @param      string $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setReportMailSubject($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->report_mail_subject !== $v) {
			$this->report_mail_subject = $v;
			$this->modifiedColumns[] = ArReportPeer::REPORT_MAIL_SUBJECT;
		}

		return $this;
	} // setReportMailSubject()

	/**
	 * Set the value of [report_mail_body] column.
	 * 
	 * @param      string $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setReportMailBody($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->report_mail_body !== $v) {
			$this->report_mail_body = $v;
			$this->modifiedColumns[] = ArReportPeer::REPORT_MAIL_BODY;
		}

		return $this;
	} // setReportMailBody()

	/**
	 * Set the value of [report_attachment_file_name] column.
	 * 
	 * @param      string $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setReportAttachmentFileName($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->report_attachment_file_name !== $v) {
			$this->report_attachment_file_name = $v;
			$this->modifiedColumns[] = ArReportPeer::REPORT_ATTACHMENT_FILE_NAME;
		}

		return $this;
	} // setReportAttachmentFileName()

	/**
	 * Set the value of [report_attachment_file_name_add_report_date] column.
	 * 
	 * @param      boolean $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setReportAttachmentFileNameAddReportDate($v)
	{
		if ($v !== null) {
			$v = (boolean) $v;
		}

		if ($this->report_attachment_file_name_add_report_date !== $v || $this->isNew()) {
			$this->report_attachment_file_name_add_report_date = $v;
			$this->modifiedColumns[] = ArReportPeer::REPORT_ATTACHMENT_FILE_NAME_ADD_REPORT_DATE;
		}

		return $this;
	} // setReportAttachmentFileNameAddReportDate()

	/**
	 * Set the value of [internal_name] column.
	 * 
	 * @param      string $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setInternalName($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->internal_name !== $v) {
			$this->internal_name = $v;
			$this->modifiedColumns[] = ArReportPeer::INTERNAL_NAME;
		}

		return $this;
	} // setInternalName()

	/**
	 * Set the value of [cached_parent_id_hierarchy] column.
	 * 
	 * @param      string $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setCachedParentIdHierarchy($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->cached_parent_id_hierarchy !== $v) {
			$this->cached_parent_id_hierarchy = $v;
			$this->modifiedColumns[] = ArReportPeer::CACHED_PARENT_ID_HIERARCHY;
		}

		return $this;
	} // setCachedParentIdHierarchy()

	/**
	 * Set the value of [legal_nr_prefix] column.
	 * 
	 * @param      string $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setLegalNrPrefix($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->legal_nr_prefix !== $v) {
			$this->legal_nr_prefix = $v;
			$this->modifiedColumns[] = ArReportPeer::LEGAL_NR_PREFIX;
		}

		return $this;
	} // setLegalNrPrefix()

	/**
	 * Set the value of [legal_consecutive_nr] column.
	 * 
	 * @param      int $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setLegalConsecutiveNr($v)
	{
		if ($v !== null) {
			$v = (int) $v;
		}

		if ($this->legal_consecutive_nr !== $v) {
			$this->legal_consecutive_nr = $v;
			$this->modifiedColumns[] = ArReportPeer::LEGAL_CONSECUTIVE_NR;
		}

		return $this;
	} // setLegalConsecutiveNr()

	/**
	 * Sets the value of [legal_date] column to a normalized version of the date/time value specified.
	 * 
	 * @param      mixed $v string, integer (timestamp), or DateTime value.  Empty string will
	 *						be treated as NULL for temporal objects.
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setLegalDate($v)
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

		if ( $this->legal_date !== null || $dt !== null ) {
			// (nested ifs are a little easier to read in this case)

			$currNorm = ($this->legal_date !== null && $tmpDt = new DateTime($this->legal_date)) ? $tmpDt->format('Y-m-d') : null;
			$newNorm = ($dt !== null) ? $dt->format('Y-m-d') : null;

			if ( ($currNorm !== $newNorm) // normalized values don't match 
					)
			{
				$this->legal_date = ($dt ? $dt->format('Y-m-d') : null);
				$this->modifiedColumns[] = ArReportPeer::LEGAL_DATE;
			}
		} // if either are not null

		return $this;
	} // setLegalDate()

	/**
	 * Set the value of [legal_sender_name] column.
	 * 
	 * @param      string $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setLegalSenderName($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->legal_sender_name !== $v) {
			$this->legal_sender_name = $v;
			$this->modifiedColumns[] = ArReportPeer::LEGAL_SENDER_NAME;
		}

		return $this;
	} // setLegalSenderName()

	/**
	 * Set the value of [legal_sender_vat] column.
	 * 
	 * @param      string $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setLegalSenderVat($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->legal_sender_vat !== $v) {
			$this->legal_sender_vat = $v;
			$this->modifiedColumns[] = ArReportPeer::LEGAL_SENDER_VAT;
		}

		return $this;
	} // setLegalSenderVat()

	/**
	 * Set the value of [legal_sender_address] column.
	 * 
	 * @param      string $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setLegalSenderAddress($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->legal_sender_address !== $v) {
			$this->legal_sender_address = $v;
			$this->modifiedColumns[] = ArReportPeer::LEGAL_SENDER_ADDRESS;
		}

		return $this;
	} // setLegalSenderAddress()

	/**
	 * Set the value of [legal_receiver_name] column.
	 * 
	 * @param      string $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setLegalReceiverName($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->legal_receiver_name !== $v) {
			$this->legal_receiver_name = $v;
			$this->modifiedColumns[] = ArReportPeer::LEGAL_RECEIVER_NAME;
		}

		return $this;
	} // setLegalReceiverName()

	/**
	 * Set the value of [legal_receiver_vat] column.
	 * 
	 * @param      string $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setLegalReceiverVat($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->legal_receiver_vat !== $v) {
			$this->legal_receiver_vat = $v;
			$this->modifiedColumns[] = ArReportPeer::LEGAL_RECEIVER_VAT;
		}

		return $this;
	} // setLegalReceiverVat()

	/**
	 * Set the value of [legal_receiver_address] column.
	 * 
	 * @param      string $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setLegalReceiverAddress($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->legal_receiver_address !== $v) {
			$this->legal_receiver_address = $v;
			$this->modifiedColumns[] = ArReportPeer::LEGAL_RECEIVER_ADDRESS;
		}

		return $this;
	} // setLegalReceiverAddress()

	/**
	 * Set the value of [total_without_tax] column.
	 * 
	 * @param      string $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setTotalWithoutTax($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->total_without_tax !== $v) {
			$this->total_without_tax = $v;
			$this->modifiedColumns[] = ArReportPeer::TOTAL_WITHOUT_TAX;
		}

		return $this;
	} // setTotalWithoutTax()

	/**
	 * Set the value of [tax] column.
	 * 
	 * @param      string $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setTax($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->tax !== $v) {
			$this->tax = $v;
			$this->modifiedColumns[] = ArReportPeer::TAX;
		}

		return $this;
	} // setTax()

	/**
	 * Set the value of [applied_vat] column.
	 * 
	 * @param      string $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setAppliedVat($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->applied_vat !== $v) {
			$this->applied_vat = $v;
			$this->modifiedColumns[] = ArReportPeer::APPLIED_VAT;
		}

		return $this;
	} // setAppliedVat()

	/**
	 * Set the value of [total_with_tax] column.
	 * 
	 * @param      string $v new value
	 * @return     ArReport The current object (for fluent API support)
	 */
	public function setTotalWithTax($v)
	{
		if ($v !== null) {
			$v = (string) $v;
		}

		if ($this->total_with_tax !== $v) {
			$this->total_with_tax = $v;
			$this->modifiedColumns[] = ArReportPeer::TOTAL_WITH_TAX;
		}

		return $this;
	} // setTotalWithTax()

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
			if ($this->is_template !== false) {
				return false;
			}

			if ($this->param_show_masked_telephone_numbers !== true) {
				return false;
			}

			if ($this->param_show_call_cost !== false) {
				return false;
			}

			if ($this->param_show_call_income !== false) {
				return false;
			}

			if ($this->param_show_also_outgoing_calls !== false) {
				return false;
			}

			if ($this->param_show_also_system_calls !== false) {
				return false;
			}

			if ($this->param_show_also_incoming_calls !== false) {
				return false;
			}

			if ($this->param_show_also_internal_calls !== false) {
				return false;
			}

			if ($this->param_show_call_details !== false) {
				return false;
			}

			if ($this->param_show_voip_provider !== false) {
				return false;
			}

			if ($this->param_show_communication_channel !== false) {
				return false;
			}

			if ($this->param_show_geographic_location !== false) {
				return false;
			}

			if ($this->param_show_connection_type !== false) {
				return false;
			}

			if ($this->param_show_cost_saving !== false) {
				return false;
			}

			if ($this->param_is_legal !== false) {
				return false;
			}

			if ($this->param_expand_to_level !== 0) {
				return false;
			}

			if ($this->produced_report_already_reviewed !== false) {
				return false;
			}

			if ($this->produced_report_is_draft !== false) {
				return false;
			}

			if ($this->produced_report_must_be_regenerated !== false) {
				return false;
			}

			if ($this->produced_report_mime_type !== 'application/pdf') {
				return false;
			}

			if ($this->produced_report_file_type_suffix !== 'pdf') {
				return false;
			}

			if ($this->report_attachment_file_name_add_report_date !== false) {
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
			$this->is_template = ($row[$startcol + 1] !== null) ? (boolean) $row[$startcol + 1] : null;
			$this->ar_report_set_id = ($row[$startcol + 2] !== null) ? (int) $row[$startcol + 2] : null;
			$this->ar_organization_unit_id = ($row[$startcol + 3] !== null) ? (int) $row[$startcol + 3] : null;
			$this->ar_user_id = ($row[$startcol + 4] !== null) ? (int) $row[$startcol + 4] : null;
			$this->ar_vendor_id = ($row[$startcol + 5] !== null) ? (int) $row[$startcol + 5] : null;
			$this->from_date = ($row[$startcol + 6] !== null) ? (string) $row[$startcol + 6] : null;
			$this->to_date = ($row[$startcol + 7] !== null) ? (string) $row[$startcol + 7] : null;
			$this->param_show_masked_telephone_numbers = ($row[$startcol + 8] !== null) ? (boolean) $row[$startcol + 8] : null;
			$this->param_show_call_cost = ($row[$startcol + 9] !== null) ? (boolean) $row[$startcol + 9] : null;
			$this->param_show_call_income = ($row[$startcol + 10] !== null) ? (boolean) $row[$startcol + 10] : null;
			$this->param_show_also_outgoing_calls = ($row[$startcol + 11] !== null) ? (boolean) $row[$startcol + 11] : null;
			$this->param_show_also_system_calls = ($row[$startcol + 12] !== null) ? (boolean) $row[$startcol + 12] : null;
			$this->param_show_also_incoming_calls = ($row[$startcol + 13] !== null) ? (boolean) $row[$startcol + 13] : null;
			$this->param_show_also_internal_calls = ($row[$startcol + 14] !== null) ? (boolean) $row[$startcol + 14] : null;
			$this->param_show_call_details = ($row[$startcol + 15] !== null) ? (boolean) $row[$startcol + 15] : null;
			$this->param_show_voip_provider = ($row[$startcol + 16] !== null) ? (boolean) $row[$startcol + 16] : null;
			$this->param_show_communication_channel = ($row[$startcol + 17] !== null) ? (boolean) $row[$startcol + 17] : null;
			$this->param_show_geographic_location = ($row[$startcol + 18] !== null) ? (boolean) $row[$startcol + 18] : null;
			$this->param_show_connection_type = ($row[$startcol + 19] !== null) ? (boolean) $row[$startcol + 19] : null;
			$this->param_show_cost_saving = ($row[$startcol + 20] !== null) ? (boolean) $row[$startcol + 20] : null;
			$this->param_is_legal = ($row[$startcol + 21] !== null) ? (boolean) $row[$startcol + 21] : null;
			$this->param_expand_to_level = ($row[$startcol + 22] !== null) ? (int) $row[$startcol + 22] : null;
			$this->ar_report_order_of_children_id = ($row[$startcol + 23] !== null) ? (int) $row[$startcol + 23] : null;
			$this->php_class_name = ($row[$startcol + 24] !== null) ? (string) $row[$startcol + 24] : null;
			$this->produced_report_generation_date = ($row[$startcol + 25] !== null) ? (string) $row[$startcol + 25] : null;
			$this->report_name = ($row[$startcol + 26] !== null) ? (string) $row[$startcol + 26] : null;
			$this->produced_report_short_description = ($row[$startcol + 27] !== null) ? (string) $row[$startcol + 27] : null;
			$this->produced_report_additional_description = ($row[$startcol + 28] !== null) ? (string) $row[$startcol + 28] : null;
			$this->produced_report_already_reviewed = ($row[$startcol + 29] !== null) ? (boolean) $row[$startcol + 29] : null;
			$this->produced_report_is_draft = ($row[$startcol + 30] !== null) ? (boolean) $row[$startcol + 30] : null;
			$this->produced_report_must_be_regenerated = ($row[$startcol + 31] !== null) ? (boolean) $row[$startcol + 31] : null;
			$this->produced_report_mime_type = ($row[$startcol + 32] !== null) ? (string) $row[$startcol + 32] : null;
			$this->produced_report_file_type_suffix = ($row[$startcol + 33] !== null) ? (string) $row[$startcol + 33] : null;
			if ($row[$startcol + 34] !== null) {
				$this->produced_report_document = fopen('php://memory', 'r+');
				fwrite($this->produced_report_document, $row[$startcol + 34]);
				rewind($this->produced_report_document);
			} else {
				$this->produced_report_document = null;
			}
			$this->produced_report_document_checksum = ($row[$startcol + 35] !== null) ? (string) $row[$startcol + 35] : null;
			$this->report_mail_subject = ($row[$startcol + 36] !== null) ? (string) $row[$startcol + 36] : null;
			$this->report_mail_body = ($row[$startcol + 37] !== null) ? (string) $row[$startcol + 37] : null;
			$this->report_attachment_file_name = ($row[$startcol + 38] !== null) ? (string) $row[$startcol + 38] : null;
			$this->report_attachment_file_name_add_report_date = ($row[$startcol + 39] !== null) ? (boolean) $row[$startcol + 39] : null;
			$this->internal_name = ($row[$startcol + 40] !== null) ? (string) $row[$startcol + 40] : null;
			$this->cached_parent_id_hierarchy = ($row[$startcol + 41] !== null) ? (string) $row[$startcol + 41] : null;
			$this->legal_nr_prefix = ($row[$startcol + 42] !== null) ? (string) $row[$startcol + 42] : null;
			$this->legal_consecutive_nr = ($row[$startcol + 43] !== null) ? (int) $row[$startcol + 43] : null;
			$this->legal_date = ($row[$startcol + 44] !== null) ? (string) $row[$startcol + 44] : null;
			$this->legal_sender_name = ($row[$startcol + 45] !== null) ? (string) $row[$startcol + 45] : null;
			$this->legal_sender_vat = ($row[$startcol + 46] !== null) ? (string) $row[$startcol + 46] : null;
			$this->legal_sender_address = ($row[$startcol + 47] !== null) ? (string) $row[$startcol + 47] : null;
			$this->legal_receiver_name = ($row[$startcol + 48] !== null) ? (string) $row[$startcol + 48] : null;
			$this->legal_receiver_vat = ($row[$startcol + 49] !== null) ? (string) $row[$startcol + 49] : null;
			$this->legal_receiver_address = ($row[$startcol + 50] !== null) ? (string) $row[$startcol + 50] : null;
			$this->total_without_tax = ($row[$startcol + 51] !== null) ? (string) $row[$startcol + 51] : null;
			$this->tax = ($row[$startcol + 52] !== null) ? (string) $row[$startcol + 52] : null;
			$this->applied_vat = ($row[$startcol + 53] !== null) ? (string) $row[$startcol + 53] : null;
			$this->total_with_tax = ($row[$startcol + 54] !== null) ? (string) $row[$startcol + 54] : null;
			$this->resetModified();

			$this->setNew(false);

			if ($rehydrate) {
				$this->ensureConsistency();
			}

			// FIXME - using NUM_COLUMNS may be clearer.
			return $startcol + 55; // 55 = ArReportPeer::NUM_COLUMNS - ArReportPeer::NUM_LAZY_LOAD_COLUMNS).

		} catch (Exception $e) {
			throw new PropelException("Error populating ArReport object", $e);
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

		if ($this->aArReportSet !== null && $this->ar_report_set_id !== $this->aArReportSet->getId()) {
			$this->aArReportSet = null;
		}
		if ($this->aArOrganizationUnit !== null && $this->ar_organization_unit_id !== $this->aArOrganizationUnit->getId()) {
			$this->aArOrganizationUnit = null;
		}
		if ($this->aArUser !== null && $this->ar_user_id !== $this->aArUser->getId()) {
			$this->aArUser = null;
		}
		if ($this->aArVendor !== null && $this->ar_vendor_id !== $this->aArVendor->getId()) {
			$this->aArVendor = null;
		}
		if ($this->aArReportOrderOfChildren !== null && $this->ar_report_order_of_children_id !== $this->aArReportOrderOfChildren->getId()) {
			$this->aArReportOrderOfChildren = null;
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
			$con = Propel::getConnection(ArReportPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		// We don't need to alter the object instance pool; we're just modifying this instance
		// already in the pool.

		$stmt = ArReportPeer::doSelectStmt($this->buildPkeyCriteria(), $con);
		$row = $stmt->fetch(PDO::FETCH_NUM);
		$stmt->closeCursor();
		if (!$row) {
			throw new PropelException('Cannot find matching row in the database to reload object values.');
		}
		$this->hydrate($row, 0, true); // rehydrate

		if ($deep) {  // also de-associate any related objects?

			$this->aArReportSet = null;
			$this->aArOrganizationUnit = null;
			$this->aArUser = null;
			$this->aArVendor = null;
			$this->aArReportOrderOfChildren = null;
			$this->collArReportAlsoFors = null;
			$this->lastArReportAlsoForCriteria = null;

			$this->collArReportSchedulers = null;
			$this->lastArReportSchedulerCriteria = null;

			$this->collArReportToReads = null;
			$this->lastArReportToReadCriteria = null;

			$this->collArReportToReadUserViews = null;
			$this->lastArReportToReadUserViewCriteria = null;

			$this->collArUserCanViewReports = null;
			$this->lastArUserCanViewReportCriteria = null;

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
			$con = Propel::getConnection(ArReportPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}
		
		$con->beginTransaction();
		try {
			$ret = $this->preDelete($con);
			if ($ret) {
				ArReportPeer::doDelete($this, $con);
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
			$con = Propel::getConnection(ArReportPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
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
				ArReportPeer::addInstanceToPool($this);
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

			if ($this->aArReportSet !== null) {
				if ($this->aArReportSet->isModified() || $this->aArReportSet->isNew()) {
					$affectedRows += $this->aArReportSet->save($con);
				}
				$this->setArReportSet($this->aArReportSet);
			}

			if ($this->aArOrganizationUnit !== null) {
				if ($this->aArOrganizationUnit->isModified() || $this->aArOrganizationUnit->isNew()) {
					$affectedRows += $this->aArOrganizationUnit->save($con);
				}
				$this->setArOrganizationUnit($this->aArOrganizationUnit);
			}

			if ($this->aArUser !== null) {
				if ($this->aArUser->isModified() || $this->aArUser->isNew()) {
					$affectedRows += $this->aArUser->save($con);
				}
				$this->setArUser($this->aArUser);
			}

			if ($this->aArVendor !== null) {
				if ($this->aArVendor->isModified() || $this->aArVendor->isNew()) {
					$affectedRows += $this->aArVendor->save($con);
				}
				$this->setArVendor($this->aArVendor);
			}

			if ($this->aArReportOrderOfChildren !== null) {
				if ($this->aArReportOrderOfChildren->isModified() || $this->aArReportOrderOfChildren->isNew()) {
					$affectedRows += $this->aArReportOrderOfChildren->save($con);
				}
				$this->setArReportOrderOfChildren($this->aArReportOrderOfChildren);
			}

			if ($this->isNew() ) {
				$this->modifiedColumns[] = ArReportPeer::ID;
			}

			// If this object has been modified, then save it to the database.
			if ($this->isModified()) {
				if ($this->isNew()) {
					$pk = ArReportPeer::doInsert($this, $con);
					$affectedRows += 1; // we are assuming that there is only 1 row per doInsert() which
										 // should always be true here (even though technically
										 // BasePeer::doInsert() can insert multiple rows).

					$this->setId($pk);  //[IMV] update autoincrement primary key

					$this->setNew(false);
				} else {
					$affectedRows += ArReportPeer::doUpdate($this, $con);
				}

				// Rewind the produced_report_document LOB column, since PDO does not rewind after inserting value.
				if ($this->produced_report_document !== null && is_resource($this->produced_report_document)) {
					rewind($this->produced_report_document);
				}

				$this->resetModified(); // [HL] After being saved an object is no longer 'modified'
			}

			if ($this->collArReportAlsoFors !== null) {
				foreach ($this->collArReportAlsoFors as $referrerFK) {
					if (!$referrerFK->isDeleted()) {
						$affectedRows += $referrerFK->save($con);
					}
				}
			}

			if ($this->collArReportSchedulers !== null) {
				foreach ($this->collArReportSchedulers as $referrerFK) {
					if (!$referrerFK->isDeleted()) {
						$affectedRows += $referrerFK->save($con);
					}
				}
			}

			if ($this->collArReportToReads !== null) {
				foreach ($this->collArReportToReads as $referrerFK) {
					if (!$referrerFK->isDeleted()) {
						$affectedRows += $referrerFK->save($con);
					}
				}
			}

			if ($this->collArReportToReadUserViews !== null) {
				foreach ($this->collArReportToReadUserViews as $referrerFK) {
					if (!$referrerFK->isDeleted()) {
						$affectedRows += $referrerFK->save($con);
					}
				}
			}

			if ($this->collArUserCanViewReports !== null) {
				foreach ($this->collArUserCanViewReports as $referrerFK) {
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

			if ($this->aArReportSet !== null) {
				if (!$this->aArReportSet->validate($columns)) {
					$failureMap = array_merge($failureMap, $this->aArReportSet->getValidationFailures());
				}
			}

			if ($this->aArOrganizationUnit !== null) {
				if (!$this->aArOrganizationUnit->validate($columns)) {
					$failureMap = array_merge($failureMap, $this->aArOrganizationUnit->getValidationFailures());
				}
			}

			if ($this->aArUser !== null) {
				if (!$this->aArUser->validate($columns)) {
					$failureMap = array_merge($failureMap, $this->aArUser->getValidationFailures());
				}
			}

			if ($this->aArVendor !== null) {
				if (!$this->aArVendor->validate($columns)) {
					$failureMap = array_merge($failureMap, $this->aArVendor->getValidationFailures());
				}
			}

			if ($this->aArReportOrderOfChildren !== null) {
				if (!$this->aArReportOrderOfChildren->validate($columns)) {
					$failureMap = array_merge($failureMap, $this->aArReportOrderOfChildren->getValidationFailures());
				}
			}


			if (($retval = ArReportPeer::doValidate($this, $columns)) !== true) {
				$failureMap = array_merge($failureMap, $retval);
			}


				if ($this->collArReportAlsoFors !== null) {
					foreach ($this->collArReportAlsoFors as $referrerFK) {
						if (!$referrerFK->validate($columns)) {
							$failureMap = array_merge($failureMap, $referrerFK->getValidationFailures());
						}
					}
				}

				if ($this->collArReportSchedulers !== null) {
					foreach ($this->collArReportSchedulers as $referrerFK) {
						if (!$referrerFK->validate($columns)) {
							$failureMap = array_merge($failureMap, $referrerFK->getValidationFailures());
						}
					}
				}

				if ($this->collArReportToReads !== null) {
					foreach ($this->collArReportToReads as $referrerFK) {
						if (!$referrerFK->validate($columns)) {
							$failureMap = array_merge($failureMap, $referrerFK->getValidationFailures());
						}
					}
				}

				if ($this->collArReportToReadUserViews !== null) {
					foreach ($this->collArReportToReadUserViews as $referrerFK) {
						if (!$referrerFK->validate($columns)) {
							$failureMap = array_merge($failureMap, $referrerFK->getValidationFailures());
						}
					}
				}

				if ($this->collArUserCanViewReports !== null) {
					foreach ($this->collArUserCanViewReports as $referrerFK) {
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
		$pos = ArReportPeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
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
				return $this->getIsTemplate();
				break;
			case 2:
				return $this->getArReportSetId();
				break;
			case 3:
				return $this->getArOrganizationUnitId();
				break;
			case 4:
				return $this->getArUserId();
				break;
			case 5:
				return $this->getArVendorId();
				break;
			case 6:
				return $this->getFromDate();
				break;
			case 7:
				return $this->getToDate();
				break;
			case 8:
				return $this->getParamShowMaskedTelephoneNumbers();
				break;
			case 9:
				return $this->getParamShowCallCost();
				break;
			case 10:
				return $this->getParamShowCallIncome();
				break;
			case 11:
				return $this->getParamShowAlsoOutgoingCalls();
				break;
			case 12:
				return $this->getParamShowAlsoSystemCalls();
				break;
			case 13:
				return $this->getParamShowAlsoIncomingCalls();
				break;
			case 14:
				return $this->getParamShowAlsoInternalCalls();
				break;
			case 15:
				return $this->getParamShowCallDetails();
				break;
			case 16:
				return $this->getParamShowVoipProvider();
				break;
			case 17:
				return $this->getParamShowCommunicationChannel();
				break;
			case 18:
				return $this->getParamShowGeographicLocation();
				break;
			case 19:
				return $this->getParamShowConnectionType();
				break;
			case 20:
				return $this->getParamShowCostSaving();
				break;
			case 21:
				return $this->getParamIsLegal();
				break;
			case 22:
				return $this->getParamExpandToLevel();
				break;
			case 23:
				return $this->getArReportOrderOfChildrenId();
				break;
			case 24:
				return $this->getPhpClassName();
				break;
			case 25:
				return $this->getProducedReportGenerationDate();
				break;
			case 26:
				return $this->getReportName();
				break;
			case 27:
				return $this->getProducedReportShortDescription();
				break;
			case 28:
				return $this->getProducedReportAdditionalDescription();
				break;
			case 29:
				return $this->getProducedReportAlreadyReviewed();
				break;
			case 30:
				return $this->getProducedReportIsDraft();
				break;
			case 31:
				return $this->getProducedReportMustBeRegenerated();
				break;
			case 32:
				return $this->getProducedReportMimeType();
				break;
			case 33:
				return $this->getProducedReportFileTypeSuffix();
				break;
			case 34:
				return $this->getProducedReportDocument();
				break;
			case 35:
				return $this->getProducedReportDocumentChecksum();
				break;
			case 36:
				return $this->getReportMailSubject();
				break;
			case 37:
				return $this->getReportMailBody();
				break;
			case 38:
				return $this->getReportAttachmentFileName();
				break;
			case 39:
				return $this->getReportAttachmentFileNameAddReportDate();
				break;
			case 40:
				return $this->getInternalName();
				break;
			case 41:
				return $this->getCachedParentIdHierarchy();
				break;
			case 42:
				return $this->getLegalNrPrefix();
				break;
			case 43:
				return $this->getLegalConsecutiveNr();
				break;
			case 44:
				return $this->getLegalDate();
				break;
			case 45:
				return $this->getLegalSenderName();
				break;
			case 46:
				return $this->getLegalSenderVat();
				break;
			case 47:
				return $this->getLegalSenderAddress();
				break;
			case 48:
				return $this->getLegalReceiverName();
				break;
			case 49:
				return $this->getLegalReceiverVat();
				break;
			case 50:
				return $this->getLegalReceiverAddress();
				break;
			case 51:
				return $this->getTotalWithoutTax();
				break;
			case 52:
				return $this->getTax();
				break;
			case 53:
				return $this->getAppliedVat();
				break;
			case 54:
				return $this->getTotalWithTax();
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
		$keys = ArReportPeer::getFieldNames($keyType);
		$result = array(
			$keys[0] => $this->getId(),
			$keys[1] => $this->getIsTemplate(),
			$keys[2] => $this->getArReportSetId(),
			$keys[3] => $this->getArOrganizationUnitId(),
			$keys[4] => $this->getArUserId(),
			$keys[5] => $this->getArVendorId(),
			$keys[6] => $this->getFromDate(),
			$keys[7] => $this->getToDate(),
			$keys[8] => $this->getParamShowMaskedTelephoneNumbers(),
			$keys[9] => $this->getParamShowCallCost(),
			$keys[10] => $this->getParamShowCallIncome(),
			$keys[11] => $this->getParamShowAlsoOutgoingCalls(),
			$keys[12] => $this->getParamShowAlsoSystemCalls(),
			$keys[13] => $this->getParamShowAlsoIncomingCalls(),
			$keys[14] => $this->getParamShowAlsoInternalCalls(),
			$keys[15] => $this->getParamShowCallDetails(),
			$keys[16] => $this->getParamShowVoipProvider(),
			$keys[17] => $this->getParamShowCommunicationChannel(),
			$keys[18] => $this->getParamShowGeographicLocation(),
			$keys[19] => $this->getParamShowConnectionType(),
			$keys[20] => $this->getParamShowCostSaving(),
			$keys[21] => $this->getParamIsLegal(),
			$keys[22] => $this->getParamExpandToLevel(),
			$keys[23] => $this->getArReportOrderOfChildrenId(),
			$keys[24] => $this->getPhpClassName(),
			$keys[25] => $this->getProducedReportGenerationDate(),
			$keys[26] => $this->getReportName(),
			$keys[27] => $this->getProducedReportShortDescription(),
			$keys[28] => $this->getProducedReportAdditionalDescription(),
			$keys[29] => $this->getProducedReportAlreadyReviewed(),
			$keys[30] => $this->getProducedReportIsDraft(),
			$keys[31] => $this->getProducedReportMustBeRegenerated(),
			$keys[32] => $this->getProducedReportMimeType(),
			$keys[33] => $this->getProducedReportFileTypeSuffix(),
			$keys[34] => $this->getProducedReportDocument(),
			$keys[35] => $this->getProducedReportDocumentChecksum(),
			$keys[36] => $this->getReportMailSubject(),
			$keys[37] => $this->getReportMailBody(),
			$keys[38] => $this->getReportAttachmentFileName(),
			$keys[39] => $this->getReportAttachmentFileNameAddReportDate(),
			$keys[40] => $this->getInternalName(),
			$keys[41] => $this->getCachedParentIdHierarchy(),
			$keys[42] => $this->getLegalNrPrefix(),
			$keys[43] => $this->getLegalConsecutiveNr(),
			$keys[44] => $this->getLegalDate(),
			$keys[45] => $this->getLegalSenderName(),
			$keys[46] => $this->getLegalSenderVat(),
			$keys[47] => $this->getLegalSenderAddress(),
			$keys[48] => $this->getLegalReceiverName(),
			$keys[49] => $this->getLegalReceiverVat(),
			$keys[50] => $this->getLegalReceiverAddress(),
			$keys[51] => $this->getTotalWithoutTax(),
			$keys[52] => $this->getTax(),
			$keys[53] => $this->getAppliedVat(),
			$keys[54] => $this->getTotalWithTax(),
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
		$pos = ArReportPeer::translateFieldName($name, $type, BasePeer::TYPE_NUM);
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
				$this->setIsTemplate($value);
				break;
			case 2:
				$this->setArReportSetId($value);
				break;
			case 3:
				$this->setArOrganizationUnitId($value);
				break;
			case 4:
				$this->setArUserId($value);
				break;
			case 5:
				$this->setArVendorId($value);
				break;
			case 6:
				$this->setFromDate($value);
				break;
			case 7:
				$this->setToDate($value);
				break;
			case 8:
				$this->setParamShowMaskedTelephoneNumbers($value);
				break;
			case 9:
				$this->setParamShowCallCost($value);
				break;
			case 10:
				$this->setParamShowCallIncome($value);
				break;
			case 11:
				$this->setParamShowAlsoOutgoingCalls($value);
				break;
			case 12:
				$this->setParamShowAlsoSystemCalls($value);
				break;
			case 13:
				$this->setParamShowAlsoIncomingCalls($value);
				break;
			case 14:
				$this->setParamShowAlsoInternalCalls($value);
				break;
			case 15:
				$this->setParamShowCallDetails($value);
				break;
			case 16:
				$this->setParamShowVoipProvider($value);
				break;
			case 17:
				$this->setParamShowCommunicationChannel($value);
				break;
			case 18:
				$this->setParamShowGeographicLocation($value);
				break;
			case 19:
				$this->setParamShowConnectionType($value);
				break;
			case 20:
				$this->setParamShowCostSaving($value);
				break;
			case 21:
				$this->setParamIsLegal($value);
				break;
			case 22:
				$this->setParamExpandToLevel($value);
				break;
			case 23:
				$this->setArReportOrderOfChildrenId($value);
				break;
			case 24:
				$this->setPhpClassName($value);
				break;
			case 25:
				$this->setProducedReportGenerationDate($value);
				break;
			case 26:
				$this->setReportName($value);
				break;
			case 27:
				$this->setProducedReportShortDescription($value);
				break;
			case 28:
				$this->setProducedReportAdditionalDescription($value);
				break;
			case 29:
				$this->setProducedReportAlreadyReviewed($value);
				break;
			case 30:
				$this->setProducedReportIsDraft($value);
				break;
			case 31:
				$this->setProducedReportMustBeRegenerated($value);
				break;
			case 32:
				$this->setProducedReportMimeType($value);
				break;
			case 33:
				$this->setProducedReportFileTypeSuffix($value);
				break;
			case 34:
				$this->setProducedReportDocument($value);
				break;
			case 35:
				$this->setProducedReportDocumentChecksum($value);
				break;
			case 36:
				$this->setReportMailSubject($value);
				break;
			case 37:
				$this->setReportMailBody($value);
				break;
			case 38:
				$this->setReportAttachmentFileName($value);
				break;
			case 39:
				$this->setReportAttachmentFileNameAddReportDate($value);
				break;
			case 40:
				$this->setInternalName($value);
				break;
			case 41:
				$this->setCachedParentIdHierarchy($value);
				break;
			case 42:
				$this->setLegalNrPrefix($value);
				break;
			case 43:
				$this->setLegalConsecutiveNr($value);
				break;
			case 44:
				$this->setLegalDate($value);
				break;
			case 45:
				$this->setLegalSenderName($value);
				break;
			case 46:
				$this->setLegalSenderVat($value);
				break;
			case 47:
				$this->setLegalSenderAddress($value);
				break;
			case 48:
				$this->setLegalReceiverName($value);
				break;
			case 49:
				$this->setLegalReceiverVat($value);
				break;
			case 50:
				$this->setLegalReceiverAddress($value);
				break;
			case 51:
				$this->setTotalWithoutTax($value);
				break;
			case 52:
				$this->setTax($value);
				break;
			case 53:
				$this->setAppliedVat($value);
				break;
			case 54:
				$this->setTotalWithTax($value);
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
		$keys = ArReportPeer::getFieldNames($keyType);

		if (array_key_exists($keys[0], $arr)) $this->setId($arr[$keys[0]]);
		if (array_key_exists($keys[1], $arr)) $this->setIsTemplate($arr[$keys[1]]);
		if (array_key_exists($keys[2], $arr)) $this->setArReportSetId($arr[$keys[2]]);
		if (array_key_exists($keys[3], $arr)) $this->setArOrganizationUnitId($arr[$keys[3]]);
		if (array_key_exists($keys[4], $arr)) $this->setArUserId($arr[$keys[4]]);
		if (array_key_exists($keys[5], $arr)) $this->setArVendorId($arr[$keys[5]]);
		if (array_key_exists($keys[6], $arr)) $this->setFromDate($arr[$keys[6]]);
		if (array_key_exists($keys[7], $arr)) $this->setToDate($arr[$keys[7]]);
		if (array_key_exists($keys[8], $arr)) $this->setParamShowMaskedTelephoneNumbers($arr[$keys[8]]);
		if (array_key_exists($keys[9], $arr)) $this->setParamShowCallCost($arr[$keys[9]]);
		if (array_key_exists($keys[10], $arr)) $this->setParamShowCallIncome($arr[$keys[10]]);
		if (array_key_exists($keys[11], $arr)) $this->setParamShowAlsoOutgoingCalls($arr[$keys[11]]);
		if (array_key_exists($keys[12], $arr)) $this->setParamShowAlsoSystemCalls($arr[$keys[12]]);
		if (array_key_exists($keys[13], $arr)) $this->setParamShowAlsoIncomingCalls($arr[$keys[13]]);
		if (array_key_exists($keys[14], $arr)) $this->setParamShowAlsoInternalCalls($arr[$keys[14]]);
		if (array_key_exists($keys[15], $arr)) $this->setParamShowCallDetails($arr[$keys[15]]);
		if (array_key_exists($keys[16], $arr)) $this->setParamShowVoipProvider($arr[$keys[16]]);
		if (array_key_exists($keys[17], $arr)) $this->setParamShowCommunicationChannel($arr[$keys[17]]);
		if (array_key_exists($keys[18], $arr)) $this->setParamShowGeographicLocation($arr[$keys[18]]);
		if (array_key_exists($keys[19], $arr)) $this->setParamShowConnectionType($arr[$keys[19]]);
		if (array_key_exists($keys[20], $arr)) $this->setParamShowCostSaving($arr[$keys[20]]);
		if (array_key_exists($keys[21], $arr)) $this->setParamIsLegal($arr[$keys[21]]);
		if (array_key_exists($keys[22], $arr)) $this->setParamExpandToLevel($arr[$keys[22]]);
		if (array_key_exists($keys[23], $arr)) $this->setArReportOrderOfChildrenId($arr[$keys[23]]);
		if (array_key_exists($keys[24], $arr)) $this->setPhpClassName($arr[$keys[24]]);
		if (array_key_exists($keys[25], $arr)) $this->setProducedReportGenerationDate($arr[$keys[25]]);
		if (array_key_exists($keys[26], $arr)) $this->setReportName($arr[$keys[26]]);
		if (array_key_exists($keys[27], $arr)) $this->setProducedReportShortDescription($arr[$keys[27]]);
		if (array_key_exists($keys[28], $arr)) $this->setProducedReportAdditionalDescription($arr[$keys[28]]);
		if (array_key_exists($keys[29], $arr)) $this->setProducedReportAlreadyReviewed($arr[$keys[29]]);
		if (array_key_exists($keys[30], $arr)) $this->setProducedReportIsDraft($arr[$keys[30]]);
		if (array_key_exists($keys[31], $arr)) $this->setProducedReportMustBeRegenerated($arr[$keys[31]]);
		if (array_key_exists($keys[32], $arr)) $this->setProducedReportMimeType($arr[$keys[32]]);
		if (array_key_exists($keys[33], $arr)) $this->setProducedReportFileTypeSuffix($arr[$keys[33]]);
		if (array_key_exists($keys[34], $arr)) $this->setProducedReportDocument($arr[$keys[34]]);
		if (array_key_exists($keys[35], $arr)) $this->setProducedReportDocumentChecksum($arr[$keys[35]]);
		if (array_key_exists($keys[36], $arr)) $this->setReportMailSubject($arr[$keys[36]]);
		if (array_key_exists($keys[37], $arr)) $this->setReportMailBody($arr[$keys[37]]);
		if (array_key_exists($keys[38], $arr)) $this->setReportAttachmentFileName($arr[$keys[38]]);
		if (array_key_exists($keys[39], $arr)) $this->setReportAttachmentFileNameAddReportDate($arr[$keys[39]]);
		if (array_key_exists($keys[40], $arr)) $this->setInternalName($arr[$keys[40]]);
		if (array_key_exists($keys[41], $arr)) $this->setCachedParentIdHierarchy($arr[$keys[41]]);
		if (array_key_exists($keys[42], $arr)) $this->setLegalNrPrefix($arr[$keys[42]]);
		if (array_key_exists($keys[43], $arr)) $this->setLegalConsecutiveNr($arr[$keys[43]]);
		if (array_key_exists($keys[44], $arr)) $this->setLegalDate($arr[$keys[44]]);
		if (array_key_exists($keys[45], $arr)) $this->setLegalSenderName($arr[$keys[45]]);
		if (array_key_exists($keys[46], $arr)) $this->setLegalSenderVat($arr[$keys[46]]);
		if (array_key_exists($keys[47], $arr)) $this->setLegalSenderAddress($arr[$keys[47]]);
		if (array_key_exists($keys[48], $arr)) $this->setLegalReceiverName($arr[$keys[48]]);
		if (array_key_exists($keys[49], $arr)) $this->setLegalReceiverVat($arr[$keys[49]]);
		if (array_key_exists($keys[50], $arr)) $this->setLegalReceiverAddress($arr[$keys[50]]);
		if (array_key_exists($keys[51], $arr)) $this->setTotalWithoutTax($arr[$keys[51]]);
		if (array_key_exists($keys[52], $arr)) $this->setTax($arr[$keys[52]]);
		if (array_key_exists($keys[53], $arr)) $this->setAppliedVat($arr[$keys[53]]);
		if (array_key_exists($keys[54], $arr)) $this->setTotalWithTax($arr[$keys[54]]);
	}

	/**
	 * Build a Criteria object containing the values of all modified columns in this object.
	 *
	 * @return     Criteria The Criteria object containing all modified values.
	 */
	public function buildCriteria()
	{
		$criteria = new Criteria(ArReportPeer::DATABASE_NAME);

		if ($this->isColumnModified(ArReportPeer::ID)) $criteria->add(ArReportPeer::ID, $this->id);
		if ($this->isColumnModified(ArReportPeer::IS_TEMPLATE)) $criteria->add(ArReportPeer::IS_TEMPLATE, $this->is_template);
		if ($this->isColumnModified(ArReportPeer::AR_REPORT_SET_ID)) $criteria->add(ArReportPeer::AR_REPORT_SET_ID, $this->ar_report_set_id);
		if ($this->isColumnModified(ArReportPeer::AR_ORGANIZATION_UNIT_ID)) $criteria->add(ArReportPeer::AR_ORGANIZATION_UNIT_ID, $this->ar_organization_unit_id);
		if ($this->isColumnModified(ArReportPeer::AR_USER_ID)) $criteria->add(ArReportPeer::AR_USER_ID, $this->ar_user_id);
		if ($this->isColumnModified(ArReportPeer::AR_VENDOR_ID)) $criteria->add(ArReportPeer::AR_VENDOR_ID, $this->ar_vendor_id);
		if ($this->isColumnModified(ArReportPeer::FROM_DATE)) $criteria->add(ArReportPeer::FROM_DATE, $this->from_date);
		if ($this->isColumnModified(ArReportPeer::TO_DATE)) $criteria->add(ArReportPeer::TO_DATE, $this->to_date);
		if ($this->isColumnModified(ArReportPeer::PARAM_SHOW_MASKED_TELEPHONE_NUMBERS)) $criteria->add(ArReportPeer::PARAM_SHOW_MASKED_TELEPHONE_NUMBERS, $this->param_show_masked_telephone_numbers);
		if ($this->isColumnModified(ArReportPeer::PARAM_SHOW_CALL_COST)) $criteria->add(ArReportPeer::PARAM_SHOW_CALL_COST, $this->param_show_call_cost);
		if ($this->isColumnModified(ArReportPeer::PARAM_SHOW_CALL_INCOME)) $criteria->add(ArReportPeer::PARAM_SHOW_CALL_INCOME, $this->param_show_call_income);
		if ($this->isColumnModified(ArReportPeer::PARAM_SHOW_ALSO_OUTGOING_CALLS)) $criteria->add(ArReportPeer::PARAM_SHOW_ALSO_OUTGOING_CALLS, $this->param_show_also_outgoing_calls);
		if ($this->isColumnModified(ArReportPeer::PARAM_SHOW_ALSO_SYSTEM_CALLS)) $criteria->add(ArReportPeer::PARAM_SHOW_ALSO_SYSTEM_CALLS, $this->param_show_also_system_calls);
		if ($this->isColumnModified(ArReportPeer::PARAM_SHOW_ALSO_INCOMING_CALLS)) $criteria->add(ArReportPeer::PARAM_SHOW_ALSO_INCOMING_CALLS, $this->param_show_also_incoming_calls);
		if ($this->isColumnModified(ArReportPeer::PARAM_SHOW_ALSO_INTERNAL_CALLS)) $criteria->add(ArReportPeer::PARAM_SHOW_ALSO_INTERNAL_CALLS, $this->param_show_also_internal_calls);
		if ($this->isColumnModified(ArReportPeer::PARAM_SHOW_CALL_DETAILS)) $criteria->add(ArReportPeer::PARAM_SHOW_CALL_DETAILS, $this->param_show_call_details);
		if ($this->isColumnModified(ArReportPeer::PARAM_SHOW_VOIP_PROVIDER)) $criteria->add(ArReportPeer::PARAM_SHOW_VOIP_PROVIDER, $this->param_show_voip_provider);
		if ($this->isColumnModified(ArReportPeer::PARAM_SHOW_COMMUNICATION_CHANNEL)) $criteria->add(ArReportPeer::PARAM_SHOW_COMMUNICATION_CHANNEL, $this->param_show_communication_channel);
		if ($this->isColumnModified(ArReportPeer::PARAM_SHOW_GEOGRAPHIC_LOCATION)) $criteria->add(ArReportPeer::PARAM_SHOW_GEOGRAPHIC_LOCATION, $this->param_show_geographic_location);
		if ($this->isColumnModified(ArReportPeer::PARAM_SHOW_CONNECTION_TYPE)) $criteria->add(ArReportPeer::PARAM_SHOW_CONNECTION_TYPE, $this->param_show_connection_type);
		if ($this->isColumnModified(ArReportPeer::PARAM_SHOW_COST_SAVING)) $criteria->add(ArReportPeer::PARAM_SHOW_COST_SAVING, $this->param_show_cost_saving);
		if ($this->isColumnModified(ArReportPeer::PARAM_IS_LEGAL)) $criteria->add(ArReportPeer::PARAM_IS_LEGAL, $this->param_is_legal);
		if ($this->isColumnModified(ArReportPeer::PARAM_EXPAND_TO_LEVEL)) $criteria->add(ArReportPeer::PARAM_EXPAND_TO_LEVEL, $this->param_expand_to_level);
		if ($this->isColumnModified(ArReportPeer::AR_REPORT_ORDER_OF_CHILDREN_ID)) $criteria->add(ArReportPeer::AR_REPORT_ORDER_OF_CHILDREN_ID, $this->ar_report_order_of_children_id);
		if ($this->isColumnModified(ArReportPeer::PHP_CLASS_NAME)) $criteria->add(ArReportPeer::PHP_CLASS_NAME, $this->php_class_name);
		if ($this->isColumnModified(ArReportPeer::PRODUCED_REPORT_GENERATION_DATE)) $criteria->add(ArReportPeer::PRODUCED_REPORT_GENERATION_DATE, $this->produced_report_generation_date);
		if ($this->isColumnModified(ArReportPeer::REPORT_NAME)) $criteria->add(ArReportPeer::REPORT_NAME, $this->report_name);
		if ($this->isColumnModified(ArReportPeer::PRODUCED_REPORT_SHORT_DESCRIPTION)) $criteria->add(ArReportPeer::PRODUCED_REPORT_SHORT_DESCRIPTION, $this->produced_report_short_description);
		if ($this->isColumnModified(ArReportPeer::PRODUCED_REPORT_ADDITIONAL_DESCRIPTION)) $criteria->add(ArReportPeer::PRODUCED_REPORT_ADDITIONAL_DESCRIPTION, $this->produced_report_additional_description);
		if ($this->isColumnModified(ArReportPeer::PRODUCED_REPORT_ALREADY_REVIEWED)) $criteria->add(ArReportPeer::PRODUCED_REPORT_ALREADY_REVIEWED, $this->produced_report_already_reviewed);
		if ($this->isColumnModified(ArReportPeer::PRODUCED_REPORT_IS_DRAFT)) $criteria->add(ArReportPeer::PRODUCED_REPORT_IS_DRAFT, $this->produced_report_is_draft);
		if ($this->isColumnModified(ArReportPeer::PRODUCED_REPORT_MUST_BE_REGENERATED)) $criteria->add(ArReportPeer::PRODUCED_REPORT_MUST_BE_REGENERATED, $this->produced_report_must_be_regenerated);
		if ($this->isColumnModified(ArReportPeer::PRODUCED_REPORT_MIME_TYPE)) $criteria->add(ArReportPeer::PRODUCED_REPORT_MIME_TYPE, $this->produced_report_mime_type);
		if ($this->isColumnModified(ArReportPeer::PRODUCED_REPORT_FILE_TYPE_SUFFIX)) $criteria->add(ArReportPeer::PRODUCED_REPORT_FILE_TYPE_SUFFIX, $this->produced_report_file_type_suffix);
		if ($this->isColumnModified(ArReportPeer::PRODUCED_REPORT_DOCUMENT)) $criteria->add(ArReportPeer::PRODUCED_REPORT_DOCUMENT, $this->produced_report_document);
		if ($this->isColumnModified(ArReportPeer::PRODUCED_REPORT_DOCUMENT_CHECKSUM)) $criteria->add(ArReportPeer::PRODUCED_REPORT_DOCUMENT_CHECKSUM, $this->produced_report_document_checksum);
		if ($this->isColumnModified(ArReportPeer::REPORT_MAIL_SUBJECT)) $criteria->add(ArReportPeer::REPORT_MAIL_SUBJECT, $this->report_mail_subject);
		if ($this->isColumnModified(ArReportPeer::REPORT_MAIL_BODY)) $criteria->add(ArReportPeer::REPORT_MAIL_BODY, $this->report_mail_body);
		if ($this->isColumnModified(ArReportPeer::REPORT_ATTACHMENT_FILE_NAME)) $criteria->add(ArReportPeer::REPORT_ATTACHMENT_FILE_NAME, $this->report_attachment_file_name);
		if ($this->isColumnModified(ArReportPeer::REPORT_ATTACHMENT_FILE_NAME_ADD_REPORT_DATE)) $criteria->add(ArReportPeer::REPORT_ATTACHMENT_FILE_NAME_ADD_REPORT_DATE, $this->report_attachment_file_name_add_report_date);
		if ($this->isColumnModified(ArReportPeer::INTERNAL_NAME)) $criteria->add(ArReportPeer::INTERNAL_NAME, $this->internal_name);
		if ($this->isColumnModified(ArReportPeer::CACHED_PARENT_ID_HIERARCHY)) $criteria->add(ArReportPeer::CACHED_PARENT_ID_HIERARCHY, $this->cached_parent_id_hierarchy);
		if ($this->isColumnModified(ArReportPeer::LEGAL_NR_PREFIX)) $criteria->add(ArReportPeer::LEGAL_NR_PREFIX, $this->legal_nr_prefix);
		if ($this->isColumnModified(ArReportPeer::LEGAL_CONSECUTIVE_NR)) $criteria->add(ArReportPeer::LEGAL_CONSECUTIVE_NR, $this->legal_consecutive_nr);
		if ($this->isColumnModified(ArReportPeer::LEGAL_DATE)) $criteria->add(ArReportPeer::LEGAL_DATE, $this->legal_date);
		if ($this->isColumnModified(ArReportPeer::LEGAL_SENDER_NAME)) $criteria->add(ArReportPeer::LEGAL_SENDER_NAME, $this->legal_sender_name);
		if ($this->isColumnModified(ArReportPeer::LEGAL_SENDER_VAT)) $criteria->add(ArReportPeer::LEGAL_SENDER_VAT, $this->legal_sender_vat);
		if ($this->isColumnModified(ArReportPeer::LEGAL_SENDER_ADDRESS)) $criteria->add(ArReportPeer::LEGAL_SENDER_ADDRESS, $this->legal_sender_address);
		if ($this->isColumnModified(ArReportPeer::LEGAL_RECEIVER_NAME)) $criteria->add(ArReportPeer::LEGAL_RECEIVER_NAME, $this->legal_receiver_name);
		if ($this->isColumnModified(ArReportPeer::LEGAL_RECEIVER_VAT)) $criteria->add(ArReportPeer::LEGAL_RECEIVER_VAT, $this->legal_receiver_vat);
		if ($this->isColumnModified(ArReportPeer::LEGAL_RECEIVER_ADDRESS)) $criteria->add(ArReportPeer::LEGAL_RECEIVER_ADDRESS, $this->legal_receiver_address);
		if ($this->isColumnModified(ArReportPeer::TOTAL_WITHOUT_TAX)) $criteria->add(ArReportPeer::TOTAL_WITHOUT_TAX, $this->total_without_tax);
		if ($this->isColumnModified(ArReportPeer::TAX)) $criteria->add(ArReportPeer::TAX, $this->tax);
		if ($this->isColumnModified(ArReportPeer::APPLIED_VAT)) $criteria->add(ArReportPeer::APPLIED_VAT, $this->applied_vat);
		if ($this->isColumnModified(ArReportPeer::TOTAL_WITH_TAX)) $criteria->add(ArReportPeer::TOTAL_WITH_TAX, $this->total_with_tax);

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
		$criteria = new Criteria(ArReportPeer::DATABASE_NAME);

		$criteria->add(ArReportPeer::ID, $this->id);

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
	 * @param      object $copyObj An object of ArReport (or compatible) type.
	 * @param      boolean $deepCopy Whether to also copy all rows that refer (by fkey) to the current row.
	 * @throws     PropelException
	 */
	public function copyInto($copyObj, $deepCopy = false)
	{

		$copyObj->setIsTemplate($this->is_template);

		$copyObj->setArReportSetId($this->ar_report_set_id);

		$copyObj->setArOrganizationUnitId($this->ar_organization_unit_id);

		$copyObj->setArUserId($this->ar_user_id);

		$copyObj->setArVendorId($this->ar_vendor_id);

		$copyObj->setFromDate($this->from_date);

		$copyObj->setToDate($this->to_date);

		$copyObj->setParamShowMaskedTelephoneNumbers($this->param_show_masked_telephone_numbers);

		$copyObj->setParamShowCallCost($this->param_show_call_cost);

		$copyObj->setParamShowCallIncome($this->param_show_call_income);

		$copyObj->setParamShowAlsoOutgoingCalls($this->param_show_also_outgoing_calls);

		$copyObj->setParamShowAlsoSystemCalls($this->param_show_also_system_calls);

		$copyObj->setParamShowAlsoIncomingCalls($this->param_show_also_incoming_calls);

		$copyObj->setParamShowAlsoInternalCalls($this->param_show_also_internal_calls);

		$copyObj->setParamShowCallDetails($this->param_show_call_details);

		$copyObj->setParamShowVoipProvider($this->param_show_voip_provider);

		$copyObj->setParamShowCommunicationChannel($this->param_show_communication_channel);

		$copyObj->setParamShowGeographicLocation($this->param_show_geographic_location);

		$copyObj->setParamShowConnectionType($this->param_show_connection_type);

		$copyObj->setParamShowCostSaving($this->param_show_cost_saving);

		$copyObj->setParamIsLegal($this->param_is_legal);

		$copyObj->setParamExpandToLevel($this->param_expand_to_level);

		$copyObj->setArReportOrderOfChildrenId($this->ar_report_order_of_children_id);

		$copyObj->setPhpClassName($this->php_class_name);

		$copyObj->setProducedReportGenerationDate($this->produced_report_generation_date);

		$copyObj->setReportName($this->report_name);

		$copyObj->setProducedReportShortDescription($this->produced_report_short_description);

		$copyObj->setProducedReportAdditionalDescription($this->produced_report_additional_description);

		$copyObj->setProducedReportAlreadyReviewed($this->produced_report_already_reviewed);

		$copyObj->setProducedReportIsDraft($this->produced_report_is_draft);

		$copyObj->setProducedReportMustBeRegenerated($this->produced_report_must_be_regenerated);

		$copyObj->setProducedReportMimeType($this->produced_report_mime_type);

		$copyObj->setProducedReportFileTypeSuffix($this->produced_report_file_type_suffix);

		$copyObj->setProducedReportDocument($this->produced_report_document);

		$copyObj->setProducedReportDocumentChecksum($this->produced_report_document_checksum);

		$copyObj->setReportMailSubject($this->report_mail_subject);

		$copyObj->setReportMailBody($this->report_mail_body);

		$copyObj->setReportAttachmentFileName($this->report_attachment_file_name);

		$copyObj->setReportAttachmentFileNameAddReportDate($this->report_attachment_file_name_add_report_date);

		$copyObj->setInternalName($this->internal_name);

		$copyObj->setCachedParentIdHierarchy($this->cached_parent_id_hierarchy);

		$copyObj->setLegalNrPrefix($this->legal_nr_prefix);

		$copyObj->setLegalConsecutiveNr($this->legal_consecutive_nr);

		$copyObj->setLegalDate($this->legal_date);

		$copyObj->setLegalSenderName($this->legal_sender_name);

		$copyObj->setLegalSenderVat($this->legal_sender_vat);

		$copyObj->setLegalSenderAddress($this->legal_sender_address);

		$copyObj->setLegalReceiverName($this->legal_receiver_name);

		$copyObj->setLegalReceiverVat($this->legal_receiver_vat);

		$copyObj->setLegalReceiverAddress($this->legal_receiver_address);

		$copyObj->setTotalWithoutTax($this->total_without_tax);

		$copyObj->setTax($this->tax);

		$copyObj->setAppliedVat($this->applied_vat);

		$copyObj->setTotalWithTax($this->total_with_tax);


		if ($deepCopy) {
			// important: temporarily setNew(false) because this affects the behavior of
			// the getter/setter methods for fkey referrer objects.
			$copyObj->setNew(false);

			foreach ($this->getArReportAlsoFors() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArReportAlsoFor($relObj->copy($deepCopy));
				}
			}

			foreach ($this->getArReportSchedulers() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArReportScheduler($relObj->copy($deepCopy));
				}
			}

			foreach ($this->getArReportToReads() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArReportToRead($relObj->copy($deepCopy));
				}
			}

			foreach ($this->getArReportToReadUserViews() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArReportToReadUserView($relObj->copy($deepCopy));
				}
			}

			foreach ($this->getArUserCanViewReports() as $relObj) {
				if ($relObj !== $this) {  // ensure that we don't try to copy a reference to ourselves
					$copyObj->addArUserCanViewReport($relObj->copy($deepCopy));
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
	 * @return     ArReport Clone of current object.
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
	 * @return     ArReportPeer
	 */
	public function getPeer()
	{
		if (self::$peer === null) {
			self::$peer = new ArReportPeer();
		}
		return self::$peer;
	}

	/**
	 * Declares an association between this object and a ArReportSet object.
	 *
	 * @param      ArReportSet $v
	 * @return     ArReport The current object (for fluent API support)
	 * @throws     PropelException
	 */
	public function setArReportSet(ArReportSet $v = null)
	{
		if ($v === null) {
			$this->setArReportSetId(NULL);
		} else {
			$this->setArReportSetId($v->getId());
		}

		$this->aArReportSet = $v;

		// Add binding for other direction of this n:n relationship.
		// If this object has already been added to the ArReportSet object, it will not be re-added.
		if ($v !== null) {
			$v->addArReport($this);
		}

		return $this;
	}


	/**
	 * Get the associated ArReportSet object
	 *
	 * @param      PropelPDO Optional Connection object.
	 * @return     ArReportSet The associated ArReportSet object.
	 * @throws     PropelException
	 */
	public function getArReportSet(PropelPDO $con = null)
	{
		if ($this->aArReportSet === null && ($this->ar_report_set_id !== null)) {
			$this->aArReportSet = ArReportSetPeer::retrieveByPk($this->ar_report_set_id);
			/* The following can be used additionally to
			   guarantee the related object contains a reference
			   to this object.  This level of coupling may, however, be
			   undesirable since it could result in an only partially populated collection
			   in the referenced object.
			   $this->aArReportSet->addArReports($this);
			 */
		}
		return $this->aArReportSet;
	}

	/**
	 * Declares an association between this object and a ArOrganizationUnit object.
	 *
	 * @param      ArOrganizationUnit $v
	 * @return     ArReport The current object (for fluent API support)
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
			$v->addArReport($this);
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
			   $this->aArOrganizationUnit->addArReports($this);
			 */
		}
		return $this->aArOrganizationUnit;
	}

	/**
	 * Declares an association between this object and a ArUser object.
	 *
	 * @param      ArUser $v
	 * @return     ArReport The current object (for fluent API support)
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
			$v->addArReport($this);
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
			   $this->aArUser->addArReports($this);
			 */
		}
		return $this->aArUser;
	}

	/**
	 * Declares an association between this object and a ArVendor object.
	 *
	 * @param      ArVendor $v
	 * @return     ArReport The current object (for fluent API support)
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
			$v->addArReport($this);
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
			   $this->aArVendor->addArReports($this);
			 */
		}
		return $this->aArVendor;
	}

	/**
	 * Declares an association between this object and a ArReportOrderOfChildren object.
	 *
	 * @param      ArReportOrderOfChildren $v
	 * @return     ArReport The current object (for fluent API support)
	 * @throws     PropelException
	 */
	public function setArReportOrderOfChildren(ArReportOrderOfChildren $v = null)
	{
		if ($v === null) {
			$this->setArReportOrderOfChildrenId(NULL);
		} else {
			$this->setArReportOrderOfChildrenId($v->getId());
		}

		$this->aArReportOrderOfChildren = $v;

		// Add binding for other direction of this n:n relationship.
		// If this object has already been added to the ArReportOrderOfChildren object, it will not be re-added.
		if ($v !== null) {
			$v->addArReport($this);
		}

		return $this;
	}


	/**
	 * Get the associated ArReportOrderOfChildren object
	 *
	 * @param      PropelPDO Optional Connection object.
	 * @return     ArReportOrderOfChildren The associated ArReportOrderOfChildren object.
	 * @throws     PropelException
	 */
	public function getArReportOrderOfChildren(PropelPDO $con = null)
	{
		if ($this->aArReportOrderOfChildren === null && ($this->ar_report_order_of_children_id !== null)) {
			$this->aArReportOrderOfChildren = ArReportOrderOfChildrenPeer::retrieveByPk($this->ar_report_order_of_children_id);
			/* The following can be used additionally to
			   guarantee the related object contains a reference
			   to this object.  This level of coupling may, however, be
			   undesirable since it could result in an only partially populated collection
			   in the referenced object.
			   $this->aArReportOrderOfChildren->addArReports($this);
			 */
		}
		return $this->aArReportOrderOfChildren;
	}

	/**
	 * Clears out the collArReportAlsoFors collection (array).
	 *
	 * This does not modify the database; however, it will remove any associated objects, causing
	 * them to be refetched by subsequent calls to accessor method.
	 *
	 * @return     void
	 * @see        addArReportAlsoFors()
	 */
	public function clearArReportAlsoFors()
	{
		$this->collArReportAlsoFors = null; // important to set this to NULL since that means it is uninitialized
	}

	/**
	 * Initializes the collArReportAlsoFors collection (array).
	 *
	 * By default this just sets the collArReportAlsoFors collection to an empty array (like clearcollArReportAlsoFors());
	 * however, you may wish to override this method in your stub class to provide setting appropriate
	 * to your application -- for example, setting the initial array to the values stored in database.
	 *
	 * @return     void
	 */
	public function initArReportAlsoFors()
	{
		$this->collArReportAlsoFors = array();
	}

	/**
	 * Gets an array of ArReportAlsoFor objects which contain a foreign key that references this object.
	 *
	 * If this collection has already been initialized with an identical Criteria, it returns the collection.
	 * Otherwise if this ArReport has previously been saved, it will retrieve
	 * related ArReportAlsoFors from storage. If this ArReport is new, it will return
	 * an empty collection or the current collection, the criteria is ignored on a new object.
	 *
	 * @param      PropelPDO $con
	 * @param      Criteria $criteria
	 * @return     array ArReportAlsoFor[]
	 * @throws     PropelException
	 */
	public function getArReportAlsoFors($criteria = null, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArReportPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReportAlsoFors === null) {
			if ($this->isNew()) {
			   $this->collArReportAlsoFors = array();
			} else {

				$criteria->add(ArReportAlsoForPeer::AR_REPORT_ID, $this->id);

				ArReportAlsoForPeer::addSelectColumns($criteria);
				$this->collArReportAlsoFors = ArReportAlsoForPeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArReportAlsoForPeer::AR_REPORT_ID, $this->id);

				ArReportAlsoForPeer::addSelectColumns($criteria);
				if (!isset($this->lastArReportAlsoForCriteria) || !$this->lastArReportAlsoForCriteria->equals($criteria)) {
					$this->collArReportAlsoFors = ArReportAlsoForPeer::doSelect($criteria, $con);
				}
			}
		}
		$this->lastArReportAlsoForCriteria = $criteria;
		return $this->collArReportAlsoFors;
	}

	/**
	 * Returns the number of related ArReportAlsoFor objects.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct
	 * @param      PropelPDO $con
	 * @return     int Count of related ArReportAlsoFor objects.
	 * @throws     PropelException
	 */
	public function countArReportAlsoFors(Criteria $criteria = null, $distinct = false, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArReportPeer::DATABASE_NAME);
		} else {
			$criteria = clone $criteria;
		}

		if ($distinct) {
			$criteria->setDistinct();
		}

		$count = null;

		if ($this->collArReportAlsoFors === null) {
			if ($this->isNew()) {
				$count = 0;
			} else {

				$criteria->add(ArReportAlsoForPeer::AR_REPORT_ID, $this->id);

				$count = ArReportAlsoForPeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArReportAlsoForPeer::AR_REPORT_ID, $this->id);

				if (!isset($this->lastArReportAlsoForCriteria) || !$this->lastArReportAlsoForCriteria->equals($criteria)) {
					$count = ArReportAlsoForPeer::doCount($criteria, false, $con);
				} else {
					$count = count($this->collArReportAlsoFors);
				}
			} else {
				$count = count($this->collArReportAlsoFors);
			}
		}
		return $count;
	}

	/**
	 * Method called to associate a ArReportAlsoFor object to this object
	 * through the ArReportAlsoFor foreign key attribute.
	 *
	 * @param      ArReportAlsoFor $l ArReportAlsoFor
	 * @return     void
	 * @throws     PropelException
	 */
	public function addArReportAlsoFor(ArReportAlsoFor $l)
	{
		if ($this->collArReportAlsoFors === null) {
			$this->initArReportAlsoFors();
		}
		if (!in_array($l, $this->collArReportAlsoFors, true)) { // only add it if the **same** object is not already associated
			array_push($this->collArReportAlsoFors, $l);
			$l->setArReport($this);
		}
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArReport is new, it will return
	 * an empty collection; or if this ArReport has previously
	 * been saved, it will retrieve related ArReportAlsoFors from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArReport.
	 */
	public function getArReportAlsoForsJoinArRole($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArReportPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReportAlsoFors === null) {
			if ($this->isNew()) {
				$this->collArReportAlsoFors = array();
			} else {

				$criteria->add(ArReportAlsoForPeer::AR_REPORT_ID, $this->id);

				$this->collArReportAlsoFors = ArReportAlsoForPeer::doSelectJoinArRole($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArReportAlsoForPeer::AR_REPORT_ID, $this->id);

			if (!isset($this->lastArReportAlsoForCriteria) || !$this->lastArReportAlsoForCriteria->equals($criteria)) {
				$this->collArReportAlsoFors = ArReportAlsoForPeer::doSelectJoinArRole($criteria, $con, $join_behavior);
			}
		}
		$this->lastArReportAlsoForCriteria = $criteria;

		return $this->collArReportAlsoFors;
	}

	/**
	 * Clears out the collArReportSchedulers collection (array).
	 *
	 * This does not modify the database; however, it will remove any associated objects, causing
	 * them to be refetched by subsequent calls to accessor method.
	 *
	 * @return     void
	 * @see        addArReportSchedulers()
	 */
	public function clearArReportSchedulers()
	{
		$this->collArReportSchedulers = null; // important to set this to NULL since that means it is uninitialized
	}

	/**
	 * Initializes the collArReportSchedulers collection (array).
	 *
	 * By default this just sets the collArReportSchedulers collection to an empty array (like clearcollArReportSchedulers());
	 * however, you may wish to override this method in your stub class to provide setting appropriate
	 * to your application -- for example, setting the initial array to the values stored in database.
	 *
	 * @return     void
	 */
	public function initArReportSchedulers()
	{
		$this->collArReportSchedulers = array();
	}

	/**
	 * Gets an array of ArReportScheduler objects which contain a foreign key that references this object.
	 *
	 * If this collection has already been initialized with an identical Criteria, it returns the collection.
	 * Otherwise if this ArReport has previously been saved, it will retrieve
	 * related ArReportSchedulers from storage. If this ArReport is new, it will return
	 * an empty collection or the current collection, the criteria is ignored on a new object.
	 *
	 * @param      PropelPDO $con
	 * @param      Criteria $criteria
	 * @return     array ArReportScheduler[]
	 * @throws     PropelException
	 */
	public function getArReportSchedulers($criteria = null, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArReportPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReportSchedulers === null) {
			if ($this->isNew()) {
			   $this->collArReportSchedulers = array();
			} else {

				$criteria->add(ArReportSchedulerPeer::AR_REPORT_ID, $this->id);

				ArReportSchedulerPeer::addSelectColumns($criteria);
				$this->collArReportSchedulers = ArReportSchedulerPeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArReportSchedulerPeer::AR_REPORT_ID, $this->id);

				ArReportSchedulerPeer::addSelectColumns($criteria);
				if (!isset($this->lastArReportSchedulerCriteria) || !$this->lastArReportSchedulerCriteria->equals($criteria)) {
					$this->collArReportSchedulers = ArReportSchedulerPeer::doSelect($criteria, $con);
				}
			}
		}
		$this->lastArReportSchedulerCriteria = $criteria;
		return $this->collArReportSchedulers;
	}

	/**
	 * Returns the number of related ArReportScheduler objects.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct
	 * @param      PropelPDO $con
	 * @return     int Count of related ArReportScheduler objects.
	 * @throws     PropelException
	 */
	public function countArReportSchedulers(Criteria $criteria = null, $distinct = false, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArReportPeer::DATABASE_NAME);
		} else {
			$criteria = clone $criteria;
		}

		if ($distinct) {
			$criteria->setDistinct();
		}

		$count = null;

		if ($this->collArReportSchedulers === null) {
			if ($this->isNew()) {
				$count = 0;
			} else {

				$criteria->add(ArReportSchedulerPeer::AR_REPORT_ID, $this->id);

				$count = ArReportSchedulerPeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArReportSchedulerPeer::AR_REPORT_ID, $this->id);

				if (!isset($this->lastArReportSchedulerCriteria) || !$this->lastArReportSchedulerCriteria->equals($criteria)) {
					$count = ArReportSchedulerPeer::doCount($criteria, false, $con);
				} else {
					$count = count($this->collArReportSchedulers);
				}
			} else {
				$count = count($this->collArReportSchedulers);
			}
		}
		return $count;
	}

	/**
	 * Method called to associate a ArReportScheduler object to this object
	 * through the ArReportScheduler foreign key attribute.
	 *
	 * @param      ArReportScheduler $l ArReportScheduler
	 * @return     void
	 * @throws     PropelException
	 */
	public function addArReportScheduler(ArReportScheduler $l)
	{
		if ($this->collArReportSchedulers === null) {
			$this->initArReportSchedulers();
		}
		if (!in_array($l, $this->collArReportSchedulers, true)) { // only add it if the **same** object is not already associated
			array_push($this->collArReportSchedulers, $l);
			$l->setArReport($this);
		}
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArReport is new, it will return
	 * an empty collection; or if this ArReport has previously
	 * been saved, it will retrieve related ArReportSchedulers from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArReport.
	 */
	public function getArReportSchedulersJoinArOrganizationUnit($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArReportPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReportSchedulers === null) {
			if ($this->isNew()) {
				$this->collArReportSchedulers = array();
			} else {

				$criteria->add(ArReportSchedulerPeer::AR_REPORT_ID, $this->id);

				$this->collArReportSchedulers = ArReportSchedulerPeer::doSelectJoinArOrganizationUnit($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArReportSchedulerPeer::AR_REPORT_ID, $this->id);

			if (!isset($this->lastArReportSchedulerCriteria) || !$this->lastArReportSchedulerCriteria->equals($criteria)) {
				$this->collArReportSchedulers = ArReportSchedulerPeer::doSelectJoinArOrganizationUnit($criteria, $con, $join_behavior);
			}
		}
		$this->lastArReportSchedulerCriteria = $criteria;

		return $this->collArReportSchedulers;
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArReport is new, it will return
	 * an empty collection; or if this ArReport has previously
	 * been saved, it will retrieve related ArReportSchedulers from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArReport.
	 */
	public function getArReportSchedulersJoinArReportGeneration($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArReportPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReportSchedulers === null) {
			if ($this->isNew()) {
				$this->collArReportSchedulers = array();
			} else {

				$criteria->add(ArReportSchedulerPeer::AR_REPORT_ID, $this->id);

				$this->collArReportSchedulers = ArReportSchedulerPeer::doSelectJoinArReportGeneration($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArReportSchedulerPeer::AR_REPORT_ID, $this->id);

			if (!isset($this->lastArReportSchedulerCriteria) || !$this->lastArReportSchedulerCriteria->equals($criteria)) {
				$this->collArReportSchedulers = ArReportSchedulerPeer::doSelectJoinArReportGeneration($criteria, $con, $join_behavior);
			}
		}
		$this->lastArReportSchedulerCriteria = $criteria;

		return $this->collArReportSchedulers;
	}

	/**
	 * Clears out the collArReportToReads collection (array).
	 *
	 * This does not modify the database; however, it will remove any associated objects, causing
	 * them to be refetched by subsequent calls to accessor method.
	 *
	 * @return     void
	 * @see        addArReportToReads()
	 */
	public function clearArReportToReads()
	{
		$this->collArReportToReads = null; // important to set this to NULL since that means it is uninitialized
	}

	/**
	 * Initializes the collArReportToReads collection (array).
	 *
	 * By default this just sets the collArReportToReads collection to an empty array (like clearcollArReportToReads());
	 * however, you may wish to override this method in your stub class to provide setting appropriate
	 * to your application -- for example, setting the initial array to the values stored in database.
	 *
	 * @return     void
	 */
	public function initArReportToReads()
	{
		$this->collArReportToReads = array();
	}

	/**
	 * Gets an array of ArReportToRead objects which contain a foreign key that references this object.
	 *
	 * If this collection has already been initialized with an identical Criteria, it returns the collection.
	 * Otherwise if this ArReport has previously been saved, it will retrieve
	 * related ArReportToReads from storage. If this ArReport is new, it will return
	 * an empty collection or the current collection, the criteria is ignored on a new object.
	 *
	 * @param      PropelPDO $con
	 * @param      Criteria $criteria
	 * @return     array ArReportToRead[]
	 * @throws     PropelException
	 */
	public function getArReportToReads($criteria = null, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArReportPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReportToReads === null) {
			if ($this->isNew()) {
			   $this->collArReportToReads = array();
			} else {

				$criteria->add(ArReportToReadPeer::AR_REPORT_ID, $this->id);

				ArReportToReadPeer::addSelectColumns($criteria);
				$this->collArReportToReads = ArReportToReadPeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArReportToReadPeer::AR_REPORT_ID, $this->id);

				ArReportToReadPeer::addSelectColumns($criteria);
				if (!isset($this->lastArReportToReadCriteria) || !$this->lastArReportToReadCriteria->equals($criteria)) {
					$this->collArReportToReads = ArReportToReadPeer::doSelect($criteria, $con);
				}
			}
		}
		$this->lastArReportToReadCriteria = $criteria;
		return $this->collArReportToReads;
	}

	/**
	 * Returns the number of related ArReportToRead objects.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct
	 * @param      PropelPDO $con
	 * @return     int Count of related ArReportToRead objects.
	 * @throws     PropelException
	 */
	public function countArReportToReads(Criteria $criteria = null, $distinct = false, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArReportPeer::DATABASE_NAME);
		} else {
			$criteria = clone $criteria;
		}

		if ($distinct) {
			$criteria->setDistinct();
		}

		$count = null;

		if ($this->collArReportToReads === null) {
			if ($this->isNew()) {
				$count = 0;
			} else {

				$criteria->add(ArReportToReadPeer::AR_REPORT_ID, $this->id);

				$count = ArReportToReadPeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArReportToReadPeer::AR_REPORT_ID, $this->id);

				if (!isset($this->lastArReportToReadCriteria) || !$this->lastArReportToReadCriteria->equals($criteria)) {
					$count = ArReportToReadPeer::doCount($criteria, false, $con);
				} else {
					$count = count($this->collArReportToReads);
				}
			} else {
				$count = count($this->collArReportToReads);
			}
		}
		return $count;
	}

	/**
	 * Method called to associate a ArReportToRead object to this object
	 * through the ArReportToRead foreign key attribute.
	 *
	 * @param      ArReportToRead $l ArReportToRead
	 * @return     void
	 * @throws     PropelException
	 */
	public function addArReportToRead(ArReportToRead $l)
	{
		if ($this->collArReportToReads === null) {
			$this->initArReportToReads();
		}
		if (!in_array($l, $this->collArReportToReads, true)) { // only add it if the **same** object is not already associated
			array_push($this->collArReportToReads, $l);
			$l->setArReport($this);
		}
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArReport is new, it will return
	 * an empty collection; or if this ArReport has previously
	 * been saved, it will retrieve related ArReportToReads from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArReport.
	 */
	public function getArReportToReadsJoinArUser($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArReportPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReportToReads === null) {
			if ($this->isNew()) {
				$this->collArReportToReads = array();
			} else {

				$criteria->add(ArReportToReadPeer::AR_REPORT_ID, $this->id);

				$this->collArReportToReads = ArReportToReadPeer::doSelectJoinArUser($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArReportToReadPeer::AR_REPORT_ID, $this->id);

			if (!isset($this->lastArReportToReadCriteria) || !$this->lastArReportToReadCriteria->equals($criteria)) {
				$this->collArReportToReads = ArReportToReadPeer::doSelectJoinArUser($criteria, $con, $join_behavior);
			}
		}
		$this->lastArReportToReadCriteria = $criteria;

		return $this->collArReportToReads;
	}

	/**
	 * Clears out the collArReportToReadUserViews collection (array).
	 *
	 * This does not modify the database; however, it will remove any associated objects, causing
	 * them to be refetched by subsequent calls to accessor method.
	 *
	 * @return     void
	 * @see        addArReportToReadUserViews()
	 */
	public function clearArReportToReadUserViews()
	{
		$this->collArReportToReadUserViews = null; // important to set this to NULL since that means it is uninitialized
	}

	/**
	 * Initializes the collArReportToReadUserViews collection (array).
	 *
	 * By default this just sets the collArReportToReadUserViews collection to an empty array (like clearcollArReportToReadUserViews());
	 * however, you may wish to override this method in your stub class to provide setting appropriate
	 * to your application -- for example, setting the initial array to the values stored in database.
	 *
	 * @return     void
	 */
	public function initArReportToReadUserViews()
	{
		$this->collArReportToReadUserViews = array();
	}

	/**
	 * Gets an array of ArReportToReadUserView objects which contain a foreign key that references this object.
	 *
	 * If this collection has already been initialized with an identical Criteria, it returns the collection.
	 * Otherwise if this ArReport has previously been saved, it will retrieve
	 * related ArReportToReadUserViews from storage. If this ArReport is new, it will return
	 * an empty collection or the current collection, the criteria is ignored on a new object.
	 *
	 * @param      PropelPDO $con
	 * @param      Criteria $criteria
	 * @return     array ArReportToReadUserView[]
	 * @throws     PropelException
	 */
	public function getArReportToReadUserViews($criteria = null, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArReportPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReportToReadUserViews === null) {
			if ($this->isNew()) {
			   $this->collArReportToReadUserViews = array();
			} else {

				$criteria->add(ArReportToReadUserViewPeer::AR_REPORT_ID, $this->id);

				ArReportToReadUserViewPeer::addSelectColumns($criteria);
				$this->collArReportToReadUserViews = ArReportToReadUserViewPeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArReportToReadUserViewPeer::AR_REPORT_ID, $this->id);

				ArReportToReadUserViewPeer::addSelectColumns($criteria);
				if (!isset($this->lastArReportToReadUserViewCriteria) || !$this->lastArReportToReadUserViewCriteria->equals($criteria)) {
					$this->collArReportToReadUserViews = ArReportToReadUserViewPeer::doSelect($criteria, $con);
				}
			}
		}
		$this->lastArReportToReadUserViewCriteria = $criteria;
		return $this->collArReportToReadUserViews;
	}

	/**
	 * Returns the number of related ArReportToReadUserView objects.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct
	 * @param      PropelPDO $con
	 * @return     int Count of related ArReportToReadUserView objects.
	 * @throws     PropelException
	 */
	public function countArReportToReadUserViews(Criteria $criteria = null, $distinct = false, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArReportPeer::DATABASE_NAME);
		} else {
			$criteria = clone $criteria;
		}

		if ($distinct) {
			$criteria->setDistinct();
		}

		$count = null;

		if ($this->collArReportToReadUserViews === null) {
			if ($this->isNew()) {
				$count = 0;
			} else {

				$criteria->add(ArReportToReadUserViewPeer::AR_REPORT_ID, $this->id);

				$count = ArReportToReadUserViewPeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArReportToReadUserViewPeer::AR_REPORT_ID, $this->id);

				if (!isset($this->lastArReportToReadUserViewCriteria) || !$this->lastArReportToReadUserViewCriteria->equals($criteria)) {
					$count = ArReportToReadUserViewPeer::doCount($criteria, false, $con);
				} else {
					$count = count($this->collArReportToReadUserViews);
				}
			} else {
				$count = count($this->collArReportToReadUserViews);
			}
		}
		return $count;
	}

	/**
	 * Method called to associate a ArReportToReadUserView object to this object
	 * through the ArReportToReadUserView foreign key attribute.
	 *
	 * @param      ArReportToReadUserView $l ArReportToReadUserView
	 * @return     void
	 * @throws     PropelException
	 */
	public function addArReportToReadUserView(ArReportToReadUserView $l)
	{
		if ($this->collArReportToReadUserViews === null) {
			$this->initArReportToReadUserViews();
		}
		if (!in_array($l, $this->collArReportToReadUserViews, true)) { // only add it if the **same** object is not already associated
			array_push($this->collArReportToReadUserViews, $l);
			$l->setArReport($this);
		}
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArReport is new, it will return
	 * an empty collection; or if this ArReport has previously
	 * been saved, it will retrieve related ArReportToReadUserViews from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArReport.
	 */
	public function getArReportToReadUserViewsJoinArReportToRead($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArReportPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReportToReadUserViews === null) {
			if ($this->isNew()) {
				$this->collArReportToReadUserViews = array();
			} else {

				$criteria->add(ArReportToReadUserViewPeer::AR_REPORT_ID, $this->id);

				$this->collArReportToReadUserViews = ArReportToReadUserViewPeer::doSelectJoinArReportToRead($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArReportToReadUserViewPeer::AR_REPORT_ID, $this->id);

			if (!isset($this->lastArReportToReadUserViewCriteria) || !$this->lastArReportToReadUserViewCriteria->equals($criteria)) {
				$this->collArReportToReadUserViews = ArReportToReadUserViewPeer::doSelectJoinArReportToRead($criteria, $con, $join_behavior);
			}
		}
		$this->lastArReportToReadUserViewCriteria = $criteria;

		return $this->collArReportToReadUserViews;
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArReport is new, it will return
	 * an empty collection; or if this ArReport has previously
	 * been saved, it will retrieve related ArReportToReadUserViews from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArReport.
	 */
	public function getArReportToReadUserViewsJoinArUser($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArReportPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReportToReadUserViews === null) {
			if ($this->isNew()) {
				$this->collArReportToReadUserViews = array();
			} else {

				$criteria->add(ArReportToReadUserViewPeer::AR_REPORT_ID, $this->id);

				$this->collArReportToReadUserViews = ArReportToReadUserViewPeer::doSelectJoinArUser($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArReportToReadUserViewPeer::AR_REPORT_ID, $this->id);

			if (!isset($this->lastArReportToReadUserViewCriteria) || !$this->lastArReportToReadUserViewCriteria->equals($criteria)) {
				$this->collArReportToReadUserViews = ArReportToReadUserViewPeer::doSelectJoinArUser($criteria, $con, $join_behavior);
			}
		}
		$this->lastArReportToReadUserViewCriteria = $criteria;

		return $this->collArReportToReadUserViews;
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArReport is new, it will return
	 * an empty collection; or if this ArReport has previously
	 * been saved, it will retrieve related ArReportToReadUserViews from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArReport.
	 */
	public function getArReportToReadUserViewsJoinArOrganizationUnit($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArReportPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArReportToReadUserViews === null) {
			if ($this->isNew()) {
				$this->collArReportToReadUserViews = array();
			} else {

				$criteria->add(ArReportToReadUserViewPeer::AR_REPORT_ID, $this->id);

				$this->collArReportToReadUserViews = ArReportToReadUserViewPeer::doSelectJoinArOrganizationUnit($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArReportToReadUserViewPeer::AR_REPORT_ID, $this->id);

			if (!isset($this->lastArReportToReadUserViewCriteria) || !$this->lastArReportToReadUserViewCriteria->equals($criteria)) {
				$this->collArReportToReadUserViews = ArReportToReadUserViewPeer::doSelectJoinArOrganizationUnit($criteria, $con, $join_behavior);
			}
		}
		$this->lastArReportToReadUserViewCriteria = $criteria;

		return $this->collArReportToReadUserViews;
	}

	/**
	 * Clears out the collArUserCanViewReports collection (array).
	 *
	 * This does not modify the database; however, it will remove any associated objects, causing
	 * them to be refetched by subsequent calls to accessor method.
	 *
	 * @return     void
	 * @see        addArUserCanViewReports()
	 */
	public function clearArUserCanViewReports()
	{
		$this->collArUserCanViewReports = null; // important to set this to NULL since that means it is uninitialized
	}

	/**
	 * Initializes the collArUserCanViewReports collection (array).
	 *
	 * By default this just sets the collArUserCanViewReports collection to an empty array (like clearcollArUserCanViewReports());
	 * however, you may wish to override this method in your stub class to provide setting appropriate
	 * to your application -- for example, setting the initial array to the values stored in database.
	 *
	 * @return     void
	 */
	public function initArUserCanViewReports()
	{
		$this->collArUserCanViewReports = array();
	}

	/**
	 * Gets an array of ArUserCanViewReport objects which contain a foreign key that references this object.
	 *
	 * If this collection has already been initialized with an identical Criteria, it returns the collection.
	 * Otherwise if this ArReport has previously been saved, it will retrieve
	 * related ArUserCanViewReports from storage. If this ArReport is new, it will return
	 * an empty collection or the current collection, the criteria is ignored on a new object.
	 *
	 * @param      PropelPDO $con
	 * @param      Criteria $criteria
	 * @return     array ArUserCanViewReport[]
	 * @throws     PropelException
	 */
	public function getArUserCanViewReports($criteria = null, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArReportPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArUserCanViewReports === null) {
			if ($this->isNew()) {
			   $this->collArUserCanViewReports = array();
			} else {

				$criteria->add(ArUserCanViewReportPeer::AR_REPORT_ID, $this->id);

				ArUserCanViewReportPeer::addSelectColumns($criteria);
				$this->collArUserCanViewReports = ArUserCanViewReportPeer::doSelect($criteria, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return the collection.


				$criteria->add(ArUserCanViewReportPeer::AR_REPORT_ID, $this->id);

				ArUserCanViewReportPeer::addSelectColumns($criteria);
				if (!isset($this->lastArUserCanViewReportCriteria) || !$this->lastArUserCanViewReportCriteria->equals($criteria)) {
					$this->collArUserCanViewReports = ArUserCanViewReportPeer::doSelect($criteria, $con);
				}
			}
		}
		$this->lastArUserCanViewReportCriteria = $criteria;
		return $this->collArUserCanViewReports;
	}

	/**
	 * Returns the number of related ArUserCanViewReport objects.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct
	 * @param      PropelPDO $con
	 * @return     int Count of related ArUserCanViewReport objects.
	 * @throws     PropelException
	 */
	public function countArUserCanViewReports(Criteria $criteria = null, $distinct = false, PropelPDO $con = null)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArReportPeer::DATABASE_NAME);
		} else {
			$criteria = clone $criteria;
		}

		if ($distinct) {
			$criteria->setDistinct();
		}

		$count = null;

		if ($this->collArUserCanViewReports === null) {
			if ($this->isNew()) {
				$count = 0;
			} else {

				$criteria->add(ArUserCanViewReportPeer::AR_REPORT_ID, $this->id);

				$count = ArUserCanViewReportPeer::doCount($criteria, false, $con);
			}
		} else {
			// criteria has no effect for a new object
			if (!$this->isNew()) {
				// the following code is to determine if a new query is
				// called for.  If the criteria is the same as the last
				// one, just return count of the collection.


				$criteria->add(ArUserCanViewReportPeer::AR_REPORT_ID, $this->id);

				if (!isset($this->lastArUserCanViewReportCriteria) || !$this->lastArUserCanViewReportCriteria->equals($criteria)) {
					$count = ArUserCanViewReportPeer::doCount($criteria, false, $con);
				} else {
					$count = count($this->collArUserCanViewReports);
				}
			} else {
				$count = count($this->collArUserCanViewReports);
			}
		}
		return $count;
	}

	/**
	 * Method called to associate a ArUserCanViewReport object to this object
	 * through the ArUserCanViewReport foreign key attribute.
	 *
	 * @param      ArUserCanViewReport $l ArUserCanViewReport
	 * @return     void
	 * @throws     PropelException
	 */
	public function addArUserCanViewReport(ArUserCanViewReport $l)
	{
		if ($this->collArUserCanViewReports === null) {
			$this->initArUserCanViewReports();
		}
		if (!in_array($l, $this->collArUserCanViewReports, true)) { // only add it if the **same** object is not already associated
			array_push($this->collArUserCanViewReports, $l);
			$l->setArReport($this);
		}
	}


	/**
	 * If this collection has already been initialized with
	 * an identical criteria, it returns the collection.
	 * Otherwise if this ArReport is new, it will return
	 * an empty collection; or if this ArReport has previously
	 * been saved, it will retrieve related ArUserCanViewReports from storage.
	 *
	 * This method is protected by default in order to keep the public
	 * api reasonable.  You can provide public methods for those you
	 * actually need in ArReport.
	 */
	public function getArUserCanViewReportsJoinArUser($criteria = null, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		if ($criteria === null) {
			$criteria = new Criteria(ArReportPeer::DATABASE_NAME);
		}
		elseif ($criteria instanceof Criteria)
		{
			$criteria = clone $criteria;
		}

		if ($this->collArUserCanViewReports === null) {
			if ($this->isNew()) {
				$this->collArUserCanViewReports = array();
			} else {

				$criteria->add(ArUserCanViewReportPeer::AR_REPORT_ID, $this->id);

				$this->collArUserCanViewReports = ArUserCanViewReportPeer::doSelectJoinArUser($criteria, $con, $join_behavior);
			}
		} else {
			// the following code is to determine if a new query is
			// called for.  If the criteria is the same as the last
			// one, just return the collection.

			$criteria->add(ArUserCanViewReportPeer::AR_REPORT_ID, $this->id);

			if (!isset($this->lastArUserCanViewReportCriteria) || !$this->lastArUserCanViewReportCriteria->equals($criteria)) {
				$this->collArUserCanViewReports = ArUserCanViewReportPeer::doSelectJoinArUser($criteria, $con, $join_behavior);
			}
		}
		$this->lastArUserCanViewReportCriteria = $criteria;

		return $this->collArUserCanViewReports;
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
			if ($this->collArReportAlsoFors) {
				foreach ((array) $this->collArReportAlsoFors as $o) {
					$o->clearAllReferences($deep);
				}
			}
			if ($this->collArReportSchedulers) {
				foreach ((array) $this->collArReportSchedulers as $o) {
					$o->clearAllReferences($deep);
				}
			}
			if ($this->collArReportToReads) {
				foreach ((array) $this->collArReportToReads as $o) {
					$o->clearAllReferences($deep);
				}
			}
			if ($this->collArReportToReadUserViews) {
				foreach ((array) $this->collArReportToReadUserViews as $o) {
					$o->clearAllReferences($deep);
				}
			}
			if ($this->collArUserCanViewReports) {
				foreach ((array) $this->collArUserCanViewReports as $o) {
					$o->clearAllReferences($deep);
				}
			}
		} // if ($deep)

		$this->collArReportAlsoFors = null;
		$this->collArReportSchedulers = null;
		$this->collArReportToReads = null;
		$this->collArReportToReadUserViews = null;
		$this->collArUserCanViewReports = null;
			$this->aArReportSet = null;
			$this->aArOrganizationUnit = null;
			$this->aArUser = null;
			$this->aArVendor = null;
			$this->aArReportOrderOfChildren = null;
	}

} // BaseArReport
