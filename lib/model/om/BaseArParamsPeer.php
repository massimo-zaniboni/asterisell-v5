<?php

/**
 * Base static class for performing query and update operations on the 'ar_params' table.
 *
 * 
 *
 * @package    lib.model.om
 */
abstract class BaseArParamsPeer {

	/** the default database name for this class */
	const DATABASE_NAME = 'propel';

	/** the table name for this class */
	const TABLE_NAME = 'ar_params';

	/** the related Propel class for this table */
	const OM_CLASS = 'ArParams';

	/** A class that can be returned by this peer. */
	const CLASS_DEFAULT = 'lib.model.ArParams';

	/** the related TableMap class for this table */
	const TM_CLASS = 'ArParamsTableMap';
	
	/** The total number of columns. */
	const NUM_COLUMNS = 45;

	/** The number of lazy-loaded columns. */
	const NUM_LAZY_LOAD_COLUMNS = 0;

	/** the column name for the ID field */
	const ID = 'ar_params.ID';

	/** the column name for the NAME field */
	const NAME = 'ar_params.NAME';

	/** the column name for the IS_DEFAULT field */
	const IS_DEFAULT = 'ar_params.IS_DEFAULT';

	/** the column name for the SERVICE_NAME field */
	const SERVICE_NAME = 'ar_params.SERVICE_NAME';

	/** the column name for the SERVICE_PROVIDER_WEBSITE field */
	const SERVICE_PROVIDER_WEBSITE = 'ar_params.SERVICE_PROVIDER_WEBSITE';

	/** the column name for the SERVICE_PROVIDER_EMAIL field */
	const SERVICE_PROVIDER_EMAIL = 'ar_params.SERVICE_PROVIDER_EMAIL';

	/** the column name for the VAT_TAX_PERC field */
	const VAT_TAX_PERC = 'ar_params.VAT_TAX_PERC';

	/** the column name for the LOGO_IMAGE field */
	const LOGO_IMAGE = 'ar_params.LOGO_IMAGE';

	/** the column name for the SLOGAN field */
	const SLOGAN = 'ar_params.SLOGAN';

	/** the column name for the LOGO_IMAGE_IN_INVOICES field */
	const LOGO_IMAGE_IN_INVOICES = 'ar_params.LOGO_IMAGE_IN_INVOICES';

	/** the column name for the FOOTER field */
	const FOOTER = 'ar_params.FOOTER';

	/** the column name for the USER_MESSAGE field */
	const USER_MESSAGE = 'ar_params.USER_MESSAGE';

	/** the column name for the LEGAL_NAME field */
	const LEGAL_NAME = 'ar_params.LEGAL_NAME';

	/** the column name for the EXTERNAL_CRM_CODE field */
	const EXTERNAL_CRM_CODE = 'ar_params.EXTERNAL_CRM_CODE';

	/** the column name for the VAT field */
	const VAT = 'ar_params.VAT';

	/** the column name for the LEGAL_ADDRESS field */
	const LEGAL_ADDRESS = 'ar_params.LEGAL_ADDRESS';

	/** the column name for the LEGAL_WEBSITE field */
	const LEGAL_WEBSITE = 'ar_params.LEGAL_WEBSITE';

	/** the column name for the LEGAL_CITY field */
	const LEGAL_CITY = 'ar_params.LEGAL_CITY';

	/** the column name for the LEGAL_ZIPCODE field */
	const LEGAL_ZIPCODE = 'ar_params.LEGAL_ZIPCODE';

	/** the column name for the LEGAL_STATE_PROVINCE field */
	const LEGAL_STATE_PROVINCE = 'ar_params.LEGAL_STATE_PROVINCE';

	/** the column name for the LEGAL_COUNTRY field */
	const LEGAL_COUNTRY = 'ar_params.LEGAL_COUNTRY';

	/** the column name for the LEGAL_EMAIL field */
	const LEGAL_EMAIL = 'ar_params.LEGAL_EMAIL';

	/** the column name for the LEGAL_PHONE field */
	const LEGAL_PHONE = 'ar_params.LEGAL_PHONE';

	/** the column name for the PHONE2 field */
	const PHONE2 = 'ar_params.PHONE2';

	/** the column name for the LEGAL_FAX field */
	const LEGAL_FAX = 'ar_params.LEGAL_FAX';

	/** the column name for the INVOICE_NOTES field */
	const INVOICE_NOTES = 'ar_params.INVOICE_NOTES';

	/** the column name for the INVOICE_PAYMENT_TERMS field */
	const INVOICE_PAYMENT_TERMS = 'ar_params.INVOICE_PAYMENT_TERMS';

	/** the column name for the INVOICE_PAYMENT_DUE_IN_XX_DAYS field */
	const INVOICE_PAYMENT_DUE_IN_XX_DAYS = 'ar_params.INVOICE_PAYMENT_DUE_IN_XX_DAYS';

	/** the column name for the SENDER_NAME_ON_INVOICING_EMAILS field */
	const SENDER_NAME_ON_INVOICING_EMAILS = 'ar_params.SENDER_NAME_ON_INVOICING_EMAILS';

	/** the column name for the INVOICING_EMAIL_ADDRESS field */
	const INVOICING_EMAIL_ADDRESS = 'ar_params.INVOICING_EMAIL_ADDRESS';

	/** the column name for the LOGO_HTML_COLOR field */
	const LOGO_HTML_COLOR = 'ar_params.LOGO_HTML_COLOR';

	/** the column name for the HTML_NOTES_ON_THE_LOGIN_FORM field */
	const HTML_NOTES_ON_THE_LOGIN_FORM = 'ar_params.HTML_NOTES_ON_THE_LOGIN_FORM';

	/** the column name for the OFFICIAL_CALLDATE field */
	const OFFICIAL_CALLDATE = 'ar_params.OFFICIAL_CALLDATE';

	/** the column name for the SCHEDULED_RERATE_FROM_OFFICIAL_CALLDATE field */
	const SCHEDULED_RERATE_FROM_OFFICIAL_CALLDATE = 'ar_params.SCHEDULED_RERATE_FROM_OFFICIAL_CALLDATE';

	/** the column name for the NEW_IMPORTED_CDRS_FROM_CALLDATE field */
	const NEW_IMPORTED_CDRS_FROM_CALLDATE = 'ar_params.NEW_IMPORTED_CDRS_FROM_CALLDATE';

	/** the column name for the NEW_IMPORTED_CDRS_TO_CALLDATE field */
	const NEW_IMPORTED_CDRS_TO_CALLDATE = 'ar_params.NEW_IMPORTED_CDRS_TO_CALLDATE';

	/** the column name for the SCHEDULED_RERATE_FROM_SPECIFIC_CALLDATE field */
	const SCHEDULED_RERATE_FROM_SPECIFIC_CALLDATE = 'ar_params.SCHEDULED_RERATE_FROM_SPECIFIC_CALLDATE';

	/** the column name for the SCHEDULED_RERATE_TO_SPECIFIC_CALLDATE field */
	const SCHEDULED_RERATE_TO_SPECIFIC_CALLDATE = 'ar_params.SCHEDULED_RERATE_TO_SPECIFIC_CALLDATE';

	/** the column name for the SCHEDULED_IMPORTED_SERVICES_RERATE_FROM_SPECIFIC_CALLDATE field */
	const SCHEDULED_IMPORTED_SERVICES_RERATE_FROM_SPECIFIC_CALLDATE = 'ar_params.SCHEDULED_IMPORTED_SERVICES_RERATE_FROM_SPECIFIC_CALLDATE';

	/** the column name for the SCHEDULED_IMPORTED_SERVICES_RERATE_TO_SPECIFIC_CALLDATE field */
	const SCHEDULED_IMPORTED_SERVICES_RERATE_TO_SPECIFIC_CALLDATE = 'ar_params.SCHEDULED_IMPORTED_SERVICES_RERATE_TO_SPECIFIC_CALLDATE';

	/** the column name for the CURRENT_COUNT_OF_RERATING_FAILED_ATTEMPTS field */
	const CURRENT_COUNT_OF_RERATING_FAILED_ATTEMPTS = 'ar_params.CURRENT_COUNT_OF_RERATING_FAILED_ATTEMPTS';

	/** the column name for the CURRENT_RERATING_EVENT_IS_RUNNING field */
	const CURRENT_RERATING_EVENT_IS_RUNNING = 'ar_params.CURRENT_RERATING_EVENT_IS_RUNNING';

	/** the column name for the SHOULD_RESCHEDULE_RERATE_FROM_OFFICIAL_CALLDATE field */
	const SHOULD_RESCHEDULE_RERATE_FROM_OFFICIAL_CALLDATE = 'ar_params.SHOULD_RESCHEDULE_RERATE_FROM_OFFICIAL_CALLDATE';

	/** the column name for the WAIT_FOR_SCHEDULED_RERATE field */
	const WAIT_FOR_SCHEDULED_RERATE = 'ar_params.WAIT_FOR_SCHEDULED_RERATE';

	/** the column name for the CLEAN_ERROR_TABLE field */
	const CLEAN_ERROR_TABLE = 'ar_params.CLEAN_ERROR_TABLE';

	/**
	 * An identiy map to hold any loaded instances of ArParams objects.
	 * This must be public so that other peer classes can access this when hydrating from JOIN
	 * queries.
	 * @var        array ArParams[]
	 */
	public static $instances = array();


	// symfony behavior
	
	/**
	 * Indicates whether the current model includes I18N.
	 */
	const IS_I18N = false;

	/**
	 * holds an array of fieldnames
	 *
	 * first dimension keys are the type constants
	 * e.g. self::$fieldNames[self::TYPE_PHPNAME][0] = 'Id'
	 */
	private static $fieldNames = array (
		BasePeer::TYPE_PHPNAME => array ('Id', 'Name', 'IsDefault', 'ServiceName', 'ServiceProviderWebsite', 'ServiceProviderEmail', 'VatTaxPerc', 'LogoImage', 'Slogan', 'LogoImageInInvoices', 'Footer', 'UserMessage', 'LegalName', 'ExternalCrmCode', 'Vat', 'LegalAddress', 'LegalWebsite', 'LegalCity', 'LegalZipcode', 'LegalStateProvince', 'LegalCountry', 'LegalEmail', 'LegalPhone', 'Phone2', 'LegalFax', 'InvoiceNotes', 'InvoicePaymentTerms', 'InvoicePaymentDueInXxDays', 'SenderNameOnInvoicingEmails', 'InvoicingEmailAddress', 'LogoHtmlColor', 'HtmlNotesOnTheLoginForm', 'OfficialCalldate', 'ScheduledRerateFromOfficialCalldate', 'NewImportedCdrsFromCalldate', 'NewImportedCdrsToCalldate', 'ScheduledRerateFromSpecificCalldate', 'ScheduledRerateToSpecificCalldate', 'ScheduledImportedServicesRerateFromSpecificCalldate', 'ScheduledImportedServicesRerateToSpecificCalldate', 'CurrentCountOfReratingFailedAttempts', 'CurrentReratingEventIsRunning', 'ShouldRescheduleRerateFromOfficialCalldate', 'WaitForScheduledRerate', 'CleanErrorTable', ),
		BasePeer::TYPE_STUDLYPHPNAME => array ('id', 'name', 'isDefault', 'serviceName', 'serviceProviderWebsite', 'serviceProviderEmail', 'vatTaxPerc', 'logoImage', 'slogan', 'logoImageInInvoices', 'footer', 'userMessage', 'legalName', 'externalCrmCode', 'vat', 'legalAddress', 'legalWebsite', 'legalCity', 'legalZipcode', 'legalStateProvince', 'legalCountry', 'legalEmail', 'legalPhone', 'phone2', 'legalFax', 'invoiceNotes', 'invoicePaymentTerms', 'invoicePaymentDueInXxDays', 'senderNameOnInvoicingEmails', 'invoicingEmailAddress', 'logoHtmlColor', 'htmlNotesOnTheLoginForm', 'officialCalldate', 'scheduledRerateFromOfficialCalldate', 'newImportedCdrsFromCalldate', 'newImportedCdrsToCalldate', 'scheduledRerateFromSpecificCalldate', 'scheduledRerateToSpecificCalldate', 'scheduledImportedServicesRerateFromSpecificCalldate', 'scheduledImportedServicesRerateToSpecificCalldate', 'currentCountOfReratingFailedAttempts', 'currentReratingEventIsRunning', 'shouldRescheduleRerateFromOfficialCalldate', 'waitForScheduledRerate', 'cleanErrorTable', ),
		BasePeer::TYPE_COLNAME => array (self::ID, self::NAME, self::IS_DEFAULT, self::SERVICE_NAME, self::SERVICE_PROVIDER_WEBSITE, self::SERVICE_PROVIDER_EMAIL, self::VAT_TAX_PERC, self::LOGO_IMAGE, self::SLOGAN, self::LOGO_IMAGE_IN_INVOICES, self::FOOTER, self::USER_MESSAGE, self::LEGAL_NAME, self::EXTERNAL_CRM_CODE, self::VAT, self::LEGAL_ADDRESS, self::LEGAL_WEBSITE, self::LEGAL_CITY, self::LEGAL_ZIPCODE, self::LEGAL_STATE_PROVINCE, self::LEGAL_COUNTRY, self::LEGAL_EMAIL, self::LEGAL_PHONE, self::PHONE2, self::LEGAL_FAX, self::INVOICE_NOTES, self::INVOICE_PAYMENT_TERMS, self::INVOICE_PAYMENT_DUE_IN_XX_DAYS, self::SENDER_NAME_ON_INVOICING_EMAILS, self::INVOICING_EMAIL_ADDRESS, self::LOGO_HTML_COLOR, self::HTML_NOTES_ON_THE_LOGIN_FORM, self::OFFICIAL_CALLDATE, self::SCHEDULED_RERATE_FROM_OFFICIAL_CALLDATE, self::NEW_IMPORTED_CDRS_FROM_CALLDATE, self::NEW_IMPORTED_CDRS_TO_CALLDATE, self::SCHEDULED_RERATE_FROM_SPECIFIC_CALLDATE, self::SCHEDULED_RERATE_TO_SPECIFIC_CALLDATE, self::SCHEDULED_IMPORTED_SERVICES_RERATE_FROM_SPECIFIC_CALLDATE, self::SCHEDULED_IMPORTED_SERVICES_RERATE_TO_SPECIFIC_CALLDATE, self::CURRENT_COUNT_OF_RERATING_FAILED_ATTEMPTS, self::CURRENT_RERATING_EVENT_IS_RUNNING, self::SHOULD_RESCHEDULE_RERATE_FROM_OFFICIAL_CALLDATE, self::WAIT_FOR_SCHEDULED_RERATE, self::CLEAN_ERROR_TABLE, ),
		BasePeer::TYPE_FIELDNAME => array ('id', 'name', 'is_default', 'service_name', 'service_provider_website', 'service_provider_email', 'vat_tax_perc', 'logo_image', 'slogan', 'logo_image_in_invoices', 'footer', 'user_message', 'legal_name', 'external_crm_code', 'vat', 'legal_address', 'legal_website', 'legal_city', 'legal_zipcode', 'legal_state_province', 'legal_country', 'legal_email', 'legal_phone', 'phone2', 'legal_fax', 'invoice_notes', 'invoice_payment_terms', 'invoice_payment_due_in_xx_days', 'sender_name_on_invoicing_emails', 'invoicing_email_address', 'logo_html_color', 'html_notes_on_the_login_form', 'official_calldate', 'scheduled_rerate_from_official_calldate', 'new_imported_cdrs_from_calldate', 'new_imported_cdrs_to_calldate', 'scheduled_rerate_from_specific_calldate', 'scheduled_rerate_to_specific_calldate', 'scheduled_imported_services_rerate_from_specific_calldate', 'scheduled_imported_services_rerate_to_specific_calldate', 'current_count_of_rerating_failed_attempts', 'current_rerating_event_is_running', 'should_reschedule_rerate_from_official_calldate', 'wait_for_scheduled_rerate', 'clean_error_table', ),
		BasePeer::TYPE_NUM => array (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, )
	);

	/**
	 * holds an array of keys for quick access to the fieldnames array
	 *
	 * first dimension keys are the type constants
	 * e.g. self::$fieldNames[BasePeer::TYPE_PHPNAME]['Id'] = 0
	 */
	private static $fieldKeys = array (
		BasePeer::TYPE_PHPNAME => array ('Id' => 0, 'Name' => 1, 'IsDefault' => 2, 'ServiceName' => 3, 'ServiceProviderWebsite' => 4, 'ServiceProviderEmail' => 5, 'VatTaxPerc' => 6, 'LogoImage' => 7, 'Slogan' => 8, 'LogoImageInInvoices' => 9, 'Footer' => 10, 'UserMessage' => 11, 'LegalName' => 12, 'ExternalCrmCode' => 13, 'Vat' => 14, 'LegalAddress' => 15, 'LegalWebsite' => 16, 'LegalCity' => 17, 'LegalZipcode' => 18, 'LegalStateProvince' => 19, 'LegalCountry' => 20, 'LegalEmail' => 21, 'LegalPhone' => 22, 'Phone2' => 23, 'LegalFax' => 24, 'InvoiceNotes' => 25, 'InvoicePaymentTerms' => 26, 'InvoicePaymentDueInXxDays' => 27, 'SenderNameOnInvoicingEmails' => 28, 'InvoicingEmailAddress' => 29, 'LogoHtmlColor' => 30, 'HtmlNotesOnTheLoginForm' => 31, 'OfficialCalldate' => 32, 'ScheduledRerateFromOfficialCalldate' => 33, 'NewImportedCdrsFromCalldate' => 34, 'NewImportedCdrsToCalldate' => 35, 'ScheduledRerateFromSpecificCalldate' => 36, 'ScheduledRerateToSpecificCalldate' => 37, 'ScheduledImportedServicesRerateFromSpecificCalldate' => 38, 'ScheduledImportedServicesRerateToSpecificCalldate' => 39, 'CurrentCountOfReratingFailedAttempts' => 40, 'CurrentReratingEventIsRunning' => 41, 'ShouldRescheduleRerateFromOfficialCalldate' => 42, 'WaitForScheduledRerate' => 43, 'CleanErrorTable' => 44, ),
		BasePeer::TYPE_STUDLYPHPNAME => array ('id' => 0, 'name' => 1, 'isDefault' => 2, 'serviceName' => 3, 'serviceProviderWebsite' => 4, 'serviceProviderEmail' => 5, 'vatTaxPerc' => 6, 'logoImage' => 7, 'slogan' => 8, 'logoImageInInvoices' => 9, 'footer' => 10, 'userMessage' => 11, 'legalName' => 12, 'externalCrmCode' => 13, 'vat' => 14, 'legalAddress' => 15, 'legalWebsite' => 16, 'legalCity' => 17, 'legalZipcode' => 18, 'legalStateProvince' => 19, 'legalCountry' => 20, 'legalEmail' => 21, 'legalPhone' => 22, 'phone2' => 23, 'legalFax' => 24, 'invoiceNotes' => 25, 'invoicePaymentTerms' => 26, 'invoicePaymentDueInXxDays' => 27, 'senderNameOnInvoicingEmails' => 28, 'invoicingEmailAddress' => 29, 'logoHtmlColor' => 30, 'htmlNotesOnTheLoginForm' => 31, 'officialCalldate' => 32, 'scheduledRerateFromOfficialCalldate' => 33, 'newImportedCdrsFromCalldate' => 34, 'newImportedCdrsToCalldate' => 35, 'scheduledRerateFromSpecificCalldate' => 36, 'scheduledRerateToSpecificCalldate' => 37, 'scheduledImportedServicesRerateFromSpecificCalldate' => 38, 'scheduledImportedServicesRerateToSpecificCalldate' => 39, 'currentCountOfReratingFailedAttempts' => 40, 'currentReratingEventIsRunning' => 41, 'shouldRescheduleRerateFromOfficialCalldate' => 42, 'waitForScheduledRerate' => 43, 'cleanErrorTable' => 44, ),
		BasePeer::TYPE_COLNAME => array (self::ID => 0, self::NAME => 1, self::IS_DEFAULT => 2, self::SERVICE_NAME => 3, self::SERVICE_PROVIDER_WEBSITE => 4, self::SERVICE_PROVIDER_EMAIL => 5, self::VAT_TAX_PERC => 6, self::LOGO_IMAGE => 7, self::SLOGAN => 8, self::LOGO_IMAGE_IN_INVOICES => 9, self::FOOTER => 10, self::USER_MESSAGE => 11, self::LEGAL_NAME => 12, self::EXTERNAL_CRM_CODE => 13, self::VAT => 14, self::LEGAL_ADDRESS => 15, self::LEGAL_WEBSITE => 16, self::LEGAL_CITY => 17, self::LEGAL_ZIPCODE => 18, self::LEGAL_STATE_PROVINCE => 19, self::LEGAL_COUNTRY => 20, self::LEGAL_EMAIL => 21, self::LEGAL_PHONE => 22, self::PHONE2 => 23, self::LEGAL_FAX => 24, self::INVOICE_NOTES => 25, self::INVOICE_PAYMENT_TERMS => 26, self::INVOICE_PAYMENT_DUE_IN_XX_DAYS => 27, self::SENDER_NAME_ON_INVOICING_EMAILS => 28, self::INVOICING_EMAIL_ADDRESS => 29, self::LOGO_HTML_COLOR => 30, self::HTML_NOTES_ON_THE_LOGIN_FORM => 31, self::OFFICIAL_CALLDATE => 32, self::SCHEDULED_RERATE_FROM_OFFICIAL_CALLDATE => 33, self::NEW_IMPORTED_CDRS_FROM_CALLDATE => 34, self::NEW_IMPORTED_CDRS_TO_CALLDATE => 35, self::SCHEDULED_RERATE_FROM_SPECIFIC_CALLDATE => 36, self::SCHEDULED_RERATE_TO_SPECIFIC_CALLDATE => 37, self::SCHEDULED_IMPORTED_SERVICES_RERATE_FROM_SPECIFIC_CALLDATE => 38, self::SCHEDULED_IMPORTED_SERVICES_RERATE_TO_SPECIFIC_CALLDATE => 39, self::CURRENT_COUNT_OF_RERATING_FAILED_ATTEMPTS => 40, self::CURRENT_RERATING_EVENT_IS_RUNNING => 41, self::SHOULD_RESCHEDULE_RERATE_FROM_OFFICIAL_CALLDATE => 42, self::WAIT_FOR_SCHEDULED_RERATE => 43, self::CLEAN_ERROR_TABLE => 44, ),
		BasePeer::TYPE_FIELDNAME => array ('id' => 0, 'name' => 1, 'is_default' => 2, 'service_name' => 3, 'service_provider_website' => 4, 'service_provider_email' => 5, 'vat_tax_perc' => 6, 'logo_image' => 7, 'slogan' => 8, 'logo_image_in_invoices' => 9, 'footer' => 10, 'user_message' => 11, 'legal_name' => 12, 'external_crm_code' => 13, 'vat' => 14, 'legal_address' => 15, 'legal_website' => 16, 'legal_city' => 17, 'legal_zipcode' => 18, 'legal_state_province' => 19, 'legal_country' => 20, 'legal_email' => 21, 'legal_phone' => 22, 'phone2' => 23, 'legal_fax' => 24, 'invoice_notes' => 25, 'invoice_payment_terms' => 26, 'invoice_payment_due_in_xx_days' => 27, 'sender_name_on_invoicing_emails' => 28, 'invoicing_email_address' => 29, 'logo_html_color' => 30, 'html_notes_on_the_login_form' => 31, 'official_calldate' => 32, 'scheduled_rerate_from_official_calldate' => 33, 'new_imported_cdrs_from_calldate' => 34, 'new_imported_cdrs_to_calldate' => 35, 'scheduled_rerate_from_specific_calldate' => 36, 'scheduled_rerate_to_specific_calldate' => 37, 'scheduled_imported_services_rerate_from_specific_calldate' => 38, 'scheduled_imported_services_rerate_to_specific_calldate' => 39, 'current_count_of_rerating_failed_attempts' => 40, 'current_rerating_event_is_running' => 41, 'should_reschedule_rerate_from_official_calldate' => 42, 'wait_for_scheduled_rerate' => 43, 'clean_error_table' => 44, ),
		BasePeer::TYPE_NUM => array (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, )
	);

	/**
	 * Translates a fieldname to another type
	 *
	 * @param      string $name field name
	 * @param      string $fromType One of the class type constants BasePeer::TYPE_PHPNAME, BasePeer::TYPE_STUDLYPHPNAME
	 *                         BasePeer::TYPE_COLNAME, BasePeer::TYPE_FIELDNAME, BasePeer::TYPE_NUM
	 * @param      string $toType   One of the class type constants
	 * @return     string translated name of the field.
	 * @throws     PropelException - if the specified name could not be found in the fieldname mappings.
	 */
	static public function translateFieldName($name, $fromType, $toType)
	{
		$toNames = self::getFieldNames($toType);
		$key = isset(self::$fieldKeys[$fromType][$name]) ? self::$fieldKeys[$fromType][$name] : null;
		if ($key === null) {
			throw new PropelException("'$name' could not be found in the field names of type '$fromType'. These are: " . print_r(self::$fieldKeys[$fromType], true));
		}
		return $toNames[$key];
	}

	/**
	 * Returns an array of field names.
	 *
	 * @param      string $type The type of fieldnames to return:
	 *                      One of the class type constants BasePeer::TYPE_PHPNAME, BasePeer::TYPE_STUDLYPHPNAME
	 *                      BasePeer::TYPE_COLNAME, BasePeer::TYPE_FIELDNAME, BasePeer::TYPE_NUM
	 * @return     array A list of field names
	 */

	static public function getFieldNames($type = BasePeer::TYPE_PHPNAME)
	{
		if (!array_key_exists($type, self::$fieldNames)) {
			throw new PropelException('Method getFieldNames() expects the parameter $type to be one of the class constants BasePeer::TYPE_PHPNAME, BasePeer::TYPE_STUDLYPHPNAME, BasePeer::TYPE_COLNAME, BasePeer::TYPE_FIELDNAME, BasePeer::TYPE_NUM. ' . $type . ' was given.');
		}
		return self::$fieldNames[$type];
	}

	/**
	 * Convenience method which changes table.column to alias.column.
	 *
	 * Using this method you can maintain SQL abstraction while using column aliases.
	 * <code>
	 *		$c->addAlias("alias1", TablePeer::TABLE_NAME);
	 *		$c->addJoin(TablePeer::alias("alias1", TablePeer::PRIMARY_KEY_COLUMN), TablePeer::PRIMARY_KEY_COLUMN);
	 * </code>
	 * @param      string $alias The alias for the current table.
	 * @param      string $column The column name for current table. (i.e. ArParamsPeer::COLUMN_NAME).
	 * @return     string
	 */
	public static function alias($alias, $column)
	{
		return str_replace(ArParamsPeer::TABLE_NAME.'.', $alias.'.', $column);
	}

	/**
	 * Add all the columns needed to create a new object.
	 *
	 * Note: any columns that were marked with lazyLoad="true" in the
	 * XML schema will not be added to the select list and only loaded
	 * on demand.
	 *
	 * @param      criteria object containing the columns to add.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function addSelectColumns(Criteria $criteria)
	{
		$criteria->addSelectColumn(ArParamsPeer::ID);
		$criteria->addSelectColumn(ArParamsPeer::NAME);
		$criteria->addSelectColumn(ArParamsPeer::IS_DEFAULT);
		$criteria->addSelectColumn(ArParamsPeer::SERVICE_NAME);
		$criteria->addSelectColumn(ArParamsPeer::SERVICE_PROVIDER_WEBSITE);
		$criteria->addSelectColumn(ArParamsPeer::SERVICE_PROVIDER_EMAIL);
		$criteria->addSelectColumn(ArParamsPeer::VAT_TAX_PERC);
		$criteria->addSelectColumn(ArParamsPeer::LOGO_IMAGE);
		$criteria->addSelectColumn(ArParamsPeer::SLOGAN);
		$criteria->addSelectColumn(ArParamsPeer::LOGO_IMAGE_IN_INVOICES);
		$criteria->addSelectColumn(ArParamsPeer::FOOTER);
		$criteria->addSelectColumn(ArParamsPeer::USER_MESSAGE);
		$criteria->addSelectColumn(ArParamsPeer::LEGAL_NAME);
		$criteria->addSelectColumn(ArParamsPeer::EXTERNAL_CRM_CODE);
		$criteria->addSelectColumn(ArParamsPeer::VAT);
		$criteria->addSelectColumn(ArParamsPeer::LEGAL_ADDRESS);
		$criteria->addSelectColumn(ArParamsPeer::LEGAL_WEBSITE);
		$criteria->addSelectColumn(ArParamsPeer::LEGAL_CITY);
		$criteria->addSelectColumn(ArParamsPeer::LEGAL_ZIPCODE);
		$criteria->addSelectColumn(ArParamsPeer::LEGAL_STATE_PROVINCE);
		$criteria->addSelectColumn(ArParamsPeer::LEGAL_COUNTRY);
		$criteria->addSelectColumn(ArParamsPeer::LEGAL_EMAIL);
		$criteria->addSelectColumn(ArParamsPeer::LEGAL_PHONE);
		$criteria->addSelectColumn(ArParamsPeer::PHONE2);
		$criteria->addSelectColumn(ArParamsPeer::LEGAL_FAX);
		$criteria->addSelectColumn(ArParamsPeer::INVOICE_NOTES);
		$criteria->addSelectColumn(ArParamsPeer::INVOICE_PAYMENT_TERMS);
		$criteria->addSelectColumn(ArParamsPeer::INVOICE_PAYMENT_DUE_IN_XX_DAYS);
		$criteria->addSelectColumn(ArParamsPeer::SENDER_NAME_ON_INVOICING_EMAILS);
		$criteria->addSelectColumn(ArParamsPeer::INVOICING_EMAIL_ADDRESS);
		$criteria->addSelectColumn(ArParamsPeer::LOGO_HTML_COLOR);
		$criteria->addSelectColumn(ArParamsPeer::HTML_NOTES_ON_THE_LOGIN_FORM);
		$criteria->addSelectColumn(ArParamsPeer::OFFICIAL_CALLDATE);
		$criteria->addSelectColumn(ArParamsPeer::SCHEDULED_RERATE_FROM_OFFICIAL_CALLDATE);
		$criteria->addSelectColumn(ArParamsPeer::NEW_IMPORTED_CDRS_FROM_CALLDATE);
		$criteria->addSelectColumn(ArParamsPeer::NEW_IMPORTED_CDRS_TO_CALLDATE);
		$criteria->addSelectColumn(ArParamsPeer::SCHEDULED_RERATE_FROM_SPECIFIC_CALLDATE);
		$criteria->addSelectColumn(ArParamsPeer::SCHEDULED_RERATE_TO_SPECIFIC_CALLDATE);
		$criteria->addSelectColumn(ArParamsPeer::SCHEDULED_IMPORTED_SERVICES_RERATE_FROM_SPECIFIC_CALLDATE);
		$criteria->addSelectColumn(ArParamsPeer::SCHEDULED_IMPORTED_SERVICES_RERATE_TO_SPECIFIC_CALLDATE);
		$criteria->addSelectColumn(ArParamsPeer::CURRENT_COUNT_OF_RERATING_FAILED_ATTEMPTS);
		$criteria->addSelectColumn(ArParamsPeer::CURRENT_RERATING_EVENT_IS_RUNNING);
		$criteria->addSelectColumn(ArParamsPeer::SHOULD_RESCHEDULE_RERATE_FROM_OFFICIAL_CALLDATE);
		$criteria->addSelectColumn(ArParamsPeer::WAIT_FOR_SCHEDULED_RERATE);
		$criteria->addSelectColumn(ArParamsPeer::CLEAN_ERROR_TABLE);
	}

	/**
	 * Returns the number of rows matching criteria.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @return     int Number of matching rows.
	 */
	public static function doCount(Criteria $criteria, $distinct = false, PropelPDO $con = null)
	{
		// we may modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArParamsPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArParamsPeer::addSelectColumns($criteria);
		}

		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		$criteria->setDbName(self::DATABASE_NAME); // Set the correct dbName

		if ($con === null) {
			$con = Propel::getConnection(ArParamsPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}
		// BasePeer returns a PDOStatement
		$stmt = BasePeer::doCount($criteria, $con);

		if ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$count = (int) $row[0];
		} else {
			$count = 0; // no rows returned; we infer that means 0 matches.
		}
		$stmt->closeCursor();
		return $count;
	}
	/**
	 * Method to select one object from the DB.
	 *
	 * @param      Criteria $criteria object used to create the SELECT statement.
	 * @param      PropelPDO $con
	 * @return     ArParams
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectOne(Criteria $criteria, PropelPDO $con = null)
	{
		$critcopy = clone $criteria;
		$critcopy->setLimit(1);
		$objects = ArParamsPeer::doSelect($critcopy, $con);
		if ($objects) {
			return $objects[0];
		}
		return null;
	}
	/**
	 * Method to do selects.
	 *
	 * @param      Criteria $criteria The Criteria object used to build the SELECT statement.
	 * @param      PropelPDO $con
	 * @return     array Array of selected Objects
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelect(Criteria $criteria, PropelPDO $con = null)
	{
		return ArParamsPeer::populateObjects(ArParamsPeer::doSelectStmt($criteria, $con));
	}
	/**
	 * Prepares the Criteria object and uses the parent doSelect() method to execute a PDOStatement.
	 *
	 * Use this method directly if you want to work with an executed statement durirectly (for example
	 * to perform your own object hydration).
	 *
	 * @param      Criteria $criteria The Criteria object used to build the SELECT statement.
	 * @param      PropelPDO $con The connection to use
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 * @return     PDOStatement The executed PDOStatement object.
	 * @see        BasePeer::doSelect()
	 */
	public static function doSelectStmt(Criteria $criteria, PropelPDO $con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArParamsPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		if (!$criteria->hasSelectClause()) {
			$criteria = clone $criteria;
			ArParamsPeer::addSelectColumns($criteria);
		}

		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		// BasePeer returns a PDOStatement
		return BasePeer::doSelect($criteria, $con);
	}
	/**
	 * Adds an object to the instance pool.
	 *
	 * Propel keeps cached copies of objects in an instance pool when they are retrieved
	 * from the database.  In some cases -- especially when you override doSelect*()
	 * methods in your stub classes -- you may need to explicitly add objects
	 * to the cache in order to ensure that the same objects are always returned by doSelect*()
	 * and retrieveByPK*() calls.
	 *
	 * @param      ArParams $value A ArParams object.
	 * @param      string $key (optional) key to use for instance map (for performance boost if key was already calculated externally).
	 */
	public static function addInstanceToPool(ArParams $obj, $key = null)
	{
		if (Propel::isInstancePoolingEnabled()) {
			if ($key === null) {
				$key = (string) $obj->getId();
			} // if key === null
			self::$instances[$key] = $obj;
		}
	}

	/**
	 * Removes an object from the instance pool.
	 *
	 * Propel keeps cached copies of objects in an instance pool when they are retrieved
	 * from the database.  In some cases -- especially when you override doDelete
	 * methods in your stub classes -- you may need to explicitly remove objects
	 * from the cache in order to prevent returning objects that no longer exist.
	 *
	 * @param      mixed $value A ArParams object or a primary key value.
	 */
	public static function removeInstanceFromPool($value)
	{
		if (Propel::isInstancePoolingEnabled() && $value !== null) {
			if (is_object($value) && $value instanceof ArParams) {
				$key = (string) $value->getId();
			} elseif (is_scalar($value)) {
				// assume we've been passed a primary key
				$key = (string) $value;
			} else {
				$e = new PropelException("Invalid value passed to removeInstanceFromPool().  Expected primary key or ArParams object; got " . (is_object($value) ? get_class($value) . ' object.' : var_export($value,true)));
				throw $e;
			}

			unset(self::$instances[$key]);
		}
	} // removeInstanceFromPool()

	/**
	 * Retrieves a string version of the primary key from the DB resultset row that can be used to uniquely identify a row in this table.
	 *
	 * For tables with a single-column primary key, that simple pkey value will be returned.  For tables with
	 * a multi-column primary key, a serialize()d version of the primary key will be returned.
	 *
	 * @param      string $key The key (@see getPrimaryKeyHash()) for this instance.
	 * @return     ArParams Found object or NULL if 1) no instance exists for specified key or 2) instance pooling has been disabled.
	 * @see        getPrimaryKeyHash()
	 */
	public static function getInstanceFromPool($key)
	{
		if (Propel::isInstancePoolingEnabled()) {
			if (isset(self::$instances[$key])) {
				return self::$instances[$key];
			}
		}
		return null; // just to be explicit
	}
	
	/**
	 * Clear the instance pool.
	 *
	 * @return     void
	 */
	public static function clearInstancePool()
	{
		self::$instances = array();
	}
	
	/**
	 * Method to invalidate the instance pool of all tables related to ar_params
	 * by a foreign key with ON DELETE CASCADE
	 */
	public static function clearRelatedInstancePool()
	{
	}

	/**
	 * Retrieves a string version of the primary key from the DB resultset row that can be used to uniquely identify a row in this table.
	 *
	 * For tables with a single-column primary key, that simple pkey value will be returned.  For tables with
	 * a multi-column primary key, a serialize()d version of the primary key will be returned.
	 *
	 * @param      array $row PropelPDO resultset row.
	 * @param      int $startcol The 0-based offset for reading from the resultset row.
	 * @return     string A string version of PK or NULL if the components of primary key in result array are all null.
	 */
	public static function getPrimaryKeyHashFromRow($row, $startcol = 0)
	{
		// If the PK cannot be derived from the row, return NULL.
		if ($row[$startcol] === null) {
			return null;
		}
		return (string) $row[$startcol];
	}

	/**
	 * The returned array will contain objects of the default type or
	 * objects that inherit from the default.
	 *
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function populateObjects(PDOStatement $stmt)
	{
		$results = array();
	
		// set the class once to avoid overhead in the loop
		$cls = ArParamsPeer::getOMClass(false);
		// populate the object(s)
		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key = ArParamsPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj = ArParamsPeer::getInstanceFromPool($key))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj->hydrate($row, 0, true); // rehydrate
				$results[] = $obj;
			} else {
				$obj = new $cls();
				$obj->hydrate($row);
				$results[] = $obj;
				ArParamsPeer::addInstanceToPool($obj, $key);
			} // if key exists
		}
		$stmt->closeCursor();
		return $results;
	}
	/**
	 * Returns the TableMap related to this peer.
	 * This method is not needed for general use but a specific application could have a need.
	 * @return     TableMap
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function getTableMap()
	{
		return Propel::getDatabaseMap(self::DATABASE_NAME)->getTable(self::TABLE_NAME);
	}

	/**
	 * Add a TableMap instance to the database for this peer class.
	 */
	public static function buildTableMap()
	{
	  $dbMap = Propel::getDatabaseMap(BaseArParamsPeer::DATABASE_NAME);
	  if (!$dbMap->hasTable(BaseArParamsPeer::TABLE_NAME))
	  {
	    $dbMap->addTableObject(new ArParamsTableMap());
	  }
	}

	/**
	 * The class that the Peer will make instances of.
	 *
	 * If $withPrefix is true, the returned path
	 * uses a dot-path notation which is tranalted into a path
	 * relative to a location on the PHP include_path.
	 * (e.g. path.to.MyClass -> 'path/to/MyClass.php')
	 *
	 * @param      boolean  Whether or not to return the path wit hthe class name 
	 * @return     string path.to.ClassName
	 */
	public static function getOMClass($withPrefix = true)
	{
		return $withPrefix ? ArParamsPeer::CLASS_DEFAULT : ArParamsPeer::OM_CLASS;
	}

	/**
	 * Method perform an INSERT on the database, given a ArParams or Criteria object.
	 *
	 * @param      mixed $values Criteria or ArParams object containing data that is used to create the INSERT statement.
	 * @param      PropelPDO $con the PropelPDO connection to use
	 * @return     mixed The new primary key.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doInsert($values, PropelPDO $con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArParamsPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		if ($values instanceof Criteria) {
			$criteria = clone $values; // rename for clarity
		} else {
			$criteria = $values->buildCriteria(); // build Criteria from ArParams object
		}

		if ($criteria->containsKey(ArParamsPeer::ID) && $criteria->keyContainsValue(ArParamsPeer::ID) ) {
			throw new PropelException('Cannot insert a value for auto-increment primary key ('.ArParamsPeer::ID.')');
		}


		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		try {
			// use transaction because $criteria could contain info
			// for more than one table (I guess, conceivably)
			$con->beginTransaction();
			$pk = BasePeer::doInsert($criteria, $con);
			$con->commit();
		} catch(PropelException $e) {
			$con->rollBack();
			throw $e;
		}

		return $pk;
	}

	/**
	 * Method perform an UPDATE on the database, given a ArParams or Criteria object.
	 *
	 * @param      mixed $values Criteria or ArParams object containing data that is used to create the UPDATE statement.
	 * @param      PropelPDO $con The connection to use (specify PropelPDO connection object to exert more control over transactions).
	 * @return     int The number of affected rows (if supported by underlying database driver).
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doUpdate($values, PropelPDO $con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArParamsPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		$selectCriteria = new Criteria(self::DATABASE_NAME);

		if ($values instanceof Criteria) {
			$criteria = clone $values; // rename for clarity

			$comparison = $criteria->getComparison(ArParamsPeer::ID);
			$selectCriteria->add(ArParamsPeer::ID, $criteria->remove(ArParamsPeer::ID), $comparison);

		} else { // $values is ArParams object
			$criteria = $values->buildCriteria(); // gets full criteria
			$selectCriteria = $values->buildPkeyCriteria(); // gets criteria w/ primary key(s)
		}

		// set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		return BasePeer::doUpdate($selectCriteria, $criteria, $con);
	}

	/**
	 * Method to DELETE all rows from the ar_params table.
	 *
	 * @return     int The number of affected rows (if supported by underlying database driver).
	 */
	public static function doDeleteAll($con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArParamsPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}
		$affectedRows = 0; // initialize var to track total num of affected rows
		try {
			// use transaction because $criteria could contain info
			// for more than one table or we could emulating ON DELETE CASCADE, etc.
			$con->beginTransaction();
			$affectedRows += BasePeer::doDeleteAll(ArParamsPeer::TABLE_NAME, $con);
			// Because this db requires some delete cascade/set null emulation, we have to
			// clear the cached instance *after* the emulation has happened (since
			// instances get re-added by the select statement contained therein).
			ArParamsPeer::clearInstancePool();
			ArParamsPeer::clearRelatedInstancePool();
			$con->commit();
			return $affectedRows;
		} catch (PropelException $e) {
			$con->rollBack();
			throw $e;
		}
	}

	/**
	 * Method perform a DELETE on the database, given a ArParams or Criteria object OR a primary key value.
	 *
	 * @param      mixed $values Criteria or ArParams object or primary key or array of primary keys
	 *              which is used to create the DELETE statement
	 * @param      PropelPDO $con the connection to use
	 * @return     int 	The number of affected rows (if supported by underlying database driver).  This includes CASCADE-related rows
	 *				if supported by native driver or if emulated using Propel.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	 public static function doDelete($values, PropelPDO $con = null)
	 {
		if ($con === null) {
			$con = Propel::getConnection(ArParamsPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		if ($values instanceof Criteria) {
			// invalidate the cache for all objects of this type, since we have no
			// way of knowing (without running a query) what objects should be invalidated
			// from the cache based on this Criteria.
			ArParamsPeer::clearInstancePool();
			// rename for clarity
			$criteria = clone $values;
		} elseif ($values instanceof ArParams) { // it's a model object
			// invalidate the cache for this single object
			ArParamsPeer::removeInstanceFromPool($values);
			// create criteria based on pk values
			$criteria = $values->buildPkeyCriteria();
		} else { // it's a primary key, or an array of pks
			$criteria = new Criteria(self::DATABASE_NAME);
			$criteria->add(ArParamsPeer::ID, (array) $values, Criteria::IN);
			// invalidate the cache for this object(s)
			foreach ((array) $values as $singleval) {
				ArParamsPeer::removeInstanceFromPool($singleval);
			}
		}

		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		$affectedRows = 0; // initialize var to track total num of affected rows

		try {
			// use transaction because $criteria could contain info
			// for more than one table or we could emulating ON DELETE CASCADE, etc.
			$con->beginTransaction();
			
			$affectedRows += BasePeer::doDelete($criteria, $con);
			ArParamsPeer::clearRelatedInstancePool();
			$con->commit();
			return $affectedRows;
		} catch (PropelException $e) {
			$con->rollBack();
			throw $e;
		}
	}

	/**
	 * Validates all modified columns of given ArParams object.
	 * If parameter $columns is either a single column name or an array of column names
	 * than only those columns are validated.
	 *
	 * NOTICE: This does not apply to primary or foreign keys for now.
	 *
	 * @param      ArParams $obj The object to validate.
	 * @param      mixed $cols Column name or array of column names.
	 *
	 * @return     mixed TRUE if all columns are valid or the error message of the first invalid column.
	 */
	public static function doValidate(ArParams $obj, $cols = null)
	{
		$columns = array();

		if ($cols) {
			$dbMap = Propel::getDatabaseMap(ArParamsPeer::DATABASE_NAME);
			$tableMap = $dbMap->getTable(ArParamsPeer::TABLE_NAME);

			if (! is_array($cols)) {
				$cols = array($cols);
			}

			foreach ($cols as $colName) {
				if ($tableMap->containsColumn($colName)) {
					$get = 'get' . $tableMap->getColumn($colName)->getPhpName();
					$columns[$colName] = $obj->$get();
				}
			}
		} else {

		}

		return BasePeer::doValidate(ArParamsPeer::DATABASE_NAME, ArParamsPeer::TABLE_NAME, $columns);
	}

	/**
	 * Retrieve a single object by pkey.
	 *
	 * @param      int $pk the primary key.
	 * @param      PropelPDO $con the connection to use
	 * @return     ArParams
	 */
	public static function retrieveByPK($pk, PropelPDO $con = null)
	{

		if (null !== ($obj = ArParamsPeer::getInstanceFromPool((string) $pk))) {
			return $obj;
		}

		if ($con === null) {
			$con = Propel::getConnection(ArParamsPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria = new Criteria(ArParamsPeer::DATABASE_NAME);
		$criteria->add(ArParamsPeer::ID, $pk);

		$v = ArParamsPeer::doSelect($criteria, $con);

		return !empty($v) > 0 ? $v[0] : null;
	}

	/**
	 * Retrieve multiple objects by pkey.
	 *
	 * @param      array $pks List of primary keys
	 * @param      PropelPDO $con the connection to use
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function retrieveByPKs($pks, PropelPDO $con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArParamsPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$objs = null;
		if (empty($pks)) {
			$objs = array();
		} else {
			$criteria = new Criteria(ArParamsPeer::DATABASE_NAME);
			$criteria->add(ArParamsPeer::ID, $pks, Criteria::IN);
			$objs = ArParamsPeer::doSelect($criteria, $con);
		}
		return $objs;
	}

	// symfony behavior
	
	/**
	 * Returns an array of arrays that contain columns in each unique index.
	 *
	 * @return array
	 */
	static public function getUniqueColumnNames()
	{
	  return array();
	}

} // BaseArParamsPeer

// This is the static code needed to register the TableMap for this table with the main Propel class.
//
BaseArParamsPeer::buildTableMap();

