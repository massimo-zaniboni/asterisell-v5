<?php


/**
 * This class defines the structure of the 'ar_params' table.
 *
 *
 *
 * This map class is used by Propel to do runtime db structure discovery.
 * For example, the createSelectSql() method checks the type of a given column used in an
 * ORDER BY clause to know whether it needs to apply SQL to make the ORDER BY case-insensitive
 * (i.e. if it's a text column type).
 *
 * @package    lib.model.map
 */
class ArParamsTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArParamsTableMap';

	/**
	 * Initialize the table attributes, columns and validators
	 * Relations are not initialized by this method since they are lazy loaded
	 *
	 * @return     void
	 * @throws     PropelException
	 */
	public function initialize()
	{
	  // attributes
		$this->setName('ar_params');
		$this->setPhpName('ArParams');
		$this->setClassname('ArParams');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addColumn('NAME', 'Name', 'VARCHAR', false, 255, null);
		$this->addColumn('IS_DEFAULT', 'IsDefault', 'BOOLEAN', false, null, null);
		$this->addColumn('SERVICE_NAME', 'ServiceName', 'VARCHAR', false, 255, null);
		$this->addColumn('SERVICE_PROVIDER_WEBSITE', 'ServiceProviderWebsite', 'VARCHAR', false, 255, null);
		$this->addColumn('SERVICE_PROVIDER_EMAIL', 'ServiceProviderEmail', 'VARCHAR', false, 255, null);
		$this->addColumn('VAT_TAX_PERC', 'VatTaxPerc', 'INTEGER', true, null, 0);
		$this->addColumn('LOGO_IMAGE', 'LogoImage', 'VARCHAR', false, 255, null);
		$this->addColumn('SLOGAN', 'Slogan', 'VARCHAR', false, 1024, null);
		$this->addColumn('LOGO_IMAGE_IN_INVOICES', 'LogoImageInInvoices', 'VARCHAR', false, 255, null);
		$this->addColumn('FOOTER', 'Footer', 'VARCHAR', false, 255, null);
		$this->addColumn('USER_MESSAGE', 'UserMessage', 'VARCHAR', false, 255, null);
		$this->addColumn('LEGAL_NAME', 'LegalName', 'VARCHAR', false, 255, null);
		$this->addColumn('EXTERNAL_CRM_CODE', 'ExternalCrmCode', 'VARCHAR', false, 255, null);
		$this->addColumn('VAT', 'Vat', 'VARCHAR', false, 255, null);
		$this->addColumn('LEGAL_ADDRESS', 'LegalAddress', 'VARCHAR', false, 255, null);
		$this->addColumn('LEGAL_WEBSITE', 'LegalWebsite', 'VARCHAR', false, 255, null);
		$this->addColumn('LEGAL_CITY', 'LegalCity', 'VARCHAR', false, 255, null);
		$this->addColumn('LEGAL_ZIPCODE', 'LegalZipcode', 'VARCHAR', false, 255, null);
		$this->addColumn('LEGAL_STATE_PROVINCE', 'LegalStateProvince', 'VARCHAR', false, 255, null);
		$this->addColumn('LEGAL_COUNTRY', 'LegalCountry', 'VARCHAR', false, 255, null);
		$this->addColumn('LEGAL_EMAIL', 'LegalEmail', 'VARCHAR', false, 255, null);
		$this->addColumn('LEGAL_PHONE', 'LegalPhone', 'VARCHAR', false, 255, null);
		$this->addColumn('PHONE2', 'Phone2', 'VARCHAR', false, 255, null);
		$this->addColumn('LEGAL_FAX', 'LegalFax', 'VARCHAR', false, 255, null);
		$this->addColumn('INVOICE_NOTES', 'InvoiceNotes', 'VARCHAR', false, 255, null);
		$this->addColumn('INVOICE_PAYMENT_TERMS', 'InvoicePaymentTerms', 'VARCHAR', false, 2048, null);
		$this->addColumn('INVOICE_PAYMENT_DUE_IN_XX_DAYS', 'InvoicePaymentDueInXxDays', 'INTEGER', false, 4, null);
		$this->addColumn('SENDER_NAME_ON_INVOICING_EMAILS', 'SenderNameOnInvoicingEmails', 'VARCHAR', false, 255, null);
		$this->addColumn('INVOICING_EMAIL_ADDRESS', 'InvoicingEmailAddress', 'VARCHAR', false, 255, null);
		$this->addColumn('LOGO_HTML_COLOR', 'LogoHtmlColor', 'VARCHAR', false, 12, null);
		$this->addColumn('HTML_NOTES_ON_THE_LOGIN_FORM', 'HtmlNotesOnTheLoginForm', 'VARCHAR', false, 2048, null);
		$this->addColumn('OFFICIAL_CALLDATE', 'OfficialCalldate', 'TIMESTAMP', false, null, null);
		$this->addColumn('SCHEDULED_RERATE_FROM_OFFICIAL_CALLDATE', 'ScheduledRerateFromOfficialCalldate', 'BOOLEAN', true, null, false);
		$this->addColumn('NEW_IMPORTED_CDRS_FROM_CALLDATE', 'NewImportedCdrsFromCalldate', 'TIMESTAMP', false, null, null);
		$this->addColumn('NEW_IMPORTED_CDRS_TO_CALLDATE', 'NewImportedCdrsToCalldate', 'TIMESTAMP', false, null, null);
		$this->addColumn('SCHEDULED_RERATE_FROM_SPECIFIC_CALLDATE', 'ScheduledRerateFromSpecificCalldate', 'TIMESTAMP', false, null, null);
		$this->addColumn('SCHEDULED_RERATE_TO_SPECIFIC_CALLDATE', 'ScheduledRerateToSpecificCalldate', 'TIMESTAMP', false, null, null);
		$this->addColumn('SCHEDULED_IMPORTED_SERVICES_RERATE_FROM_SPECIFIC_CALLDATE', 'ScheduledImportedServicesRerateFromSpecificCalldate', 'TIMESTAMP', false, null, null);
		$this->addColumn('SCHEDULED_IMPORTED_SERVICES_RERATE_TO_SPECIFIC_CALLDATE', 'ScheduledImportedServicesRerateToSpecificCalldate', 'TIMESTAMP', false, null, null);
		$this->addColumn('CURRENT_COUNT_OF_RERATING_FAILED_ATTEMPTS', 'CurrentCountOfReratingFailedAttempts', 'INTEGER', true, null, 0);
		$this->addColumn('CURRENT_RERATING_EVENT_IS_RUNNING', 'CurrentReratingEventIsRunning', 'BOOLEAN', true, null, false);
		$this->addColumn('SHOULD_RESCHEDULE_RERATE_FROM_OFFICIAL_CALLDATE', 'ShouldRescheduleRerateFromOfficialCalldate', 'BOOLEAN', true, null, false);
		$this->addColumn('WAIT_FOR_SCHEDULED_RERATE', 'WaitForScheduledRerate', 'BOOLEAN', true, null, true);
		$this->addColumn('CLEAN_ERROR_TABLE', 'CleanErrorTable', 'INTEGER', true, null, 0);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
	} // buildRelations()

	/**
	 * 
	 * Gets the list of behaviors registered for this table
	 * 
	 * @return array Associative array (name => parameters) of behaviors
	 */
	public function getBehaviors()
	{
		return array(
			'symfony' => array('form' => 'true', 'filter' => 'true', ),
		);
	} // getBehaviors()

} // ArParamsTableMap
