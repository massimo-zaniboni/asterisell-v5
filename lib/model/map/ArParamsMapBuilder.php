<?php



class ArParamsMapBuilder implements MapBuilder {

	
	const CLASS_NAME = 'lib.model.map.ArParamsMapBuilder';

	
	private $dbMap;

	
	public function isBuilt()
	{
		return ($this->dbMap !== null);
	}

	
	public function getDatabaseMap()
	{
		return $this->dbMap;
	}

	
	public function doBuild()
	{
		$this->dbMap = Propel::getDatabaseMap(ArParamsPeer::DATABASE_NAME);

		$tMap = $this->dbMap->addTable(ArParamsPeer::TABLE_NAME);
		$tMap->setPhpName('ArParams');
		$tMap->setClassname('ArParams');

		$tMap->setUseIdGenerator(true);

		$tMap->addPrimaryKey('ID', 'Id', 'INTEGER', true, null);

		$tMap->addColumn('NAME', 'Name', 'VARCHAR', false, 255);

		$tMap->addColumn('IS_DEFAULT', 'IsDefault', 'BOOLEAN', false, null);

		$tMap->addColumn('SERVICE_NAME', 'ServiceName', 'VARCHAR', false, 255);

		$tMap->addColumn('SERVICE_PROVIDER_WEBSITE', 'ServiceProviderWebsite', 'VARCHAR', false, 255);

		$tMap->addColumn('SERVICE_PROVIDER_EMAIL', 'ServiceProviderEmail', 'VARCHAR', false, 255);

		$tMap->addColumn('VAT_TAX_PERC', 'VatTaxPerc', 'INTEGER', true, 20);

		$tMap->addColumn('LOGO_IMAGE', 'LogoImage', 'VARCHAR', false, 255);

		$tMap->addColumn('SLOGAN', 'Slogan', 'VARCHAR', false, 1024);

		$tMap->addColumn('LOGO_IMAGE_IN_INVOICES', 'LogoImageInInvoices', 'VARCHAR', false, 255);

		$tMap->addColumn('FOOTER', 'Footer', 'VARCHAR', false, 255);

		$tMap->addColumn('USER_MESSAGE', 'UserMessage', 'VARCHAR', false, 255);

		$tMap->addColumn('LEGAL_NAME', 'LegalName', 'VARCHAR', false, 255);

		$tMap->addColumn('EXTERNAL_CRM_CODE', 'ExternalCrmCode', 'VARCHAR', false, 255);

		$tMap->addColumn('VAT', 'Vat', 'VARCHAR', false, 255);

		$tMap->addColumn('LEGAL_ADDRESS', 'LegalAddress', 'VARCHAR', false, 255);

		$tMap->addColumn('LEGAL_WEBSITE', 'LegalWebsite', 'VARCHAR', false, 255);

		$tMap->addColumn('LEGAL_CITY', 'LegalCity', 'VARCHAR', false, 255);

		$tMap->addColumn('LEGAL_ZIPCODE', 'LegalZipcode', 'VARCHAR', false, 255);

		$tMap->addColumn('LEGAL_STATE_PROVINCE', 'LegalStateProvince', 'VARCHAR', false, 255);

		$tMap->addColumn('LEGAL_COUNTRY', 'LegalCountry', 'VARCHAR', false, 255);

		$tMap->addColumn('LEGAL_EMAIL', 'LegalEmail', 'VARCHAR', false, 255);

		$tMap->addColumn('LEGAL_PHONE', 'LegalPhone', 'VARCHAR', false, 255);

		$tMap->addColumn('PHONE2', 'Phone2', 'VARCHAR', false, 255);

		$tMap->addColumn('LEGAL_FAX', 'LegalFax', 'VARCHAR', false, 255);

		$tMap->addColumn('INVOICE_NOTES', 'InvoiceNotes', 'VARCHAR', false, 255);

		$tMap->addColumn('INVOICE_PAYMENT_TERMS', 'InvoicePaymentTerms', 'VARCHAR', false, 2048);

		$tMap->addColumn('SENDER_NAME_ON_INVOICING_EMAILS', 'SenderNameOnInvoicingEmails', 'VARCHAR', false, 255);

		$tMap->addColumn('INVOICING_EMAIL_ADDRESS', 'InvoicingEmailAddress', 'VARCHAR', false, 255);

		$tMap->addColumn('ACCOUNTANT_EMAIL_ADDRESS', 'AccountantEmailAddress', 'VARCHAR', false, 255);

		$tMap->addColumn('SMTP_HOST', 'SmtpHost', 'VARCHAR', false, 250);

		$tMap->addColumn('SMTP_PORT', 'SmtpPort', 'INTEGER', false, 4);

		$tMap->addColumn('SMTP_USERNAME', 'SmtpUsername', 'VARCHAR', false, 255);

		$tMap->addColumn('SMTP_PASSWORD', 'SmtpPassword', 'VARCHAR', false, 255);

		$tMap->addColumn('SMTP_ENCRYPTION', 'SmtpEncryption', 'VARCHAR', false, 255);

		$tMap->addColumn('SMTP_RECONNECT_AFTER_NR_OF_MESSAGES', 'SmtpReconnectAfterNrOfMessages', 'INTEGER', false, 4);

		$tMap->addColumn('SMTP_SECONDS_OF_PAUSE_AFTER_RECONNECTION', 'SmtpSecondsOfPauseAfterReconnection', 'INTEGER', false, 2);

		$tMap->addColumn('CURRENT_INVOICE_NR', 'CurrentInvoiceNr', 'INTEGER', true, 11);

		$tMap->addColumn('LOGO_HTML_COLOR', 'LogoHtmlColor', 'VARCHAR', false, 12);

		$tMap->addColumn('PAYMENT_DAYS', 'PaymentDays', 'INTEGER', false, 20);

		$tMap->addColumn('RECONNECTION_FEE', 'ReconnectionFee', 'VARCHAR', false, 40);

		$tMap->addColumn('INFO_TELEPHONE_NUMBER', 'InfoTelephoneNumber', 'VARCHAR', false, 512);

		$tMap->addColumn('LATE_PAYMENT_FEE', 'LatePaymentFee', 'VARCHAR', false, 40);

		$tMap->addColumn('ETF_BBS', 'EtfBbs', 'VARCHAR', false, 512);

		$tMap->addColumn('ETF_ACC_NO', 'EtfAccNo', 'VARCHAR', false, 512);

		$tMap->addColumn('ACCOUNT_DEPARTMENT', 'AccountDepartment', 'VARCHAR', false, 512);

		$tMap->addColumn('DIRECT_DEBIT_PAYMENT_EMAIL', 'DirectDebitPaymentEmail', 'VARCHAR', false, 512);

		$tMap->addColumn('DIRECT_DEBIT_PAYMENT_TELEPHONE_NUMBER', 'DirectDebitPaymentTelephoneNumber', 'VARCHAR', false, 512);

		$tMap->addColumn('LOGIN_URN', 'LoginUrn', 'VARCHAR', false, 512);

	} 
} 