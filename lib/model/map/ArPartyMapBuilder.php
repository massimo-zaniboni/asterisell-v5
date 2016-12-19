<?php



class ArPartyMapBuilder implements MapBuilder {

	
	const CLASS_NAME = 'lib.model.map.ArPartyMapBuilder';

	
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
		$this->dbMap = Propel::getDatabaseMap(ArPartyPeer::DATABASE_NAME);

		$tMap = $this->dbMap->addTable(ArPartyPeer::TABLE_NAME);
		$tMap->setPhpName('ArParty');
		$tMap->setClassname('ArParty');

		$tMap->setUseIdGenerator(true);

		$tMap->addPrimaryKey('ID', 'Id', 'INTEGER', true, null);

		$tMap->addColumn('CUSTOMER_OR_VENDOR', 'CustomerOrVendor', 'CHAR', true, 1);

		$tMap->addColumn('NAME', 'Name', 'VARCHAR', false, 255);

		$tMap->addColumn('EXTERNAL_CRM_CODE', 'ExternalCrmCode', 'VARCHAR', false, 255);

		$tMap->addColumn('VAT', 'Vat', 'VARCHAR', false, 255);

		$tMap->addColumn('LEGAL_ADDRESS', 'LegalAddress', 'VARCHAR', false, 255);

		$tMap->addColumn('LEGAL_CITY', 'LegalCity', 'VARCHAR', false, 255);

		$tMap->addColumn('LEGAL_ZIPCODE', 'LegalZipcode', 'VARCHAR', false, 255);

		$tMap->addColumn('LEGAL_STATE_PROVINCE', 'LegalStateProvince', 'VARCHAR', false, 255);

		$tMap->addColumn('LEGAL_COUNTRY', 'LegalCountry', 'VARCHAR', false, 255);

		$tMap->addColumn('EMAIL', 'Email', 'VARCHAR', false, 255);

		$tMap->addColumn('PHONE', 'Phone', 'VARCHAR', false, 255);

		$tMap->addColumn('PHONE2', 'Phone2', 'VARCHAR', false, 255);

		$tMap->addColumn('FAX', 'Fax', 'VARCHAR', false, 255);

		$tMap->addForeignKey('AR_RATE_CATEGORY_ID', 'ArRateCategoryId', 'INTEGER', 'ar_rate_category', 'ID', false, null);

		$tMap->addForeignKey('AR_PARAMS_ID', 'ArParamsId', 'INTEGER', 'ar_params', 'ID', false, null);

		$tMap->addColumn('MAX_LIMIT_30', 'MaxLimit30', 'INTEGER', false, 20);

		$tMap->addColumn('LAST_EMAIL_ADVISE_FOR_MAX_LIMIT_30', 'LastEmailAdviseForMaxLimit30', 'TIMESTAMP', false, null);

		$tMap->addColumn('IS_ACTIVE', 'IsActive', 'BOOLEAN', true, null);

		$tMap->addColumn('IS_RESELLER', 'IsReseller', 'BOOLEAN', true, null);

		$tMap->addColumn('RESELLER_CODE', 'ResellerCode', 'VARCHAR', false, 255);

	} 
} 