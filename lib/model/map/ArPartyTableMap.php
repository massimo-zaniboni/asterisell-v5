<?php


/**
 * This class defines the structure of the 'ar_party' table.
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
class ArPartyTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArPartyTableMap';

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
		$this->setName('ar_party');
		$this->setPhpName('ArParty');
		$this->setClassname('ArParty');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addColumn('NAME', 'Name', 'VARCHAR', false, 255, null);
		$this->addColumn('COMPACT_NAME', 'CompactName', 'VARCHAR', false, 255, null);
		$this->addColumn('NOTE', 'Note', 'VARCHAR', false, 1024, null);
		$this->addColumn('EXTERNAL_CRM_CODE', 'ExternalCrmCode', 'VARCHAR', false, 255, null);
		$this->addColumn('CONTRACT_NUMBER', 'ContractNumber', 'VARCHAR', false, 255, null);
		$this->addColumn('VAT', 'Vat', 'VARCHAR', false, 255, null);
		$this->addColumn('LEGAL_REGISTRATION_NUMBER', 'LegalRegistrationNumber', 'VARCHAR', false, 255, null);
		$this->addColumn('IS_BILLABLE', 'IsBillable', 'BOOLEAN', true, null, false);
		$this->addColumn('LEGAL_ADDRESS', 'LegalAddress', 'VARCHAR', false, 255, null);
		$this->addColumn('LEGAL_CITY', 'LegalCity', 'VARCHAR', false, 255, null);
		$this->addColumn('LEGAL_ZIPCODE', 'LegalZipcode', 'VARCHAR', false, 255, null);
		$this->addColumn('LEGAL_STATE_PROVINCE', 'LegalStateProvince', 'VARCHAR', false, 255, null);
		$this->addColumn('LEGAL_COUNTRY', 'LegalCountry', 'VARCHAR', false, 255, null);
		$this->addColumn('EMAIL', 'Email', 'VARCHAR', false, 255, null);
		$this->addColumn('CONTACT_NAME', 'ContactName', 'VARCHAR', false, 255, null);
		$this->addColumn('PHONE', 'Phone', 'VARCHAR', false, 255, null);
		$this->addColumn('PHONE2', 'Phone2', 'VARCHAR', false, 255, null);
		$this->addColumn('FAX', 'Fax', 'VARCHAR', false, 255, null);
		$this->addColumn('WEB_SITE', 'WebSite', 'VARCHAR', false, 120, null);
		$this->addColumn('MAX_LIMIT_30', 'MaxLimit30', 'BIGINT', false, null, null);
		$this->addColumn('LAST_EMAIL_ADVISE_FOR_MAX_LIMIT_30', 'LastEmailAdviseForMaxLimit30', 'TIMESTAMP', false, null, null);
		$this->addColumn('IS_ACTIVE', 'IsActive', 'BOOLEAN', true, null, true);
		$this->addForeignKey('AR_RESELLER_ID', 'ArResellerId', 'INTEGER', 'ar_reseller', 'ID', false, null, null);
		$this->addColumn('MIGRATION_FIELD_FOR_TELEPHONE', 'MigrationFieldForTelephone', 'VARCHAR', false, 255, null);
		$this->addColumn('MIGRATION_FIELD_FOR_ADSL', 'MigrationFieldForAdsl', 'VARCHAR', false, 255, null);
		$this->addColumn('PAYMENT_IBAN', 'PaymentIban', 'VARCHAR', false, 255, null);
		$this->addColumn('PAYMENT_BIC', 'PaymentBic', 'VARCHAR', false, 255, null);
		$this->addColumn('PAYMENT_SEPA', 'PaymentSepa', 'VARCHAR', false, 255, null);
		$this->addColumn('PAYMENT_INFO', 'PaymentInfo', 'VARCHAR', false, 255, null);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
    $this->addRelation('ArReseller', 'ArReseller', RelationMap::MANY_TO_ONE, array('ar_reseller_id' => 'id', ), null, null);
    $this->addRelation('ArPartyHasTag', 'ArPartyHasTag', RelationMap::ONE_TO_MANY, array('id' => 'ar_party_id', ), 'CASCADE', null);
    $this->addRelation('ArOrganizationUnitHasStructure', 'ArOrganizationUnitHasStructure', RelationMap::ONE_TO_MANY, array('id' => 'ar_party_id', ), null, null);
    $this->addRelation('ArVendor', 'ArVendor', RelationMap::ONE_TO_MANY, array('id' => 'ar_party_id', ), null, null);
    $this->addRelation('ArUser', 'ArUser', RelationMap::ONE_TO_MANY, array('id' => 'ar_party_id', ), null, null);
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

} // ArPartyTableMap
