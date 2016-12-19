<?php


/**
 * This class defines the structure of the 'ar_cdr' table.
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
class ArCdrTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArCdrTableMap';

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
		$this->setName('ar_cdr');
		$this->setPhpName('ArCdr');
		$this->setClassname('ArCdr');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addColumn('CALLDATE', 'Calldate', 'TIMESTAMP', true, null, null);
		$this->addColumn('TO_CALLDATE', 'ToCalldate', 'TIMESTAMP', false, null, null);
		$this->addColumn('IS_IMPORTED_SERVICE_CDR', 'IsImportedServiceCdr', 'BOOLEAN', true, null, false);
		$this->addColumn('COUNT_OF_CALLS', 'CountOfCalls', 'INTEGER', true, null, 1);
		$this->addColumn('DESTINATION_TYPE', 'DestinationType', 'INTEGER', true, 1, 0);
		$this->addColumn('IS_REDIRECT', 'IsRedirect', 'BOOLEAN', true, null, false);
		$this->addColumn('DURATION', 'Duration', 'INTEGER', true, null, null);
		$this->addColumn('BILLSEC', 'Billsec', 'INTEGER', true, null, null);
		$this->addForeignKey('AR_ORGANIZATION_UNIT_ID', 'ArOrganizationUnitId', 'INTEGER', 'ar_organization_unit', 'ID', false, null, null);
		$this->addColumn('CACHED_PARENT_ID_HIERARCHY', 'CachedParentIdHierarchy', 'VARCHAR', false, 850, null);
		$this->addColumn('BILLABLE_AR_ORGANIZATION_UNIT_ID', 'BillableArOrganizationUnitId', 'INTEGER', false, null, null);
		$this->addColumn('BUNDLE_AR_ORGANIZATION_UNIT_ID', 'BundleArOrganizationUnitId', 'INTEGER', false, null, null);
		$this->addColumn('INCOME', 'Income', 'BIGINT', false, null, null);
		$this->addColumn('COST_SAVING', 'CostSaving', 'BIGINT', false, null, null);
		$this->addForeignKey('AR_VENDOR_ID', 'ArVendorId', 'INTEGER', 'ar_vendor', 'ID', false, null, null);
		$this->addForeignKey('AR_COMMUNICATION_CHANNEL_TYPE_ID', 'ArCommunicationChannelTypeId', 'INTEGER', 'ar_communication_channel_type', 'ID', false, null, null);
		$this->addColumn('COST', 'Cost', 'BIGINT', false, null, null);
		$this->addColumn('EXPECTED_COST', 'ExpectedCost', 'BIGINT', false, null, null);
		$this->addForeignKey('AR_TELEPHONE_PREFIX_ID', 'ArTelephonePrefixId', 'INTEGER', 'ar_telephone_prefix', 'ID', false, null, null);
		$this->addColumn('CACHED_EXTERNAL_TELEPHONE_NUMBER', 'CachedExternalTelephoneNumber', 'VARCHAR', false, 1024, null);
		$this->addColumn('EXTERNAL_TELEPHONE_NUMBER_WITH_APPLIED_PORTABILITY', 'ExternalTelephoneNumberWithAppliedPortability', 'VARCHAR', false, 1024, null);
		$this->addColumn('CACHED_MASKED_EXTERNAL_TELEPHONE_NUMBER', 'CachedMaskedExternalTelephoneNumber', 'VARCHAR', false, 1024, null);
		$this->addColumn('ERROR_DESTINATION_TYPE', 'ErrorDestinationType', 'INTEGER', true, 1, 0);
		$this->addColumn('AR_PROBLEM_DUPLICATION_KEY', 'ArProblemDuplicationKey', 'VARCHAR', false, 255, null);
		$this->addColumn('DEBUG_COST_RATE', 'DebugCostRate', 'VARCHAR', false, 2048, null);
		$this->addColumn('DEBUG_INCOME_RATE', 'DebugIncomeRate', 'VARCHAR', false, 2048, null);
		$this->addColumn('DEBUG_RESIDUAL_INCOME_RATE', 'DebugResidualIncomeRate', 'VARCHAR', false, 2048, null);
		$this->addColumn('DEBUG_RESIDUAL_CALL_DURATION', 'DebugResidualCallDuration', 'INTEGER', false, null, null);
		$this->addColumn('DEBUG_BUNDLE_LEFT_CALLS', 'DebugBundleLeftCalls', 'INTEGER', false, null, null);
		$this->addColumn('DEBUG_BUNDLE_LEFT_DURATION', 'DebugBundleLeftDuration', 'INTEGER', false, null, null);
		$this->addColumn('DEBUG_BUNDLE_LEFT_COST', 'DebugBundleLeftCost', 'BIGINT', false, null, null);
		$this->addColumn('DEBUG_RATING_DETAILS', 'DebugRatingDetails', 'VARCHAR', false, 10000, null);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
    $this->addRelation('ArOrganizationUnit', 'ArOrganizationUnit', RelationMap::MANY_TO_ONE, array('ar_organization_unit_id' => 'id', ), null, null);
    $this->addRelation('ArVendor', 'ArVendor', RelationMap::MANY_TO_ONE, array('ar_vendor_id' => 'id', ), null, null);
    $this->addRelation('ArCommunicationChannelType', 'ArCommunicationChannelType', RelationMap::MANY_TO_ONE, array('ar_communication_channel_type_id' => 'id', ), null, null);
    $this->addRelation('ArTelephonePrefix', 'ArTelephonePrefix', RelationMap::MANY_TO_ONE, array('ar_telephone_prefix_id' => 'id', ), null, null);
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

} // ArCdrTableMap
