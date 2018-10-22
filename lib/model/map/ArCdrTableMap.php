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
		$this->setUseIdGenerator(false);
		// columns
		$this->addPrimaryKey('CALLDATE', 'Calldate', 'TIMESTAMP', true, null, null);
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, 0);
		$this->addPrimaryKey('IS_SERVICE_CDR', 'IsServiceCdr', 'BOOLEAN', true, null, false);
		$this->addColumn('TO_CALLDATE', 'ToCalldate', 'TIMESTAMP', false, null, null);
		$this->addColumn('COUNT_OF_CALLS', 'CountOfCalls', 'INTEGER', true, null, 1);
		$this->addColumn('DESTINATION_TYPE', 'DestinationType', 'SMALLINT', true, null, 0);
		$this->addColumn('IS_REDIRECT', 'IsRedirect', 'BOOLEAN', true, null, false);
		$this->addColumn('DURATION', 'Duration', 'INTEGER', true, null, 0);
		$this->addColumn('BILLSEC', 'Billsec', 'INTEGER', true, null, 0);
		$this->addColumn('AR_ORGANIZATION_UNIT_ID', 'ArOrganizationUnitId', 'INTEGER', false, null, null);
		$this->addColumn('CACHED_PARENT_ID_HIERARCHY', 'CachedParentIdHierarchy', 'VARBINARY', false, 850, null);
		$this->addColumn('BILLABLE_AR_ORGANIZATION_UNIT_ID', 'BillableArOrganizationUnitId', 'INTEGER', false, null, null);
		$this->addColumn('BUNDLE_AR_ORGANIZATION_UNIT_ID', 'BundleArOrganizationUnitId', 'INTEGER', false, null, null);
		$this->addColumn('INCOME', 'Income', 'BIGINT', false, null, null);
		$this->addColumn('COST_SAVING', 'CostSaving', 'BIGINT', false, null, null);
		$this->addColumn('AR_VENDOR_ID', 'ArVendorId', 'INTEGER', false, null, null);
		$this->addColumn('AR_COMMUNICATION_CHANNEL_TYPE_ID', 'ArCommunicationChannelTypeId', 'INTEGER', false, null, null);
		$this->addColumn('COST', 'Cost', 'BIGINT', false, null, null);
		$this->addColumn('EXPECTED_COST', 'ExpectedCost', 'BIGINT', false, null, null);
		$this->addColumn('AR_TELEPHONE_PREFIX_ID', 'ArTelephonePrefixId', 'INTEGER', false, null, null);
		$this->addColumn('CACHED_EXTERNAL_TELEPHONE_NUMBER', 'CachedExternalTelephoneNumber', 'VARCHAR', false, 1024, null);
		$this->addColumn('EXTERNAL_TELEPHONE_NUMBER_WITH_APPLIED_PORTABILITY', 'ExternalTelephoneNumberWithAppliedPortability', 'VARCHAR', false, 1024, null);
		$this->addColumn('CACHED_MASKED_EXTERNAL_TELEPHONE_NUMBER', 'CachedMaskedExternalTelephoneNumber', 'VARCHAR', false, 1024, null);
		$this->addColumn('ERROR_DESTINATION_TYPE', 'ErrorDestinationType', 'SMALLINT', true, null, 0);
		$this->addColumn('AR_PROBLEM_DUPLICATION_KEY', 'ArProblemDuplicationKey', 'VARCHAR', false, 255, null);
		$this->addColumn('DEBUG_COST_RATE', 'DebugCostRate', 'VARCHAR', false, 512, null);
		$this->addColumn('DEBUG_INCOME_RATE', 'DebugIncomeRate', 'VARCHAR', false, 512, null);
		$this->addColumn('DEBUG_RESIDUAL_INCOME_RATE', 'DebugResidualIncomeRate', 'VARCHAR', false, 512, null);
		$this->addColumn('DEBUG_RESIDUAL_CALL_DURATION', 'DebugResidualCallDuration', 'INTEGER', false, null, null);
		$this->addColumn('DEBUG_BUNDLE_LEFT_CALLS', 'DebugBundleLeftCalls', 'INTEGER', false, null, null);
		$this->addColumn('DEBUG_BUNDLE_LEFT_DURATION', 'DebugBundleLeftDuration', 'INTEGER', false, null, null);
		$this->addColumn('DEBUG_BUNDLE_LEFT_COST', 'DebugBundleLeftCost', 'BIGINT', false, null, null);
		$this->addColumn('DEBUG_RATING_DETAILS', 'DebugRatingDetails', 'VARCHAR', false, 5000, null);
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

} // ArCdrTableMap
