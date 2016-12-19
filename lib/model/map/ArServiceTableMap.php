<?php


/**
 * This class defines the structure of the 'ar_service' table.
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
class ArServiceTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArServiceTableMap';

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
		$this->setName('ar_service');
		$this->setPhpName('ArService');
		$this->setClassname('ArService');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addColumn('INTERNAL_NAME', 'InternalName', 'VARCHAR', false, 255, null);
		$this->addColumn('CUSTOMER_NAME', 'CustomerName', 'VARCHAR', false, 255, null);
		$this->addColumn('CUSTOMER_DESCRIPTION', 'CustomerDescription', 'VARCHAR', false, 2048, null);
		$this->addColumn('VENDOR_NAME', 'VendorName', 'VARCHAR', false, 255, null);
		$this->addColumn('VENDOR_DESCRIPTION', 'VendorDescription', 'VARCHAR', false, 2048, null);
		$this->addColumn('EXTERNAL_CRM_CODE', 'ExternalCrmCode', 'VARCHAR', false, 255, null);
		$this->addColumn('CUSTOMER_PRICE_DEPEND_FROM_ACTIVATION_DATE', 'CustomerPriceDependFromActivationDate', 'BOOLEAN', true, null, false);
		$this->addColumn('CUSTOMER_PRICE_CHANGE_WITH_PRICE_LIST', 'CustomerPriceChangeWithPriceList', 'BOOLEAN', true, null, false);
		$this->addColumn('IS_ENABLED', 'IsEnabled', 'BOOLEAN', true, null, true);
		$this->addColumn('IS_APPLIED_ONLY_ONE_TIME', 'IsAppliedOnlyOneTime', 'BOOLEAN', true, null, false);
		$this->addColumn('SCHEDULE_TIMEFRAME', 'ScheduleTimeframe', 'VARCHAR', false, 255, null);
		$this->addColumn('WAS_COMPILED', 'WasCompiled', 'BOOLEAN', true, null, false);
		$this->addColumn('SCHEDULE_FROM', 'ScheduleFrom', 'VARCHAR', false, 255, null);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
    $this->addRelation('ArServicePrice', 'ArServicePrice', RelationMap::ONE_TO_MANY, array('id' => 'ar_service_id', ), null, null);
    $this->addRelation('ArAssignedService', 'ArAssignedService', RelationMap::ONE_TO_MANY, array('id' => 'ar_service_id', ), null, null);
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

} // ArServiceTableMap
