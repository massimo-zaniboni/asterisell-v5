<?php


/**
 * This class defines the structure of the 'ar_assigned_service' table.
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
class ArAssignedServiceTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArAssignedServiceTableMap';

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
		$this->setName('ar_assigned_service');
		$this->setPhpName('ArAssignedService');
		$this->setClassname('ArAssignedService');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addColumn('INTERNAL_NAME', 'InternalName', 'VARCHAR', false, 255, null);
		$this->addColumn('EXTERNAL_CRM_CODE', 'ExternalCrmCode', 'VARCHAR', false, 255, null);
		$this->addColumn('FROM_DATE', 'FromDate', 'TIMESTAMP', true, null, null);
		$this->addForeignKey('AR_SERVICE_ID', 'ArServiceId', 'INTEGER', 'ar_service', 'ID', true, null, null);
		$this->addForeignKey('AR_ORGANIZATION_UNIT_ID', 'ArOrganizationUnitId', 'INTEGER', 'ar_organization_unit', 'ID', true, null, null);
		$this->addColumn('NR_OF_ITEMS', 'NrOfItems', 'BIGINT', false, null, null);
		$this->addColumn('DISCOUNT', 'Discount', 'BIGINT', true, null, null);
		$this->addColumn('NOTE', 'Note', 'VARCHAR', false, 1024, null);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
    $this->addRelation('ArService', 'ArService', RelationMap::MANY_TO_ONE, array('ar_service_id' => 'id', ), null, null);
    $this->addRelation('ArOrganizationUnit', 'ArOrganizationUnit', RelationMap::MANY_TO_ONE, array('ar_organization_unit_id' => 'id', ), null, null);
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

} // ArAssignedServiceTableMap
