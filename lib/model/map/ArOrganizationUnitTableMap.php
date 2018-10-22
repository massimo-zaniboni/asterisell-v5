<?php


/**
 * This class defines the structure of the 'ar_organization_unit' table.
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
class ArOrganizationUnitTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArOrganizationUnitTableMap';

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
		$this->setName('ar_organization_unit');
		$this->setPhpName('ArOrganizationUnit');
		$this->setClassname('ArOrganizationUnit');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addColumn('INTERNAL_NAME', 'InternalName', 'VARCHAR', false, 200, null);
		$this->addColumn('INTERNAL_NAME2', 'InternalName2', 'VARCHAR', false, 200, null);
		$this->addColumn('INTERNAL_CHECKSUM1', 'InternalChecksum1', 'VARCHAR', false, 200, null);
		$this->addColumn('INTERNAL_CHECKSUM2', 'InternalChecksum2', 'VARCHAR', false, 200, null);
		$this->addColumn('INTERNAL_CHECKSUM3', 'InternalChecksum3', 'VARCHAR', false, 200, null);
		$this->addColumn('INTERNAL_CHECKSUM4', 'InternalChecksum4', 'VARCHAR', false, 200, null);
		$this->addColumn('EXPORT_CODE', 'ExportCode', 'VARCHAR', false, 200, null);
		$this->addColumn('AUTOMATICALLY_MANAGED_FROM', 'AutomaticallyManagedFrom', 'INTEGER', true, null, 0);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
    $this->addRelation('ArOrganizationUnitHasStructureRelatedByArOrganizationUnitId', 'ArOrganizationUnitHasStructure', RelationMap::ONE_TO_MANY, array('id' => 'ar_organization_unit_id', ), null, null);
    $this->addRelation('ArOrganizationUnitHasStructureRelatedByArParentOrganizationUnitId', 'ArOrganizationUnitHasStructure', RelationMap::ONE_TO_MANY, array('id' => 'ar_parent_organization_unit_id', ), null, null);
    $this->addRelation('ArExpandedExtensions', 'ArExpandedExtensions', RelationMap::ONE_TO_MANY, array('id' => 'ar_organization_unit_id', ), null, null);
    $this->addRelation('ArUser', 'ArUser', RelationMap::ONE_TO_MANY, array('id' => 'ar_organization_unit_id', ), null, null);
    $this->addRelation('ArReport', 'ArReport', RelationMap::ONE_TO_MANY, array('id' => 'ar_organization_unit_id', ), null, null);
    $this->addRelation('ArReportScheduler', 'ArReportScheduler', RelationMap::ONE_TO_MANY, array('id' => 'ar_organization_unit_id', ), null, null);
    $this->addRelation('ArPostponedReport', 'ArPostponedReport', RelationMap::ONE_TO_MANY, array('id' => 'ar_organization_unit_id', ), 'CASCADE', null);
    $this->addRelation('ArPostponedReportTmp', 'ArPostponedReportTmp', RelationMap::ONE_TO_ONE, array('id' => 'ar_organization_unit_id', ), 'CASCADE', null);
    $this->addRelation('ArReportToReadUserView', 'ArReportToReadUserView', RelationMap::ONE_TO_MANY, array('id' => 'ar_organization_unit_id', ), null, null);
    $this->addRelation('ArInstanceStatus', 'ArInstanceStatus', RelationMap::ONE_TO_MANY, array('id' => 'ar_organization_unit_id', ), null, null);
    $this->addRelation('ArAssignedService', 'ArAssignedService', RelationMap::ONE_TO_MANY, array('id' => 'ar_organization_unit_id', ), null, null);
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

} // ArOrganizationUnitTableMap
