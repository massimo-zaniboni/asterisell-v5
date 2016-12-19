<?php


/**
 * This class defines the structure of the 'ar_organization_unit_has_structure' table.
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
class ArOrganizationUnitHasStructureTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArOrganizationUnitHasStructureTableMap';

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
		$this->setName('ar_organization_unit_has_structure');
		$this->setPhpName('ArOrganizationUnitHasStructure');
		$this->setClassname('ArOrganizationUnitHasStructure');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addForeignKey('AR_ORGANIZATION_UNIT_ID', 'ArOrganizationUnitId', 'INTEGER', 'ar_organization_unit', 'ID', false, null, null);
		$this->addForeignKey('AR_ORGANIZATION_UNIT_TYPE_ID', 'ArOrganizationUnitTypeId', 'INTEGER', 'ar_organization_unit_type', 'ID', false, null, null);
		$this->addForeignKey('AR_PARENT_ORGANIZATION_UNIT_ID', 'ArParentOrganizationUnitId', 'INTEGER', 'ar_organization_unit', 'ID', false, null, null);
		$this->addColumn('FROM', 'From', 'TIMESTAMP', true, null, null);
		$this->addColumn('EXISTS', 'Exists', 'BOOLEAN', true, null, true);
		$this->addForeignKey('AR_RATE_CATEGORY_ID', 'ArRateCategoryId', 'INTEGER', 'ar_rate_category', 'ID', false, null, null);
		$this->addForeignKey('AR_PARTY_ID', 'ArPartyId', 'INTEGER', 'ar_party', 'ID', false, null, null);
		$this->addColumn('EXTENSION_CODES', 'ExtensionCodes', 'VARCHAR', false, 5024, null);
		$this->addColumn('EXTENSION_NAME', 'ExtensionName', 'VARCHAR', false, 1024, null);
		$this->addColumn('EXTENSION_USER_CODE', 'ExtensionUserCode', 'VARCHAR', false, 1024, null);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
    $this->addRelation('ArOrganizationUnitRelatedByArOrganizationUnitId', 'ArOrganizationUnit', RelationMap::MANY_TO_ONE, array('ar_organization_unit_id' => 'id', ), null, null);
    $this->addRelation('ArOrganizationUnitType', 'ArOrganizationUnitType', RelationMap::MANY_TO_ONE, array('ar_organization_unit_type_id' => 'id', ), null, null);
    $this->addRelation('ArOrganizationUnitRelatedByArParentOrganizationUnitId', 'ArOrganizationUnit', RelationMap::MANY_TO_ONE, array('ar_parent_organization_unit_id' => 'id', ), null, null);
    $this->addRelation('ArRateCategory', 'ArRateCategory', RelationMap::MANY_TO_ONE, array('ar_rate_category_id' => 'id', ), null, null);
    $this->addRelation('ArParty', 'ArParty', RelationMap::MANY_TO_ONE, array('ar_party_id' => 'id', ), null, null);
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

} // ArOrganizationUnitHasStructureTableMap
