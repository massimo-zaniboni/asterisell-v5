<?php



class ArOrganizationUnitHasStrutctureMapBuilder {

	
	const CLASS_NAME = 'lib.model.map.ArOrganizationUnitHasStrutctureMapBuilder';

	
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
		$this->dbMap = Propel::getDatabaseMap('propel');

		$tMap = $this->dbMap->addTable('ar_organization_unit_has_strutcture');
		$tMap->setPhpName('ArOrganizationUnitHasStrutcture');

		$tMap->setUseIdGenerator(true);

		$tMap->addPrimaryKey('ID', 'Id', 'int', CreoleTypes::INTEGER, true, null);

		$tMap->addForeignKey('AR_ORGANIZATION_UNIT_ID', 'ArOrganizationUnitId', 'int', CreoleTypes::INTEGER, 'ar_organization_unit', 'ID', false, null);

		$tMap->addForeignKey('AR_ORGANIZATION_UNIT_TYPE_ID', 'ArOrganizationUnitTypeId', 'int', CreoleTypes::INTEGER, 'ar_organization_unit_type', 'ID', false, null);

		$tMap->addForeignKey('AR_PARENT_ORGANIZATION_UNIT_ID', 'ArParentOrganizationUnitId', 'int', CreoleTypes::INTEGER, 'ar_organization_unit', 'ID', true, 20);

		$tMap->addColumn('FROM', 'From', 'int', CreoleTypes::TIMESTAMP, false, null);

		$tMap->addColumn('TO', 'To', 'int', CreoleTypes::TIMESTAMP, false, null);

	} 
} 