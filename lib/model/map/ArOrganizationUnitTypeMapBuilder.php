<?php



class ArOrganizationUnitTypeMapBuilder {

	
	const CLASS_NAME = 'lib.model.map.ArOrganizationUnitTypeMapBuilder';

	
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

		$tMap = $this->dbMap->addTable('ar_organization_unit_type');
		$tMap->setPhpName('ArOrganizationUnitType');

		$tMap->setUseIdGenerator(true);

		$tMap->addPrimaryKey('ID', 'Id', 'int', CreoleTypes::INTEGER, true, null);

		$tMap->addColumn('NAME', 'Name', 'string', CreoleTypes::VARCHAR, false, 1024);

		$tMap->addColumn('HIEARCHY_LEVEL', 'HiearchyLevel', 'int', CreoleTypes::INTEGER, false, null);

	} 
} 