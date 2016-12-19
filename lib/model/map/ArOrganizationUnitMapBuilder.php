<?php



class ArOrganizationUnitMapBuilder {

	
	const CLASS_NAME = 'lib.model.map.ArOrganizationUnitMapBuilder';

	
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

		$tMap = $this->dbMap->addTable('ar_organization_unit');
		$tMap->setPhpName('ArOrganizationUnit');

		$tMap->setUseIdGenerator(true);

		$tMap->addPrimaryKey('ID', 'Id', 'int', CreoleTypes::INTEGER, true, null);

		$tMap->addColumn('NAME_PREFIX', 'NamePrefix', 'string', CreoleTypes::VARCHAR, false, 1024);

		$tMap->addColumn('CALCULATED_NAME_SUFFIX', 'CalculatedNameSuffix', 'string', CreoleTypes::VARCHAR, false, 1024);

		$tMap->addForeignKey('AR_PARTY_ID', 'ArPartyId', 'int', CreoleTypes::INTEGER, 'ar_party', 'ID', false, null);

		$tMap->addForeignKey('AR_EXTENSION_ID', 'ArExtensionId', 'int', CreoleTypes::INTEGER, 'ar_extension', 'ID', false, null);

	} 
} 