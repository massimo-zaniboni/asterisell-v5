<?php



class ArAsteriskAccountRangeCreationMapBuilder {

	
	const CLASS_NAME = 'lib.model.map.ArAsteriskAccountRangeCreationMapBuilder';

	
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

		$tMap = $this->dbMap->addTable('ar_asterisk_account_range_creation');
		$tMap->setPhpName('ArAsteriskAccountRangeCreation');

		$tMap->setUseIdGenerator(true);

		$tMap->addPrimaryKey('ID', 'Id', 'int', CreoleTypes::INTEGER, true, null);

	} 
} 