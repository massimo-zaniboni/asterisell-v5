<?php



class ArCustomRateFormMapBuilder implements MapBuilder {

	
	const CLASS_NAME = 'lib.model.map.ArCustomRateFormMapBuilder';

	
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
		$this->dbMap = Propel::getDatabaseMap(ArCustomRateFormPeer::DATABASE_NAME);

		$tMap = $this->dbMap->addTable(ArCustomRateFormPeer::TABLE_NAME);
		$tMap->setPhpName('ArCustomRateForm');
		$tMap->setClassname('ArCustomRateForm');

		$tMap->setUseIdGenerator(false);

		$tMap->addForeignPrimaryKey('ID', 'Id', 'INTEGER' , 'ar_rate', 'ID', true, 20);

	} 
} 