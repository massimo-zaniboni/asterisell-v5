<?php



class ArRateCategoryMapBuilder implements MapBuilder {

	
	const CLASS_NAME = 'lib.model.map.ArRateCategoryMapBuilder';

	
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
		$this->dbMap = Propel::getDatabaseMap(ArRateCategoryPeer::DATABASE_NAME);

		$tMap = $this->dbMap->addTable(ArRateCategoryPeer::TABLE_NAME);
		$tMap->setPhpName('ArRateCategory');
		$tMap->setClassname('ArRateCategory');

		$tMap->setUseIdGenerator(true);

		$tMap->addPrimaryKey('ID', 'Id', 'INTEGER', true, null);

		$tMap->addColumn('NAME', 'Name', 'VARCHAR', false, 255);

	} 
} 