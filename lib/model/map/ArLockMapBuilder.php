<?php



class ArLockMapBuilder implements MapBuilder {

	
	const CLASS_NAME = 'lib.model.map.ArLockMapBuilder';

	
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
		$this->dbMap = Propel::getDatabaseMap(ArLockPeer::DATABASE_NAME);

		$tMap = $this->dbMap->addTable(ArLockPeer::TABLE_NAME);
		$tMap->setPhpName('ArLock');
		$tMap->setClassname('ArLock');

		$tMap->setUseIdGenerator(true);

		$tMap->addPrimaryKey('ID', 'Id', 'INTEGER', true, null);

		$tMap->addColumn('NAME', 'Name', 'CHAR', true, 255);

		$tMap->addColumn('TIME', 'Time', 'TIMESTAMP', false, null);

		$tMap->addColumn('INFO', 'Info', 'VARCHAR', false, 255);

	} 
} 