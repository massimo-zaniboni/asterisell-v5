<?php



class ArDatabaseVersionMapBuilder implements MapBuilder {

	
	const CLASS_NAME = 'lib.model.map.ArDatabaseVersionMapBuilder';

	
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
		$this->dbMap = Propel::getDatabaseMap(ArDatabaseVersionPeer::DATABASE_NAME);

		$tMap = $this->dbMap->addTable(ArDatabaseVersionPeer::TABLE_NAME);
		$tMap->setPhpName('ArDatabaseVersion');
		$tMap->setClassname('ArDatabaseVersion');

		$tMap->setUseIdGenerator(true);

		$tMap->addPrimaryKey('ID', 'Id', 'INTEGER', true, null);

		$tMap->addColumn('VERSION', 'Version', 'VARCHAR', false, 255);

		$tMap->addColumn('INSTALLATION_DATE', 'InstallationDate', 'TIMESTAMP', false, null);

	} 
} 