<?php



class ArNumberPortabilityMapBuilder implements MapBuilder {

	
	const CLASS_NAME = 'lib.model.map.ArNumberPortabilityMapBuilder';

	
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
		$this->dbMap = Propel::getDatabaseMap(ArNumberPortabilityPeer::DATABASE_NAME);

		$tMap = $this->dbMap->addTable(ArNumberPortabilityPeer::TABLE_NAME);
		$tMap->setPhpName('ArNumberPortability');
		$tMap->setClassname('ArNumberPortability');

		$tMap->setUseIdGenerator(true);

		$tMap->addPrimaryKey('ID', 'Id', 'INTEGER', true, null);

		$tMap->addColumn('TELEPHONE_NUMBER', 'TelephoneNumber', 'VARCHAR', true, 255);

		$tMap->addColumn('PORTED_TELEPHONE_NUMBER', 'PortedTelephoneNumber', 'VARCHAR', true, 255);

		$tMap->addColumn('FROM_DATE', 'FromDate', 'TIMESTAMP', false, null);

	} 
} 