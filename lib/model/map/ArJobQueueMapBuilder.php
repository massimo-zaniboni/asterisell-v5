<?php



class ArJobQueueMapBuilder implements MapBuilder {

	
	const CLASS_NAME = 'lib.model.map.ArJobQueueMapBuilder';

	
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
		$this->dbMap = Propel::getDatabaseMap(ArJobQueuePeer::DATABASE_NAME);

		$tMap = $this->dbMap->addTable(ArJobQueuePeer::TABLE_NAME);
		$tMap->setPhpName('ArJobQueue');
		$tMap->setClassname('ArJobQueue');

		$tMap->setUseIdGenerator(true);

		$tMap->addPrimaryKey('ID', 'Id', 'INTEGER', true, null);

		$tMap->addColumn('IS_PART_OF', 'IsPartOf', 'INTEGER', true, 11);

		$tMap->addColumn('STATE', 'State', 'INTEGER', true, 1);

		$tMap->addColumn('CREATED_AT', 'CreatedAt', 'TIMESTAMP', false, null);

		$tMap->addColumn('START_AT', 'StartAt', 'TIMESTAMP', false, null);

		$tMap->addColumn('END_AT', 'EndAt', 'TIMESTAMP', false, null);

		$tMap->addColumn('DESCRIPTION', 'Description', 'VARCHAR', true, 12000);

		$tMap->addColumn('PHP_DATA_JOB_SERIALIZATION', 'PhpDataJobSerialization', 'CLOB', false, null);

	} 
} 