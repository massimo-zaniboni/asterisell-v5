<?php



class ArUpgradingJobMapBuilder {

	
	const CLASS_NAME = 'lib.model.map.ArUpgradingJobMapBuilder';

	
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

		$tMap = $this->dbMap->addTable('ar_upgrading_job');
		$tMap->setPhpName('ArUpgradingJob');

		$tMap->setUseIdGenerator(true);

		$tMap->addPrimaryKey('ID', 'Id', 'int', CreoleTypes::INTEGER, true, null);

		$tMap->addColumn('CODE', 'Code', 'string', CreoleTypes::CHAR, true, 2048);

		$tMap->addColumn('START_TIME', 'StartTime', 'int', CreoleTypes::TIMESTAMP, false, null);

		$tMap->addColumn('END_TIME', 'EndTime', 'int', CreoleTypes::TIMESTAMP, false, null);

		$tMap->addColumn('SUCESSFUL', 'Sucessful', 'boolean', CreoleTypes::BOOLEAN, false, null);

		$tMap->addColumn('STATUS_MESSAGE', 'StatusMessage', 'string', CreoleTypes::LONGVARCHAR, false, null);

	} 
} 