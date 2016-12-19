<?php



class ArPermissionMapBuilder {

	
	const CLASS_NAME = 'lib.model.map.ArPermissionMapBuilder';

	
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

		$tMap = $this->dbMap->addTable('ar_permission');
		$tMap->setPhpName('ArPermission');

		$tMap->setUseIdGenerator(true);

		$tMap->addPrimaryKey('ID', 'Id', 'int', CreoleTypes::INTEGER, true, null);

		$tMap->addColumn('NAME', 'Name', 'string', CreoleTypes::VARCHAR, false, 1024);

		$tMap->addColumn('DESCRIPTION', 'Description', 'string', CreoleTypes::LONGVARCHAR, false, null);

		$tMap->addColumn('IS_STILL_APPLICABLE_IN_THE_PAST', 'IsStillApplicableInThePast', 'boolean', CreoleTypes::BOOLEAN, false, null);

	} 
} 