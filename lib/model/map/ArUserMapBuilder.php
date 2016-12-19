<?php



class ArUserMapBuilder {

	
	const CLASS_NAME = 'lib.model.map.ArUserMapBuilder';

	
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

		$tMap = $this->dbMap->addTable('ar_user');
		$tMap->setPhpName('ArUser');

		$tMap->setUseIdGenerator(true);

		$tMap->addPrimaryKey('ID', 'Id', 'int', CreoleTypes::INTEGER, true, null);

		$tMap->addColumn('NAME', 'Name', 'string', CreoleTypes::VARCHAR, false, 1024);

		$tMap->addForeignKey('AR_WEB_ACCOUNT_ID', 'ArWebAccountId', 'int', CreoleTypes::INTEGER, 'ar_web_account', 'ID', false, null);

		$tMap->addColumn('NOTE', 'Note', 'string', CreoleTypes::LONGVARCHAR, false, null);

	} 
} 