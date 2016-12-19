<?php



class ArRoleToUserHasAccessMapBuilder {

	
	const CLASS_NAME = 'lib.model.map.ArRoleToUserHasAccessMapBuilder';

	
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

		$tMap = $this->dbMap->addTable('ar_role_to_user_has_access');
		$tMap->setPhpName('ArRoleToUserHasAccess');

		$tMap->setUseIdGenerator(false);

		$tMap->addForeignPrimaryKey('AR_ROLE_ID', 'ArRoleId', 'int' , CreoleTypes::INTEGER, 'ar_role', 'ID', true, 20);

		$tMap->addForeignPrimaryKey('AR_USER_HAS_ACCESS_ID', 'ArUserHasAccessId', 'int' , CreoleTypes::INTEGER, 'ar_user_has_access', 'ID', true, 20);

	} 
} 