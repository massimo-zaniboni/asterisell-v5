<?php



class ArPermissionToUserHasAccessMapBuilder {

	
	const CLASS_NAME = 'lib.model.map.ArPermissionToUserHasAccessMapBuilder';

	
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

		$tMap = $this->dbMap->addTable('ar_permission_to_user_has_access');
		$tMap->setPhpName('ArPermissionToUserHasAccess');

		$tMap->setUseIdGenerator(false);

		$tMap->addForeignPrimaryKey('AR_PERMISSION_ID', 'ArPermissionId', 'int' , CreoleTypes::INTEGER, 'ar_permission', 'ID', true, 20);

		$tMap->addForeignPrimaryKey('AR_USER_HAS_ACCESS_ID', 'ArUserHasAccessId', 'int' , CreoleTypes::INTEGER, 'ar_user_has_access', 'ID', true, 20);

	} 
} 