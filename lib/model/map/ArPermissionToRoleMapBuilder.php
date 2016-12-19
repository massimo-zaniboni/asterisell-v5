<?php



class ArPermissionToRoleMapBuilder {

	
	const CLASS_NAME = 'lib.model.map.ArPermissionToRoleMapBuilder';

	
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

		$tMap = $this->dbMap->addTable('ar_permission_to_role');
		$tMap->setPhpName('ArPermissionToRole');

		$tMap->setUseIdGenerator(false);

		$tMap->addForeignPrimaryKey('AR_PERMISSION_ID', 'ArPermissionId', 'int' , CreoleTypes::INTEGER, 'ar_permission', 'ID', true, 20);

		$tMap->addForeignPrimaryKey('AR_ROLE_ID', 'ArRoleId', 'int' , CreoleTypes::INTEGER, 'ar_role', 'ID', true, 20);

	} 
} 