<?php



class ArTelephonePrefixMapBuilder implements MapBuilder {

	
	const CLASS_NAME = 'lib.model.map.ArTelephonePrefixMapBuilder';

	
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
		$this->dbMap = Propel::getDatabaseMap(ArTelephonePrefixPeer::DATABASE_NAME);

		$tMap = $this->dbMap->addTable(ArTelephonePrefixPeer::TABLE_NAME);
		$tMap->setPhpName('ArTelephonePrefix');
		$tMap->setClassname('ArTelephonePrefix');

		$tMap->setUseIdGenerator(true);

<<<<<<< HEAD
		$tMap->addPrimaryKey('ID', 'Id', 'INTEGER', true, null);

		$tMap->addColumn('PREFIX', 'Prefix', 'VARCHAR', true, 255);

		$tMap->addColumn('NAME', 'Name', 'VARCHAR', false, 255);

		$tMap->addColumn('GEOGRAPHIC_LOCATION', 'GeographicLocation', 'VARCHAR', false, 255);

		$tMap->addColumn('OPERATOR_TYPE', 'OperatorType', 'VARCHAR', false, 255);
=======
		$tMap->addPrimaryKey('ID', 'Id', 'int', CreoleTypes::INTEGER, true, null);

		$tMap->addColumn('PREFIX', 'Prefix', 'string', CreoleTypes::VARCHAR, true, 255);

		$tMap->addColumn('NAME', 'Name', 'string', CreoleTypes::VARCHAR, false, 255);

		$tMap->addColumn('GEOGRAPHIC_LOCATION', 'GeographicLocation', 'string', CreoleTypes::VARCHAR, false, 255);

		$tMap->addColumn('OPERATOR_TYPE', 'OperatorType', 'string', CreoleTypes::VARCHAR, false, 255);

		$tMap->addColumn('NEVER_MASK_NUMBER', 'NeverMaskNumber', 'boolean', CreoleTypes::BOOLEAN, true, null);
>>>>>>> f28f9838cd47f701401b08297cb0448b8a90a5c6

	} 
} 