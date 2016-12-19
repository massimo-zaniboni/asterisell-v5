<?php



class ArRateMapBuilder implements MapBuilder {

	
	const CLASS_NAME = 'lib.model.map.ArRateMapBuilder';

	
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
		$this->dbMap = Propel::getDatabaseMap(ArRatePeer::DATABASE_NAME);

		$tMap = $this->dbMap->addTable(ArRatePeer::TABLE_NAME);
		$tMap->setPhpName('ArRate');
		$tMap->setClassname('ArRate');

		$tMap->setUseIdGenerator(true);

		$tMap->addPrimaryKey('ID', 'Id', 'INTEGER', true, null);

		$tMap->addColumn('DESTINATION_TYPE', 'DestinationType', 'INTEGER', true, 1);

		$tMap->addColumn('IS_EXCEPTION', 'IsException', 'BOOLEAN', true, null);

		$tMap->addForeignKey('AR_RATE_CATEGORY_ID', 'ArRateCategoryId', 'INTEGER', 'ar_rate_category', 'ID', false, null);

		$tMap->addForeignKey('AR_PARTY_ID', 'ArPartyId', 'INTEGER', 'ar_party', 'ID', false, null);

		$tMap->addColumn('START_TIME', 'StartTime', 'TIMESTAMP', true, null);

		$tMap->addColumn('END_TIME', 'EndTime', 'TIMESTAMP', false, null);

		$tMap->addColumn('PHP_CLASS_SERIALIZATION', 'PhpClassSerialization', 'CLOB', false, null);

		$tMap->addColumn('USER_INPUT', 'UserInput', 'CLOB', false, null);

		$tMap->addColumn('NOTE', 'Note', 'LONGVARCHAR', false, null);

	} 
} 