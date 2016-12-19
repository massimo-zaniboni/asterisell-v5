<?php



class ArRateIncrementalInfoMapBuilder implements MapBuilder {

	
	const CLASS_NAME = 'lib.model.map.ArRateIncrementalInfoMapBuilder';

	
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
		$this->dbMap = Propel::getDatabaseMap(ArRateIncrementalInfoPeer::DATABASE_NAME);

		$tMap = $this->dbMap->addTable(ArRateIncrementalInfoPeer::TABLE_NAME);
		$tMap->setPhpName('ArRateIncrementalInfo');
		$tMap->setClassname('ArRateIncrementalInfo');

		$tMap->setUseIdGenerator(true);

		$tMap->addPrimaryKey('ID', 'Id', 'INTEGER', true, null);

		$tMap->addForeignKey('AR_PARTY_ID', 'ArPartyId', 'INTEGER', 'ar_party', 'ID', false, null);

		$tMap->addForeignKey('AR_RATE_ID', 'ArRateId', 'INTEGER', 'ar_rate', 'ID', false, null);

		$tMap->addColumn('PERIOD', 'Period', 'VARCHAR', false, 1024);

		$tMap->addColumn('LAST_PROCESSED_CDR_DATE', 'LastProcessedCdrDate', 'TIMESTAMP', false, null);

		$tMap->addColumn('LAST_PROCESSED_CDR_ID', 'LastProcessedCdrId', 'INTEGER', false, 20);

		$tMap->addColumn('BUNDLE_RATE', 'BundleRate', 'CLOB', false, null);

	} 
} 