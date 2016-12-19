<?php


/**
 * This class defines the structure of the 'ar_rate_incremental_info' table.
 *
 *
 *
 * This map class is used by Propel to do runtime db structure discovery.
 * For example, the createSelectSql() method checks the type of a given column used in an
 * ORDER BY clause to know whether it needs to apply SQL to make the ORDER BY case-insensitive
 * (i.e. if it's a text column type).
 *
 * @package    lib.model.map
 */
class ArRateIncrementalInfoTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArRateIncrementalInfoTableMap';

	/**
	 * Initialize the table attributes, columns and validators
	 * Relations are not initialized by this method since they are lazy loaded
	 *
	 * @return     void
	 * @throws     PropelException
	 */
	public function initialize()
	{
	  // attributes
		$this->setName('ar_rate_incremental_info');
		$this->setPhpName('ArRateIncrementalInfo');
		$this->setClassname('ArRateIncrementalInfo');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addForeignKey('AR_PARTY_ID', 'ArPartyId', 'INTEGER', 'ar_party', 'ID', false, null, null);
		$this->addForeignKey('AR_RATE_ID', 'ArRateId', 'INTEGER', 'ar_rate', 'ID', false, null, null);
		$this->addColumn('PERIOD', 'Period', 'VARCHAR', false, 1024, null);
		$this->addColumn('LAST_PROCESSED_AR_CDR_DATE', 'LastProcessedArCdrDate', 'TIMESTAMP', false, null, null);
		$this->addColumn('LAST_PROCESSED_AR_CDR_ID', 'LastProcessedArCdrId', 'INTEGER', false, null, null);
		$this->addColumn('BUNDLE_RATE', 'BundleRate', 'CLOB', false, null, null);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
    $this->addRelation('ArParty', 'ArParty', RelationMap::MANY_TO_ONE, array('ar_party_id' => 'id', ), null, null);
    $this->addRelation('ArRate', 'ArRate', RelationMap::MANY_TO_ONE, array('ar_rate_id' => 'id', ), null, null);
	} // buildRelations()

	/**
	 * 
	 * Gets the list of behaviors registered for this table
	 * 
	 * @return array Associative array (name => parameters) of behaviors
	 */
	public function getBehaviors()
	{
		return array(
			'symfony' => array('form' => 'true', 'filter' => 'true', ),
		);
	} // getBehaviors()

} // ArRateIncrementalInfoTableMap
