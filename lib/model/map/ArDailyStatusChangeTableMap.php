<?php


/**
 * This class defines the structure of the 'ar_daily_status_change' table.
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
class ArDailyStatusChangeTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArDailyStatusChangeTableMap';

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
		$this->setName('ar_daily_status_change');
		$this->setPhpName('ArDailyStatusChange');
		$this->setClassname('ArDailyStatusChange');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(false);
		// columns
		$this->addPrimaryKey('DAY', 'Day', 'DATE', true, null, null);
		$this->addPrimaryKey('IS_SERVICE_CDR', 'IsServiceCdr', 'BOOLEAN', true, null, null);
		$this->addForeignPrimaryKey('AR_DAILY_STATUS_JOB_ID', 'ArDailyStatusJobId', 'INTEGER' , 'ar_daily_status_job', 'ID', true, null, null);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
    $this->addRelation('ArDailyStatusJob', 'ArDailyStatusJob', RelationMap::MANY_TO_ONE, array('ar_daily_status_job_id' => 'id', ), 'CASCADE', null);
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

} // ArDailyStatusChangeTableMap
