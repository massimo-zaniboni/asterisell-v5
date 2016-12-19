<?php


/**
 * This class defines the structure of the 'ar_upgrading_job' table.
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
class ArUpgradingJobTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArUpgradingJobTableMap';

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
		$this->setName('ar_upgrading_job');
		$this->setPhpName('ArUpgradingJob');
		$this->setClassname('ArUpgradingJob');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addColumn('CODE', 'Code', 'VARCHAR', true, 2048, null);
		$this->addColumn('START_TIME', 'StartTime', 'TIMESTAMP', false, null, null);
		$this->addColumn('END_TIME', 'EndTime', 'TIMESTAMP', false, null, null);
		$this->addColumn('SUCESSFUL', 'Sucessful', 'BOOLEAN', false, null, null);
		$this->addColumn('STATUS_MESSAGE', 'StatusMessage', 'LONGVARCHAR', false, null, null);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
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

} // ArUpgradingJobTableMap
