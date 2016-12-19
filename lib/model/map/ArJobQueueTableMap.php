<?php


/**
 * This class defines the structure of the 'ar_job_queue' table.
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
class ArJobQueueTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArJobQueueTableMap';

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
		$this->setName('ar_job_queue');
		$this->setPhpName('ArJobQueue');
		$this->setClassname('ArJobQueue');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addColumn('IS_PART_OF', 'IsPartOf', 'INTEGER', true, null, null);
		$this->addColumn('STATE', 'State', 'INTEGER', true, 1, 0);
		$this->addColumn('CREATED_AT', 'CreatedAt', 'TIMESTAMP', false, null, null);
		$this->addColumn('START_AT', 'StartAt', 'TIMESTAMP', false, null, null);
		$this->addColumn('END_AT', 'EndAt', 'TIMESTAMP', false, null, null);
		$this->addColumn('DESCRIPTION', 'Description', 'VARCHAR', true, 12000, null);
		$this->addColumn('PHP_DATA_JOB_SERIALIZATION', 'PhpDataJobSerialization', 'CLOB', false, null, null);
		$this->addColumn('INTERNAL_NAME', 'InternalName', 'VARCHAR', false, 512, null);
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
			'symfony_timestampable' => array('create_column' => 'created_at', ),
		);
	} // getBehaviors()

} // ArJobQueueTableMap
