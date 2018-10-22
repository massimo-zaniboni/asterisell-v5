<?php


/**
 * This class defines the structure of the 'ar_test_cached_errors' table.
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
class ArTestCachedErrorsTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArTestCachedErrorsTableMap';

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
		$this->setName('ar_test_cached_errors');
		$this->setPhpName('ArTestCachedErrors');
		$this->setClassname('ArTestCachedErrors');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(false);
		// columns
		$this->addPrimaryKey('CALLDATE', 'Calldate', 'DATE', true, null, null);
		$this->addPrimaryKey('DESTINATION_TYPE', 'DestinationType', 'SMALLINT', true, null, null);
		$this->addPrimaryKey('ERROR_DESTINATION_TYPE', 'ErrorDestinationType', 'SMALLINT', true, null, 0);
		$this->addColumn('COUNT_OF_CALLS', 'CountOfCalls', 'BIGINT', true, null, null);
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

} // ArTestCachedErrorsTableMap
