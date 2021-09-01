<?php


/**
 * This class defines the structure of the 'ar_source_cdr' table.
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
class ArSourceCdrTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArSourceCdrTableMap';

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
		$this->setName('ar_source_cdr');
		$this->setPhpName('ArSourceCdr');
		$this->setClassname('ArSourceCdr');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(false);
		// columns
		$this->addPrimaryKey('CALLDATE', 'Calldate', 'TIMESTAMP', true, null, null);
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addColumn('AR_CDR_PROVIDER_ID', 'ArCdrProviderId', 'INTEGER', true, null, null);
		$this->addColumn('AR_PHYSICAL_FORMAT_ID', 'ArPhysicalFormatId', 'INTEGER', true, null, null);
		$this->addColumn('CONTENT', 'Content', 'VARCHAR', false, 10000, null);
		$this->addColumn('IS_HACKED', 'IsHacked', 'BOOLEAN', true, null, false);
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

} // ArSourceCdrTableMap
