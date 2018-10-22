<?php


/**
 * This class defines the structure of the 'ar_cdr_provider' table.
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
class ArCdrProviderTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArCdrProviderTableMap';

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
		$this->setName('ar_cdr_provider');
		$this->setPhpName('ArCdrProvider');
		$this->setClassname('ArCdrProvider');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addColumn('INTERNAL_NAME', 'InternalName', 'VARCHAR', false, 255, null);
		$this->addColumn('DESCRIPTION', 'Description', 'VARCHAR', false, 2048, null);
		$this->addColumn('LAST_IMPORTED_ID', 'LastImportedId', 'BIGINT', false, null, null);
		$this->addColumn('LAST_IMPORTED_DATA', 'LastImportedData', 'VARCHAR', false, 2048, null);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
    $this->addRelation('ArSourceCsvFile', 'ArSourceCsvFile', RelationMap::ONE_TO_MANY, array('id' => 'ar_cdr_provider_id', ), null, null);
    $this->addRelation('ArRemoteFile', 'ArRemoteFile', RelationMap::ONE_TO_MANY, array('id' => 'ar_cdr_provider_id', ), null, null);
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

} // ArCdrProviderTableMap
