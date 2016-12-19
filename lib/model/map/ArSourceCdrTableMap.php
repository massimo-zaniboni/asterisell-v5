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
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addForeignKey('AR_CDR_PROVIDER_ID', 'ArCdrProviderId', 'INTEGER', 'ar_cdr_provider', 'ID', false, null, null);
		$this->addForeignKey('AR_PHYSICAL_FORMAT_ID', 'ArPhysicalFormatId', 'INTEGER', 'ar_physical_format', 'ID', false, null, null);
		$this->addColumn('CALLDATE', 'Calldate', 'TIMESTAMP', true, null, null);
		$this->addColumn('IS_IMPORTED_SERVICE_CDR', 'IsImportedServiceCdr', 'BOOLEAN', true, null, false);
		$this->addColumn('CONTENT', 'Content', 'VARCHAR', false, 51200, null);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
    $this->addRelation('ArCdrProvider', 'ArCdrProvider', RelationMap::MANY_TO_ONE, array('ar_cdr_provider_id' => 'id', ), null, null);
    $this->addRelation('ArPhysicalFormat', 'ArPhysicalFormat', RelationMap::MANY_TO_ONE, array('ar_physical_format_id' => 'id', ), null, null);
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
