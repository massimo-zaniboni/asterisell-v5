<?php


/**
 * This class defines the structure of the 'ar_reseller' table.
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
class ArResellerTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArResellerTableMap';

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
		$this->setName('ar_reseller');
		$this->setPhpName('ArReseller');
		$this->setClassname('ArReseller');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addColumn('INTERNAL_NAME', 'InternalName', 'VARCHAR', false, 255, null);
		$this->addColumn('NAME', 'Name', 'VARCHAR', true, 1204, null);
		$this->addColumn('NOTE', 'Note', 'VARCHAR', false, 2048, null);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
    $this->addRelation('ArParty', 'ArParty', RelationMap::ONE_TO_MANY, array('id' => 'ar_reseller_id', ), null, null);
    $this->addRelation('ArRateSharedWithReseller', 'ArRateSharedWithReseller', RelationMap::ONE_TO_MANY, array('id' => 'ar_reseller_id', ), null, null);
    $this->addRelation('ArWholesaleNumber', 'ArWholesaleNumber', RelationMap::ONE_TO_MANY, array('id' => 'ar_reseller_id', ), null, null);
    $this->addRelation('ArWholesaleReplaceProc', 'ArWholesaleReplaceProc', RelationMap::ONE_TO_MANY, array('id' => 'ar_reseller_id', ), null, null);
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

} // ArResellerTableMap
