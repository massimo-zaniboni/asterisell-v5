<?php


/**
 * This class defines the structure of the 'ar_rate_shared_with_reseller' table.
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
class ArRateSharedWithResellerTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArRateSharedWithResellerTableMap';

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
		$this->setName('ar_rate_shared_with_reseller');
		$this->setPhpName('ArRateSharedWithReseller');
		$this->setClassname('ArRateSharedWithReseller');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addForeignKey('AR_RATE_ID', 'ArRateId', 'INTEGER', 'ar_rate', 'ID', false, null, null);
		$this->addForeignKey('AR_RESELLER_ID', 'ArResellerId', 'INTEGER', 'ar_reseller', 'ID', false, null, null);
		$this->addColumn('IS_EXPORTED', 'IsExported', 'BOOLEAN', true, null, false);
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
    $this->addRelation('ArRate', 'ArRate', RelationMap::MANY_TO_ONE, array('ar_rate_id' => 'id', ), 'CASCADE', null);
    $this->addRelation('ArReseller', 'ArReseller', RelationMap::MANY_TO_ONE, array('ar_reseller_id' => 'id', ), null, null);
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

} // ArRateSharedWithResellerTableMap
