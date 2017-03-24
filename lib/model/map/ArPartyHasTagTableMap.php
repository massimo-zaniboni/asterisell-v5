<?php


/**
 * This class defines the structure of the 'ar_party_has_tag' table.
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
class ArPartyHasTagTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArPartyHasTagTableMap';

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
		$this->setName('ar_party_has_tag');
		$this->setPhpName('ArPartyHasTag');
		$this->setClassname('ArPartyHasTag');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(false);
		// columns
		$this->addForeignPrimaryKey('AR_PARTY_ID', 'ArPartyId', 'INTEGER' , 'ar_party', 'ID', true, null, null);
		$this->addForeignPrimaryKey('AR_TAG_ID', 'ArTagId', 'INTEGER' , 'ar_tag', 'ID', true, null, null);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
    $this->addRelation('ArParty', 'ArParty', RelationMap::MANY_TO_ONE, array('ar_party_id' => 'id', ), 'CASCADE', null);
    $this->addRelation('ArTag', 'ArTag', RelationMap::MANY_TO_ONE, array('ar_tag_id' => 'id', ), 'CASCADE', null);
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

} // ArPartyHasTagTableMap
