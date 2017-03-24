<?php


/**
 * This class defines the structure of the 'ar_tag' table.
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
class ArTagTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArTagTableMap';

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
		$this->setName('ar_tag');
		$this->setPhpName('ArTag');
		$this->setClassname('ArTag');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addColumn('INTERNAL_NAME', 'InternalName', 'VARCHAR', true, 255, null);
		$this->addColumn('NOTE_FOR_ADMIN', 'NoteForAdmin', 'VARCHAR', false, 1024, null);
		$this->addColumn('NAME_FOR_CUSTOMER', 'NameForCustomer', 'VARCHAR', true, 512, '');
		$this->addColumn('NOTE_FOR_CUSTOMER', 'NoteForCustomer', 'VARCHAR', true, 1204, '');
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
    $this->addRelation('ArPartyHasTag', 'ArPartyHasTag', RelationMap::ONE_TO_MANY, array('id' => 'ar_tag_id', ), 'CASCADE', null);
    $this->addRelation('ArReport', 'ArReport', RelationMap::ONE_TO_MANY, array('id' => 'ar_tag_id', ), null, null);
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

} // ArTagTableMap
