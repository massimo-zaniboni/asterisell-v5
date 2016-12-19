<?php


/**
 * This class defines the structure of the 'ar_role' table.
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
class ArRoleTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArRoleTableMap';

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
		$this->setName('ar_role');
		$this->setPhpName('ArRole');
		$this->setClassname('ArRole');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addColumn('NAME', 'Name', 'VARCHAR', false, 1024, null);
		$this->addColumn('POWER', 'Power', 'INTEGER', true, 5, null);
		$this->addColumn('DESCRIPTION', 'Description', 'LONGVARCHAR', false, null, null);
		$this->addColumn('INTERNAL_NAME', 'InternalName', 'VARCHAR', false, 200, null);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
    $this->addRelation('ArUserHasRole', 'ArUserHasRole', RelationMap::ONE_TO_MANY, array('id' => 'ar_role_id', ), null, null);
    $this->addRelation('ArRoleHasPermission', 'ArRoleHasPermission', RelationMap::ONE_TO_MANY, array('id' => 'ar_role_id', ), null, null);
    $this->addRelation('ArReportAlsoFor', 'ArReportAlsoFor', RelationMap::ONE_TO_MANY, array('id' => 'ar_role_id', ), 'CASCADE', null);
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

} // ArRoleTableMap
