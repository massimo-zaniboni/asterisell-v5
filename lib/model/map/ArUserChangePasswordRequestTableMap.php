<?php


/**
 * This class defines the structure of the 'ar_user_change_password_request' table.
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
class ArUserChangePasswordRequestTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArUserChangePasswordRequestTableMap';

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
		$this->setName('ar_user_change_password_request');
		$this->setPhpName('ArUserChangePasswordRequest');
		$this->setClassname('ArUserChangePasswordRequest');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addForeignKey('AR_USER_ID', 'ArUserId', 'INTEGER', 'ar_user', 'ID', false, null, null);
		$this->addColumn('AT_DATE', 'AtDate', 'TIMESTAMP', true, null, null);
		$this->addColumn('OLD_PASSWORD', 'OldPassword', 'VARCHAR', false, 1024, null);
		$this->addColumn('NEW_PASSWORD', 'NewPassword', 'VARCHAR', false, 1024, null);
		$this->addColumn('IS_PROCESSED', 'IsProcessed', 'BOOLEAN', true, null, false);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
    $this->addRelation('ArUser', 'ArUser', RelationMap::MANY_TO_ONE, array('ar_user_id' => 'id', ), null, null);
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

} // ArUserChangePasswordRequestTableMap
