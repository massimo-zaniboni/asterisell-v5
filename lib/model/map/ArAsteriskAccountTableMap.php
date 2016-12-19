<?php


/**
 * This class defines the structure of the 'ar_asterisk_account' table.
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
class ArAsteriskAccountTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArAsteriskAccountTableMap';

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
		$this->setName('ar_asterisk_account');
		$this->setPhpName('ArAsteriskAccount');
		$this->setClassname('ArAsteriskAccount');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addColumn('NAME', 'Name', 'VARCHAR', false, 255, null);
		$this->addColumn('ACCOUNT_CODE', 'AccountCode', 'VARCHAR', true, 255, null);
		$this->addForeignKey('AR_OFFICE_ID', 'ArOfficeId', 'INTEGER', 'ar_office', 'ID', false, null, null);
		$this->addColumn('IS_ACTIVE', 'IsActive', 'BOOLEAN', true, null, true);
		$this->addForeignKey('AR_RATE_CATEGORY_ID', 'ArRateCategoryId', 'INTEGER', 'ar_rate_category', 'ID', false, null, null);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
    $this->addRelation('ArOffice', 'ArOffice', RelationMap::MANY_TO_ONE, array('ar_office_id' => 'id', ), null, null);
    $this->addRelation('ArRateCategory', 'ArRateCategory', RelationMap::MANY_TO_ONE, array('ar_rate_category_id' => 'id', ), null, null);
    $this->addRelation('ArCdr', 'ArCdr', RelationMap::ONE_TO_MANY, array('id' => 'ar_asterisk_account_id', ), null, null);
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

} // ArAsteriskAccountTableMap
