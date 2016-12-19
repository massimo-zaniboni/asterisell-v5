<?php


/**
 * This class defines the structure of the 'ar_asterisk_account_range' table.
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
class ArAsteriskAccountRangeTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArAsteriskAccountRangeTableMap';

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
		$this->setName('ar_asterisk_account_range');
		$this->setPhpName('ArAsteriskAccountRange');
		$this->setClassname('ArAsteriskAccountRange');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addForeignKey('AR_ORGANIZATION_UNIT_ID', 'ArOrganizationUnitId', 'INTEGER', 'ar_organization_unit', 'ID', false, null, null);
		$this->addColumn('SYSTEM_PREFIX', 'SystemPrefix', 'VARCHAR', false, 255, null);
		$this->addColumn('SYSTEM_SUFFIX', 'SystemSuffix', 'VARCHAR', false, 255, null);
		$this->addColumn('SYSTEM_START_RANGE', 'SystemStartRange', 'VARCHAR', true, 18, null);
		$this->addColumn('SYSTEM_END_RANGE', 'SystemEndRange', 'VARCHAR', true, 18, null);
		$this->addColumn('SYSTEM_LEADING_ZERO', 'SystemLeadingZero', 'INTEGER', true, 4, null);
		$this->addColumn('IS_DELETE', 'IsDelete', 'BOOLEAN', true, null, false);
		$this->addColumn('IS_PHYSICAL_DELETE', 'IsPhysicalDelete', 'BOOLEAN', true, null, false);
		$this->addColumn('USER_PREFIX', 'UserPrefix', 'VARCHAR', false, 255, null);
		$this->addColumn('USER_SUFFIX', 'UserSuffix', 'VARCHAR', false, 255, null);
		$this->addColumn('USER_START_RANGE', 'UserStartRange', 'VARCHAR', true, 18, null);
		$this->addColumn('GENERATE_RANGE_FOR_USERS', 'GenerateRangeForUsers', 'BOOLEAN', true, null, true);
		$this->addColumn('USER_LEADING_ZERO', 'UserLeadingZero', 'INTEGER', true, 4, null);
		$this->addColumn('USER_NOTE', 'UserNote', 'VARCHAR', false, 6048, null);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
    $this->addRelation('ArOrganizationUnit', 'ArOrganizationUnit', RelationMap::MANY_TO_ONE, array('ar_organization_unit_id' => 'id', ), null, null);
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

} // ArAsteriskAccountRangeTableMap
