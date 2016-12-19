<?php


/**
 * This class defines the structure of the 'ar_itc_organizations' table.
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
class ArItcOrganizationsTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArItcOrganizationsTableMap';

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
		$this->setName('ar_itc_organizations');
		$this->setPhpName('ArItcOrganizations');
		$this->setClassname('ArItcOrganizations');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addColumn('ACCOUNT_CODE', 'AccountCode', 'VARCHAR', true, 255, null);
		$this->addColumn('DEFINITION_TIME', 'DefinitionTime', 'TIMESTAMP', false, null, null);
		$this->addColumn('ORG', 'Org', 'VARCHAR', false, 1024, null);
		$this->addColumn('NAME', 'Name', 'VARCHAR', false, 1024, null);
		$this->addColumn('DESCRIPTION', 'Description', 'VARCHAR', false, 1024, null);
		$this->addColumn('EMAIL', 'Email', 'VARCHAR', false, 1024, null);
		$this->addColumn('PARENT', 'Parent', 'VARCHAR', false, 1024, null);
		$this->addColumn('CALCULATED_ACCOUNT_CODE', 'CalculatedAccountCode', 'VARCHAR', false, 9046, null);
		$this->addColumn('IS_NEW', 'IsNew', 'BOOLEAN', false, null, null);
		$this->addColumn('IS_MAYBE_MODIFIED', 'IsMaybeModified', 'BOOLEAN', false, null, null);
		$this->addColumn('IS_TO_REMOVE', 'IsToRemove', 'BOOLEAN', false, null, null);
		$this->addColumn('CAN_BE_REMOVED', 'CanBeRemoved', 'BOOLEAN', false, null, null);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
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

} // ArItcOrganizationsTableMap
