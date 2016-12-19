<?php


/**
 * This class defines the structure of the 'ar_report_needed_permissions' table.
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
class ArReportNeededPermissionsTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArReportNeededPermissionsTableMap';

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
		$this->setName('ar_report_needed_permissions');
		$this->setPhpName('ArReportNeededPermissions');
		$this->setClassname('ArReportNeededPermissions');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addForeignKey('AR_REPORT_ID', 'ArReportId', 'INTEGER', 'ar_report', 'ID', false, null, null);
		$this->addForeignKey('AR_PERMISSION_ID', 'ArPermissionId', 'INTEGER', 'ar_permission', 'ID', false, null, null);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
    $this->addRelation('ArReport', 'ArReport', RelationMap::MANY_TO_ONE, array('ar_report_id' => 'id', ), null, null);
    $this->addRelation('ArPermission', 'ArPermission', RelationMap::MANY_TO_ONE, array('ar_permission_id' => 'id', ), null, null);
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

} // ArReportNeededPermissionsTableMap
