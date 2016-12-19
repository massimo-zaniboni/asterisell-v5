<?php


/**
 * This class defines the structure of the 'ar_organization_backup_of_changes' table.
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
class ArOrganizationBackupOfChangesTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArOrganizationBackupOfChangesTableMap';

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
		$this->setName('ar_organization_backup_of_changes');
		$this->setPhpName('ArOrganizationBackupOfChanges');
		$this->setClassname('ArOrganizationBackupOfChanges');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addColumn('BACKUP_AT_DATE', 'BackupAtDate', 'TIMESTAMP', true, null, null);
		$this->addColumn('APPLICATION_VERSION', 'ApplicationVersion', 'VARCHAR', false, 255, null);
		$this->addColumn('MD5_SUM', 'Md5Sum', 'VARCHAR', false, 2048, null);
		$this->addColumn('YAML_EXPORT_AT_DATE', 'YamlExportAtDate', 'BLOB', false, null, null);
		$this->addColumn('SQL_TABLES', 'SqlTables', 'BLOB', false, null, null);
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

} // ArOrganizationBackupOfChangesTableMap
