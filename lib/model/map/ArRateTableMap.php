<?php


/**
 * This class defines the structure of the 'ar_rate' table.
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
class ArRateTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArRateTableMap';

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
		$this->setName('ar_rate');
		$this->setPhpName('ArRate');
		$this->setClassname('ArRate');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addForeignKey('AR_VENDOR_ID', 'ArVendorId', 'INTEGER', 'ar_vendor', 'ID', false, null, null);
		$this->addForeignKey('AR_RATE_FORMAT_ID', 'ArRateFormatId', 'INTEGER', 'ar_rate_format', 'ID', false, null, null);
		$this->addColumn('FROM_TIME', 'FromTime', 'TIMESTAMP', true, null, null);
		$this->addColumn('INTERNAL_NAME', 'InternalName', 'VARCHAR', false, 512, null);
		$this->addForeignKey('AR_RATE_ID', 'ArRateId', 'INTEGER', 'ar_rate', 'ID', false, null, null);
		$this->addColumn('SHORT_DESCRIPTION', 'ShortDescription', 'VARCHAR', false, 1024, null);
		$this->addColumn('NOTE', 'Note', 'LONGVARCHAR', false, null, null);
		$this->addColumn('IS_EXPORTED_TO_RESELLERS', 'IsExportedToResellers', 'BOOLEAN', true, null, false);
		$this->addColumn('WAS_COMPILED', 'WasCompiled', 'BOOLEAN', true, null, false);
		$this->addColumn('SOURCE_DATA_FILE', 'SourceDataFile', 'BLOB', false, null, null);
		$this->addColumn('BACKUP_SOURCE_DATA_FILE', 'BackupSourceDataFile', 'BLOB', false, null, null);
		$this->addColumn('HTML_DESCRIPTION', 'HtmlDescription', 'BLOB', false, null, null);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
    $this->addRelation('ArVendor', 'ArVendor', RelationMap::MANY_TO_ONE, array('ar_vendor_id' => 'id', ), null, null);
    $this->addRelation('ArRateFormat', 'ArRateFormat', RelationMap::MANY_TO_ONE, array('ar_rate_format_id' => 'id', ), null, null);
    $this->addRelation('ArRateRelatedByArRateId', 'ArRate', RelationMap::MANY_TO_ONE, array('ar_rate_id' => 'id', ), null, null);
    $this->addRelation('ArRateRelatedByArRateId', 'ArRate', RelationMap::ONE_TO_MANY, array('id' => 'ar_rate_id', ), null, null);
    $this->addRelation('ArRateSharedWithReseller', 'ArRateSharedWithReseller', RelationMap::ONE_TO_MANY, array('id' => 'ar_rate_id', ), 'CASCADE', null);
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

} // ArRateTableMap
