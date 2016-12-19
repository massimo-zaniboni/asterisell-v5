<?php


/**
 * This class defines the structure of the 'ar_source_csv_file' table.
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
class ArSourceCsvFileTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArSourceCsvFileTableMap';

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
		$this->setName('ar_source_csv_file');
		$this->setPhpName('ArSourceCsvFile');
		$this->setClassname('ArSourceCsvFile');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addForeignKey('AR_CDR_PROVIDER_ID', 'ArCdrProviderId', 'INTEGER', 'ar_cdr_provider', 'ID', false, null, null);
		$this->addForeignKey('AR_PHYSICAL_FORMAT_ID', 'ArPhysicalFormatId', 'INTEGER', 'ar_physical_format', 'ID', false, null, null);
		$this->addColumn('RETRIEVED_FROM_SERVER', 'RetrievedFromServer', 'VARCHAR', false, 1024, null);
		$this->addColumn('IS_STATUS', 'IsStatus', 'BOOLEAN', true, null, null);
		$this->addColumn('IS_CALLDATE_PROCESSED', 'IsCalldateProcessed', 'BOOLEAN', true, null, false);
		$this->addColumn('IS_IMPORTED', 'IsImported', 'BOOLEAN', true, null, false);
		$this->addColumn('IS_ACTIVE_INFO', 'IsActiveInfo', 'BOOLEAN', true, null, true);
		$this->addColumn('MIN_CALLDATE', 'MinCalldate', 'TIMESTAMP', false, null, null);
		$this->addColumn('MAX_CALLDATE', 'MaxCalldate', 'TIMESTAMP', false, null, null);
		$this->addColumn('NAME', 'Name', 'VARCHAR', true, 255, null);
		$this->addColumn('ARCHIVE_DIRECTORY', 'ArchiveDirectory', 'VARCHAR', true, 2048, null);
		$this->addColumn('CHECKSUM', 'Checksum', 'VARCHAR', false, 2048, null);
		$this->addColumn('RECEIVING_DATE', 'ReceivingDate', 'TIMESTAMP', false, null, null);
		$this->addColumn('SERIOUS_PROCESSING_ERRORS', 'SeriousProcessingErrors', 'BOOLEAN', true, null, false);
		$this->addColumn('TOT_LINES', 'TotLines', 'INTEGER', false, null, null);
		$this->addColumn('LINES_WITH_ERRORS', 'LinesWithErrors', 'INTEGER', false, null, null);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
    $this->addRelation('ArCdrProvider', 'ArCdrProvider', RelationMap::MANY_TO_ONE, array('ar_cdr_provider_id' => 'id', ), null, null);
    $this->addRelation('ArPhysicalFormat', 'ArPhysicalFormat', RelationMap::MANY_TO_ONE, array('ar_physical_format_id' => 'id', ), null, null);
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

} // ArSourceCsvFileTableMap
