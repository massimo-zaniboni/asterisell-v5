<?php


/**
 * This class defines the structure of the 'ar_itc_source_csv_file' table.
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
class ArItcSourceCsvFileTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArItcSourceCsvFileTableMap';

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
		$this->setName('ar_itc_source_csv_file');
		$this->setPhpName('ArItcSourceCsvFile');
		$this->setClassname('ArItcSourceCsvFile');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addColumn('NAME', 'Name', 'VARCHAR', true, 255, null);
		$this->addColumn('CHECKSUM', 'Checksum', 'VARCHAR', false, 2048, null);
		$this->addColumn('PROCESSING_DATE', 'ProcessingDate', 'TIMESTAMP', false, null, null);
		$this->addColumn('MIN_CALLDATE', 'MinCalldate', 'TIMESTAMP', false, null, null);
		$this->addColumn('MAX_CALLDATE', 'MaxCalldate', 'TIMESTAMP', false, null, null);
		$this->addColumn('TOT_LINES', 'TotLines', 'INTEGER', false, null, null);
		$this->addColumn('LINES_WITH_ERRORS', 'LinesWithErrors', 'INTEGER', false, null, null);
		$this->addColumn('CONTENT_TYPE', 'ContentType', 'VARCHAR', false, 255, null);
		$this->addColumn('RETRIEVED_FROM_SERVER', 'RetrievedFromServer', 'VARCHAR', false, 1024, null);
		$this->addColumn('IMPORTED', 'Imported', 'BOOLEAN', true, null, false);
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

} // ArItcSourceCsvFileTableMap
