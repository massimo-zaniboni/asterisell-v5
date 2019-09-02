<?php


/**
 * This class defines the structure of the 'ar_wholesale_update_proc' table.
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
class ArWholesaleUpdateProcTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArWholesaleUpdateProcTableMap';

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
		$this->setName('ar_wholesale_update_proc');
		$this->setPhpName('ArWholesaleUpdateProc');
		$this->setClassname('ArWholesaleUpdateProc');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(false);
		// columns
		$this->addPrimaryKey('FOREIGN_ID', 'ForeignId', 'INTEGER', true, null, null);
		$this->addColumn('CSV_COMMENT', 'CsvComment', 'VARCHAR', false, 255, null);
		$this->addColumn('CSV_LAST_DATE', 'CsvLastDate', 'TIMESTAMP', false, null, null);
		$this->addColumn('CSV_IS_CURRENT', 'CsvIsCurrent', 'BOOLEAN', true, null, null);
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

} // ArWholesaleUpdateProcTableMap
