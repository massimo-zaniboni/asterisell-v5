<?php


/**
 * This class defines the structure of the 'ar_wholesale_number' table.
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
class ArWholesaleNumberTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArWholesaleNumberTableMap';

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
		$this->setName('ar_wholesale_number');
		$this->setPhpName('ArWholesaleNumber');
		$this->setClassname('ArWholesaleNumber');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addColumn('TELEPHONE_NUMBER', 'TelephoneNumber', 'VARCHAR', true, 255, null);
		$this->addColumn('FROM_DATE', 'FromDate', 'TIMESTAMP', true, null, null);
		$this->addColumn('EXISTS', 'Exists', 'BOOLEAN', true, null, true);
		$this->addColumn('EXTENSION_CODES', 'ExtensionCodes', 'VARCHAR', false, 5024, null);
		$this->addColumn('USE_DEFAULT_EXTENSION_CODES', 'UseDefaultExtensionCodes', 'BOOLEAN', false, null, null);
		$this->addForeignKey('AR_RESELLER_ID', 'ArResellerId', 'INTEGER', 'ar_reseller', 'ID', false, null, null);
		$this->addForeignKey('AR_WHOLESALE_CARRIER_ID', 'ArWholesaleCarrierId', 'INTEGER', 'ar_wholesale_carrier', 'ID', false, null, null);
		$this->addColumn('INCOME_PRICE', 'IncomePrice', 'BIGINT', true, null, 0);
		$this->addColumn('COST_PRICE', 'CostPrice', 'BIGINT', true, null, 0);
		$this->addColumn('CSV_COMMENT', 'CsvComment', 'VARCHAR', false, 255, null);
		$this->addColumn('CSV_LAST_DATE', 'CsvLastDate', 'TIMESTAMP', false, null, null);
		$this->addColumn('CSV_TO_DELETE', 'CsvToDelete', 'BOOLEAN', true, null, false);
		$this->addColumn('CSV_IS_CURRENT', 'CsvIsCurrent', 'BOOLEAN', true, null, false);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
    $this->addRelation('ArReseller', 'ArReseller', RelationMap::MANY_TO_ONE, array('ar_reseller_id' => 'id', ), null, null);
    $this->addRelation('ArWholesaleCarrier', 'ArWholesaleCarrier', RelationMap::MANY_TO_ONE, array('ar_wholesale_carrier_id' => 'id', ), null, null);
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

} // ArWholesaleNumberTableMap
