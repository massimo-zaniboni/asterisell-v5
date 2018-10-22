<?php


/**
 * This class defines the structure of the 'list_cdrs_admin_3_fast' table.
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
class ListCdrsAdmin3FastTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ListCdrsAdmin3FastTableMap';

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
		$this->setName('list_cdrs_admin_3_fast');
		$this->setPhpName('ListCdrsAdmin3Fast');
		$this->setClassname('ListCdrsAdmin3Fast');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addColumn('DESTINATION_TYPE', 'DestinationType', 'INTEGER', true, 1, 0);
		$this->addColumn('ERROR_DESTINATION_TYPE', 'ErrorDestinationType', 'INTEGER', true, 1, 0);
		$this->addColumn('OPERATOR_TYPE', 'OperatorType', 'VARCHAR', false, 255, null);
		$this->addColumn('AR_COMMUNICATION_CHANNEL_TYPE_ID', 'ArCommunicationChannelTypeId', 'INTEGER', false, null, null);
		$this->addColumn('VENDOR_ID', 'VendorId', 'INTEGER', false, null, null);
		$this->addColumn('COUNT_OF_CALLS', 'CountOfCalls', 'INTEGER', false, null, null);
		$this->addColumn('BILLSEC', 'Billsec', 'INTEGER', false, null, null);
		$this->addColumn('INCOME', 'Income', 'INTEGER', false, null, null);
		$this->addColumn('COST', 'Cost', 'INTEGER', false, null, null);
		$this->addColumn('COST_SAVING', 'CostSaving', 'INTEGER', false, null, null);
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

} // ListCdrsAdmin3FastTableMap
