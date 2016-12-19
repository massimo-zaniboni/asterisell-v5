<?php


/**
 * This class defines the structure of the 'ar_cached_grouped_cdr' table.
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
class ArCachedGroupedCdrTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArCachedGroupedCdrTableMap';

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
		$this->setName('ar_cached_grouped_cdr');
		$this->setPhpName('ArCachedGroupedCdr');
		$this->setClassname('ArCachedGroupedCdr');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addColumn('CALLDATE', 'Calldate', 'DATE', true, null, null);
		$this->addColumn('CACHED_PARENT_ID_HIERARCHY', 'CachedParentIdHierarchy', 'VARCHAR', false, 850, null);
		$this->addColumn('DESTINATION_TYPE', 'DestinationType', 'INTEGER', true, 1, null);
		$this->addColumn('COUNT_OF_CALLS', 'CountOfCalls', 'INTEGER', true, null, null);
		$this->addColumn('BILLSEC', 'Billsec', 'INTEGER', true, null, null);
		$this->addColumn('INCOME', 'Income', 'BIGINT', true, null, null);
		$this->addColumn('COST_SAVING', 'CostSaving', 'BIGINT', true, null, null);
		$this->addColumn('COST', 'Cost', 'BIGINT', true, null, null);
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

} // ArCachedGroupedCdrTableMap
