<?php


/**
 * This class defines the structure of the 'ar_global_permissions' table.
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
class ArGlobalPermissionsTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArGlobalPermissionsTableMap';

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
		$this->setName('ar_global_permissions');
		$this->setPhpName('ArGlobalPermissions');
		$this->setClassname('ArGlobalPermissions');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addColumn('SHOW_CALL_COST', 'ShowCallCost', 'BOOLEAN', false, null, null);
		$this->addColumn('SHOW_CALL_INCOME', 'ShowCallIncome', 'BOOLEAN', false, null, null);
		$this->addColumn('SHOW_OUTGOING_CALLS', 'ShowOutgoingCalls', 'BOOLEAN', false, null, null);
		$this->addColumn('SHOW_INCOMING_CALLS', 'ShowIncomingCalls', 'BOOLEAN', false, null, null);
		$this->addColumn('SHOW_INTERNAL_CALLS', 'ShowInternalCalls', 'BOOLEAN', false, null, null);
		$this->addColumn('SHOW_VOIP_PROVIDER', 'ShowVoipProvider', 'BOOLEAN', false, null, null);
		$this->addColumn('SHOW_COMMUNICATION_CHANNEL', 'ShowCommunicationChannel', 'BOOLEAN', false, null, null);
		$this->addColumn('SHOW_COST_SAVING', 'ShowCostSaving', 'BOOLEAN', false, null, null);
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

} // ArGlobalPermissionsTableMap
