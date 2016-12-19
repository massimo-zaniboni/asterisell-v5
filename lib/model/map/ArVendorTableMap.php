<?php


/**
 * This class defines the structure of the 'ar_vendor' table.
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
class ArVendorTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArVendorTableMap';

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
		$this->setName('ar_vendor');
		$this->setPhpName('ArVendor');
		$this->setClassname('ArVendor');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addColumn('INTERNAL_NAME', 'InternalName', 'VARCHAR', false, 200, null);
		$this->addForeignKey('AR_PARTY_ID', 'ArPartyId', 'INTEGER', 'ar_party', 'ID', false, null, null);
		$this->addColumn('IS_INTERNAL', 'IsInternal', 'BOOLEAN', true, null, false);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
    $this->addRelation('ArParty', 'ArParty', RelationMap::MANY_TO_ONE, array('ar_party_id' => 'id', ), null, null);
    $this->addRelation('ArCdr', 'ArCdr', RelationMap::ONE_TO_MANY, array('id' => 'ar_vendor_id', ), null, null);
    $this->addRelation('ArRate', 'ArRate', RelationMap::ONE_TO_MANY, array('id' => 'ar_vendor_id', ), null, null);
    $this->addRelation('ArVendorDomain', 'ArVendorDomain', RelationMap::ONE_TO_MANY, array('id' => 'ar_vendor_id', ), null, null);
    $this->addRelation('ArReport', 'ArReport', RelationMap::ONE_TO_MANY, array('id' => 'ar_vendor_id', ), null, null);
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

} // ArVendorTableMap
