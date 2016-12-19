<?php


/**
 * This class defines the structure of the 'ar_vendor_domain' table.
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
class ArVendorDomainTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArVendorDomainTableMap';

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
		$this->setName('ar_vendor_domain');
		$this->setPhpName('ArVendorDomain');
		$this->setClassname('ArVendorDomain');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addColumn('INTERNAL_NAME', 'InternalName', 'VARCHAR', false, 200, null);
		$this->addForeignKey('AR_VENDOR_ID', 'ArVendorId', 'INTEGER', 'ar_vendor', 'ID', false, null, null);
		$this->addForeignKey('AR_COMMUNICATION_CHANNEL_TYPE_ID', 'ArCommunicationChannelTypeId', 'INTEGER', 'ar_communication_channel_type', 'ID', false, null, null);
		$this->addColumn('DOMAIN', 'Domain', 'VARCHAR', true, 255, null);
		$this->addColumn('IS_PREFIX', 'IsPrefix', 'BOOLEAN', true, null, false);
		$this->addColumn('IS_SUFFIX', 'IsSuffix', 'BOOLEAN', true, null, false);
		$this->addColumn('FROM', 'From', 'TIMESTAMP', true, null, null);
		$this->addColumn('TO', 'To', 'TIMESTAMP', false, null, null);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
    $this->addRelation('ArVendor', 'ArVendor', RelationMap::MANY_TO_ONE, array('ar_vendor_id' => 'id', ), null, null);
    $this->addRelation('ArCommunicationChannelType', 'ArCommunicationChannelType', RelationMap::MANY_TO_ONE, array('ar_communication_channel_type_id' => 'id', ), null, null);
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

} // ArVendorDomainTableMap
