<?php


/**
 * This class defines the structure of the 'ar_rate_format' table.
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
class ArRateFormatTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArRateFormatTableMap';

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
		$this->setName('ar_rate_format');
		$this->setPhpName('ArRateFormat');
		$this->setClassname('ArRateFormat');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addColumn('SHORT_DESCRIPTION', 'ShortDescription', 'LONGVARCHAR', false, null, null);
		$this->addColumn('DETAILED_DESCRIPTION', 'DetailedDescription', 'LONGVARCHAR', false, null, null);
		$this->addColumn('INTERNAL_NAME', 'InternalName', 'VARCHAR', true, 255, null);
		$this->addColumn('ORDER_NAME', 'OrderName', 'VARCHAR', false, 255, null);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
    $this->addRelation('ArRate', 'ArRate', RelationMap::ONE_TO_MANY, array('id' => 'ar_rate_format_id', ), null, null);
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

} // ArRateFormatTableMap
