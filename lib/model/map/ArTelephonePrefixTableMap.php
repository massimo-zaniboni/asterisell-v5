<?php


/**
 * This class defines the structure of the 'ar_telephone_prefix' table.
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
class ArTelephonePrefixTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArTelephonePrefixTableMap';

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
		$this->setName('ar_telephone_prefix');
		$this->setPhpName('ArTelephonePrefix');
		$this->setClassname('ArTelephonePrefix');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addColumn('PREFIX', 'Prefix', 'VARCHAR', true, 255, null);
		$this->addColumn('MATCH_ONLY_NUMBERS_WITH_N_DIGITS', 'MatchOnlyNumbersWithNDigits', 'INTEGER', false, null, null);
		$this->addColumn('NAME', 'Name', 'VARCHAR', false, 1024, null);
		$this->addColumn('GEOGRAPHIC_LOCATION', 'GeographicLocation', 'VARCHAR', false, 1024, null);
		$this->addColumn('OPERATOR_TYPE', 'OperatorType', 'VARCHAR', false, 1024, null);
		$this->addColumn('DISPLAY_PRIORITY_LEVEL', 'DisplayPriorityLevel', 'INTEGER', true, null, 0);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
    $this->addRelation('ArCdr', 'ArCdr', RelationMap::ONE_TO_MANY, array('id' => 'ar_telephone_prefix_id', ), null, null);
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

} // ArTelephonePrefixTableMap
