<?php


/**
 * This class defines the structure of the 'ar_user_can_view_report' table.
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
class ArUserCanViewReportTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArUserCanViewReportTableMap';

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
		$this->setName('ar_user_can_view_report');
		$this->setPhpName('ArUserCanViewReport');
		$this->setClassname('ArUserCanViewReport');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(false);
		// columns
		$this->addForeignPrimaryKey('AR_USER_ID', 'ArUserId', 'INTEGER' , 'ar_user', 'ID', true, null, null);
		$this->addForeignPrimaryKey('AR_REPORT_ID', 'ArReportId', 'INTEGER' , 'ar_report', 'ID', true, null, null);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
    $this->addRelation('ArUser', 'ArUser', RelationMap::MANY_TO_ONE, array('ar_user_id' => 'id', ), 'CASCADE', null);
    $this->addRelation('ArReport', 'ArReport', RelationMap::MANY_TO_ONE, array('ar_report_id' => 'id', ), 'CASCADE', null);
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

} // ArUserCanViewReportTableMap
