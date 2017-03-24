<?php


/**
 * This class defines the structure of the 'ar_postponed_report_tmp' table.
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
class ArPostponedReportTmpTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArPostponedReportTmpTableMap';

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
		$this->setName('ar_postponed_report_tmp');
		$this->setPhpName('ArPostponedReportTmp');
		$this->setClassname('ArPostponedReportTmp');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(false);
		// columns
		$this->addForeignPrimaryKey('AR_ORGANIZATION_UNIT_ID', 'ArOrganizationUnitId', 'INTEGER' , 'ar_organization_unit', 'ID', true, null, null);
		$this->addColumn('FROM_DATE', 'FromDate', 'TIMESTAMP', true, null, null);
		$this->addColumn('IS_BILLED', 'IsBilled', 'BOOLEAN', true, null, null);
		$this->addColumn('IS_PROCESSED', 'IsProcessed', 'BOOLEAN', true, null, null);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
    $this->addRelation('ArOrganizationUnit', 'ArOrganizationUnit', RelationMap::MANY_TO_ONE, array('ar_organization_unit_id' => 'id', ), 'CASCADE', null);
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

} // ArPostponedReportTmpTableMap
