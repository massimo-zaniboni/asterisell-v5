<?php


/**
 * This class defines the structure of the 'ar_report_to_read_user_view' table.
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
class ArReportToReadUserViewTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArReportToReadUserViewTableMap';

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
		$this->setName('ar_report_to_read_user_view');
		$this->setPhpName('ArReportToReadUserView');
		$this->setClassname('ArReportToReadUserView');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addForeignKey('AR_REPORT_TO_READ_ID', 'ArReportToReadId', 'INTEGER', 'ar_report_to_read', 'ID', false, null, null);
		$this->addForeignKey('AR_REPORT_ID', 'ArReportId', 'INTEGER', 'ar_report', 'ID', false, null, null);
		$this->addForeignKey('AR_USER_ID', 'ArUserId', 'INTEGER', 'ar_user', 'ID', false, null, null);
		$this->addColumn('SEEN_OR_RECEIVED_FROM_USER', 'SeenOrReceivedFromUser', 'BOOLEAN', true, null, false);
		$this->addForeignKey('AR_ORGANIZATION_UNIT_ID', 'ArOrganizationUnitId', 'INTEGER', 'ar_organization_unit', 'ID', false, null, null);
		$this->addColumn('FROM_DATE', 'FromDate', 'TIMESTAMP', false, null, null);
		$this->addColumn('TO_DATE', 'ToDate', 'TIMESTAMP', false, null, null);
		$this->addColumn('PRODUCED_REPORT_GENERATION_DATE', 'ProducedReportGenerationDate', 'TIMESTAMP', false, null, null);
		$this->addColumn('PRODUCED_REPORT_SHORT_DESCRIPTION', 'ProducedReportShortDescription', 'VARCHAR', false, 2048, null);
		$this->addColumn('PRODUCED_REPORT_ADDITIONAL_DESCRIPTION', 'ProducedReportAdditionalDescription', 'VARCHAR', false, 2048, null);
		$this->addColumn('PRODUCED_REPORT_ALREADY_REVIEWED', 'ProducedReportAlreadyReviewed', 'BOOLEAN', true, null, false);
		$this->addColumn('PRODUCED_REPORT_IS_DRAFT', 'ProducedReportIsDraft', 'BOOLEAN', true, null, false);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
    $this->addRelation('ArReportToRead', 'ArReportToRead', RelationMap::MANY_TO_ONE, array('ar_report_to_read_id' => 'id', ), null, null);
    $this->addRelation('ArReport', 'ArReport', RelationMap::MANY_TO_ONE, array('ar_report_id' => 'id', ), null, null);
    $this->addRelation('ArUser', 'ArUser', RelationMap::MANY_TO_ONE, array('ar_user_id' => 'id', ), null, null);
    $this->addRelation('ArOrganizationUnit', 'ArOrganizationUnit', RelationMap::MANY_TO_ONE, array('ar_organization_unit_id' => 'id', ), null, null);
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

} // ArReportToReadUserViewTableMap
