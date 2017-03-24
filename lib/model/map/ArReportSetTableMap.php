<?php


/**
 * This class defines the structure of the 'ar_report_set' table.
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
class ArReportSetTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArReportSetTableMap';

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
		$this->setName('ar_report_set');
		$this->setPhpName('ArReportSet');
		$this->setClassname('ArReportSet');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addForeignKey('AR_REPORT_SCHEDULER_ID', 'ArReportSchedulerId', 'INTEGER', 'ar_report_scheduler', 'ID', false, null, null);
		$this->addColumn('FROM_DATE', 'FromDate', 'TIMESTAMP', true, null, null);
		$this->addColumn('TO_DATE', 'ToDate', 'TIMESTAMP', true, null, null);
		$this->addColumn('MUST_BE_REVIEWED', 'MustBeReviewed', 'BOOLEAN', true, null, true);
		$this->addColumn('POSTPONED_FIELDS_ARE_UPDATED', 'PostponedFieldsAreUpdated', 'BOOLEAN', true, null, true);
		$this->addColumn('POSTPONED_REPORTS', 'PostponedReports', 'INTEGER', true, null, 0);
		$this->addColumn('POSTPONED_AMOUNT', 'PostponedAmount', 'BIGINT', true, null, 0);
		$this->addColumn('REPORTS', 'Reports', 'INTEGER', true, null, 0);
		$this->addColumn('AMOUNT', 'Amount', 'BIGINT', true, null, 0);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
    $this->addRelation('ArReportScheduler', 'ArReportScheduler', RelationMap::MANY_TO_ONE, array('ar_report_scheduler_id' => 'id', ), null, null);
    $this->addRelation('ArReportRelatedByArReportSetId', 'ArReport', RelationMap::ONE_TO_MANY, array('id' => 'ar_report_set_id', ), 'CASCADE', null);
    $this->addRelation('ArReportRelatedByAboutArReportSetId', 'ArReport', RelationMap::ONE_TO_MANY, array('id' => 'about_ar_report_set_id', ), 'CASCADE', null);
    $this->addRelation('ArPostponedReport', 'ArPostponedReport', RelationMap::ONE_TO_MANY, array('id' => 'ar_report_set_id', ), 'CASCADE', null);
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

} // ArReportSetTableMap
