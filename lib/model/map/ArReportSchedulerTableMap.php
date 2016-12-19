<?php


/**
 * This class defines the structure of the 'ar_report_scheduler' table.
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
class ArReportSchedulerTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArReportSchedulerTableMap';

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
		$this->setName('ar_report_scheduler');
		$this->setPhpName('ArReportScheduler');
		$this->setClassname('ArReportScheduler');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addColumn('IS_ACTIVE', 'IsActive', 'BOOLEAN', false, null, null);
		$this->addColumn('LAST_EXECUTION_DATE', 'LastExecutionDate', 'TIMESTAMP', false, null, null);
		$this->addColumn('LAST_FROM_DATE', 'LastFromDate', 'TIMESTAMP', false, null, null);
		$this->addColumn('LAST_TO_DATE', 'LastToDate', 'TIMESTAMP', false, null, null);
		$this->addForeignKey('AR_REPORT_ID', 'ArReportId', 'INTEGER', 'ar_report', 'ID', false, null, null);
		$this->addForeignKey('AR_ORGANIZATION_UNIT_ID', 'ArOrganizationUnitId', 'INTEGER', 'ar_organization_unit', 'ID', false, null, null);
		$this->addColumn('SHORT_DESCRIPTION', 'ShortDescription', 'VARCHAR', false, 2048, null);
		$this->addColumn('ADDITIONAL_DESCRIPTION', 'AdditionalDescription', 'VARCHAR', false, 2048, null);
		$this->addColumn('NOTE', 'Note', 'VARCHAR', false, 2048, null);
		$this->addColumn('PRODUCED_REPORT_MUST_BE_REVIEWED', 'ProducedReportMustBeReviewed', 'BOOLEAN', true, null, true);
		$this->addForeignKey('AR_REPORT_GENERATION_ID', 'ArReportGenerationId', 'INTEGER', 'ar_report_generation', 'ID', false, null, null);
		$this->addColumn('SCHEDULE_EVERY_X_DAYS', 'ScheduleEveryXDays', 'INTEGER', false, null, null);
		$this->addColumn('SCHEDULE_EVERY_X_MONTHS', 'ScheduleEveryXMonths', 'INTEGER', false, null, null);
		$this->addColumn('START_GENERATION_AFTER_X_HOURS', 'StartGenerationAfterXHours', 'INTEGER', true, null, 2);
		$this->addColumn('INTERNAL_NAME', 'InternalName', 'VARCHAR', false, 512, null);
		$this->addColumn('AR_LEGAL_DATE_GENERATION_METHOD_ID', 'ArLegalDateGenerationMethodId', 'INTEGER', false, null, null);
		$this->addColumn('DAYS_TO_ADD_TO_LEGAL_DATE_GENERATION_METHOD', 'DaysToAddToLegalDateGenerationMethod', 'INTEGER', false, null, null);
		$this->addColumn('IS_YEARLY_LEGAL_NUMERATION', 'IsYearlyLegalNumeration', 'BOOLEAN', false, null, null);
		$this->addColumn('GENERATE_ONLY_IF_THERE_IS_COST', 'GenerateOnlyIfThereIsCost', 'BOOLEAN', true, null, false);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
    $this->addRelation('ArReport', 'ArReport', RelationMap::MANY_TO_ONE, array('ar_report_id' => 'id', ), null, null);
    $this->addRelation('ArOrganizationUnit', 'ArOrganizationUnit', RelationMap::MANY_TO_ONE, array('ar_organization_unit_id' => 'id', ), null, null);
    $this->addRelation('ArReportGeneration', 'ArReportGeneration', RelationMap::MANY_TO_ONE, array('ar_report_generation_id' => 'id', ), null, null);
    $this->addRelation('ArReportSet', 'ArReportSet', RelationMap::ONE_TO_MANY, array('id' => 'ar_report_scheduler_id', ), null, null);
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

} // ArReportSchedulerTableMap
