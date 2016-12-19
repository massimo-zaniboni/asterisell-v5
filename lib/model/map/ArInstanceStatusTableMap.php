<?php


/**
 * This class defines the structure of the 'ar_instance_status' table.
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
class ArInstanceStatusTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArInstanceStatusTableMap';

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
		$this->setName('ar_instance_status');
		$this->setPhpName('ArInstanceStatus');
		$this->setClassname('ArInstanceStatus');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addColumn('INTERNAL_NAME', 'InternalName', 'VARCHAR', false, 255, null);
		$this->addColumn('INSTANCE_CODE', 'InstanceCode', 'VARCHAR', false, 1024, null);
		$this->addForeignKey('AR_ORGANIZATION_UNIT_ID', 'ArOrganizationUnitId', 'INTEGER', 'ar_organization_unit', 'ID', false, null, null);
		$this->addColumn('APPLICATION_VERSION', 'ApplicationVersion', 'VARCHAR', false, 1024, null);
		$this->addColumn('NR_OF_CRITICAL_ERRORS', 'NrOfCriticalErrors', 'INTEGER', false, null, null);
		$this->addColumn('NR_OF_IMPORTANT_ERRORS', 'NrOfImportantErrors', 'INTEGER', false, null, null);
		$this->addColumn('NR_OF_WARNING_ERRORS', 'NrOfWarningErrors', 'INTEGER', false, null, null);
		$this->addColumn('NR_OF_EXTENSIONS', 'NrOfExtensions', 'INTEGER', false, null, null);
		$this->addColumn('NR_OF_UNSPECIFIED_EXTENSIONS', 'NrOfUnspecifiedExtensions', 'INTEGER', false, null, null);
		$this->addColumn('PROPERTY_NR_ERRORS_OUTGOING_PREVIOUS_MONTH', 'PropertyNrErrorsOutgoingPreviousMonth', 'INTEGER', false, null, null);
		$this->addColumn('PROPERTY_NR_ERRORS_INCOMING_PREVIOUS_MONTH', 'PropertyNrErrorsIncomingPreviousMonth', 'INTEGER', false, null, null);
		$this->addColumn('PROPERTY_NR_ERRORS_INTERNAL_PREVIOUS_MONTH', 'PropertyNrErrorsInternalPreviousMonth', 'INTEGER', false, null, null);
		$this->addColumn('PROPERTY_NR_ERRORS_OUTGOING_LAST_30_DAYS', 'PropertyNrErrorsOutgoingLast30Days', 'INTEGER', false, null, null);
		$this->addColumn('PROPERTY_NR_ERRORS_INCOMING_LAST_30_DAYS', 'PropertyNrErrorsIncomingLast30Days', 'INTEGER', false, null, null);
		$this->addColumn('PROPERTY_NR_ERRORS_INTERNAL_LAST_30_DAYS', 'PropertyNrErrorsInternalLast30Days', 'INTEGER', false, null, null);
		$this->addColumn('PROPERTY_NR_OUTGOING_PREVIOUS_MONTH', 'PropertyNrOutgoingPreviousMonth', 'INTEGER', false, null, null);
		$this->addColumn('PROPERTY_NR_INCOMING_PREVIOUS_MONTH', 'PropertyNrIncomingPreviousMonth', 'INTEGER', false, null, null);
		$this->addColumn('PROPERTY_NR_INTERNAL_PREVIOUS_MONTH', 'PropertyNrInternalPreviousMonth', 'INTEGER', false, null, null);
		$this->addColumn('PROPERTY_NR_OUTGOING_LAST_30_DAYS', 'PropertyNrOutgoingLast30Days', 'INTEGER', false, null, null);
		$this->addColumn('PROPERTY_NR_INCOMING_LAST_30_DAYS', 'PropertyNrIncomingLast30Days', 'INTEGER', false, null, null);
		$this->addColumn('PROPERTY_NR_INTERNAL_LAST_30_DAYS', 'PropertyNrInternalLast30Days', 'INTEGER', false, null, null);
		$this->addColumn('LAST_PROCESSED_CDR_TIMESTAMP', 'LastProcessedCdrTimestamp', 'TIMESTAMP', false, null, null);
		$this->addColumn('INFO_TIMESTAMP', 'InfoTimestamp', 'TIMESTAMP', false, null, null);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
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

} // ArInstanceStatusTableMap
