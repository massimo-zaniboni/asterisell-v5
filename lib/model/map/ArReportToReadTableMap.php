<?php


/**
 * This class defines the structure of the 'ar_report_to_read' table.
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
class ArReportToReadTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArReportToReadTableMap';

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
		$this->setName('ar_report_to_read');
		$this->setPhpName('ArReportToRead');
		$this->setClassname('ArReportToRead');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addForeignKey('AR_REPORT_ID', 'ArReportId', 'INTEGER', 'ar_report', 'ID', true, null, null);
		$this->addForeignKey('AR_USER_ID', 'ArUserId', 'INTEGER', 'ar_user', 'ID', false, null, null);
		$this->addColumn('SEEN_OR_RECEIVED_FROM_USER', 'SeenOrReceivedFromUser', 'BOOLEAN', true, null, false);
		$this->addColumn('MUST_BE_SENT_TO_EMAIL', 'MustBeSentToEmail', 'BOOLEAN', true, null, false);
		$this->addColumn('SENT_TO_EMAIL_AT_DATE', 'SentToEmailAtDate', 'TIMESTAMP', false, null, null);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
    $this->addRelation('ArReport', 'ArReport', RelationMap::MANY_TO_ONE, array('ar_report_id' => 'id', ), 'CASCADE', null);
    $this->addRelation('ArUser', 'ArUser', RelationMap::MANY_TO_ONE, array('ar_user_id' => 'id', ), 'CASCADE', null);
    $this->addRelation('ArReportToReadUserView', 'ArReportToReadUserView', RelationMap::ONE_TO_MANY, array('id' => 'ar_report_to_read_id', ), null, null);
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

} // ArReportToReadTableMap
