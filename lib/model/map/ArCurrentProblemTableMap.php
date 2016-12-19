<?php


/**
 * This class defines the structure of the 'ar_current_problem' table.
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
class ArCurrentProblemTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArCurrentProblemTableMap';

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
		$this->setName('ar_current_problem');
		$this->setPhpName('ArCurrentProblem');
		$this->setClassname('ArCurrentProblem');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(false);
		// columns
		$this->addPrimaryKey('DUPLICATION_KEY', 'DuplicationKey', 'VARCHAR', true, 255, null);
		$this->addForeignKey('AR_PROBLEM_TYPE_ID', 'ArProblemTypeId', 'INTEGER', 'ar_problem_type', 'ID', false, null, null);
		$this->addForeignKey('AR_PROBLEM_DOMAIN_ID', 'ArProblemDomainId', 'INTEGER', 'ar_problem_domain', 'ID', false, null, null);
		$this->addForeignKey('AR_PROBLEM_RESPONSIBLE_ID', 'ArProblemResponsibleId', 'INTEGER', 'ar_problem_responsible', 'ID', false, null, null);
		$this->addColumn('CREATED_AT', 'CreatedAt', 'TIMESTAMP', false, null, null);
		$this->addColumn('GARBAGE_COLLECTION_KEY', 'GarbageCollectionKey', 'VARCHAR', false, 1024, null);
		$this->addColumn('GARBAGE_COLLECTION_FROM', 'GarbageCollectionFrom', 'TIMESTAMP', false, null, null);
		$this->addColumn('GARBAGE_COLLECTION_TO', 'GarbageCollectionTo', 'TIMESTAMP', false, null, null);
		$this->addColumn('DESCRIPTION', 'Description', 'LONGVARCHAR', false, null, null);
		$this->addColumn('EFFECT', 'Effect', 'LONGVARCHAR', false, null, null);
		$this->addColumn('PROPOSED_SOLUTION', 'ProposedSolution', 'LONGVARCHAR', false, null, null);
		$this->addColumn('SIGNALED_TO_ADMIN', 'SignaledToAdmin', 'BOOLEAN', true, null, false);
		$this->addColumn('COUNT_OF_CDRS', 'CountOfCdrs', 'INTEGER', true, null, 0);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
    $this->addRelation('ArProblemType', 'ArProblemType', RelationMap::MANY_TO_ONE, array('ar_problem_type_id' => 'id', ), null, null);
    $this->addRelation('ArProblemDomain', 'ArProblemDomain', RelationMap::MANY_TO_ONE, array('ar_problem_domain_id' => 'id', ), null, null);
    $this->addRelation('ArProblemResponsible', 'ArProblemResponsible', RelationMap::MANY_TO_ONE, array('ar_problem_responsible_id' => 'id', ), null, null);
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
			'symfony_timestampable' => array('create_column' => 'created_at', ),
		);
	} // getBehaviors()

} // ArCurrentProblemTableMap
