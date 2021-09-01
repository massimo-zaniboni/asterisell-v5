<?php


/**
 * This class defines the structure of the 'ar_specific_rate_calc' table.
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
class ArSpecificRateCalcTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArSpecificRateCalcTableMap';

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
		$this->setName('ar_specific_rate_calc');
		$this->setPhpName('ArSpecificRateCalc');
		$this->setClassname('ArSpecificRateCalc');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addColumn('NOTE', 'Note', 'LONGVARCHAR', true, null, null);
		$this->addForeignKey('AR_RATE_ID', 'ArRateId', 'INTEGER', 'ar_rate', 'ID', false, null, null);
		$this->addColumn('SPECIFIC_RATE_NAME', 'SpecificRateName', 'VARCHAR', true, 255, null);
		$this->addColumn('PRICE_CATEGORY_NAME', 'PriceCategoryName', 'VARCHAR', true, 255, null);
		$this->addColumn('MEDIUMTEXT_SPECIFIC_RATE_IN_MATCH_ALL', 'MediumtextSpecificRateInMatchAll', 'LONGVARCHAR', true, null, null);
		$this->addColumn('MEDIUMTEXT_SPECIFIC_RATE_IN_MATCH_EXACT', 'MediumtextSpecificRateInMatchExact', 'LONGVARCHAR', true, null, null);
		$this->addColumn('MEDIUMTEXT_SPECIFIC_RATE_OUT', 'MediumtextSpecificRateOut', 'LONGVARCHAR', true, null, null);
		$this->addColumn('RATE_PLAN_OUT', 'RatePlanOut', 'LONGVARCHAR', true, null, null);
		$this->addColumn('MEDIUMTEXT_BASE_RATE_DIFF', 'MediumtextBaseRateDiff', 'LONGVARCHAR', true, null, null);
		$this->addColumn('CALC_INFO', 'CalcInfo', 'LONGVARCHAR', true, null, null);
		$this->addColumn('CALC_ERROR', 'CalcError', 'LONGVARCHAR', false, null, null);
		$this->addColumn('IS_RECALC', 'IsRecalc', 'BOOLEAN', true, null, false);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
    $this->addRelation('ArRate', 'ArRate', RelationMap::MANY_TO_ONE, array('ar_rate_id' => 'id', ), null, null);
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

} // ArSpecificRateCalcTableMap
