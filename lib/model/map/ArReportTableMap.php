<?php


/**
 * This class defines the structure of the 'ar_report' table.
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
class ArReportTableMap extends TableMap {

	/**
	 * The (dot-path) name of this class
	 */
	const CLASS_NAME = 'lib.model.map.ArReportTableMap';

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
		$this->setName('ar_report');
		$this->setPhpName('ArReport');
		$this->setClassname('ArReport');
		$this->setPackage('lib.model');
		$this->setUseIdGenerator(true);
		// columns
		$this->addPrimaryKey('ID', 'Id', 'INTEGER', true, null, null);
		$this->addColumn('IS_TEMPLATE', 'IsTemplate', 'BOOLEAN', true, null, false);
		$this->addForeignKey('AR_REPORT_SET_ID', 'ArReportSetId', 'INTEGER', 'ar_report_set', 'ID', false, null, null);
		$this->addForeignKey('ABOUT_AR_REPORT_SET_ID', 'AboutArReportSetId', 'INTEGER', 'ar_report_set', 'ID', false, null, null);
		$this->addForeignKey('AR_ORGANIZATION_UNIT_ID', 'ArOrganizationUnitId', 'INTEGER', 'ar_organization_unit', 'ID', false, null, null);
		$this->addForeignKey('AR_USER_ID', 'ArUserId', 'INTEGER', 'ar_user', 'ID', false, null, null);
		$this->addForeignKey('AR_VENDOR_ID', 'ArVendorId', 'INTEGER', 'ar_vendor', 'ID', false, null, null);
		$this->addForeignKey('AR_TAG_ID', 'ArTagId', 'INTEGER', 'ar_tag', 'ID', false, null, null);
		$this->addColumn('FROM_DATE', 'FromDate', 'TIMESTAMP', false, null, null);
		$this->addColumn('TO_DATE', 'ToDate', 'TIMESTAMP', false, null, null);
		$this->addColumn('PARAM_SHOW_MASKED_TELEPHONE_NUMBERS', 'ParamShowMaskedTelephoneNumbers', 'BOOLEAN', true, null, true);
		$this->addColumn('PARAM_SHOW_CALL_COST', 'ParamShowCallCost', 'BOOLEAN', true, null, false);
		$this->addColumn('PARAM_SHOW_CALL_INCOME', 'ParamShowCallIncome', 'BOOLEAN', true, null, false);
		$this->addColumn('PARAM_SHOW_ALSO_OUTGOING_CALLS', 'ParamShowAlsoOutgoingCalls', 'BOOLEAN', true, null, false);
		$this->addColumn('PARAM_SHOW_ALSO_SYSTEM_CALLS', 'ParamShowAlsoSystemCalls', 'BOOLEAN', true, null, false);
		$this->addColumn('PARAM_SHOW_ALSO_INCOMING_CALLS', 'ParamShowAlsoIncomingCalls', 'BOOLEAN', true, null, false);
		$this->addColumn('PARAM_SHOW_ALSO_INTERNAL_CALLS', 'ParamShowAlsoInternalCalls', 'BOOLEAN', true, null, false);
		$this->addColumn('PARAM_SHOW_CALL_DETAILS', 'ParamShowCallDetails', 'BOOLEAN', true, null, false);
		$this->addColumn('PARAM_SHOW_VOIP_PROVIDER', 'ParamShowVoipProvider', 'BOOLEAN', true, null, false);
		$this->addColumn('PARAM_SHOW_COMMUNICATION_CHANNEL', 'ParamShowCommunicationChannel', 'BOOLEAN', true, null, false);
		$this->addColumn('PARAM_SHOW_GEOGRAPHIC_LOCATION', 'ParamShowGeographicLocation', 'BOOLEAN', true, null, false);
		$this->addColumn('PARAM_SHOW_CONNECTION_TYPE', 'ParamShowConnectionType', 'BOOLEAN', true, null, false);
		$this->addColumn('PARAM_SHOW_COST_SAVING', 'ParamShowCostSaving', 'BOOLEAN', true, null, false);
		$this->addColumn('PARAM_IS_LEGAL', 'ParamIsLegal', 'BOOLEAN', true, null, false);
		$this->addColumn('PARAM_EXPAND_TO_LEVEL', 'ParamExpandToLevel', 'INTEGER', true, 4, 0);
		$this->addForeignKey('AR_REPORT_ORDER_OF_CHILDREN_ID', 'ArReportOrderOfChildrenId', 'INTEGER', 'ar_report_order_of_children', 'ID', false, null, null);
		$this->addColumn('PHP_CLASS_NAME', 'PhpClassName', 'VARCHAR', false, 1024, null);
		$this->addColumn('PRODUCED_REPORT_GENERATION_DATE', 'ProducedReportGenerationDate', 'TIMESTAMP', false, null, null);
		$this->addColumn('REPORT_NAME', 'ReportName', 'VARCHAR', false, 1024, null);
		$this->addColumn('PRODUCED_REPORT_SHORT_DESCRIPTION', 'ProducedReportShortDescription', 'VARCHAR', false, 512, null);
		$this->addColumn('PRODUCED_REPORT_ADDITIONAL_DESCRIPTION', 'ProducedReportAdditionalDescription', 'VARCHAR', false, 1024, null);
		$this->addColumn('PRODUCED_REPORT_ALREADY_REVIEWED', 'ProducedReportAlreadyReviewed', 'BOOLEAN', true, null, false);
		$this->addColumn('PRODUCED_REPORT_IS_DRAFT', 'ProducedReportIsDraft', 'BOOLEAN', true, null, false);
		$this->addColumn('PRODUCED_REPORT_MUST_BE_REGENERATED', 'ProducedReportMustBeRegenerated', 'BOOLEAN', true, null, false);
		$this->addColumn('PRODUCED_REPORT_MIME_TYPE', 'ProducedReportMimeType', 'VARCHAR', true, 128, 'application/pdf');
		$this->addColumn('PRODUCED_REPORT_FILE_TYPE_SUFFIX', 'ProducedReportFileTypeSuffix', 'VARCHAR', true, 25, 'pdf');
		$this->addColumn('PRODUCED_REPORT_DOCUMENT', 'ProducedReportDocument', 'BLOB', false, null, null);
		$this->addColumn('PRODUCED_REPORT_DOCUMENT_CHECKSUM', 'ProducedReportDocumentChecksum', 'VARCHAR', false, 1024, null);
		$this->addColumn('REPORT_MAIL_SUBJECT', 'ReportMailSubject', 'VARCHAR', false, 512, null);
		$this->addColumn('REPORT_MAIL_BODY', 'ReportMailBody', 'VARCHAR', false, 2048, null);
		$this->addColumn('REPORT_ATTACHMENT_FILE_NAME', 'ReportAttachmentFileName', 'VARCHAR', false, 255, null);
		$this->addColumn('REPORT_ATTACHMENT_FILE_NAME_ADD_REPORT_DATE', 'ReportAttachmentFileNameAddReportDate', 'BOOLEAN', true, null, false);
		$this->addColumn('INTERNAL_NAME', 'InternalName', 'VARCHAR', false, 512, null);
		$this->addColumn('CACHED_PARENT_ID_HIERARCHY', 'CachedParentIdHierarchy', 'VARBINARY', false, 850, null);
		$this->addColumn('LEGAL_NR_PREFIX', 'LegalNrPrefix', 'VARCHAR', true, 80, '');
		$this->addColumn('LEGAL_CONSECUTIVE_NR', 'LegalConsecutiveNr', 'INTEGER', false, null, null);
		$this->addColumn('LEGAL_DATE', 'LegalDate', 'DATE', false, null, null);
		$this->addColumn('LEGAL_SENDER_NAME', 'LegalSenderName', 'VARCHAR', false, 255, null);
		$this->addColumn('LEGAL_SENDER_VAT', 'LegalSenderVat', 'VARCHAR', false, 255, null);
		$this->addColumn('LEGAL_SENDER_ADDRESS', 'LegalSenderAddress', 'VARCHAR', false, 1024, null);
		$this->addColumn('LEGAL_RECEIVER_NAME', 'LegalReceiverName', 'VARCHAR', false, 255, null);
		$this->addColumn('LEGAL_RECEIVER_VAT', 'LegalReceiverVat', 'VARCHAR', false, 255, null);
		$this->addColumn('LEGAL_RECEIVER_ADDRESS', 'LegalReceiverAddress', 'VARCHAR', false, 1024, null);
		$this->addColumn('TOTAL_WITHOUT_TAX', 'TotalWithoutTax', 'BIGINT', true, null, 0);
		$this->addColumn('TAX', 'Tax', 'BIGINT', true, null, 0);
		$this->addColumn('APPLIED_VAT', 'AppliedVat', 'BIGINT', true, null, 0);
		$this->addColumn('TOTAL_WITH_TAX', 'TotalWithTax', 'BIGINT', true, null, 0);
		// validators
	} // initialize()

	/**
	 * Build the RelationMap objects for this table relationships
	 */
	public function buildRelations()
	{
    $this->addRelation('ArReportSetRelatedByArReportSetId', 'ArReportSet', RelationMap::MANY_TO_ONE, array('ar_report_set_id' => 'id', ), 'CASCADE', null);
    $this->addRelation('ArReportSetRelatedByAboutArReportSetId', 'ArReportSet', RelationMap::MANY_TO_ONE, array('about_ar_report_set_id' => 'id', ), 'CASCADE', null);
    $this->addRelation('ArOrganizationUnit', 'ArOrganizationUnit', RelationMap::MANY_TO_ONE, array('ar_organization_unit_id' => 'id', ), null, null);
    $this->addRelation('ArUser', 'ArUser', RelationMap::MANY_TO_ONE, array('ar_user_id' => 'id', ), null, null);
    $this->addRelation('ArVendor', 'ArVendor', RelationMap::MANY_TO_ONE, array('ar_vendor_id' => 'id', ), null, null);
    $this->addRelation('ArTag', 'ArTag', RelationMap::MANY_TO_ONE, array('ar_tag_id' => 'id', ), null, null);
    $this->addRelation('ArReportOrderOfChildren', 'ArReportOrderOfChildren', RelationMap::MANY_TO_ONE, array('ar_report_order_of_children_id' => 'id', ), null, null);
    $this->addRelation('ArReportAlsoFor', 'ArReportAlsoFor', RelationMap::ONE_TO_MANY, array('id' => 'ar_report_id', ), 'CASCADE', null);
    $this->addRelation('ArReportScheduler', 'ArReportScheduler', RelationMap::ONE_TO_MANY, array('id' => 'ar_report_id', ), null, null);
    $this->addRelation('ArReportToRead', 'ArReportToRead', RelationMap::ONE_TO_MANY, array('id' => 'ar_report_id', ), 'CASCADE', null);
    $this->addRelation('ArReportToReadUserView', 'ArReportToReadUserView', RelationMap::ONE_TO_MANY, array('id' => 'ar_report_id', ), null, null);
    $this->addRelation('ArUserCanViewReport', 'ArUserCanViewReport', RelationMap::ONE_TO_MANY, array('id' => 'ar_report_id', ), 'CASCADE', null);
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

} // ArReportTableMap
