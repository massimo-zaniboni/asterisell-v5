<?php

/**
 * Base static class for performing query and update operations on the 'ar_report' table.
 *
 * 
 *
 * @package    lib.model.om
 */
abstract class BaseArReportPeer {

	/** the default database name for this class */
	const DATABASE_NAME = 'propel';

	/** the table name for this class */
	const TABLE_NAME = 'ar_report';

	/** the related Propel class for this table */
	const OM_CLASS = 'ArReport';

	/** A class that can be returned by this peer. */
	const CLASS_DEFAULT = 'lib.model.ArReport';

	/** the related TableMap class for this table */
	const TM_CLASS = 'ArReportTableMap';
	
	/** The total number of columns. */
	const NUM_COLUMNS = 57;

	/** The number of lazy-loaded columns. */
	const NUM_LAZY_LOAD_COLUMNS = 0;

	/** the column name for the ID field */
	const ID = 'ar_report.ID';

	/** the column name for the IS_TEMPLATE field */
	const IS_TEMPLATE = 'ar_report.IS_TEMPLATE';

	/** the column name for the AR_REPORT_SET_ID field */
	const AR_REPORT_SET_ID = 'ar_report.AR_REPORT_SET_ID';

	/** the column name for the ABOUT_AR_REPORT_SET_ID field */
	const ABOUT_AR_REPORT_SET_ID = 'ar_report.ABOUT_AR_REPORT_SET_ID';

	/** the column name for the AR_ORGANIZATION_UNIT_ID field */
	const AR_ORGANIZATION_UNIT_ID = 'ar_report.AR_ORGANIZATION_UNIT_ID';

	/** the column name for the AR_USER_ID field */
	const AR_USER_ID = 'ar_report.AR_USER_ID';

	/** the column name for the AR_VENDOR_ID field */
	const AR_VENDOR_ID = 'ar_report.AR_VENDOR_ID';

	/** the column name for the AR_TAG_ID field */
	const AR_TAG_ID = 'ar_report.AR_TAG_ID';

	/** the column name for the FROM_DATE field */
	const FROM_DATE = 'ar_report.FROM_DATE';

	/** the column name for the TO_DATE field */
	const TO_DATE = 'ar_report.TO_DATE';

	/** the column name for the PARAM_SHOW_MASKED_TELEPHONE_NUMBERS field */
	const PARAM_SHOW_MASKED_TELEPHONE_NUMBERS = 'ar_report.PARAM_SHOW_MASKED_TELEPHONE_NUMBERS';

	/** the column name for the PARAM_SHOW_CALL_COST field */
	const PARAM_SHOW_CALL_COST = 'ar_report.PARAM_SHOW_CALL_COST';

	/** the column name for the PARAM_SHOW_CALL_INCOME field */
	const PARAM_SHOW_CALL_INCOME = 'ar_report.PARAM_SHOW_CALL_INCOME';

	/** the column name for the PARAM_SHOW_ALSO_OUTGOING_CALLS field */
	const PARAM_SHOW_ALSO_OUTGOING_CALLS = 'ar_report.PARAM_SHOW_ALSO_OUTGOING_CALLS';

	/** the column name for the PARAM_SHOW_ALSO_SYSTEM_CALLS field */
	const PARAM_SHOW_ALSO_SYSTEM_CALLS = 'ar_report.PARAM_SHOW_ALSO_SYSTEM_CALLS';

	/** the column name for the PARAM_SHOW_ALSO_INCOMING_CALLS field */
	const PARAM_SHOW_ALSO_INCOMING_CALLS = 'ar_report.PARAM_SHOW_ALSO_INCOMING_CALLS';

	/** the column name for the PARAM_SHOW_ALSO_INTERNAL_CALLS field */
	const PARAM_SHOW_ALSO_INTERNAL_CALLS = 'ar_report.PARAM_SHOW_ALSO_INTERNAL_CALLS';

	/** the column name for the PARAM_SHOW_CALL_DETAILS field */
	const PARAM_SHOW_CALL_DETAILS = 'ar_report.PARAM_SHOW_CALL_DETAILS';

	/** the column name for the PARAM_SHOW_VOIP_PROVIDER field */
	const PARAM_SHOW_VOIP_PROVIDER = 'ar_report.PARAM_SHOW_VOIP_PROVIDER';

	/** the column name for the PARAM_SHOW_COMMUNICATION_CHANNEL field */
	const PARAM_SHOW_COMMUNICATION_CHANNEL = 'ar_report.PARAM_SHOW_COMMUNICATION_CHANNEL';

	/** the column name for the PARAM_SHOW_GEOGRAPHIC_LOCATION field */
	const PARAM_SHOW_GEOGRAPHIC_LOCATION = 'ar_report.PARAM_SHOW_GEOGRAPHIC_LOCATION';

	/** the column name for the PARAM_SHOW_CONNECTION_TYPE field */
	const PARAM_SHOW_CONNECTION_TYPE = 'ar_report.PARAM_SHOW_CONNECTION_TYPE';

	/** the column name for the PARAM_SHOW_COST_SAVING field */
	const PARAM_SHOW_COST_SAVING = 'ar_report.PARAM_SHOW_COST_SAVING';

	/** the column name for the PARAM_IS_LEGAL field */
	const PARAM_IS_LEGAL = 'ar_report.PARAM_IS_LEGAL';

	/** the column name for the PARAM_EXPAND_TO_LEVEL field */
	const PARAM_EXPAND_TO_LEVEL = 'ar_report.PARAM_EXPAND_TO_LEVEL';

	/** the column name for the AR_REPORT_ORDER_OF_CHILDREN_ID field */
	const AR_REPORT_ORDER_OF_CHILDREN_ID = 'ar_report.AR_REPORT_ORDER_OF_CHILDREN_ID';

	/** the column name for the PHP_CLASS_NAME field */
	const PHP_CLASS_NAME = 'ar_report.PHP_CLASS_NAME';

	/** the column name for the PRODUCED_REPORT_GENERATION_DATE field */
	const PRODUCED_REPORT_GENERATION_DATE = 'ar_report.PRODUCED_REPORT_GENERATION_DATE';

	/** the column name for the REPORT_NAME field */
	const REPORT_NAME = 'ar_report.REPORT_NAME';

	/** the column name for the PRODUCED_REPORT_SHORT_DESCRIPTION field */
	const PRODUCED_REPORT_SHORT_DESCRIPTION = 'ar_report.PRODUCED_REPORT_SHORT_DESCRIPTION';

	/** the column name for the PRODUCED_REPORT_ADDITIONAL_DESCRIPTION field */
	const PRODUCED_REPORT_ADDITIONAL_DESCRIPTION = 'ar_report.PRODUCED_REPORT_ADDITIONAL_DESCRIPTION';

	/** the column name for the PRODUCED_REPORT_ALREADY_REVIEWED field */
	const PRODUCED_REPORT_ALREADY_REVIEWED = 'ar_report.PRODUCED_REPORT_ALREADY_REVIEWED';

	/** the column name for the PRODUCED_REPORT_IS_DRAFT field */
	const PRODUCED_REPORT_IS_DRAFT = 'ar_report.PRODUCED_REPORT_IS_DRAFT';

	/** the column name for the PRODUCED_REPORT_MUST_BE_REGENERATED field */
	const PRODUCED_REPORT_MUST_BE_REGENERATED = 'ar_report.PRODUCED_REPORT_MUST_BE_REGENERATED';

	/** the column name for the PRODUCED_REPORT_MIME_TYPE field */
	const PRODUCED_REPORT_MIME_TYPE = 'ar_report.PRODUCED_REPORT_MIME_TYPE';

	/** the column name for the PRODUCED_REPORT_FILE_TYPE_SUFFIX field */
	const PRODUCED_REPORT_FILE_TYPE_SUFFIX = 'ar_report.PRODUCED_REPORT_FILE_TYPE_SUFFIX';

	/** the column name for the PRODUCED_REPORT_DOCUMENT field */
	const PRODUCED_REPORT_DOCUMENT = 'ar_report.PRODUCED_REPORT_DOCUMENT';

	/** the column name for the PRODUCED_REPORT_DOCUMENT_CHECKSUM field */
	const PRODUCED_REPORT_DOCUMENT_CHECKSUM = 'ar_report.PRODUCED_REPORT_DOCUMENT_CHECKSUM';

	/** the column name for the REPORT_MAIL_SUBJECT field */
	const REPORT_MAIL_SUBJECT = 'ar_report.REPORT_MAIL_SUBJECT';

	/** the column name for the REPORT_MAIL_BODY field */
	const REPORT_MAIL_BODY = 'ar_report.REPORT_MAIL_BODY';

	/** the column name for the REPORT_ATTACHMENT_FILE_NAME field */
	const REPORT_ATTACHMENT_FILE_NAME = 'ar_report.REPORT_ATTACHMENT_FILE_NAME';

	/** the column name for the REPORT_ATTACHMENT_FILE_NAME_ADD_REPORT_DATE field */
	const REPORT_ATTACHMENT_FILE_NAME_ADD_REPORT_DATE = 'ar_report.REPORT_ATTACHMENT_FILE_NAME_ADD_REPORT_DATE';

	/** the column name for the INTERNAL_NAME field */
	const INTERNAL_NAME = 'ar_report.INTERNAL_NAME';

	/** the column name for the CACHED_PARENT_ID_HIERARCHY field */
	const CACHED_PARENT_ID_HIERARCHY = 'ar_report.CACHED_PARENT_ID_HIERARCHY';

	/** the column name for the LEGAL_NR_PREFIX field */
	const LEGAL_NR_PREFIX = 'ar_report.LEGAL_NR_PREFIX';

	/** the column name for the LEGAL_CONSECUTIVE_NR field */
	const LEGAL_CONSECUTIVE_NR = 'ar_report.LEGAL_CONSECUTIVE_NR';

	/** the column name for the LEGAL_DATE field */
	const LEGAL_DATE = 'ar_report.LEGAL_DATE';

	/** the column name for the LEGAL_SENDER_NAME field */
	const LEGAL_SENDER_NAME = 'ar_report.LEGAL_SENDER_NAME';

	/** the column name for the LEGAL_SENDER_VAT field */
	const LEGAL_SENDER_VAT = 'ar_report.LEGAL_SENDER_VAT';

	/** the column name for the LEGAL_SENDER_ADDRESS field */
	const LEGAL_SENDER_ADDRESS = 'ar_report.LEGAL_SENDER_ADDRESS';

	/** the column name for the LEGAL_RECEIVER_NAME field */
	const LEGAL_RECEIVER_NAME = 'ar_report.LEGAL_RECEIVER_NAME';

	/** the column name for the LEGAL_RECEIVER_VAT field */
	const LEGAL_RECEIVER_VAT = 'ar_report.LEGAL_RECEIVER_VAT';

	/** the column name for the LEGAL_RECEIVER_ADDRESS field */
	const LEGAL_RECEIVER_ADDRESS = 'ar_report.LEGAL_RECEIVER_ADDRESS';

	/** the column name for the TOTAL_WITHOUT_TAX field */
	const TOTAL_WITHOUT_TAX = 'ar_report.TOTAL_WITHOUT_TAX';

	/** the column name for the TAX field */
	const TAX = 'ar_report.TAX';

	/** the column name for the APPLIED_VAT field */
	const APPLIED_VAT = 'ar_report.APPLIED_VAT';

	/** the column name for the TOTAL_WITH_TAX field */
	const TOTAL_WITH_TAX = 'ar_report.TOTAL_WITH_TAX';

	/**
	 * An identiy map to hold any loaded instances of ArReport objects.
	 * This must be public so that other peer classes can access this when hydrating from JOIN
	 * queries.
	 * @var        array ArReport[]
	 */
	public static $instances = array();


	// symfony behavior
	
	/**
	 * Indicates whether the current model includes I18N.
	 */
	const IS_I18N = false;

	/**
	 * holds an array of fieldnames
	 *
	 * first dimension keys are the type constants
	 * e.g. self::$fieldNames[self::TYPE_PHPNAME][0] = 'Id'
	 */
	private static $fieldNames = array (
		BasePeer::TYPE_PHPNAME => array ('Id', 'IsTemplate', 'ArReportSetId', 'AboutArReportSetId', 'ArOrganizationUnitId', 'ArUserId', 'ArVendorId', 'ArTagId', 'FromDate', 'ToDate', 'ParamShowMaskedTelephoneNumbers', 'ParamShowCallCost', 'ParamShowCallIncome', 'ParamShowAlsoOutgoingCalls', 'ParamShowAlsoSystemCalls', 'ParamShowAlsoIncomingCalls', 'ParamShowAlsoInternalCalls', 'ParamShowCallDetails', 'ParamShowVoipProvider', 'ParamShowCommunicationChannel', 'ParamShowGeographicLocation', 'ParamShowConnectionType', 'ParamShowCostSaving', 'ParamIsLegal', 'ParamExpandToLevel', 'ArReportOrderOfChildrenId', 'PhpClassName', 'ProducedReportGenerationDate', 'ReportName', 'ProducedReportShortDescription', 'ProducedReportAdditionalDescription', 'ProducedReportAlreadyReviewed', 'ProducedReportIsDraft', 'ProducedReportMustBeRegenerated', 'ProducedReportMimeType', 'ProducedReportFileTypeSuffix', 'ProducedReportDocument', 'ProducedReportDocumentChecksum', 'ReportMailSubject', 'ReportMailBody', 'ReportAttachmentFileName', 'ReportAttachmentFileNameAddReportDate', 'InternalName', 'CachedParentIdHierarchy', 'LegalNrPrefix', 'LegalConsecutiveNr', 'LegalDate', 'LegalSenderName', 'LegalSenderVat', 'LegalSenderAddress', 'LegalReceiverName', 'LegalReceiverVat', 'LegalReceiverAddress', 'TotalWithoutTax', 'Tax', 'AppliedVat', 'TotalWithTax', ),
		BasePeer::TYPE_STUDLYPHPNAME => array ('id', 'isTemplate', 'arReportSetId', 'aboutArReportSetId', 'arOrganizationUnitId', 'arUserId', 'arVendorId', 'arTagId', 'fromDate', 'toDate', 'paramShowMaskedTelephoneNumbers', 'paramShowCallCost', 'paramShowCallIncome', 'paramShowAlsoOutgoingCalls', 'paramShowAlsoSystemCalls', 'paramShowAlsoIncomingCalls', 'paramShowAlsoInternalCalls', 'paramShowCallDetails', 'paramShowVoipProvider', 'paramShowCommunicationChannel', 'paramShowGeographicLocation', 'paramShowConnectionType', 'paramShowCostSaving', 'paramIsLegal', 'paramExpandToLevel', 'arReportOrderOfChildrenId', 'phpClassName', 'producedReportGenerationDate', 'reportName', 'producedReportShortDescription', 'producedReportAdditionalDescription', 'producedReportAlreadyReviewed', 'producedReportIsDraft', 'producedReportMustBeRegenerated', 'producedReportMimeType', 'producedReportFileTypeSuffix', 'producedReportDocument', 'producedReportDocumentChecksum', 'reportMailSubject', 'reportMailBody', 'reportAttachmentFileName', 'reportAttachmentFileNameAddReportDate', 'internalName', 'cachedParentIdHierarchy', 'legalNrPrefix', 'legalConsecutiveNr', 'legalDate', 'legalSenderName', 'legalSenderVat', 'legalSenderAddress', 'legalReceiverName', 'legalReceiverVat', 'legalReceiverAddress', 'totalWithoutTax', 'tax', 'appliedVat', 'totalWithTax', ),
		BasePeer::TYPE_COLNAME => array (self::ID, self::IS_TEMPLATE, self::AR_REPORT_SET_ID, self::ABOUT_AR_REPORT_SET_ID, self::AR_ORGANIZATION_UNIT_ID, self::AR_USER_ID, self::AR_VENDOR_ID, self::AR_TAG_ID, self::FROM_DATE, self::TO_DATE, self::PARAM_SHOW_MASKED_TELEPHONE_NUMBERS, self::PARAM_SHOW_CALL_COST, self::PARAM_SHOW_CALL_INCOME, self::PARAM_SHOW_ALSO_OUTGOING_CALLS, self::PARAM_SHOW_ALSO_SYSTEM_CALLS, self::PARAM_SHOW_ALSO_INCOMING_CALLS, self::PARAM_SHOW_ALSO_INTERNAL_CALLS, self::PARAM_SHOW_CALL_DETAILS, self::PARAM_SHOW_VOIP_PROVIDER, self::PARAM_SHOW_COMMUNICATION_CHANNEL, self::PARAM_SHOW_GEOGRAPHIC_LOCATION, self::PARAM_SHOW_CONNECTION_TYPE, self::PARAM_SHOW_COST_SAVING, self::PARAM_IS_LEGAL, self::PARAM_EXPAND_TO_LEVEL, self::AR_REPORT_ORDER_OF_CHILDREN_ID, self::PHP_CLASS_NAME, self::PRODUCED_REPORT_GENERATION_DATE, self::REPORT_NAME, self::PRODUCED_REPORT_SHORT_DESCRIPTION, self::PRODUCED_REPORT_ADDITIONAL_DESCRIPTION, self::PRODUCED_REPORT_ALREADY_REVIEWED, self::PRODUCED_REPORT_IS_DRAFT, self::PRODUCED_REPORT_MUST_BE_REGENERATED, self::PRODUCED_REPORT_MIME_TYPE, self::PRODUCED_REPORT_FILE_TYPE_SUFFIX, self::PRODUCED_REPORT_DOCUMENT, self::PRODUCED_REPORT_DOCUMENT_CHECKSUM, self::REPORT_MAIL_SUBJECT, self::REPORT_MAIL_BODY, self::REPORT_ATTACHMENT_FILE_NAME, self::REPORT_ATTACHMENT_FILE_NAME_ADD_REPORT_DATE, self::INTERNAL_NAME, self::CACHED_PARENT_ID_HIERARCHY, self::LEGAL_NR_PREFIX, self::LEGAL_CONSECUTIVE_NR, self::LEGAL_DATE, self::LEGAL_SENDER_NAME, self::LEGAL_SENDER_VAT, self::LEGAL_SENDER_ADDRESS, self::LEGAL_RECEIVER_NAME, self::LEGAL_RECEIVER_VAT, self::LEGAL_RECEIVER_ADDRESS, self::TOTAL_WITHOUT_TAX, self::TAX, self::APPLIED_VAT, self::TOTAL_WITH_TAX, ),
		BasePeer::TYPE_FIELDNAME => array ('id', 'is_template', 'ar_report_set_id', 'about_ar_report_set_id', 'ar_organization_unit_id', 'ar_user_id', 'ar_vendor_id', 'ar_tag_id', 'from_date', 'to_date', 'param_show_masked_telephone_numbers', 'param_show_call_cost', 'param_show_call_income', 'param_show_also_outgoing_calls', 'param_show_also_system_calls', 'param_show_also_incoming_calls', 'param_show_also_internal_calls', 'param_show_call_details', 'param_show_voip_provider', 'param_show_communication_channel', 'param_show_geographic_location', 'param_show_connection_type', 'param_show_cost_saving', 'param_is_legal', 'param_expand_to_level', 'ar_report_order_of_children_id', 'php_class_name', 'produced_report_generation_date', 'report_name', 'produced_report_short_description', 'produced_report_additional_description', 'produced_report_already_reviewed', 'produced_report_is_draft', 'produced_report_must_be_regenerated', 'produced_report_mime_type', 'produced_report_file_type_suffix', 'produced_report_document', 'produced_report_document_checksum', 'report_mail_subject', 'report_mail_body', 'report_attachment_file_name', 'report_attachment_file_name_add_report_date', 'internal_name', 'cached_parent_id_hierarchy', 'legal_nr_prefix', 'legal_consecutive_nr', 'legal_date', 'legal_sender_name', 'legal_sender_vat', 'legal_sender_address', 'legal_receiver_name', 'legal_receiver_vat', 'legal_receiver_address', 'total_without_tax', 'tax', 'applied_vat', 'total_with_tax', ),
		BasePeer::TYPE_NUM => array (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, )
	);

	/**
	 * holds an array of keys for quick access to the fieldnames array
	 *
	 * first dimension keys are the type constants
	 * e.g. self::$fieldNames[BasePeer::TYPE_PHPNAME]['Id'] = 0
	 */
	private static $fieldKeys = array (
		BasePeer::TYPE_PHPNAME => array ('Id' => 0, 'IsTemplate' => 1, 'ArReportSetId' => 2, 'AboutArReportSetId' => 3, 'ArOrganizationUnitId' => 4, 'ArUserId' => 5, 'ArVendorId' => 6, 'ArTagId' => 7, 'FromDate' => 8, 'ToDate' => 9, 'ParamShowMaskedTelephoneNumbers' => 10, 'ParamShowCallCost' => 11, 'ParamShowCallIncome' => 12, 'ParamShowAlsoOutgoingCalls' => 13, 'ParamShowAlsoSystemCalls' => 14, 'ParamShowAlsoIncomingCalls' => 15, 'ParamShowAlsoInternalCalls' => 16, 'ParamShowCallDetails' => 17, 'ParamShowVoipProvider' => 18, 'ParamShowCommunicationChannel' => 19, 'ParamShowGeographicLocation' => 20, 'ParamShowConnectionType' => 21, 'ParamShowCostSaving' => 22, 'ParamIsLegal' => 23, 'ParamExpandToLevel' => 24, 'ArReportOrderOfChildrenId' => 25, 'PhpClassName' => 26, 'ProducedReportGenerationDate' => 27, 'ReportName' => 28, 'ProducedReportShortDescription' => 29, 'ProducedReportAdditionalDescription' => 30, 'ProducedReportAlreadyReviewed' => 31, 'ProducedReportIsDraft' => 32, 'ProducedReportMustBeRegenerated' => 33, 'ProducedReportMimeType' => 34, 'ProducedReportFileTypeSuffix' => 35, 'ProducedReportDocument' => 36, 'ProducedReportDocumentChecksum' => 37, 'ReportMailSubject' => 38, 'ReportMailBody' => 39, 'ReportAttachmentFileName' => 40, 'ReportAttachmentFileNameAddReportDate' => 41, 'InternalName' => 42, 'CachedParentIdHierarchy' => 43, 'LegalNrPrefix' => 44, 'LegalConsecutiveNr' => 45, 'LegalDate' => 46, 'LegalSenderName' => 47, 'LegalSenderVat' => 48, 'LegalSenderAddress' => 49, 'LegalReceiverName' => 50, 'LegalReceiverVat' => 51, 'LegalReceiverAddress' => 52, 'TotalWithoutTax' => 53, 'Tax' => 54, 'AppliedVat' => 55, 'TotalWithTax' => 56, ),
		BasePeer::TYPE_STUDLYPHPNAME => array ('id' => 0, 'isTemplate' => 1, 'arReportSetId' => 2, 'aboutArReportSetId' => 3, 'arOrganizationUnitId' => 4, 'arUserId' => 5, 'arVendorId' => 6, 'arTagId' => 7, 'fromDate' => 8, 'toDate' => 9, 'paramShowMaskedTelephoneNumbers' => 10, 'paramShowCallCost' => 11, 'paramShowCallIncome' => 12, 'paramShowAlsoOutgoingCalls' => 13, 'paramShowAlsoSystemCalls' => 14, 'paramShowAlsoIncomingCalls' => 15, 'paramShowAlsoInternalCalls' => 16, 'paramShowCallDetails' => 17, 'paramShowVoipProvider' => 18, 'paramShowCommunicationChannel' => 19, 'paramShowGeographicLocation' => 20, 'paramShowConnectionType' => 21, 'paramShowCostSaving' => 22, 'paramIsLegal' => 23, 'paramExpandToLevel' => 24, 'arReportOrderOfChildrenId' => 25, 'phpClassName' => 26, 'producedReportGenerationDate' => 27, 'reportName' => 28, 'producedReportShortDescription' => 29, 'producedReportAdditionalDescription' => 30, 'producedReportAlreadyReviewed' => 31, 'producedReportIsDraft' => 32, 'producedReportMustBeRegenerated' => 33, 'producedReportMimeType' => 34, 'producedReportFileTypeSuffix' => 35, 'producedReportDocument' => 36, 'producedReportDocumentChecksum' => 37, 'reportMailSubject' => 38, 'reportMailBody' => 39, 'reportAttachmentFileName' => 40, 'reportAttachmentFileNameAddReportDate' => 41, 'internalName' => 42, 'cachedParentIdHierarchy' => 43, 'legalNrPrefix' => 44, 'legalConsecutiveNr' => 45, 'legalDate' => 46, 'legalSenderName' => 47, 'legalSenderVat' => 48, 'legalSenderAddress' => 49, 'legalReceiverName' => 50, 'legalReceiverVat' => 51, 'legalReceiverAddress' => 52, 'totalWithoutTax' => 53, 'tax' => 54, 'appliedVat' => 55, 'totalWithTax' => 56, ),
		BasePeer::TYPE_COLNAME => array (self::ID => 0, self::IS_TEMPLATE => 1, self::AR_REPORT_SET_ID => 2, self::ABOUT_AR_REPORT_SET_ID => 3, self::AR_ORGANIZATION_UNIT_ID => 4, self::AR_USER_ID => 5, self::AR_VENDOR_ID => 6, self::AR_TAG_ID => 7, self::FROM_DATE => 8, self::TO_DATE => 9, self::PARAM_SHOW_MASKED_TELEPHONE_NUMBERS => 10, self::PARAM_SHOW_CALL_COST => 11, self::PARAM_SHOW_CALL_INCOME => 12, self::PARAM_SHOW_ALSO_OUTGOING_CALLS => 13, self::PARAM_SHOW_ALSO_SYSTEM_CALLS => 14, self::PARAM_SHOW_ALSO_INCOMING_CALLS => 15, self::PARAM_SHOW_ALSO_INTERNAL_CALLS => 16, self::PARAM_SHOW_CALL_DETAILS => 17, self::PARAM_SHOW_VOIP_PROVIDER => 18, self::PARAM_SHOW_COMMUNICATION_CHANNEL => 19, self::PARAM_SHOW_GEOGRAPHIC_LOCATION => 20, self::PARAM_SHOW_CONNECTION_TYPE => 21, self::PARAM_SHOW_COST_SAVING => 22, self::PARAM_IS_LEGAL => 23, self::PARAM_EXPAND_TO_LEVEL => 24, self::AR_REPORT_ORDER_OF_CHILDREN_ID => 25, self::PHP_CLASS_NAME => 26, self::PRODUCED_REPORT_GENERATION_DATE => 27, self::REPORT_NAME => 28, self::PRODUCED_REPORT_SHORT_DESCRIPTION => 29, self::PRODUCED_REPORT_ADDITIONAL_DESCRIPTION => 30, self::PRODUCED_REPORT_ALREADY_REVIEWED => 31, self::PRODUCED_REPORT_IS_DRAFT => 32, self::PRODUCED_REPORT_MUST_BE_REGENERATED => 33, self::PRODUCED_REPORT_MIME_TYPE => 34, self::PRODUCED_REPORT_FILE_TYPE_SUFFIX => 35, self::PRODUCED_REPORT_DOCUMENT => 36, self::PRODUCED_REPORT_DOCUMENT_CHECKSUM => 37, self::REPORT_MAIL_SUBJECT => 38, self::REPORT_MAIL_BODY => 39, self::REPORT_ATTACHMENT_FILE_NAME => 40, self::REPORT_ATTACHMENT_FILE_NAME_ADD_REPORT_DATE => 41, self::INTERNAL_NAME => 42, self::CACHED_PARENT_ID_HIERARCHY => 43, self::LEGAL_NR_PREFIX => 44, self::LEGAL_CONSECUTIVE_NR => 45, self::LEGAL_DATE => 46, self::LEGAL_SENDER_NAME => 47, self::LEGAL_SENDER_VAT => 48, self::LEGAL_SENDER_ADDRESS => 49, self::LEGAL_RECEIVER_NAME => 50, self::LEGAL_RECEIVER_VAT => 51, self::LEGAL_RECEIVER_ADDRESS => 52, self::TOTAL_WITHOUT_TAX => 53, self::TAX => 54, self::APPLIED_VAT => 55, self::TOTAL_WITH_TAX => 56, ),
		BasePeer::TYPE_FIELDNAME => array ('id' => 0, 'is_template' => 1, 'ar_report_set_id' => 2, 'about_ar_report_set_id' => 3, 'ar_organization_unit_id' => 4, 'ar_user_id' => 5, 'ar_vendor_id' => 6, 'ar_tag_id' => 7, 'from_date' => 8, 'to_date' => 9, 'param_show_masked_telephone_numbers' => 10, 'param_show_call_cost' => 11, 'param_show_call_income' => 12, 'param_show_also_outgoing_calls' => 13, 'param_show_also_system_calls' => 14, 'param_show_also_incoming_calls' => 15, 'param_show_also_internal_calls' => 16, 'param_show_call_details' => 17, 'param_show_voip_provider' => 18, 'param_show_communication_channel' => 19, 'param_show_geographic_location' => 20, 'param_show_connection_type' => 21, 'param_show_cost_saving' => 22, 'param_is_legal' => 23, 'param_expand_to_level' => 24, 'ar_report_order_of_children_id' => 25, 'php_class_name' => 26, 'produced_report_generation_date' => 27, 'report_name' => 28, 'produced_report_short_description' => 29, 'produced_report_additional_description' => 30, 'produced_report_already_reviewed' => 31, 'produced_report_is_draft' => 32, 'produced_report_must_be_regenerated' => 33, 'produced_report_mime_type' => 34, 'produced_report_file_type_suffix' => 35, 'produced_report_document' => 36, 'produced_report_document_checksum' => 37, 'report_mail_subject' => 38, 'report_mail_body' => 39, 'report_attachment_file_name' => 40, 'report_attachment_file_name_add_report_date' => 41, 'internal_name' => 42, 'cached_parent_id_hierarchy' => 43, 'legal_nr_prefix' => 44, 'legal_consecutive_nr' => 45, 'legal_date' => 46, 'legal_sender_name' => 47, 'legal_sender_vat' => 48, 'legal_sender_address' => 49, 'legal_receiver_name' => 50, 'legal_receiver_vat' => 51, 'legal_receiver_address' => 52, 'total_without_tax' => 53, 'tax' => 54, 'applied_vat' => 55, 'total_with_tax' => 56, ),
		BasePeer::TYPE_NUM => array (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, )
	);

	/**
	 * Translates a fieldname to another type
	 *
	 * @param      string $name field name
	 * @param      string $fromType One of the class type constants BasePeer::TYPE_PHPNAME, BasePeer::TYPE_STUDLYPHPNAME
	 *                         BasePeer::TYPE_COLNAME, BasePeer::TYPE_FIELDNAME, BasePeer::TYPE_NUM
	 * @param      string $toType   One of the class type constants
	 * @return     string translated name of the field.
	 * @throws     PropelException - if the specified name could not be found in the fieldname mappings.
	 */
	static public function translateFieldName($name, $fromType, $toType)
	{
		$toNames = self::getFieldNames($toType);
		$key = isset(self::$fieldKeys[$fromType][$name]) ? self::$fieldKeys[$fromType][$name] : null;
		if ($key === null) {
			throw new PropelException("'$name' could not be found in the field names of type '$fromType'. These are: " . print_r(self::$fieldKeys[$fromType], true));
		}
		return $toNames[$key];
	}

	/**
	 * Returns an array of field names.
	 *
	 * @param      string $type The type of fieldnames to return:
	 *                      One of the class type constants BasePeer::TYPE_PHPNAME, BasePeer::TYPE_STUDLYPHPNAME
	 *                      BasePeer::TYPE_COLNAME, BasePeer::TYPE_FIELDNAME, BasePeer::TYPE_NUM
	 * @return     array A list of field names
	 */

	static public function getFieldNames($type = BasePeer::TYPE_PHPNAME)
	{
		if (!array_key_exists($type, self::$fieldNames)) {
			throw new PropelException('Method getFieldNames() expects the parameter $type to be one of the class constants BasePeer::TYPE_PHPNAME, BasePeer::TYPE_STUDLYPHPNAME, BasePeer::TYPE_COLNAME, BasePeer::TYPE_FIELDNAME, BasePeer::TYPE_NUM. ' . $type . ' was given.');
		}
		return self::$fieldNames[$type];
	}

	/**
	 * Convenience method which changes table.column to alias.column.
	 *
	 * Using this method you can maintain SQL abstraction while using column aliases.
	 * <code>
	 *		$c->addAlias("alias1", TablePeer::TABLE_NAME);
	 *		$c->addJoin(TablePeer::alias("alias1", TablePeer::PRIMARY_KEY_COLUMN), TablePeer::PRIMARY_KEY_COLUMN);
	 * </code>
	 * @param      string $alias The alias for the current table.
	 * @param      string $column The column name for current table. (i.e. ArReportPeer::COLUMN_NAME).
	 * @return     string
	 */
	public static function alias($alias, $column)
	{
		return str_replace(ArReportPeer::TABLE_NAME.'.', $alias.'.', $column);
	}

	/**
	 * Add all the columns needed to create a new object.
	 *
	 * Note: any columns that were marked with lazyLoad="true" in the
	 * XML schema will not be added to the select list and only loaded
	 * on demand.
	 *
	 * @param      criteria object containing the columns to add.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function addSelectColumns(Criteria $criteria)
	{
		$criteria->addSelectColumn(ArReportPeer::ID);
		$criteria->addSelectColumn(ArReportPeer::IS_TEMPLATE);
		$criteria->addSelectColumn(ArReportPeer::AR_REPORT_SET_ID);
		$criteria->addSelectColumn(ArReportPeer::ABOUT_AR_REPORT_SET_ID);
		$criteria->addSelectColumn(ArReportPeer::AR_ORGANIZATION_UNIT_ID);
		$criteria->addSelectColumn(ArReportPeer::AR_USER_ID);
		$criteria->addSelectColumn(ArReportPeer::AR_VENDOR_ID);
		$criteria->addSelectColumn(ArReportPeer::AR_TAG_ID);
		$criteria->addSelectColumn(ArReportPeer::FROM_DATE);
		$criteria->addSelectColumn(ArReportPeer::TO_DATE);
		$criteria->addSelectColumn(ArReportPeer::PARAM_SHOW_MASKED_TELEPHONE_NUMBERS);
		$criteria->addSelectColumn(ArReportPeer::PARAM_SHOW_CALL_COST);
		$criteria->addSelectColumn(ArReportPeer::PARAM_SHOW_CALL_INCOME);
		$criteria->addSelectColumn(ArReportPeer::PARAM_SHOW_ALSO_OUTGOING_CALLS);
		$criteria->addSelectColumn(ArReportPeer::PARAM_SHOW_ALSO_SYSTEM_CALLS);
		$criteria->addSelectColumn(ArReportPeer::PARAM_SHOW_ALSO_INCOMING_CALLS);
		$criteria->addSelectColumn(ArReportPeer::PARAM_SHOW_ALSO_INTERNAL_CALLS);
		$criteria->addSelectColumn(ArReportPeer::PARAM_SHOW_CALL_DETAILS);
		$criteria->addSelectColumn(ArReportPeer::PARAM_SHOW_VOIP_PROVIDER);
		$criteria->addSelectColumn(ArReportPeer::PARAM_SHOW_COMMUNICATION_CHANNEL);
		$criteria->addSelectColumn(ArReportPeer::PARAM_SHOW_GEOGRAPHIC_LOCATION);
		$criteria->addSelectColumn(ArReportPeer::PARAM_SHOW_CONNECTION_TYPE);
		$criteria->addSelectColumn(ArReportPeer::PARAM_SHOW_COST_SAVING);
		$criteria->addSelectColumn(ArReportPeer::PARAM_IS_LEGAL);
		$criteria->addSelectColumn(ArReportPeer::PARAM_EXPAND_TO_LEVEL);
		$criteria->addSelectColumn(ArReportPeer::AR_REPORT_ORDER_OF_CHILDREN_ID);
		$criteria->addSelectColumn(ArReportPeer::PHP_CLASS_NAME);
		$criteria->addSelectColumn(ArReportPeer::PRODUCED_REPORT_GENERATION_DATE);
		$criteria->addSelectColumn(ArReportPeer::REPORT_NAME);
		$criteria->addSelectColumn(ArReportPeer::PRODUCED_REPORT_SHORT_DESCRIPTION);
		$criteria->addSelectColumn(ArReportPeer::PRODUCED_REPORT_ADDITIONAL_DESCRIPTION);
		$criteria->addSelectColumn(ArReportPeer::PRODUCED_REPORT_ALREADY_REVIEWED);
		$criteria->addSelectColumn(ArReportPeer::PRODUCED_REPORT_IS_DRAFT);
		$criteria->addSelectColumn(ArReportPeer::PRODUCED_REPORT_MUST_BE_REGENERATED);
		$criteria->addSelectColumn(ArReportPeer::PRODUCED_REPORT_MIME_TYPE);
		$criteria->addSelectColumn(ArReportPeer::PRODUCED_REPORT_FILE_TYPE_SUFFIX);
		$criteria->addSelectColumn(ArReportPeer::PRODUCED_REPORT_DOCUMENT);
		$criteria->addSelectColumn(ArReportPeer::PRODUCED_REPORT_DOCUMENT_CHECKSUM);
		$criteria->addSelectColumn(ArReportPeer::REPORT_MAIL_SUBJECT);
		$criteria->addSelectColumn(ArReportPeer::REPORT_MAIL_BODY);
		$criteria->addSelectColumn(ArReportPeer::REPORT_ATTACHMENT_FILE_NAME);
		$criteria->addSelectColumn(ArReportPeer::REPORT_ATTACHMENT_FILE_NAME_ADD_REPORT_DATE);
		$criteria->addSelectColumn(ArReportPeer::INTERNAL_NAME);
		$criteria->addSelectColumn(ArReportPeer::CACHED_PARENT_ID_HIERARCHY);
		$criteria->addSelectColumn(ArReportPeer::LEGAL_NR_PREFIX);
		$criteria->addSelectColumn(ArReportPeer::LEGAL_CONSECUTIVE_NR);
		$criteria->addSelectColumn(ArReportPeer::LEGAL_DATE);
		$criteria->addSelectColumn(ArReportPeer::LEGAL_SENDER_NAME);
		$criteria->addSelectColumn(ArReportPeer::LEGAL_SENDER_VAT);
		$criteria->addSelectColumn(ArReportPeer::LEGAL_SENDER_ADDRESS);
		$criteria->addSelectColumn(ArReportPeer::LEGAL_RECEIVER_NAME);
		$criteria->addSelectColumn(ArReportPeer::LEGAL_RECEIVER_VAT);
		$criteria->addSelectColumn(ArReportPeer::LEGAL_RECEIVER_ADDRESS);
		$criteria->addSelectColumn(ArReportPeer::TOTAL_WITHOUT_TAX);
		$criteria->addSelectColumn(ArReportPeer::TAX);
		$criteria->addSelectColumn(ArReportPeer::APPLIED_VAT);
		$criteria->addSelectColumn(ArReportPeer::TOTAL_WITH_TAX);
	}

	/**
	 * Returns the number of rows matching criteria.
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @return     int Number of matching rows.
	 */
	public static function doCount(Criteria $criteria, $distinct = false, PropelPDO $con = null)
	{
		// we may modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArReportPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArReportPeer::addSelectColumns($criteria);
		}

		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		$criteria->setDbName(self::DATABASE_NAME); // Set the correct dbName

		if ($con === null) {
			$con = Propel::getConnection(ArReportPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}
		// BasePeer returns a PDOStatement
		$stmt = BasePeer::doCount($criteria, $con);

		if ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$count = (int) $row[0];
		} else {
			$count = 0; // no rows returned; we infer that means 0 matches.
		}
		$stmt->closeCursor();
		return $count;
	}
	/**
	 * Method to select one object from the DB.
	 *
	 * @param      Criteria $criteria object used to create the SELECT statement.
	 * @param      PropelPDO $con
	 * @return     ArReport
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectOne(Criteria $criteria, PropelPDO $con = null)
	{
		$critcopy = clone $criteria;
		$critcopy->setLimit(1);
		$objects = ArReportPeer::doSelect($critcopy, $con);
		if ($objects) {
			return $objects[0];
		}
		return null;
	}
	/**
	 * Method to do selects.
	 *
	 * @param      Criteria $criteria The Criteria object used to build the SELECT statement.
	 * @param      PropelPDO $con
	 * @return     array Array of selected Objects
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelect(Criteria $criteria, PropelPDO $con = null)
	{
		return ArReportPeer::populateObjects(ArReportPeer::doSelectStmt($criteria, $con));
	}
	/**
	 * Prepares the Criteria object and uses the parent doSelect() method to execute a PDOStatement.
	 *
	 * Use this method directly if you want to work with an executed statement durirectly (for example
	 * to perform your own object hydration).
	 *
	 * @param      Criteria $criteria The Criteria object used to build the SELECT statement.
	 * @param      PropelPDO $con The connection to use
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 * @return     PDOStatement The executed PDOStatement object.
	 * @see        BasePeer::doSelect()
	 */
	public static function doSelectStmt(Criteria $criteria, PropelPDO $con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArReportPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		if (!$criteria->hasSelectClause()) {
			$criteria = clone $criteria;
			ArReportPeer::addSelectColumns($criteria);
		}

		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		// BasePeer returns a PDOStatement
		return BasePeer::doSelect($criteria, $con);
	}
	/**
	 * Adds an object to the instance pool.
	 *
	 * Propel keeps cached copies of objects in an instance pool when they are retrieved
	 * from the database.  In some cases -- especially when you override doSelect*()
	 * methods in your stub classes -- you may need to explicitly add objects
	 * to the cache in order to ensure that the same objects are always returned by doSelect*()
	 * and retrieveByPK*() calls.
	 *
	 * @param      ArReport $value A ArReport object.
	 * @param      string $key (optional) key to use for instance map (for performance boost if key was already calculated externally).
	 */
	public static function addInstanceToPool(ArReport $obj, $key = null)
	{
		if (Propel::isInstancePoolingEnabled()) {
			if ($key === null) {
				$key = (string) $obj->getId();
			} // if key === null
			self::$instances[$key] = $obj;
		}
	}

	/**
	 * Removes an object from the instance pool.
	 *
	 * Propel keeps cached copies of objects in an instance pool when they are retrieved
	 * from the database.  In some cases -- especially when you override doDelete
	 * methods in your stub classes -- you may need to explicitly remove objects
	 * from the cache in order to prevent returning objects that no longer exist.
	 *
	 * @param      mixed $value A ArReport object or a primary key value.
	 */
	public static function removeInstanceFromPool($value)
	{
		if (Propel::isInstancePoolingEnabled() && $value !== null) {
			if (is_object($value) && $value instanceof ArReport) {
				$key = (string) $value->getId();
			} elseif (is_scalar($value)) {
				// assume we've been passed a primary key
				$key = (string) $value;
			} else {
				$e = new PropelException("Invalid value passed to removeInstanceFromPool().  Expected primary key or ArReport object; got " . (is_object($value) ? get_class($value) . ' object.' : var_export($value,true)));
				throw $e;
			}

			unset(self::$instances[$key]);
		}
	} // removeInstanceFromPool()

	/**
	 * Retrieves a string version of the primary key from the DB resultset row that can be used to uniquely identify a row in this table.
	 *
	 * For tables with a single-column primary key, that simple pkey value will be returned.  For tables with
	 * a multi-column primary key, a serialize()d version of the primary key will be returned.
	 *
	 * @param      string $key The key (@see getPrimaryKeyHash()) for this instance.
	 * @return     ArReport Found object or NULL if 1) no instance exists for specified key or 2) instance pooling has been disabled.
	 * @see        getPrimaryKeyHash()
	 */
	public static function getInstanceFromPool($key)
	{
		if (Propel::isInstancePoolingEnabled()) {
			if (isset(self::$instances[$key])) {
				return self::$instances[$key];
			}
		}
		return null; // just to be explicit
	}
	
	/**
	 * Clear the instance pool.
	 *
	 * @return     void
	 */
	public static function clearInstancePool()
	{
		self::$instances = array();
	}
	
	/**
	 * Method to invalidate the instance pool of all tables related to ar_report
	 * by a foreign key with ON DELETE CASCADE
	 */
	public static function clearRelatedInstancePool()
	{
		// invalidate objects in ArReportAlsoForPeer instance pool, since one or more of them may be deleted by ON DELETE CASCADE rule.
		ArReportAlsoForPeer::clearInstancePool();

		// invalidate objects in ArReportToReadPeer instance pool, since one or more of them may be deleted by ON DELETE CASCADE rule.
		ArReportToReadPeer::clearInstancePool();

		// invalidate objects in ArUserCanViewReportPeer instance pool, since one or more of them may be deleted by ON DELETE CASCADE rule.
		ArUserCanViewReportPeer::clearInstancePool();

	}

	/**
	 * Retrieves a string version of the primary key from the DB resultset row that can be used to uniquely identify a row in this table.
	 *
	 * For tables with a single-column primary key, that simple pkey value will be returned.  For tables with
	 * a multi-column primary key, a serialize()d version of the primary key will be returned.
	 *
	 * @param      array $row PropelPDO resultset row.
	 * @param      int $startcol The 0-based offset for reading from the resultset row.
	 * @return     string A string version of PK or NULL if the components of primary key in result array are all null.
	 */
	public static function getPrimaryKeyHashFromRow($row, $startcol = 0)
	{
		// If the PK cannot be derived from the row, return NULL.
		if ($row[$startcol] === null) {
			return null;
		}
		return (string) $row[$startcol];
	}

	/**
	 * The returned array will contain objects of the default type or
	 * objects that inherit from the default.
	 *
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function populateObjects(PDOStatement $stmt)
	{
		$results = array();
	
		// set the class once to avoid overhead in the loop
		$cls = ArReportPeer::getOMClass(false);
		// populate the object(s)
		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key = ArReportPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj = ArReportPeer::getInstanceFromPool($key))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj->hydrate($row, 0, true); // rehydrate
				$results[] = $obj;
			} else {
				$obj = new $cls();
				$obj->hydrate($row);
				$results[] = $obj;
				ArReportPeer::addInstanceToPool($obj, $key);
			} // if key exists
		}
		$stmt->closeCursor();
		return $results;
	}

	/**
	 * Returns the number of rows matching criteria, joining the related ArReportSetRelatedByArReportSetId table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinArReportSetRelatedByArReportSetId(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArReportPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArReportPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArReportPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArReportPeer::AR_REPORT_SET_ID, ArReportSetPeer::ID, $join_behavior);

		$stmt = BasePeer::doCount($criteria, $con);

		if ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$count = (int) $row[0];
		} else {
			$count = 0; // no rows returned; we infer that means 0 matches.
		}
		$stmt->closeCursor();
		return $count;
	}


	/**
	 * Returns the number of rows matching criteria, joining the related ArReportSetRelatedByAboutArReportSetId table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinArReportSetRelatedByAboutArReportSetId(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArReportPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArReportPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArReportPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArReportPeer::ABOUT_AR_REPORT_SET_ID, ArReportSetPeer::ID, $join_behavior);

		$stmt = BasePeer::doCount($criteria, $con);

		if ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$count = (int) $row[0];
		} else {
			$count = 0; // no rows returned; we infer that means 0 matches.
		}
		$stmt->closeCursor();
		return $count;
	}


	/**
	 * Returns the number of rows matching criteria, joining the related ArOrganizationUnit table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinArOrganizationUnit(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArReportPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArReportPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArReportPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArReportPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$stmt = BasePeer::doCount($criteria, $con);

		if ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$count = (int) $row[0];
		} else {
			$count = 0; // no rows returned; we infer that means 0 matches.
		}
		$stmt->closeCursor();
		return $count;
	}


	/**
	 * Returns the number of rows matching criteria, joining the related ArUser table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinArUser(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArReportPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArReportPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArReportPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArReportPeer::AR_USER_ID, ArUserPeer::ID, $join_behavior);

		$stmt = BasePeer::doCount($criteria, $con);

		if ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$count = (int) $row[0];
		} else {
			$count = 0; // no rows returned; we infer that means 0 matches.
		}
		$stmt->closeCursor();
		return $count;
	}


	/**
	 * Returns the number of rows matching criteria, joining the related ArVendor table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinArVendor(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArReportPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArReportPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArReportPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArReportPeer::AR_VENDOR_ID, ArVendorPeer::ID, $join_behavior);

		$stmt = BasePeer::doCount($criteria, $con);

		if ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$count = (int) $row[0];
		} else {
			$count = 0; // no rows returned; we infer that means 0 matches.
		}
		$stmt->closeCursor();
		return $count;
	}


	/**
	 * Returns the number of rows matching criteria, joining the related ArTag table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinArTag(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArReportPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArReportPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArReportPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArReportPeer::AR_TAG_ID, ArTagPeer::ID, $join_behavior);

		$stmt = BasePeer::doCount($criteria, $con);

		if ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$count = (int) $row[0];
		} else {
			$count = 0; // no rows returned; we infer that means 0 matches.
		}
		$stmt->closeCursor();
		return $count;
	}


	/**
	 * Returns the number of rows matching criteria, joining the related ArReportOrderOfChildren table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinArReportOrderOfChildren(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArReportPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArReportPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArReportPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArReportPeer::AR_REPORT_ORDER_OF_CHILDREN_ID, ArReportOrderOfChildrenPeer::ID, $join_behavior);

		$stmt = BasePeer::doCount($criteria, $con);

		if ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$count = (int) $row[0];
		} else {
			$count = 0; // no rows returned; we infer that means 0 matches.
		}
		$stmt->closeCursor();
		return $count;
	}


	/**
	 * Selects a collection of ArReport objects pre-filled with their ArReportSet objects.
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArReport objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinArReportSetRelatedByArReportSetId(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArReportPeer::addSelectColumns($criteria);
		$startcol = (ArReportPeer::NUM_COLUMNS - ArReportPeer::NUM_LAZY_LOAD_COLUMNS);
		ArReportSetPeer::addSelectColumns($criteria);

		$criteria->addJoin(ArReportPeer::AR_REPORT_SET_ID, ArReportSetPeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArReportPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArReportPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {

				$cls = ArReportPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArReportPeer::addInstanceToPool($obj1, $key1);
			} // if $obj1 already loaded

			$key2 = ArReportSetPeer::getPrimaryKeyHashFromRow($row, $startcol);
			if ($key2 !== null) {
				$obj2 = ArReportSetPeer::getInstanceFromPool($key2);
				if (!$obj2) {

					$cls = ArReportSetPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol);
					ArReportSetPeer::addInstanceToPool($obj2, $key2);
				} // if obj2 already loaded
				
				// Add the $obj1 (ArReport) to $obj2 (ArReportSet)
				$obj2->addArReportRelatedByArReportSetId($obj1);

			} // if joined row was not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArReport objects pre-filled with their ArReportSet objects.
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArReport objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinArReportSetRelatedByAboutArReportSetId(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArReportPeer::addSelectColumns($criteria);
		$startcol = (ArReportPeer::NUM_COLUMNS - ArReportPeer::NUM_LAZY_LOAD_COLUMNS);
		ArReportSetPeer::addSelectColumns($criteria);

		$criteria->addJoin(ArReportPeer::ABOUT_AR_REPORT_SET_ID, ArReportSetPeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArReportPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArReportPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {

				$cls = ArReportPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArReportPeer::addInstanceToPool($obj1, $key1);
			} // if $obj1 already loaded

			$key2 = ArReportSetPeer::getPrimaryKeyHashFromRow($row, $startcol);
			if ($key2 !== null) {
				$obj2 = ArReportSetPeer::getInstanceFromPool($key2);
				if (!$obj2) {

					$cls = ArReportSetPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol);
					ArReportSetPeer::addInstanceToPool($obj2, $key2);
				} // if obj2 already loaded
				
				// Add the $obj1 (ArReport) to $obj2 (ArReportSet)
				$obj2->addArReportRelatedByAboutArReportSetId($obj1);

			} // if joined row was not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArReport objects pre-filled with their ArOrganizationUnit objects.
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArReport objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinArOrganizationUnit(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArReportPeer::addSelectColumns($criteria);
		$startcol = (ArReportPeer::NUM_COLUMNS - ArReportPeer::NUM_LAZY_LOAD_COLUMNS);
		ArOrganizationUnitPeer::addSelectColumns($criteria);

		$criteria->addJoin(ArReportPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArReportPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArReportPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {

				$cls = ArReportPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArReportPeer::addInstanceToPool($obj1, $key1);
			} // if $obj1 already loaded

			$key2 = ArOrganizationUnitPeer::getPrimaryKeyHashFromRow($row, $startcol);
			if ($key2 !== null) {
				$obj2 = ArOrganizationUnitPeer::getInstanceFromPool($key2);
				if (!$obj2) {

					$cls = ArOrganizationUnitPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol);
					ArOrganizationUnitPeer::addInstanceToPool($obj2, $key2);
				} // if obj2 already loaded
				
				// Add the $obj1 (ArReport) to $obj2 (ArOrganizationUnit)
				$obj2->addArReport($obj1);

			} // if joined row was not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArReport objects pre-filled with their ArUser objects.
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArReport objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinArUser(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArReportPeer::addSelectColumns($criteria);
		$startcol = (ArReportPeer::NUM_COLUMNS - ArReportPeer::NUM_LAZY_LOAD_COLUMNS);
		ArUserPeer::addSelectColumns($criteria);

		$criteria->addJoin(ArReportPeer::AR_USER_ID, ArUserPeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArReportPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArReportPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {

				$cls = ArReportPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArReportPeer::addInstanceToPool($obj1, $key1);
			} // if $obj1 already loaded

			$key2 = ArUserPeer::getPrimaryKeyHashFromRow($row, $startcol);
			if ($key2 !== null) {
				$obj2 = ArUserPeer::getInstanceFromPool($key2);
				if (!$obj2) {

					$cls = ArUserPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol);
					ArUserPeer::addInstanceToPool($obj2, $key2);
				} // if obj2 already loaded
				
				// Add the $obj1 (ArReport) to $obj2 (ArUser)
				$obj2->addArReport($obj1);

			} // if joined row was not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArReport objects pre-filled with their ArVendor objects.
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArReport objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinArVendor(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArReportPeer::addSelectColumns($criteria);
		$startcol = (ArReportPeer::NUM_COLUMNS - ArReportPeer::NUM_LAZY_LOAD_COLUMNS);
		ArVendorPeer::addSelectColumns($criteria);

		$criteria->addJoin(ArReportPeer::AR_VENDOR_ID, ArVendorPeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArReportPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArReportPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {

				$cls = ArReportPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArReportPeer::addInstanceToPool($obj1, $key1);
			} // if $obj1 already loaded

			$key2 = ArVendorPeer::getPrimaryKeyHashFromRow($row, $startcol);
			if ($key2 !== null) {
				$obj2 = ArVendorPeer::getInstanceFromPool($key2);
				if (!$obj2) {

					$cls = ArVendorPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol);
					ArVendorPeer::addInstanceToPool($obj2, $key2);
				} // if obj2 already loaded
				
				// Add the $obj1 (ArReport) to $obj2 (ArVendor)
				$obj2->addArReport($obj1);

			} // if joined row was not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArReport objects pre-filled with their ArTag objects.
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArReport objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinArTag(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArReportPeer::addSelectColumns($criteria);
		$startcol = (ArReportPeer::NUM_COLUMNS - ArReportPeer::NUM_LAZY_LOAD_COLUMNS);
		ArTagPeer::addSelectColumns($criteria);

		$criteria->addJoin(ArReportPeer::AR_TAG_ID, ArTagPeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArReportPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArReportPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {

				$cls = ArReportPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArReportPeer::addInstanceToPool($obj1, $key1);
			} // if $obj1 already loaded

			$key2 = ArTagPeer::getPrimaryKeyHashFromRow($row, $startcol);
			if ($key2 !== null) {
				$obj2 = ArTagPeer::getInstanceFromPool($key2);
				if (!$obj2) {

					$cls = ArTagPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol);
					ArTagPeer::addInstanceToPool($obj2, $key2);
				} // if obj2 already loaded
				
				// Add the $obj1 (ArReport) to $obj2 (ArTag)
				$obj2->addArReport($obj1);

			} // if joined row was not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArReport objects pre-filled with their ArReportOrderOfChildren objects.
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArReport objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinArReportOrderOfChildren(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArReportPeer::addSelectColumns($criteria);
		$startcol = (ArReportPeer::NUM_COLUMNS - ArReportPeer::NUM_LAZY_LOAD_COLUMNS);
		ArReportOrderOfChildrenPeer::addSelectColumns($criteria);

		$criteria->addJoin(ArReportPeer::AR_REPORT_ORDER_OF_CHILDREN_ID, ArReportOrderOfChildrenPeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArReportPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArReportPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {

				$cls = ArReportPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArReportPeer::addInstanceToPool($obj1, $key1);
			} // if $obj1 already loaded

			$key2 = ArReportOrderOfChildrenPeer::getPrimaryKeyHashFromRow($row, $startcol);
			if ($key2 !== null) {
				$obj2 = ArReportOrderOfChildrenPeer::getInstanceFromPool($key2);
				if (!$obj2) {

					$cls = ArReportOrderOfChildrenPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol);
					ArReportOrderOfChildrenPeer::addInstanceToPool($obj2, $key2);
				} // if obj2 already loaded
				
				// Add the $obj1 (ArReport) to $obj2 (ArReportOrderOfChildren)
				$obj2->addArReport($obj1);

			} // if joined row was not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Returns the number of rows matching criteria, joining all related tables
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinAll(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArReportPeer::TABLE_NAME);

		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArReportPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY won't ever affect the count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArReportPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria->addJoin(ArReportPeer::AR_REPORT_SET_ID, ArReportSetPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::ABOUT_AR_REPORT_SET_ID, ArReportSetPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_USER_ID, ArUserPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_VENDOR_ID, ArVendorPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_TAG_ID, ArTagPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_REPORT_ORDER_OF_CHILDREN_ID, ArReportOrderOfChildrenPeer::ID, $join_behavior);

		$stmt = BasePeer::doCount($criteria, $con);

		if ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$count = (int) $row[0];
		} else {
			$count = 0; // no rows returned; we infer that means 0 matches.
		}
		$stmt->closeCursor();
		return $count;
	}

	/**
	 * Selects a collection of ArReport objects pre-filled with all related objects.
	 *
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArReport objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinAll(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArReportPeer::addSelectColumns($criteria);
		$startcol2 = (ArReportPeer::NUM_COLUMNS - ArReportPeer::NUM_LAZY_LOAD_COLUMNS);

		ArReportSetPeer::addSelectColumns($criteria);
		$startcol3 = $startcol2 + (ArReportSetPeer::NUM_COLUMNS - ArReportSetPeer::NUM_LAZY_LOAD_COLUMNS);

		ArReportSetPeer::addSelectColumns($criteria);
		$startcol4 = $startcol3 + (ArReportSetPeer::NUM_COLUMNS - ArReportSetPeer::NUM_LAZY_LOAD_COLUMNS);

		ArOrganizationUnitPeer::addSelectColumns($criteria);
		$startcol5 = $startcol4 + (ArOrganizationUnitPeer::NUM_COLUMNS - ArOrganizationUnitPeer::NUM_LAZY_LOAD_COLUMNS);

		ArUserPeer::addSelectColumns($criteria);
		$startcol6 = $startcol5 + (ArUserPeer::NUM_COLUMNS - ArUserPeer::NUM_LAZY_LOAD_COLUMNS);

		ArVendorPeer::addSelectColumns($criteria);
		$startcol7 = $startcol6 + (ArVendorPeer::NUM_COLUMNS - ArVendorPeer::NUM_LAZY_LOAD_COLUMNS);

		ArTagPeer::addSelectColumns($criteria);
		$startcol8 = $startcol7 + (ArTagPeer::NUM_COLUMNS - ArTagPeer::NUM_LAZY_LOAD_COLUMNS);

		ArReportOrderOfChildrenPeer::addSelectColumns($criteria);
		$startcol9 = $startcol8 + (ArReportOrderOfChildrenPeer::NUM_COLUMNS - ArReportOrderOfChildrenPeer::NUM_LAZY_LOAD_COLUMNS);

		$criteria->addJoin(ArReportPeer::AR_REPORT_SET_ID, ArReportSetPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::ABOUT_AR_REPORT_SET_ID, ArReportSetPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_USER_ID, ArUserPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_VENDOR_ID, ArVendorPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_TAG_ID, ArTagPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_REPORT_ORDER_OF_CHILDREN_ID, ArReportOrderOfChildrenPeer::ID, $join_behavior);

		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArReportPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArReportPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {
				$cls = ArReportPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArReportPeer::addInstanceToPool($obj1, $key1);
			} // if obj1 already loaded

			// Add objects for joined ArReportSet rows

			$key2 = ArReportSetPeer::getPrimaryKeyHashFromRow($row, $startcol2);
			if ($key2 !== null) {
				$obj2 = ArReportSetPeer::getInstanceFromPool($key2);
				if (!$obj2) {

					$cls = ArReportSetPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol2);
					ArReportSetPeer::addInstanceToPool($obj2, $key2);
				} // if obj2 loaded

				// Add the $obj1 (ArReport) to the collection in $obj2 (ArReportSet)
				$obj2->addArReportRelatedByArReportSetId($obj1);
			} // if joined row not null

			// Add objects for joined ArReportSet rows

			$key3 = ArReportSetPeer::getPrimaryKeyHashFromRow($row, $startcol3);
			if ($key3 !== null) {
				$obj3 = ArReportSetPeer::getInstanceFromPool($key3);
				if (!$obj3) {

					$cls = ArReportSetPeer::getOMClass(false);

					$obj3 = new $cls();
					$obj3->hydrate($row, $startcol3);
					ArReportSetPeer::addInstanceToPool($obj3, $key3);
				} // if obj3 loaded

				// Add the $obj1 (ArReport) to the collection in $obj3 (ArReportSet)
				$obj3->addArReportRelatedByAboutArReportSetId($obj1);
			} // if joined row not null

			// Add objects for joined ArOrganizationUnit rows

			$key4 = ArOrganizationUnitPeer::getPrimaryKeyHashFromRow($row, $startcol4);
			if ($key4 !== null) {
				$obj4 = ArOrganizationUnitPeer::getInstanceFromPool($key4);
				if (!$obj4) {

					$cls = ArOrganizationUnitPeer::getOMClass(false);

					$obj4 = new $cls();
					$obj4->hydrate($row, $startcol4);
					ArOrganizationUnitPeer::addInstanceToPool($obj4, $key4);
				} // if obj4 loaded

				// Add the $obj1 (ArReport) to the collection in $obj4 (ArOrganizationUnit)
				$obj4->addArReport($obj1);
			} // if joined row not null

			// Add objects for joined ArUser rows

			$key5 = ArUserPeer::getPrimaryKeyHashFromRow($row, $startcol5);
			if ($key5 !== null) {
				$obj5 = ArUserPeer::getInstanceFromPool($key5);
				if (!$obj5) {

					$cls = ArUserPeer::getOMClass(false);

					$obj5 = new $cls();
					$obj5->hydrate($row, $startcol5);
					ArUserPeer::addInstanceToPool($obj5, $key5);
				} // if obj5 loaded

				// Add the $obj1 (ArReport) to the collection in $obj5 (ArUser)
				$obj5->addArReport($obj1);
			} // if joined row not null

			// Add objects for joined ArVendor rows

			$key6 = ArVendorPeer::getPrimaryKeyHashFromRow($row, $startcol6);
			if ($key6 !== null) {
				$obj6 = ArVendorPeer::getInstanceFromPool($key6);
				if (!$obj6) {

					$cls = ArVendorPeer::getOMClass(false);

					$obj6 = new $cls();
					$obj6->hydrate($row, $startcol6);
					ArVendorPeer::addInstanceToPool($obj6, $key6);
				} // if obj6 loaded

				// Add the $obj1 (ArReport) to the collection in $obj6 (ArVendor)
				$obj6->addArReport($obj1);
			} // if joined row not null

			// Add objects for joined ArTag rows

			$key7 = ArTagPeer::getPrimaryKeyHashFromRow($row, $startcol7);
			if ($key7 !== null) {
				$obj7 = ArTagPeer::getInstanceFromPool($key7);
				if (!$obj7) {

					$cls = ArTagPeer::getOMClass(false);

					$obj7 = new $cls();
					$obj7->hydrate($row, $startcol7);
					ArTagPeer::addInstanceToPool($obj7, $key7);
				} // if obj7 loaded

				// Add the $obj1 (ArReport) to the collection in $obj7 (ArTag)
				$obj7->addArReport($obj1);
			} // if joined row not null

			// Add objects for joined ArReportOrderOfChildren rows

			$key8 = ArReportOrderOfChildrenPeer::getPrimaryKeyHashFromRow($row, $startcol8);
			if ($key8 !== null) {
				$obj8 = ArReportOrderOfChildrenPeer::getInstanceFromPool($key8);
				if (!$obj8) {

					$cls = ArReportOrderOfChildrenPeer::getOMClass(false);

					$obj8 = new $cls();
					$obj8->hydrate($row, $startcol8);
					ArReportOrderOfChildrenPeer::addInstanceToPool($obj8, $key8);
				} // if obj8 loaded

				// Add the $obj1 (ArReport) to the collection in $obj8 (ArReportOrderOfChildren)
				$obj8->addArReport($obj1);
			} // if joined row not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Returns the number of rows matching criteria, joining the related ArReportSetRelatedByArReportSetId table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinAllExceptArReportSetRelatedByArReportSetId(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArReportPeer::TABLE_NAME);
		
		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArReportPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY should not affect count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArReportPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}
	
		$criteria->addJoin(ArReportPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_USER_ID, ArUserPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_VENDOR_ID, ArVendorPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_TAG_ID, ArTagPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_REPORT_ORDER_OF_CHILDREN_ID, ArReportOrderOfChildrenPeer::ID, $join_behavior);

		$stmt = BasePeer::doCount($criteria, $con);

		if ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$count = (int) $row[0];
		} else {
			$count = 0; // no rows returned; we infer that means 0 matches.
		}
		$stmt->closeCursor();
		return $count;
	}


	/**
	 * Returns the number of rows matching criteria, joining the related ArReportSetRelatedByAboutArReportSetId table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinAllExceptArReportSetRelatedByAboutArReportSetId(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArReportPeer::TABLE_NAME);
		
		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArReportPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY should not affect count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArReportPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}
	
		$criteria->addJoin(ArReportPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_USER_ID, ArUserPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_VENDOR_ID, ArVendorPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_TAG_ID, ArTagPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_REPORT_ORDER_OF_CHILDREN_ID, ArReportOrderOfChildrenPeer::ID, $join_behavior);

		$stmt = BasePeer::doCount($criteria, $con);

		if ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$count = (int) $row[0];
		} else {
			$count = 0; // no rows returned; we infer that means 0 matches.
		}
		$stmt->closeCursor();
		return $count;
	}


	/**
	 * Returns the number of rows matching criteria, joining the related ArOrganizationUnit table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinAllExceptArOrganizationUnit(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArReportPeer::TABLE_NAME);
		
		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArReportPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY should not affect count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArReportPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}
	
		$criteria->addJoin(ArReportPeer::AR_REPORT_SET_ID, ArReportSetPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::ABOUT_AR_REPORT_SET_ID, ArReportSetPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_USER_ID, ArUserPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_VENDOR_ID, ArVendorPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_TAG_ID, ArTagPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_REPORT_ORDER_OF_CHILDREN_ID, ArReportOrderOfChildrenPeer::ID, $join_behavior);

		$stmt = BasePeer::doCount($criteria, $con);

		if ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$count = (int) $row[0];
		} else {
			$count = 0; // no rows returned; we infer that means 0 matches.
		}
		$stmt->closeCursor();
		return $count;
	}


	/**
	 * Returns the number of rows matching criteria, joining the related ArUser table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinAllExceptArUser(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArReportPeer::TABLE_NAME);
		
		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArReportPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY should not affect count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArReportPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}
	
		$criteria->addJoin(ArReportPeer::AR_REPORT_SET_ID, ArReportSetPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::ABOUT_AR_REPORT_SET_ID, ArReportSetPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_VENDOR_ID, ArVendorPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_TAG_ID, ArTagPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_REPORT_ORDER_OF_CHILDREN_ID, ArReportOrderOfChildrenPeer::ID, $join_behavior);

		$stmt = BasePeer::doCount($criteria, $con);

		if ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$count = (int) $row[0];
		} else {
			$count = 0; // no rows returned; we infer that means 0 matches.
		}
		$stmt->closeCursor();
		return $count;
	}


	/**
	 * Returns the number of rows matching criteria, joining the related ArVendor table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinAllExceptArVendor(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArReportPeer::TABLE_NAME);
		
		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArReportPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY should not affect count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArReportPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}
	
		$criteria->addJoin(ArReportPeer::AR_REPORT_SET_ID, ArReportSetPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::ABOUT_AR_REPORT_SET_ID, ArReportSetPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_USER_ID, ArUserPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_TAG_ID, ArTagPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_REPORT_ORDER_OF_CHILDREN_ID, ArReportOrderOfChildrenPeer::ID, $join_behavior);

		$stmt = BasePeer::doCount($criteria, $con);

		if ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$count = (int) $row[0];
		} else {
			$count = 0; // no rows returned; we infer that means 0 matches.
		}
		$stmt->closeCursor();
		return $count;
	}


	/**
	 * Returns the number of rows matching criteria, joining the related ArTag table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinAllExceptArTag(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArReportPeer::TABLE_NAME);
		
		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArReportPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY should not affect count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArReportPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}
	
		$criteria->addJoin(ArReportPeer::AR_REPORT_SET_ID, ArReportSetPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::ABOUT_AR_REPORT_SET_ID, ArReportSetPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_USER_ID, ArUserPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_VENDOR_ID, ArVendorPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_REPORT_ORDER_OF_CHILDREN_ID, ArReportOrderOfChildrenPeer::ID, $join_behavior);

		$stmt = BasePeer::doCount($criteria, $con);

		if ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$count = (int) $row[0];
		} else {
			$count = 0; // no rows returned; we infer that means 0 matches.
		}
		$stmt->closeCursor();
		return $count;
	}


	/**
	 * Returns the number of rows matching criteria, joining the related ArReportOrderOfChildren table
	 *
	 * @param      Criteria $criteria
	 * @param      boolean $distinct Whether to select only distinct columns; deprecated: use Criteria->setDistinct() instead.
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     int Number of matching rows.
	 */
	public static function doCountJoinAllExceptArReportOrderOfChildren(Criteria $criteria, $distinct = false, PropelPDO $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		// we're going to modify criteria, so copy it first
		$criteria = clone $criteria;

		// We need to set the primary table name, since in the case that there are no WHERE columns
		// it will be impossible for the BasePeer::createSelectSql() method to determine which
		// tables go into the FROM clause.
		$criteria->setPrimaryTableName(ArReportPeer::TABLE_NAME);
		
		if ($distinct && !in_array(Criteria::DISTINCT, $criteria->getSelectModifiers())) {
			$criteria->setDistinct();
		}

		if (!$criteria->hasSelectClause()) {
			ArReportPeer::addSelectColumns($criteria);
		}
		
		$criteria->clearOrderByColumns(); // ORDER BY should not affect count
		
		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		if ($con === null) {
			$con = Propel::getConnection(ArReportPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}
	
		$criteria->addJoin(ArReportPeer::AR_REPORT_SET_ID, ArReportSetPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::ABOUT_AR_REPORT_SET_ID, ArReportSetPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_USER_ID, ArUserPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_VENDOR_ID, ArVendorPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_TAG_ID, ArTagPeer::ID, $join_behavior);

		$stmt = BasePeer::doCount($criteria, $con);

		if ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$count = (int) $row[0];
		} else {
			$count = 0; // no rows returned; we infer that means 0 matches.
		}
		$stmt->closeCursor();
		return $count;
	}


	/**
	 * Selects a collection of ArReport objects pre-filled with all related objects except ArReportSetRelatedByArReportSetId.
	 *
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArReport objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinAllExceptArReportSetRelatedByArReportSetId(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		// $criteria->getDbName() will return the same object if not set to another value
		// so == check is okay and faster
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArReportPeer::addSelectColumns($criteria);
		$startcol2 = (ArReportPeer::NUM_COLUMNS - ArReportPeer::NUM_LAZY_LOAD_COLUMNS);

		ArOrganizationUnitPeer::addSelectColumns($criteria);
		$startcol3 = $startcol2 + (ArOrganizationUnitPeer::NUM_COLUMNS - ArOrganizationUnitPeer::NUM_LAZY_LOAD_COLUMNS);

		ArUserPeer::addSelectColumns($criteria);
		$startcol4 = $startcol3 + (ArUserPeer::NUM_COLUMNS - ArUserPeer::NUM_LAZY_LOAD_COLUMNS);

		ArVendorPeer::addSelectColumns($criteria);
		$startcol5 = $startcol4 + (ArVendorPeer::NUM_COLUMNS - ArVendorPeer::NUM_LAZY_LOAD_COLUMNS);

		ArTagPeer::addSelectColumns($criteria);
		$startcol6 = $startcol5 + (ArTagPeer::NUM_COLUMNS - ArTagPeer::NUM_LAZY_LOAD_COLUMNS);

		ArReportOrderOfChildrenPeer::addSelectColumns($criteria);
		$startcol7 = $startcol6 + (ArReportOrderOfChildrenPeer::NUM_COLUMNS - ArReportOrderOfChildrenPeer::NUM_LAZY_LOAD_COLUMNS);

		$criteria->addJoin(ArReportPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_USER_ID, ArUserPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_VENDOR_ID, ArVendorPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_TAG_ID, ArTagPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_REPORT_ORDER_OF_CHILDREN_ID, ArReportOrderOfChildrenPeer::ID, $join_behavior);


		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArReportPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArReportPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {
				$cls = ArReportPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArReportPeer::addInstanceToPool($obj1, $key1);
			} // if obj1 already loaded

				// Add objects for joined ArOrganizationUnit rows

				$key2 = ArOrganizationUnitPeer::getPrimaryKeyHashFromRow($row, $startcol2);
				if ($key2 !== null) {
					$obj2 = ArOrganizationUnitPeer::getInstanceFromPool($key2);
					if (!$obj2) {
	
						$cls = ArOrganizationUnitPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol2);
					ArOrganizationUnitPeer::addInstanceToPool($obj2, $key2);
				} // if $obj2 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj2 (ArOrganizationUnit)
				$obj2->addArReport($obj1);

			} // if joined row is not null

				// Add objects for joined ArUser rows

				$key3 = ArUserPeer::getPrimaryKeyHashFromRow($row, $startcol3);
				if ($key3 !== null) {
					$obj3 = ArUserPeer::getInstanceFromPool($key3);
					if (!$obj3) {
	
						$cls = ArUserPeer::getOMClass(false);

					$obj3 = new $cls();
					$obj3->hydrate($row, $startcol3);
					ArUserPeer::addInstanceToPool($obj3, $key3);
				} // if $obj3 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj3 (ArUser)
				$obj3->addArReport($obj1);

			} // if joined row is not null

				// Add objects for joined ArVendor rows

				$key4 = ArVendorPeer::getPrimaryKeyHashFromRow($row, $startcol4);
				if ($key4 !== null) {
					$obj4 = ArVendorPeer::getInstanceFromPool($key4);
					if (!$obj4) {
	
						$cls = ArVendorPeer::getOMClass(false);

					$obj4 = new $cls();
					$obj4->hydrate($row, $startcol4);
					ArVendorPeer::addInstanceToPool($obj4, $key4);
				} // if $obj4 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj4 (ArVendor)
				$obj4->addArReport($obj1);

			} // if joined row is not null

				// Add objects for joined ArTag rows

				$key5 = ArTagPeer::getPrimaryKeyHashFromRow($row, $startcol5);
				if ($key5 !== null) {
					$obj5 = ArTagPeer::getInstanceFromPool($key5);
					if (!$obj5) {
	
						$cls = ArTagPeer::getOMClass(false);

					$obj5 = new $cls();
					$obj5->hydrate($row, $startcol5);
					ArTagPeer::addInstanceToPool($obj5, $key5);
				} // if $obj5 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj5 (ArTag)
				$obj5->addArReport($obj1);

			} // if joined row is not null

				// Add objects for joined ArReportOrderOfChildren rows

				$key6 = ArReportOrderOfChildrenPeer::getPrimaryKeyHashFromRow($row, $startcol6);
				if ($key6 !== null) {
					$obj6 = ArReportOrderOfChildrenPeer::getInstanceFromPool($key6);
					if (!$obj6) {
	
						$cls = ArReportOrderOfChildrenPeer::getOMClass(false);

					$obj6 = new $cls();
					$obj6->hydrate($row, $startcol6);
					ArReportOrderOfChildrenPeer::addInstanceToPool($obj6, $key6);
				} // if $obj6 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj6 (ArReportOrderOfChildren)
				$obj6->addArReport($obj1);

			} // if joined row is not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArReport objects pre-filled with all related objects except ArReportSetRelatedByAboutArReportSetId.
	 *
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArReport objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinAllExceptArReportSetRelatedByAboutArReportSetId(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		// $criteria->getDbName() will return the same object if not set to another value
		// so == check is okay and faster
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArReportPeer::addSelectColumns($criteria);
		$startcol2 = (ArReportPeer::NUM_COLUMNS - ArReportPeer::NUM_LAZY_LOAD_COLUMNS);

		ArOrganizationUnitPeer::addSelectColumns($criteria);
		$startcol3 = $startcol2 + (ArOrganizationUnitPeer::NUM_COLUMNS - ArOrganizationUnitPeer::NUM_LAZY_LOAD_COLUMNS);

		ArUserPeer::addSelectColumns($criteria);
		$startcol4 = $startcol3 + (ArUserPeer::NUM_COLUMNS - ArUserPeer::NUM_LAZY_LOAD_COLUMNS);

		ArVendorPeer::addSelectColumns($criteria);
		$startcol5 = $startcol4 + (ArVendorPeer::NUM_COLUMNS - ArVendorPeer::NUM_LAZY_LOAD_COLUMNS);

		ArTagPeer::addSelectColumns($criteria);
		$startcol6 = $startcol5 + (ArTagPeer::NUM_COLUMNS - ArTagPeer::NUM_LAZY_LOAD_COLUMNS);

		ArReportOrderOfChildrenPeer::addSelectColumns($criteria);
		$startcol7 = $startcol6 + (ArReportOrderOfChildrenPeer::NUM_COLUMNS - ArReportOrderOfChildrenPeer::NUM_LAZY_LOAD_COLUMNS);

		$criteria->addJoin(ArReportPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_USER_ID, ArUserPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_VENDOR_ID, ArVendorPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_TAG_ID, ArTagPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_REPORT_ORDER_OF_CHILDREN_ID, ArReportOrderOfChildrenPeer::ID, $join_behavior);


		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArReportPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArReportPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {
				$cls = ArReportPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArReportPeer::addInstanceToPool($obj1, $key1);
			} // if obj1 already loaded

				// Add objects for joined ArOrganizationUnit rows

				$key2 = ArOrganizationUnitPeer::getPrimaryKeyHashFromRow($row, $startcol2);
				if ($key2 !== null) {
					$obj2 = ArOrganizationUnitPeer::getInstanceFromPool($key2);
					if (!$obj2) {
	
						$cls = ArOrganizationUnitPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol2);
					ArOrganizationUnitPeer::addInstanceToPool($obj2, $key2);
				} // if $obj2 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj2 (ArOrganizationUnit)
				$obj2->addArReport($obj1);

			} // if joined row is not null

				// Add objects for joined ArUser rows

				$key3 = ArUserPeer::getPrimaryKeyHashFromRow($row, $startcol3);
				if ($key3 !== null) {
					$obj3 = ArUserPeer::getInstanceFromPool($key3);
					if (!$obj3) {
	
						$cls = ArUserPeer::getOMClass(false);

					$obj3 = new $cls();
					$obj3->hydrate($row, $startcol3);
					ArUserPeer::addInstanceToPool($obj3, $key3);
				} // if $obj3 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj3 (ArUser)
				$obj3->addArReport($obj1);

			} // if joined row is not null

				// Add objects for joined ArVendor rows

				$key4 = ArVendorPeer::getPrimaryKeyHashFromRow($row, $startcol4);
				if ($key4 !== null) {
					$obj4 = ArVendorPeer::getInstanceFromPool($key4);
					if (!$obj4) {
	
						$cls = ArVendorPeer::getOMClass(false);

					$obj4 = new $cls();
					$obj4->hydrate($row, $startcol4);
					ArVendorPeer::addInstanceToPool($obj4, $key4);
				} // if $obj4 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj4 (ArVendor)
				$obj4->addArReport($obj1);

			} // if joined row is not null

				// Add objects for joined ArTag rows

				$key5 = ArTagPeer::getPrimaryKeyHashFromRow($row, $startcol5);
				if ($key5 !== null) {
					$obj5 = ArTagPeer::getInstanceFromPool($key5);
					if (!$obj5) {
	
						$cls = ArTagPeer::getOMClass(false);

					$obj5 = new $cls();
					$obj5->hydrate($row, $startcol5);
					ArTagPeer::addInstanceToPool($obj5, $key5);
				} // if $obj5 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj5 (ArTag)
				$obj5->addArReport($obj1);

			} // if joined row is not null

				// Add objects for joined ArReportOrderOfChildren rows

				$key6 = ArReportOrderOfChildrenPeer::getPrimaryKeyHashFromRow($row, $startcol6);
				if ($key6 !== null) {
					$obj6 = ArReportOrderOfChildrenPeer::getInstanceFromPool($key6);
					if (!$obj6) {
	
						$cls = ArReportOrderOfChildrenPeer::getOMClass(false);

					$obj6 = new $cls();
					$obj6->hydrate($row, $startcol6);
					ArReportOrderOfChildrenPeer::addInstanceToPool($obj6, $key6);
				} // if $obj6 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj6 (ArReportOrderOfChildren)
				$obj6->addArReport($obj1);

			} // if joined row is not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArReport objects pre-filled with all related objects except ArOrganizationUnit.
	 *
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArReport objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinAllExceptArOrganizationUnit(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		// $criteria->getDbName() will return the same object if not set to another value
		// so == check is okay and faster
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArReportPeer::addSelectColumns($criteria);
		$startcol2 = (ArReportPeer::NUM_COLUMNS - ArReportPeer::NUM_LAZY_LOAD_COLUMNS);

		ArReportSetPeer::addSelectColumns($criteria);
		$startcol3 = $startcol2 + (ArReportSetPeer::NUM_COLUMNS - ArReportSetPeer::NUM_LAZY_LOAD_COLUMNS);

		ArReportSetPeer::addSelectColumns($criteria);
		$startcol4 = $startcol3 + (ArReportSetPeer::NUM_COLUMNS - ArReportSetPeer::NUM_LAZY_LOAD_COLUMNS);

		ArUserPeer::addSelectColumns($criteria);
		$startcol5 = $startcol4 + (ArUserPeer::NUM_COLUMNS - ArUserPeer::NUM_LAZY_LOAD_COLUMNS);

		ArVendorPeer::addSelectColumns($criteria);
		$startcol6 = $startcol5 + (ArVendorPeer::NUM_COLUMNS - ArVendorPeer::NUM_LAZY_LOAD_COLUMNS);

		ArTagPeer::addSelectColumns($criteria);
		$startcol7 = $startcol6 + (ArTagPeer::NUM_COLUMNS - ArTagPeer::NUM_LAZY_LOAD_COLUMNS);

		ArReportOrderOfChildrenPeer::addSelectColumns($criteria);
		$startcol8 = $startcol7 + (ArReportOrderOfChildrenPeer::NUM_COLUMNS - ArReportOrderOfChildrenPeer::NUM_LAZY_LOAD_COLUMNS);

		$criteria->addJoin(ArReportPeer::AR_REPORT_SET_ID, ArReportSetPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::ABOUT_AR_REPORT_SET_ID, ArReportSetPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_USER_ID, ArUserPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_VENDOR_ID, ArVendorPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_TAG_ID, ArTagPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_REPORT_ORDER_OF_CHILDREN_ID, ArReportOrderOfChildrenPeer::ID, $join_behavior);


		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArReportPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArReportPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {
				$cls = ArReportPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArReportPeer::addInstanceToPool($obj1, $key1);
			} // if obj1 already loaded

				// Add objects for joined ArReportSet rows

				$key2 = ArReportSetPeer::getPrimaryKeyHashFromRow($row, $startcol2);
				if ($key2 !== null) {
					$obj2 = ArReportSetPeer::getInstanceFromPool($key2);
					if (!$obj2) {
	
						$cls = ArReportSetPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol2);
					ArReportSetPeer::addInstanceToPool($obj2, $key2);
				} // if $obj2 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj2 (ArReportSet)
				$obj2->addArReportRelatedByArReportSetId($obj1);

			} // if joined row is not null

				// Add objects for joined ArReportSet rows

				$key3 = ArReportSetPeer::getPrimaryKeyHashFromRow($row, $startcol3);
				if ($key3 !== null) {
					$obj3 = ArReportSetPeer::getInstanceFromPool($key3);
					if (!$obj3) {
	
						$cls = ArReportSetPeer::getOMClass(false);

					$obj3 = new $cls();
					$obj3->hydrate($row, $startcol3);
					ArReportSetPeer::addInstanceToPool($obj3, $key3);
				} // if $obj3 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj3 (ArReportSet)
				$obj3->addArReportRelatedByAboutArReportSetId($obj1);

			} // if joined row is not null

				// Add objects for joined ArUser rows

				$key4 = ArUserPeer::getPrimaryKeyHashFromRow($row, $startcol4);
				if ($key4 !== null) {
					$obj4 = ArUserPeer::getInstanceFromPool($key4);
					if (!$obj4) {
	
						$cls = ArUserPeer::getOMClass(false);

					$obj4 = new $cls();
					$obj4->hydrate($row, $startcol4);
					ArUserPeer::addInstanceToPool($obj4, $key4);
				} // if $obj4 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj4 (ArUser)
				$obj4->addArReport($obj1);

			} // if joined row is not null

				// Add objects for joined ArVendor rows

				$key5 = ArVendorPeer::getPrimaryKeyHashFromRow($row, $startcol5);
				if ($key5 !== null) {
					$obj5 = ArVendorPeer::getInstanceFromPool($key5);
					if (!$obj5) {
	
						$cls = ArVendorPeer::getOMClass(false);

					$obj5 = new $cls();
					$obj5->hydrate($row, $startcol5);
					ArVendorPeer::addInstanceToPool($obj5, $key5);
				} // if $obj5 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj5 (ArVendor)
				$obj5->addArReport($obj1);

			} // if joined row is not null

				// Add objects for joined ArTag rows

				$key6 = ArTagPeer::getPrimaryKeyHashFromRow($row, $startcol6);
				if ($key6 !== null) {
					$obj6 = ArTagPeer::getInstanceFromPool($key6);
					if (!$obj6) {
	
						$cls = ArTagPeer::getOMClass(false);

					$obj6 = new $cls();
					$obj6->hydrate($row, $startcol6);
					ArTagPeer::addInstanceToPool($obj6, $key6);
				} // if $obj6 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj6 (ArTag)
				$obj6->addArReport($obj1);

			} // if joined row is not null

				// Add objects for joined ArReportOrderOfChildren rows

				$key7 = ArReportOrderOfChildrenPeer::getPrimaryKeyHashFromRow($row, $startcol7);
				if ($key7 !== null) {
					$obj7 = ArReportOrderOfChildrenPeer::getInstanceFromPool($key7);
					if (!$obj7) {
	
						$cls = ArReportOrderOfChildrenPeer::getOMClass(false);

					$obj7 = new $cls();
					$obj7->hydrate($row, $startcol7);
					ArReportOrderOfChildrenPeer::addInstanceToPool($obj7, $key7);
				} // if $obj7 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj7 (ArReportOrderOfChildren)
				$obj7->addArReport($obj1);

			} // if joined row is not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArReport objects pre-filled with all related objects except ArUser.
	 *
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArReport objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinAllExceptArUser(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		// $criteria->getDbName() will return the same object if not set to another value
		// so == check is okay and faster
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArReportPeer::addSelectColumns($criteria);
		$startcol2 = (ArReportPeer::NUM_COLUMNS - ArReportPeer::NUM_LAZY_LOAD_COLUMNS);

		ArReportSetPeer::addSelectColumns($criteria);
		$startcol3 = $startcol2 + (ArReportSetPeer::NUM_COLUMNS - ArReportSetPeer::NUM_LAZY_LOAD_COLUMNS);

		ArReportSetPeer::addSelectColumns($criteria);
		$startcol4 = $startcol3 + (ArReportSetPeer::NUM_COLUMNS - ArReportSetPeer::NUM_LAZY_LOAD_COLUMNS);

		ArOrganizationUnitPeer::addSelectColumns($criteria);
		$startcol5 = $startcol4 + (ArOrganizationUnitPeer::NUM_COLUMNS - ArOrganizationUnitPeer::NUM_LAZY_LOAD_COLUMNS);

		ArVendorPeer::addSelectColumns($criteria);
		$startcol6 = $startcol5 + (ArVendorPeer::NUM_COLUMNS - ArVendorPeer::NUM_LAZY_LOAD_COLUMNS);

		ArTagPeer::addSelectColumns($criteria);
		$startcol7 = $startcol6 + (ArTagPeer::NUM_COLUMNS - ArTagPeer::NUM_LAZY_LOAD_COLUMNS);

		ArReportOrderOfChildrenPeer::addSelectColumns($criteria);
		$startcol8 = $startcol7 + (ArReportOrderOfChildrenPeer::NUM_COLUMNS - ArReportOrderOfChildrenPeer::NUM_LAZY_LOAD_COLUMNS);

		$criteria->addJoin(ArReportPeer::AR_REPORT_SET_ID, ArReportSetPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::ABOUT_AR_REPORT_SET_ID, ArReportSetPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_VENDOR_ID, ArVendorPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_TAG_ID, ArTagPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_REPORT_ORDER_OF_CHILDREN_ID, ArReportOrderOfChildrenPeer::ID, $join_behavior);


		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArReportPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArReportPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {
				$cls = ArReportPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArReportPeer::addInstanceToPool($obj1, $key1);
			} // if obj1 already loaded

				// Add objects for joined ArReportSet rows

				$key2 = ArReportSetPeer::getPrimaryKeyHashFromRow($row, $startcol2);
				if ($key2 !== null) {
					$obj2 = ArReportSetPeer::getInstanceFromPool($key2);
					if (!$obj2) {
	
						$cls = ArReportSetPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol2);
					ArReportSetPeer::addInstanceToPool($obj2, $key2);
				} // if $obj2 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj2 (ArReportSet)
				$obj2->addArReportRelatedByArReportSetId($obj1);

			} // if joined row is not null

				// Add objects for joined ArReportSet rows

				$key3 = ArReportSetPeer::getPrimaryKeyHashFromRow($row, $startcol3);
				if ($key3 !== null) {
					$obj3 = ArReportSetPeer::getInstanceFromPool($key3);
					if (!$obj3) {
	
						$cls = ArReportSetPeer::getOMClass(false);

					$obj3 = new $cls();
					$obj3->hydrate($row, $startcol3);
					ArReportSetPeer::addInstanceToPool($obj3, $key3);
				} // if $obj3 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj3 (ArReportSet)
				$obj3->addArReportRelatedByAboutArReportSetId($obj1);

			} // if joined row is not null

				// Add objects for joined ArOrganizationUnit rows

				$key4 = ArOrganizationUnitPeer::getPrimaryKeyHashFromRow($row, $startcol4);
				if ($key4 !== null) {
					$obj4 = ArOrganizationUnitPeer::getInstanceFromPool($key4);
					if (!$obj4) {
	
						$cls = ArOrganizationUnitPeer::getOMClass(false);

					$obj4 = new $cls();
					$obj4->hydrate($row, $startcol4);
					ArOrganizationUnitPeer::addInstanceToPool($obj4, $key4);
				} // if $obj4 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj4 (ArOrganizationUnit)
				$obj4->addArReport($obj1);

			} // if joined row is not null

				// Add objects for joined ArVendor rows

				$key5 = ArVendorPeer::getPrimaryKeyHashFromRow($row, $startcol5);
				if ($key5 !== null) {
					$obj5 = ArVendorPeer::getInstanceFromPool($key5);
					if (!$obj5) {
	
						$cls = ArVendorPeer::getOMClass(false);

					$obj5 = new $cls();
					$obj5->hydrate($row, $startcol5);
					ArVendorPeer::addInstanceToPool($obj5, $key5);
				} // if $obj5 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj5 (ArVendor)
				$obj5->addArReport($obj1);

			} // if joined row is not null

				// Add objects for joined ArTag rows

				$key6 = ArTagPeer::getPrimaryKeyHashFromRow($row, $startcol6);
				if ($key6 !== null) {
					$obj6 = ArTagPeer::getInstanceFromPool($key6);
					if (!$obj6) {
	
						$cls = ArTagPeer::getOMClass(false);

					$obj6 = new $cls();
					$obj6->hydrate($row, $startcol6);
					ArTagPeer::addInstanceToPool($obj6, $key6);
				} // if $obj6 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj6 (ArTag)
				$obj6->addArReport($obj1);

			} // if joined row is not null

				// Add objects for joined ArReportOrderOfChildren rows

				$key7 = ArReportOrderOfChildrenPeer::getPrimaryKeyHashFromRow($row, $startcol7);
				if ($key7 !== null) {
					$obj7 = ArReportOrderOfChildrenPeer::getInstanceFromPool($key7);
					if (!$obj7) {
	
						$cls = ArReportOrderOfChildrenPeer::getOMClass(false);

					$obj7 = new $cls();
					$obj7->hydrate($row, $startcol7);
					ArReportOrderOfChildrenPeer::addInstanceToPool($obj7, $key7);
				} // if $obj7 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj7 (ArReportOrderOfChildren)
				$obj7->addArReport($obj1);

			} // if joined row is not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArReport objects pre-filled with all related objects except ArVendor.
	 *
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArReport objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinAllExceptArVendor(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		// $criteria->getDbName() will return the same object if not set to another value
		// so == check is okay and faster
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArReportPeer::addSelectColumns($criteria);
		$startcol2 = (ArReportPeer::NUM_COLUMNS - ArReportPeer::NUM_LAZY_LOAD_COLUMNS);

		ArReportSetPeer::addSelectColumns($criteria);
		$startcol3 = $startcol2 + (ArReportSetPeer::NUM_COLUMNS - ArReportSetPeer::NUM_LAZY_LOAD_COLUMNS);

		ArReportSetPeer::addSelectColumns($criteria);
		$startcol4 = $startcol3 + (ArReportSetPeer::NUM_COLUMNS - ArReportSetPeer::NUM_LAZY_LOAD_COLUMNS);

		ArOrganizationUnitPeer::addSelectColumns($criteria);
		$startcol5 = $startcol4 + (ArOrganizationUnitPeer::NUM_COLUMNS - ArOrganizationUnitPeer::NUM_LAZY_LOAD_COLUMNS);

		ArUserPeer::addSelectColumns($criteria);
		$startcol6 = $startcol5 + (ArUserPeer::NUM_COLUMNS - ArUserPeer::NUM_LAZY_LOAD_COLUMNS);

		ArTagPeer::addSelectColumns($criteria);
		$startcol7 = $startcol6 + (ArTagPeer::NUM_COLUMNS - ArTagPeer::NUM_LAZY_LOAD_COLUMNS);

		ArReportOrderOfChildrenPeer::addSelectColumns($criteria);
		$startcol8 = $startcol7 + (ArReportOrderOfChildrenPeer::NUM_COLUMNS - ArReportOrderOfChildrenPeer::NUM_LAZY_LOAD_COLUMNS);

		$criteria->addJoin(ArReportPeer::AR_REPORT_SET_ID, ArReportSetPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::ABOUT_AR_REPORT_SET_ID, ArReportSetPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_USER_ID, ArUserPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_TAG_ID, ArTagPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_REPORT_ORDER_OF_CHILDREN_ID, ArReportOrderOfChildrenPeer::ID, $join_behavior);


		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArReportPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArReportPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {
				$cls = ArReportPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArReportPeer::addInstanceToPool($obj1, $key1);
			} // if obj1 already loaded

				// Add objects for joined ArReportSet rows

				$key2 = ArReportSetPeer::getPrimaryKeyHashFromRow($row, $startcol2);
				if ($key2 !== null) {
					$obj2 = ArReportSetPeer::getInstanceFromPool($key2);
					if (!$obj2) {
	
						$cls = ArReportSetPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol2);
					ArReportSetPeer::addInstanceToPool($obj2, $key2);
				} // if $obj2 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj2 (ArReportSet)
				$obj2->addArReportRelatedByArReportSetId($obj1);

			} // if joined row is not null

				// Add objects for joined ArReportSet rows

				$key3 = ArReportSetPeer::getPrimaryKeyHashFromRow($row, $startcol3);
				if ($key3 !== null) {
					$obj3 = ArReportSetPeer::getInstanceFromPool($key3);
					if (!$obj3) {
	
						$cls = ArReportSetPeer::getOMClass(false);

					$obj3 = new $cls();
					$obj3->hydrate($row, $startcol3);
					ArReportSetPeer::addInstanceToPool($obj3, $key3);
				} // if $obj3 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj3 (ArReportSet)
				$obj3->addArReportRelatedByAboutArReportSetId($obj1);

			} // if joined row is not null

				// Add objects for joined ArOrganizationUnit rows

				$key4 = ArOrganizationUnitPeer::getPrimaryKeyHashFromRow($row, $startcol4);
				if ($key4 !== null) {
					$obj4 = ArOrganizationUnitPeer::getInstanceFromPool($key4);
					if (!$obj4) {
	
						$cls = ArOrganizationUnitPeer::getOMClass(false);

					$obj4 = new $cls();
					$obj4->hydrate($row, $startcol4);
					ArOrganizationUnitPeer::addInstanceToPool($obj4, $key4);
				} // if $obj4 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj4 (ArOrganizationUnit)
				$obj4->addArReport($obj1);

			} // if joined row is not null

				// Add objects for joined ArUser rows

				$key5 = ArUserPeer::getPrimaryKeyHashFromRow($row, $startcol5);
				if ($key5 !== null) {
					$obj5 = ArUserPeer::getInstanceFromPool($key5);
					if (!$obj5) {
	
						$cls = ArUserPeer::getOMClass(false);

					$obj5 = new $cls();
					$obj5->hydrate($row, $startcol5);
					ArUserPeer::addInstanceToPool($obj5, $key5);
				} // if $obj5 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj5 (ArUser)
				$obj5->addArReport($obj1);

			} // if joined row is not null

				// Add objects for joined ArTag rows

				$key6 = ArTagPeer::getPrimaryKeyHashFromRow($row, $startcol6);
				if ($key6 !== null) {
					$obj6 = ArTagPeer::getInstanceFromPool($key6);
					if (!$obj6) {
	
						$cls = ArTagPeer::getOMClass(false);

					$obj6 = new $cls();
					$obj6->hydrate($row, $startcol6);
					ArTagPeer::addInstanceToPool($obj6, $key6);
				} // if $obj6 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj6 (ArTag)
				$obj6->addArReport($obj1);

			} // if joined row is not null

				// Add objects for joined ArReportOrderOfChildren rows

				$key7 = ArReportOrderOfChildrenPeer::getPrimaryKeyHashFromRow($row, $startcol7);
				if ($key7 !== null) {
					$obj7 = ArReportOrderOfChildrenPeer::getInstanceFromPool($key7);
					if (!$obj7) {
	
						$cls = ArReportOrderOfChildrenPeer::getOMClass(false);

					$obj7 = new $cls();
					$obj7->hydrate($row, $startcol7);
					ArReportOrderOfChildrenPeer::addInstanceToPool($obj7, $key7);
				} // if $obj7 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj7 (ArReportOrderOfChildren)
				$obj7->addArReport($obj1);

			} // if joined row is not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArReport objects pre-filled with all related objects except ArTag.
	 *
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArReport objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinAllExceptArTag(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		// $criteria->getDbName() will return the same object if not set to another value
		// so == check is okay and faster
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArReportPeer::addSelectColumns($criteria);
		$startcol2 = (ArReportPeer::NUM_COLUMNS - ArReportPeer::NUM_LAZY_LOAD_COLUMNS);

		ArReportSetPeer::addSelectColumns($criteria);
		$startcol3 = $startcol2 + (ArReportSetPeer::NUM_COLUMNS - ArReportSetPeer::NUM_LAZY_LOAD_COLUMNS);

		ArReportSetPeer::addSelectColumns($criteria);
		$startcol4 = $startcol3 + (ArReportSetPeer::NUM_COLUMNS - ArReportSetPeer::NUM_LAZY_LOAD_COLUMNS);

		ArOrganizationUnitPeer::addSelectColumns($criteria);
		$startcol5 = $startcol4 + (ArOrganizationUnitPeer::NUM_COLUMNS - ArOrganizationUnitPeer::NUM_LAZY_LOAD_COLUMNS);

		ArUserPeer::addSelectColumns($criteria);
		$startcol6 = $startcol5 + (ArUserPeer::NUM_COLUMNS - ArUserPeer::NUM_LAZY_LOAD_COLUMNS);

		ArVendorPeer::addSelectColumns($criteria);
		$startcol7 = $startcol6 + (ArVendorPeer::NUM_COLUMNS - ArVendorPeer::NUM_LAZY_LOAD_COLUMNS);

		ArReportOrderOfChildrenPeer::addSelectColumns($criteria);
		$startcol8 = $startcol7 + (ArReportOrderOfChildrenPeer::NUM_COLUMNS - ArReportOrderOfChildrenPeer::NUM_LAZY_LOAD_COLUMNS);

		$criteria->addJoin(ArReportPeer::AR_REPORT_SET_ID, ArReportSetPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::ABOUT_AR_REPORT_SET_ID, ArReportSetPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_USER_ID, ArUserPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_VENDOR_ID, ArVendorPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_REPORT_ORDER_OF_CHILDREN_ID, ArReportOrderOfChildrenPeer::ID, $join_behavior);


		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArReportPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArReportPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {
				$cls = ArReportPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArReportPeer::addInstanceToPool($obj1, $key1);
			} // if obj1 already loaded

				// Add objects for joined ArReportSet rows

				$key2 = ArReportSetPeer::getPrimaryKeyHashFromRow($row, $startcol2);
				if ($key2 !== null) {
					$obj2 = ArReportSetPeer::getInstanceFromPool($key2);
					if (!$obj2) {
	
						$cls = ArReportSetPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol2);
					ArReportSetPeer::addInstanceToPool($obj2, $key2);
				} // if $obj2 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj2 (ArReportSet)
				$obj2->addArReportRelatedByArReportSetId($obj1);

			} // if joined row is not null

				// Add objects for joined ArReportSet rows

				$key3 = ArReportSetPeer::getPrimaryKeyHashFromRow($row, $startcol3);
				if ($key3 !== null) {
					$obj3 = ArReportSetPeer::getInstanceFromPool($key3);
					if (!$obj3) {
	
						$cls = ArReportSetPeer::getOMClass(false);

					$obj3 = new $cls();
					$obj3->hydrate($row, $startcol3);
					ArReportSetPeer::addInstanceToPool($obj3, $key3);
				} // if $obj3 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj3 (ArReportSet)
				$obj3->addArReportRelatedByAboutArReportSetId($obj1);

			} // if joined row is not null

				// Add objects for joined ArOrganizationUnit rows

				$key4 = ArOrganizationUnitPeer::getPrimaryKeyHashFromRow($row, $startcol4);
				if ($key4 !== null) {
					$obj4 = ArOrganizationUnitPeer::getInstanceFromPool($key4);
					if (!$obj4) {
	
						$cls = ArOrganizationUnitPeer::getOMClass(false);

					$obj4 = new $cls();
					$obj4->hydrate($row, $startcol4);
					ArOrganizationUnitPeer::addInstanceToPool($obj4, $key4);
				} // if $obj4 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj4 (ArOrganizationUnit)
				$obj4->addArReport($obj1);

			} // if joined row is not null

				// Add objects for joined ArUser rows

				$key5 = ArUserPeer::getPrimaryKeyHashFromRow($row, $startcol5);
				if ($key5 !== null) {
					$obj5 = ArUserPeer::getInstanceFromPool($key5);
					if (!$obj5) {
	
						$cls = ArUserPeer::getOMClass(false);

					$obj5 = new $cls();
					$obj5->hydrate($row, $startcol5);
					ArUserPeer::addInstanceToPool($obj5, $key5);
				} // if $obj5 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj5 (ArUser)
				$obj5->addArReport($obj1);

			} // if joined row is not null

				// Add objects for joined ArVendor rows

				$key6 = ArVendorPeer::getPrimaryKeyHashFromRow($row, $startcol6);
				if ($key6 !== null) {
					$obj6 = ArVendorPeer::getInstanceFromPool($key6);
					if (!$obj6) {
	
						$cls = ArVendorPeer::getOMClass(false);

					$obj6 = new $cls();
					$obj6->hydrate($row, $startcol6);
					ArVendorPeer::addInstanceToPool($obj6, $key6);
				} // if $obj6 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj6 (ArVendor)
				$obj6->addArReport($obj1);

			} // if joined row is not null

				// Add objects for joined ArReportOrderOfChildren rows

				$key7 = ArReportOrderOfChildrenPeer::getPrimaryKeyHashFromRow($row, $startcol7);
				if ($key7 !== null) {
					$obj7 = ArReportOrderOfChildrenPeer::getInstanceFromPool($key7);
					if (!$obj7) {
	
						$cls = ArReportOrderOfChildrenPeer::getOMClass(false);

					$obj7 = new $cls();
					$obj7->hydrate($row, $startcol7);
					ArReportOrderOfChildrenPeer::addInstanceToPool($obj7, $key7);
				} // if $obj7 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj7 (ArReportOrderOfChildren)
				$obj7->addArReport($obj1);

			} // if joined row is not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}


	/**
	 * Selects a collection of ArReport objects pre-filled with all related objects except ArReportOrderOfChildren.
	 *
	 * @param      Criteria  $criteria
	 * @param      PropelPDO $con
	 * @param      String    $join_behavior the type of joins to use, defaults to Criteria::LEFT_JOIN
	 * @return     array Array of ArReport objects.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doSelectJoinAllExceptArReportOrderOfChildren(Criteria $criteria, $con = null, $join_behavior = Criteria::LEFT_JOIN)
	{
		$criteria = clone $criteria;

		// Set the correct dbName if it has not been overridden
		// $criteria->getDbName() will return the same object if not set to another value
		// so == check is okay and faster
		if ($criteria->getDbName() == Propel::getDefaultDB()) {
			$criteria->setDbName(self::DATABASE_NAME);
		}

		ArReportPeer::addSelectColumns($criteria);
		$startcol2 = (ArReportPeer::NUM_COLUMNS - ArReportPeer::NUM_LAZY_LOAD_COLUMNS);

		ArReportSetPeer::addSelectColumns($criteria);
		$startcol3 = $startcol2 + (ArReportSetPeer::NUM_COLUMNS - ArReportSetPeer::NUM_LAZY_LOAD_COLUMNS);

		ArReportSetPeer::addSelectColumns($criteria);
		$startcol4 = $startcol3 + (ArReportSetPeer::NUM_COLUMNS - ArReportSetPeer::NUM_LAZY_LOAD_COLUMNS);

		ArOrganizationUnitPeer::addSelectColumns($criteria);
		$startcol5 = $startcol4 + (ArOrganizationUnitPeer::NUM_COLUMNS - ArOrganizationUnitPeer::NUM_LAZY_LOAD_COLUMNS);

		ArUserPeer::addSelectColumns($criteria);
		$startcol6 = $startcol5 + (ArUserPeer::NUM_COLUMNS - ArUserPeer::NUM_LAZY_LOAD_COLUMNS);

		ArVendorPeer::addSelectColumns($criteria);
		$startcol7 = $startcol6 + (ArVendorPeer::NUM_COLUMNS - ArVendorPeer::NUM_LAZY_LOAD_COLUMNS);

		ArTagPeer::addSelectColumns($criteria);
		$startcol8 = $startcol7 + (ArTagPeer::NUM_COLUMNS - ArTagPeer::NUM_LAZY_LOAD_COLUMNS);

		$criteria->addJoin(ArReportPeer::AR_REPORT_SET_ID, ArReportSetPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::ABOUT_AR_REPORT_SET_ID, ArReportSetPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_USER_ID, ArUserPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_VENDOR_ID, ArVendorPeer::ID, $join_behavior);

		$criteria->addJoin(ArReportPeer::AR_TAG_ID, ArTagPeer::ID, $join_behavior);


		$stmt = BasePeer::doSelect($criteria, $con);
		$results = array();

		while ($row = $stmt->fetch(PDO::FETCH_NUM)) {
			$key1 = ArReportPeer::getPrimaryKeyHashFromRow($row, 0);
			if (null !== ($obj1 = ArReportPeer::getInstanceFromPool($key1))) {
				// We no longer rehydrate the object, since this can cause data loss.
				// See http://propel.phpdb.org/trac/ticket/509
				// $obj1->hydrate($row, 0, true); // rehydrate
			} else {
				$cls = ArReportPeer::getOMClass(false);

				$obj1 = new $cls();
				$obj1->hydrate($row);
				ArReportPeer::addInstanceToPool($obj1, $key1);
			} // if obj1 already loaded

				// Add objects for joined ArReportSet rows

				$key2 = ArReportSetPeer::getPrimaryKeyHashFromRow($row, $startcol2);
				if ($key2 !== null) {
					$obj2 = ArReportSetPeer::getInstanceFromPool($key2);
					if (!$obj2) {
	
						$cls = ArReportSetPeer::getOMClass(false);

					$obj2 = new $cls();
					$obj2->hydrate($row, $startcol2);
					ArReportSetPeer::addInstanceToPool($obj2, $key2);
				} // if $obj2 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj2 (ArReportSet)
				$obj2->addArReportRelatedByArReportSetId($obj1);

			} // if joined row is not null

				// Add objects for joined ArReportSet rows

				$key3 = ArReportSetPeer::getPrimaryKeyHashFromRow($row, $startcol3);
				if ($key3 !== null) {
					$obj3 = ArReportSetPeer::getInstanceFromPool($key3);
					if (!$obj3) {
	
						$cls = ArReportSetPeer::getOMClass(false);

					$obj3 = new $cls();
					$obj3->hydrate($row, $startcol3);
					ArReportSetPeer::addInstanceToPool($obj3, $key3);
				} // if $obj3 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj3 (ArReportSet)
				$obj3->addArReportRelatedByAboutArReportSetId($obj1);

			} // if joined row is not null

				// Add objects for joined ArOrganizationUnit rows

				$key4 = ArOrganizationUnitPeer::getPrimaryKeyHashFromRow($row, $startcol4);
				if ($key4 !== null) {
					$obj4 = ArOrganizationUnitPeer::getInstanceFromPool($key4);
					if (!$obj4) {
	
						$cls = ArOrganizationUnitPeer::getOMClass(false);

					$obj4 = new $cls();
					$obj4->hydrate($row, $startcol4);
					ArOrganizationUnitPeer::addInstanceToPool($obj4, $key4);
				} // if $obj4 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj4 (ArOrganizationUnit)
				$obj4->addArReport($obj1);

			} // if joined row is not null

				// Add objects for joined ArUser rows

				$key5 = ArUserPeer::getPrimaryKeyHashFromRow($row, $startcol5);
				if ($key5 !== null) {
					$obj5 = ArUserPeer::getInstanceFromPool($key5);
					if (!$obj5) {
	
						$cls = ArUserPeer::getOMClass(false);

					$obj5 = new $cls();
					$obj5->hydrate($row, $startcol5);
					ArUserPeer::addInstanceToPool($obj5, $key5);
				} // if $obj5 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj5 (ArUser)
				$obj5->addArReport($obj1);

			} // if joined row is not null

				// Add objects for joined ArVendor rows

				$key6 = ArVendorPeer::getPrimaryKeyHashFromRow($row, $startcol6);
				if ($key6 !== null) {
					$obj6 = ArVendorPeer::getInstanceFromPool($key6);
					if (!$obj6) {
	
						$cls = ArVendorPeer::getOMClass(false);

					$obj6 = new $cls();
					$obj6->hydrate($row, $startcol6);
					ArVendorPeer::addInstanceToPool($obj6, $key6);
				} // if $obj6 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj6 (ArVendor)
				$obj6->addArReport($obj1);

			} // if joined row is not null

				// Add objects for joined ArTag rows

				$key7 = ArTagPeer::getPrimaryKeyHashFromRow($row, $startcol7);
				if ($key7 !== null) {
					$obj7 = ArTagPeer::getInstanceFromPool($key7);
					if (!$obj7) {
	
						$cls = ArTagPeer::getOMClass(false);

					$obj7 = new $cls();
					$obj7->hydrate($row, $startcol7);
					ArTagPeer::addInstanceToPool($obj7, $key7);
				} // if $obj7 already loaded

				// Add the $obj1 (ArReport) to the collection in $obj7 (ArTag)
				$obj7->addArReport($obj1);

			} // if joined row is not null

			$results[] = $obj1;
		}
		$stmt->closeCursor();
		return $results;
	}

	/**
	 * Returns the TableMap related to this peer.
	 * This method is not needed for general use but a specific application could have a need.
	 * @return     TableMap
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function getTableMap()
	{
		return Propel::getDatabaseMap(self::DATABASE_NAME)->getTable(self::TABLE_NAME);
	}

	/**
	 * Add a TableMap instance to the database for this peer class.
	 */
	public static function buildTableMap()
	{
	  $dbMap = Propel::getDatabaseMap(BaseArReportPeer::DATABASE_NAME);
	  if (!$dbMap->hasTable(BaseArReportPeer::TABLE_NAME))
	  {
	    $dbMap->addTableObject(new ArReportTableMap());
	  }
	}

	/**
	 * The class that the Peer will make instances of.
	 *
	 * If $withPrefix is true, the returned path
	 * uses a dot-path notation which is tranalted into a path
	 * relative to a location on the PHP include_path.
	 * (e.g. path.to.MyClass -> 'path/to/MyClass.php')
	 *
	 * @param      boolean  Whether or not to return the path wit hthe class name 
	 * @return     string path.to.ClassName
	 */
	public static function getOMClass($withPrefix = true)
	{
		return $withPrefix ? ArReportPeer::CLASS_DEFAULT : ArReportPeer::OM_CLASS;
	}

	/**
	 * Method perform an INSERT on the database, given a ArReport or Criteria object.
	 *
	 * @param      mixed $values Criteria or ArReport object containing data that is used to create the INSERT statement.
	 * @param      PropelPDO $con the PropelPDO connection to use
	 * @return     mixed The new primary key.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doInsert($values, PropelPDO $con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArReportPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		if ($values instanceof Criteria) {
			$criteria = clone $values; // rename for clarity
		} else {
			$criteria = $values->buildCriteria(); // build Criteria from ArReport object
		}

		if ($criteria->containsKey(ArReportPeer::ID) && $criteria->keyContainsValue(ArReportPeer::ID) ) {
			throw new PropelException('Cannot insert a value for auto-increment primary key ('.ArReportPeer::ID.')');
		}


		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		try {
			// use transaction because $criteria could contain info
			// for more than one table (I guess, conceivably)
			$con->beginTransaction();
			$pk = BasePeer::doInsert($criteria, $con);
			$con->commit();
		} catch(PropelException $e) {
			$con->rollBack();
			throw $e;
		}

		return $pk;
	}

	/**
	 * Method perform an UPDATE on the database, given a ArReport or Criteria object.
	 *
	 * @param      mixed $values Criteria or ArReport object containing data that is used to create the UPDATE statement.
	 * @param      PropelPDO $con The connection to use (specify PropelPDO connection object to exert more control over transactions).
	 * @return     int The number of affected rows (if supported by underlying database driver).
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function doUpdate($values, PropelPDO $con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArReportPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		$selectCriteria = new Criteria(self::DATABASE_NAME);

		if ($values instanceof Criteria) {
			$criteria = clone $values; // rename for clarity

			$comparison = $criteria->getComparison(ArReportPeer::ID);
			$selectCriteria->add(ArReportPeer::ID, $criteria->remove(ArReportPeer::ID), $comparison);

		} else { // $values is ArReport object
			$criteria = $values->buildCriteria(); // gets full criteria
			$selectCriteria = $values->buildPkeyCriteria(); // gets criteria w/ primary key(s)
		}

		// set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		return BasePeer::doUpdate($selectCriteria, $criteria, $con);
	}

	/**
	 * Method to DELETE all rows from the ar_report table.
	 *
	 * @return     int The number of affected rows (if supported by underlying database driver).
	 */
	public static function doDeleteAll($con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArReportPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}
		$affectedRows = 0; // initialize var to track total num of affected rows
		try {
			// use transaction because $criteria could contain info
			// for more than one table or we could emulating ON DELETE CASCADE, etc.
			$con->beginTransaction();
			$affectedRows += BasePeer::doDeleteAll(ArReportPeer::TABLE_NAME, $con);
			// Because this db requires some delete cascade/set null emulation, we have to
			// clear the cached instance *after* the emulation has happened (since
			// instances get re-added by the select statement contained therein).
			ArReportPeer::clearInstancePool();
			ArReportPeer::clearRelatedInstancePool();
			$con->commit();
			return $affectedRows;
		} catch (PropelException $e) {
			$con->rollBack();
			throw $e;
		}
	}

	/**
	 * Method perform a DELETE on the database, given a ArReport or Criteria object OR a primary key value.
	 *
	 * @param      mixed $values Criteria or ArReport object or primary key or array of primary keys
	 *              which is used to create the DELETE statement
	 * @param      PropelPDO $con the connection to use
	 * @return     int 	The number of affected rows (if supported by underlying database driver).  This includes CASCADE-related rows
	 *				if supported by native driver or if emulated using Propel.
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	 public static function doDelete($values, PropelPDO $con = null)
	 {
		if ($con === null) {
			$con = Propel::getConnection(ArReportPeer::DATABASE_NAME, Propel::CONNECTION_WRITE);
		}

		if ($values instanceof Criteria) {
			// invalidate the cache for all objects of this type, since we have no
			// way of knowing (without running a query) what objects should be invalidated
			// from the cache based on this Criteria.
			ArReportPeer::clearInstancePool();
			// rename for clarity
			$criteria = clone $values;
		} elseif ($values instanceof ArReport) { // it's a model object
			// invalidate the cache for this single object
			ArReportPeer::removeInstanceFromPool($values);
			// create criteria based on pk values
			$criteria = $values->buildPkeyCriteria();
		} else { // it's a primary key, or an array of pks
			$criteria = new Criteria(self::DATABASE_NAME);
			$criteria->add(ArReportPeer::ID, (array) $values, Criteria::IN);
			// invalidate the cache for this object(s)
			foreach ((array) $values as $singleval) {
				ArReportPeer::removeInstanceFromPool($singleval);
			}
		}

		// Set the correct dbName
		$criteria->setDbName(self::DATABASE_NAME);

		$affectedRows = 0; // initialize var to track total num of affected rows

		try {
			// use transaction because $criteria could contain info
			// for more than one table or we could emulating ON DELETE CASCADE, etc.
			$con->beginTransaction();
			
			$affectedRows += BasePeer::doDelete($criteria, $con);
			ArReportPeer::clearRelatedInstancePool();
			$con->commit();
			return $affectedRows;
		} catch (PropelException $e) {
			$con->rollBack();
			throw $e;
		}
	}

	/**
	 * Validates all modified columns of given ArReport object.
	 * If parameter $columns is either a single column name or an array of column names
	 * than only those columns are validated.
	 *
	 * NOTICE: This does not apply to primary or foreign keys for now.
	 *
	 * @param      ArReport $obj The object to validate.
	 * @param      mixed $cols Column name or array of column names.
	 *
	 * @return     mixed TRUE if all columns are valid or the error message of the first invalid column.
	 */
	public static function doValidate(ArReport $obj, $cols = null)
	{
		$columns = array();

		if ($cols) {
			$dbMap = Propel::getDatabaseMap(ArReportPeer::DATABASE_NAME);
			$tableMap = $dbMap->getTable(ArReportPeer::TABLE_NAME);

			if (! is_array($cols)) {
				$cols = array($cols);
			}

			foreach ($cols as $colName) {
				if ($tableMap->containsColumn($colName)) {
					$get = 'get' . $tableMap->getColumn($colName)->getPhpName();
					$columns[$colName] = $obj->$get();
				}
			}
		} else {

		}

		return BasePeer::doValidate(ArReportPeer::DATABASE_NAME, ArReportPeer::TABLE_NAME, $columns);
	}

	/**
	 * Retrieve a single object by pkey.
	 *
	 * @param      int $pk the primary key.
	 * @param      PropelPDO $con the connection to use
	 * @return     ArReport
	 */
	public static function retrieveByPK($pk, PropelPDO $con = null)
	{

		if (null !== ($obj = ArReportPeer::getInstanceFromPool((string) $pk))) {
			return $obj;
		}

		if ($con === null) {
			$con = Propel::getConnection(ArReportPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$criteria = new Criteria(ArReportPeer::DATABASE_NAME);
		$criteria->add(ArReportPeer::ID, $pk);

		$v = ArReportPeer::doSelect($criteria, $con);

		return !empty($v) > 0 ? $v[0] : null;
	}

	/**
	 * Retrieve multiple objects by pkey.
	 *
	 * @param      array $pks List of primary keys
	 * @param      PropelPDO $con the connection to use
	 * @throws     PropelException Any exceptions caught during processing will be
	 *		 rethrown wrapped into a PropelException.
	 */
	public static function retrieveByPKs($pks, PropelPDO $con = null)
	{
		if ($con === null) {
			$con = Propel::getConnection(ArReportPeer::DATABASE_NAME, Propel::CONNECTION_READ);
		}

		$objs = null;
		if (empty($pks)) {
			$objs = array();
		} else {
			$criteria = new Criteria(ArReportPeer::DATABASE_NAME);
			$criteria->add(ArReportPeer::ID, $pks, Criteria::IN);
			$objs = ArReportPeer::doSelect($criteria, $con);
		}
		return $objs;
	}

	// symfony behavior
	
	/**
	 * Returns an array of arrays that contain columns in each unique index.
	 *
	 * @return array
	 */
	static public function getUniqueColumnNames()
	{
	  return array();
	}

} // BaseArReportPeer

// This is the static code needed to register the TableMap for this table with the main Propel class.
//
BaseArReportPeer::buildTableMap();

