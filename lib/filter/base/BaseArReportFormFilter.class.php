<?php

/**
 * ArReport filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArReportFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'is_template'                                 => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'ar_report_set_id'                            => new sfWidgetFormPropelChoice(array('model' => 'ArReportSet', 'add_empty' => true)),
      'ar_organization_unit_id'                     => new sfWidgetFormPropelChoice(array('model' => 'ArOrganizationUnit', 'add_empty' => true)),
      'ar_user_id'                                  => new sfWidgetFormPropelChoice(array('model' => 'ArUser', 'add_empty' => true)),
      'ar_vendor_id'                                => new sfWidgetFormPropelChoice(array('model' => 'ArVendor', 'add_empty' => true)),
      'from_date'                                   => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'to_date'                                     => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'param_show_masked_telephone_numbers'         => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'param_show_call_cost'                        => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'param_show_call_income'                      => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'param_show_also_outgoing_calls'              => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'param_show_also_system_calls'                => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'param_show_also_incoming_calls'              => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'param_show_also_internal_calls'              => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'param_show_call_details'                     => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'param_show_voip_provider'                    => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'param_show_communication_channel'            => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'param_show_geographic_location'              => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'param_show_connection_type'                  => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'param_show_cost_saving'                      => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'param_is_legal'                              => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'param_expand_to_level'                       => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'ar_report_order_of_children_id'              => new sfWidgetFormPropelChoice(array('model' => 'ArReportOrderOfChildren', 'add_empty' => true)),
      'php_class_name'                              => new sfWidgetFormFilterInput(),
      'produced_report_generation_date'             => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'report_name'                                 => new sfWidgetFormFilterInput(),
      'produced_report_short_description'           => new sfWidgetFormFilterInput(),
      'produced_report_additional_description'      => new sfWidgetFormFilterInput(),
      'produced_report_already_reviewed'            => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'produced_report_is_draft'                    => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'produced_report_must_be_regenerated'         => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'produced_report_mime_type'                   => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'produced_report_file_type_suffix'            => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'produced_report_document'                    => new sfWidgetFormFilterInput(),
      'produced_report_document_checksum'           => new sfWidgetFormFilterInput(),
      'report_mail_subject'                         => new sfWidgetFormFilterInput(),
      'report_mail_body'                            => new sfWidgetFormFilterInput(),
      'report_attachment_file_name'                 => new sfWidgetFormFilterInput(),
      'report_attachment_file_name_add_report_date' => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'internal_name'                               => new sfWidgetFormFilterInput(),
      'cached_parent_id_hierarchy'                  => new sfWidgetFormFilterInput(),
      'legal_nr_prefix'                             => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'legal_consecutive_nr'                        => new sfWidgetFormFilterInput(),
      'legal_date'                                  => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'legal_sender_name'                           => new sfWidgetFormFilterInput(),
      'legal_sender_vat'                            => new sfWidgetFormFilterInput(),
      'legal_sender_address'                        => new sfWidgetFormFilterInput(),
      'legal_receiver_name'                         => new sfWidgetFormFilterInput(),
      'legal_receiver_vat'                          => new sfWidgetFormFilterInput(),
      'legal_receiver_address'                      => new sfWidgetFormFilterInput(),
      'total_without_tax'                           => new sfWidgetFormFilterInput(),
      'tax'                                         => new sfWidgetFormFilterInput(),
      'applied_vat'                                 => new sfWidgetFormFilterInput(),
      'total_with_tax'                              => new sfWidgetFormFilterInput(),
      'ar_report_also_for_list'                     => new sfWidgetFormPropelChoice(array('model' => 'ArRole', 'add_empty' => true)),
      'ar_user_can_view_report_list'                => new sfWidgetFormPropelChoice(array('model' => 'ArUser', 'add_empty' => true)),
    ));

    $this->setValidators(array(
      'is_template'                                 => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'ar_report_set_id'                            => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArReportSet', 'column' => 'id')),
      'ar_organization_unit_id'                     => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArOrganizationUnit', 'column' => 'id')),
      'ar_user_id'                                  => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArUser', 'column' => 'id')),
      'ar_vendor_id'                                => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArVendor', 'column' => 'id')),
      'from_date'                                   => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'to_date'                                     => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'param_show_masked_telephone_numbers'         => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'param_show_call_cost'                        => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'param_show_call_income'                      => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'param_show_also_outgoing_calls'              => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'param_show_also_system_calls'                => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'param_show_also_incoming_calls'              => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'param_show_also_internal_calls'              => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'param_show_call_details'                     => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'param_show_voip_provider'                    => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'param_show_communication_channel'            => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'param_show_geographic_location'              => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'param_show_connection_type'                  => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'param_show_cost_saving'                      => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'param_is_legal'                              => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'param_expand_to_level'                       => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'ar_report_order_of_children_id'              => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArReportOrderOfChildren', 'column' => 'id')),
      'php_class_name'                              => new sfValidatorPass(array('required' => false)),
      'produced_report_generation_date'             => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'report_name'                                 => new sfValidatorPass(array('required' => false)),
      'produced_report_short_description'           => new sfValidatorPass(array('required' => false)),
      'produced_report_additional_description'      => new sfValidatorPass(array('required' => false)),
      'produced_report_already_reviewed'            => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'produced_report_is_draft'                    => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'produced_report_must_be_regenerated'         => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'produced_report_mime_type'                   => new sfValidatorPass(array('required' => false)),
      'produced_report_file_type_suffix'            => new sfValidatorPass(array('required' => false)),
      'produced_report_document'                    => new sfValidatorPass(array('required' => false)),
      'produced_report_document_checksum'           => new sfValidatorPass(array('required' => false)),
      'report_mail_subject'                         => new sfValidatorPass(array('required' => false)),
      'report_mail_body'                            => new sfValidatorPass(array('required' => false)),
      'report_attachment_file_name'                 => new sfValidatorPass(array('required' => false)),
      'report_attachment_file_name_add_report_date' => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'internal_name'                               => new sfValidatorPass(array('required' => false)),
      'cached_parent_id_hierarchy'                  => new sfValidatorPass(array('required' => false)),
      'legal_nr_prefix'                             => new sfValidatorPass(array('required' => false)),
      'legal_consecutive_nr'                        => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'legal_date'                                  => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'legal_sender_name'                           => new sfValidatorPass(array('required' => false)),
      'legal_sender_vat'                            => new sfValidatorPass(array('required' => false)),
      'legal_sender_address'                        => new sfValidatorPass(array('required' => false)),
      'legal_receiver_name'                         => new sfValidatorPass(array('required' => false)),
      'legal_receiver_vat'                          => new sfValidatorPass(array('required' => false)),
      'legal_receiver_address'                      => new sfValidatorPass(array('required' => false)),
      'total_without_tax'                           => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'tax'                                         => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'applied_vat'                                 => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'total_with_tax'                              => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'ar_report_also_for_list'                     => new sfValidatorPropelChoice(array('model' => 'ArRole', 'required' => false)),
      'ar_user_can_view_report_list'                => new sfValidatorPropelChoice(array('model' => 'ArUser', 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_report_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function addArReportAlsoForListColumnCriteria(Criteria $criteria, $field, $values)
  {
    if (!is_array($values))
    {
      $values = array($values);
    }

    if (!count($values))
    {
      return;
    }

    $criteria->addJoin(ArReportAlsoForPeer::AR_REPORT_ID, ArReportPeer::ID);

    $value = array_pop($values);
    $criterion = $criteria->getNewCriterion(ArReportAlsoForPeer::AR_ROLE_ID, $value);

    foreach ($values as $value)
    {
      $criterion->addOr($criteria->getNewCriterion(ArReportAlsoForPeer::AR_ROLE_ID, $value));
    }

    $criteria->add($criterion);
  }

  public function addArUserCanViewReportListColumnCriteria(Criteria $criteria, $field, $values)
  {
    if (!is_array($values))
    {
      $values = array($values);
    }

    if (!count($values))
    {
      return;
    }

    $criteria->addJoin(ArUserCanViewReportPeer::AR_REPORT_ID, ArReportPeer::ID);

    $value = array_pop($values);
    $criterion = $criteria->getNewCriterion(ArUserCanViewReportPeer::AR_USER_ID, $value);

    foreach ($values as $value)
    {
      $criterion->addOr($criteria->getNewCriterion(ArUserCanViewReportPeer::AR_USER_ID, $value));
    }

    $criteria->add($criterion);
  }

  public function getModelName()
  {
    return 'ArReport';
  }

  public function getFields()
  {
    return array(
      'id'                                          => 'Number',
      'is_template'                                 => 'Boolean',
      'ar_report_set_id'                            => 'ForeignKey',
      'ar_organization_unit_id'                     => 'ForeignKey',
      'ar_user_id'                                  => 'ForeignKey',
      'ar_vendor_id'                                => 'ForeignKey',
      'from_date'                                   => 'Date',
      'to_date'                                     => 'Date',
      'param_show_masked_telephone_numbers'         => 'Boolean',
      'param_show_call_cost'                        => 'Boolean',
      'param_show_call_income'                      => 'Boolean',
      'param_show_also_outgoing_calls'              => 'Boolean',
      'param_show_also_system_calls'                => 'Boolean',
      'param_show_also_incoming_calls'              => 'Boolean',
      'param_show_also_internal_calls'              => 'Boolean',
      'param_show_call_details'                     => 'Boolean',
      'param_show_voip_provider'                    => 'Boolean',
      'param_show_communication_channel'            => 'Boolean',
      'param_show_geographic_location'              => 'Boolean',
      'param_show_connection_type'                  => 'Boolean',
      'param_show_cost_saving'                      => 'Boolean',
      'param_is_legal'                              => 'Boolean',
      'param_expand_to_level'                       => 'Number',
      'ar_report_order_of_children_id'              => 'ForeignKey',
      'php_class_name'                              => 'Text',
      'produced_report_generation_date'             => 'Date',
      'report_name'                                 => 'Text',
      'produced_report_short_description'           => 'Text',
      'produced_report_additional_description'      => 'Text',
      'produced_report_already_reviewed'            => 'Boolean',
      'produced_report_is_draft'                    => 'Boolean',
      'produced_report_must_be_regenerated'         => 'Boolean',
      'produced_report_mime_type'                   => 'Text',
      'produced_report_file_type_suffix'            => 'Text',
      'produced_report_document'                    => 'Text',
      'produced_report_document_checksum'           => 'Text',
      'report_mail_subject'                         => 'Text',
      'report_mail_body'                            => 'Text',
      'report_attachment_file_name'                 => 'Text',
      'report_attachment_file_name_add_report_date' => 'Boolean',
      'internal_name'                               => 'Text',
      'cached_parent_id_hierarchy'                  => 'Text',
      'legal_nr_prefix'                             => 'Text',
      'legal_consecutive_nr'                        => 'Number',
      'legal_date'                                  => 'Date',
      'legal_sender_name'                           => 'Text',
      'legal_sender_vat'                            => 'Text',
      'legal_sender_address'                        => 'Text',
      'legal_receiver_name'                         => 'Text',
      'legal_receiver_vat'                          => 'Text',
      'legal_receiver_address'                      => 'Text',
      'total_without_tax'                           => 'Number',
      'tax'                                         => 'Number',
      'applied_vat'                                 => 'Number',
      'total_with_tax'                              => 'Number',
      'ar_report_also_for_list'                     => 'ManyKey',
      'ar_user_can_view_report_list'                => 'ManyKey',
    );
  }
}
