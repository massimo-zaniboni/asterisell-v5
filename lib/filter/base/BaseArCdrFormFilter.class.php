<?php

/**
 * ArCdr filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArCdrFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'calldate'                                           => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate(), 'with_empty' => false)),
      'to_calldate'                                        => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'is_imported_service_cdr'                            => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'count_of_calls'                                     => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'destination_type'                                   => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'is_redirect'                                        => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'duration'                                           => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'billsec'                                            => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'ar_organization_unit_id'                            => new sfWidgetFormPropelChoice(array('model' => 'ArOrganizationUnit', 'add_empty' => true)),
      'cached_parent_id_hierarchy'                         => new sfWidgetFormFilterInput(),
      'billable_ar_organization_unit_id'                   => new sfWidgetFormFilterInput(),
      'bundle_ar_organization_unit_id'                     => new sfWidgetFormFilterInput(),
      'income'                                             => new sfWidgetFormFilterInput(),
      'cost_saving'                                        => new sfWidgetFormFilterInput(),
      'ar_vendor_id'                                       => new sfWidgetFormPropelChoice(array('model' => 'ArVendor', 'add_empty' => true)),
      'ar_communication_channel_type_id'                   => new sfWidgetFormPropelChoice(array('model' => 'ArCommunicationChannelType', 'add_empty' => true)),
      'cost'                                               => new sfWidgetFormFilterInput(),
      'expected_cost'                                      => new sfWidgetFormFilterInput(),
      'ar_telephone_prefix_id'                             => new sfWidgetFormPropelChoice(array('model' => 'ArTelephonePrefix', 'add_empty' => true)),
      'cached_external_telephone_number'                   => new sfWidgetFormFilterInput(),
      'external_telephone_number_with_applied_portability' => new sfWidgetFormFilterInput(),
      'cached_masked_external_telephone_number'            => new sfWidgetFormFilterInput(),
      'error_destination_type'                             => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'ar_problem_duplication_key'                         => new sfWidgetFormFilterInput(),
      'debug_cost_rate'                                    => new sfWidgetFormFilterInput(),
      'debug_income_rate'                                  => new sfWidgetFormFilterInput(),
      'debug_residual_income_rate'                         => new sfWidgetFormFilterInput(),
      'debug_residual_call_duration'                       => new sfWidgetFormFilterInput(),
      'debug_bundle_left_calls'                            => new sfWidgetFormFilterInput(),
      'debug_bundle_left_duration'                         => new sfWidgetFormFilterInput(),
      'debug_bundle_left_cost'                             => new sfWidgetFormFilterInput(),
      'debug_rating_details'                               => new sfWidgetFormFilterInput(),
    ));

    $this->setValidators(array(
      'calldate'                                           => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'to_calldate'                                        => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'is_imported_service_cdr'                            => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'count_of_calls'                                     => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'destination_type'                                   => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'is_redirect'                                        => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'duration'                                           => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'billsec'                                            => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'ar_organization_unit_id'                            => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArOrganizationUnit', 'column' => 'id')),
      'cached_parent_id_hierarchy'                         => new sfValidatorPass(array('required' => false)),
      'billable_ar_organization_unit_id'                   => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'bundle_ar_organization_unit_id'                     => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'income'                                             => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'cost_saving'                                        => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'ar_vendor_id'                                       => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArVendor', 'column' => 'id')),
      'ar_communication_channel_type_id'                   => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArCommunicationChannelType', 'column' => 'id')),
      'cost'                                               => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'expected_cost'                                      => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'ar_telephone_prefix_id'                             => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArTelephonePrefix', 'column' => 'id')),
      'cached_external_telephone_number'                   => new sfValidatorPass(array('required' => false)),
      'external_telephone_number_with_applied_portability' => new sfValidatorPass(array('required' => false)),
      'cached_masked_external_telephone_number'            => new sfValidatorPass(array('required' => false)),
      'error_destination_type'                             => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'ar_problem_duplication_key'                         => new sfValidatorPass(array('required' => false)),
      'debug_cost_rate'                                    => new sfValidatorPass(array('required' => false)),
      'debug_income_rate'                                  => new sfValidatorPass(array('required' => false)),
      'debug_residual_income_rate'                         => new sfValidatorPass(array('required' => false)),
      'debug_residual_call_duration'                       => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'debug_bundle_left_calls'                            => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'debug_bundle_left_duration'                         => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'debug_bundle_left_cost'                             => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'debug_rating_details'                               => new sfValidatorPass(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_cdr_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArCdr';
  }

  public function getFields()
  {
    return array(
      'id'                                                 => 'Number',
      'calldate'                                           => 'Date',
      'to_calldate'                                        => 'Date',
      'is_imported_service_cdr'                            => 'Boolean',
      'count_of_calls'                                     => 'Number',
      'destination_type'                                   => 'Number',
      'is_redirect'                                        => 'Boolean',
      'duration'                                           => 'Number',
      'billsec'                                            => 'Number',
      'ar_organization_unit_id'                            => 'ForeignKey',
      'cached_parent_id_hierarchy'                         => 'Text',
      'billable_ar_organization_unit_id'                   => 'Number',
      'bundle_ar_organization_unit_id'                     => 'Number',
      'income'                                             => 'Number',
      'cost_saving'                                        => 'Number',
      'ar_vendor_id'                                       => 'ForeignKey',
      'ar_communication_channel_type_id'                   => 'ForeignKey',
      'cost'                                               => 'Number',
      'expected_cost'                                      => 'Number',
      'ar_telephone_prefix_id'                             => 'ForeignKey',
      'cached_external_telephone_number'                   => 'Text',
      'external_telephone_number_with_applied_portability' => 'Text',
      'cached_masked_external_telephone_number'            => 'Text',
      'error_destination_type'                             => 'Number',
      'ar_problem_duplication_key'                         => 'Text',
      'debug_cost_rate'                                    => 'Text',
      'debug_income_rate'                                  => 'Text',
      'debug_residual_income_rate'                         => 'Text',
      'debug_residual_call_duration'                       => 'Number',
      'debug_bundle_left_calls'                            => 'Number',
      'debug_bundle_left_duration'                         => 'Number',
      'debug_bundle_left_cost'                             => 'Number',
      'debug_rating_details'                               => 'Text',
    );
  }
}
