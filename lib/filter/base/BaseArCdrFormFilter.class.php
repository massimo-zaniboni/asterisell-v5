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
      'to_calldate'                                        => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'count_of_calls'                                     => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'destination_type'                                   => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'is_redirect'                                        => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'duration'                                           => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'billsec'                                            => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'ar_organization_unit_id'                            => new sfWidgetFormFilterInput(),
      'cached_parent_id_hierarchy'                         => new sfWidgetFormFilterInput(),
      'billable_ar_organization_unit_id'                   => new sfWidgetFormFilterInput(),
      'bundle_ar_organization_unit_id'                     => new sfWidgetFormFilterInput(),
      'income'                                             => new sfWidgetFormFilterInput(),
      'cost_saving'                                        => new sfWidgetFormFilterInput(),
      'ar_vendor_id'                                       => new sfWidgetFormFilterInput(),
      'ar_communication_channel_type_id'                   => new sfWidgetFormFilterInput(),
      'cost'                                               => new sfWidgetFormFilterInput(),
      'expected_cost'                                      => new sfWidgetFormFilterInput(),
      'ar_telephone_prefix_id'                             => new sfWidgetFormFilterInput(),
      'cached_external_telephone_number'                   => new sfWidgetFormFilterInput(),
      'external_telephone_number_with_applied_portability' => new sfWidgetFormFilterInput(),
      'cached_masked_external_telephone_number'            => new sfWidgetFormFilterInput(),
      'error_destination_type'                             => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'ar_problem_duplication_key'                         => new sfWidgetFormFilterInput(),
      'debug_cost_rate'                                    => new sfWidgetFormFilterInput(),
      'debug_income_rate'                                  => new sfWidgetFormFilterInput(),
      'imported_info'                                      => new sfWidgetFormFilterInput(),
      'exported_internal_telephone_number'                 => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'exported_billable_customer_ar_party_id'             => new sfWidgetFormFilterInput(),
      'from_source_cdr_id'                                 => new sfWidgetFormFilterInput(),
    ));

    $this->setValidators(array(
      'to_calldate'                                        => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'count_of_calls'                                     => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'destination_type'                                   => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'is_redirect'                                        => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'duration'                                           => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'billsec'                                            => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'ar_organization_unit_id'                            => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'cached_parent_id_hierarchy'                         => new sfValidatorPass(array('required' => false)),
      'billable_ar_organization_unit_id'                   => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'bundle_ar_organization_unit_id'                     => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'income'                                             => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'cost_saving'                                        => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'ar_vendor_id'                                       => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'ar_communication_channel_type_id'                   => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'cost'                                               => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'expected_cost'                                      => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'ar_telephone_prefix_id'                             => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'cached_external_telephone_number'                   => new sfValidatorPass(array('required' => false)),
      'external_telephone_number_with_applied_portability' => new sfValidatorPass(array('required' => false)),
      'cached_masked_external_telephone_number'            => new sfValidatorPass(array('required' => false)),
      'error_destination_type'                             => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'ar_problem_duplication_key'                         => new sfValidatorPass(array('required' => false)),
      'debug_cost_rate'                                    => new sfValidatorPass(array('required' => false)),
      'debug_income_rate'                                  => new sfValidatorPass(array('required' => false)),
      'imported_info'                                      => new sfValidatorPass(array('required' => false)),
      'exported_internal_telephone_number'                 => new sfValidatorPass(array('required' => false)),
      'exported_billable_customer_ar_party_id'             => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'from_source_cdr_id'                                 => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
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
      'calldate'                                           => 'Date',
      'id'                                                 => 'Number',
      'is_service_cdr'                                     => 'Boolean',
      'to_calldate'                                        => 'Date',
      'count_of_calls'                                     => 'Number',
      'destination_type'                                   => 'Number',
      'is_redirect'                                        => 'Boolean',
      'duration'                                           => 'Number',
      'billsec'                                            => 'Number',
      'ar_organization_unit_id'                            => 'Number',
      'cached_parent_id_hierarchy'                         => 'Text',
      'billable_ar_organization_unit_id'                   => 'Number',
      'bundle_ar_organization_unit_id'                     => 'Number',
      'income'                                             => 'Number',
      'cost_saving'                                        => 'Number',
      'ar_vendor_id'                                       => 'Number',
      'ar_communication_channel_type_id'                   => 'Number',
      'cost'                                               => 'Number',
      'expected_cost'                                      => 'Number',
      'ar_telephone_prefix_id'                             => 'Number',
      'cached_external_telephone_number'                   => 'Text',
      'external_telephone_number_with_applied_portability' => 'Text',
      'cached_masked_external_telephone_number'            => 'Text',
      'error_destination_type'                             => 'Number',
      'ar_problem_duplication_key'                         => 'Text',
      'debug_cost_rate'                                    => 'Text',
      'debug_income_rate'                                  => 'Text',
      'imported_info'                                      => 'Text',
      'exported_internal_telephone_number'                 => 'Text',
      'exported_billable_customer_ar_party_id'             => 'Number',
      'from_source_cdr_id'                                 => 'Number',
    );
  }
}
