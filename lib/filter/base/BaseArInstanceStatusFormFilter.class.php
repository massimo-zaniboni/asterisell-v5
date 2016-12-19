<?php

/**
 * ArInstanceStatus filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArInstanceStatusFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'internal_name'                              => new sfWidgetFormFilterInput(),
      'instance_code'                              => new sfWidgetFormFilterInput(),
      'ar_organization_unit_id'                    => new sfWidgetFormPropelChoice(array('model' => 'ArOrganizationUnit', 'add_empty' => true)),
      'application_version'                        => new sfWidgetFormFilterInput(),
      'nr_of_critical_errors'                      => new sfWidgetFormFilterInput(),
      'nr_of_important_errors'                     => new sfWidgetFormFilterInput(),
      'nr_of_warning_errors'                       => new sfWidgetFormFilterInput(),
      'nr_of_extensions'                           => new sfWidgetFormFilterInput(),
      'nr_of_unspecified_extensions'               => new sfWidgetFormFilterInput(),
      'property_nr_errors_outgoing_previous_month' => new sfWidgetFormFilterInput(),
      'property_nr_errors_incoming_previous_month' => new sfWidgetFormFilterInput(),
      'property_nr_errors_internal_previous_month' => new sfWidgetFormFilterInput(),
      'property_nr_errors_outgoing_last_30_days'   => new sfWidgetFormFilterInput(),
      'property_nr_errors_incoming_last_30_days'   => new sfWidgetFormFilterInput(),
      'property_nr_errors_internal_last_30_days'   => new sfWidgetFormFilterInput(),
      'property_nr_outgoing_previous_month'        => new sfWidgetFormFilterInput(),
      'property_nr_incoming_previous_month'        => new sfWidgetFormFilterInput(),
      'property_nr_internal_previous_month'        => new sfWidgetFormFilterInput(),
      'property_nr_outgoing_last_30_days'          => new sfWidgetFormFilterInput(),
      'property_nr_incoming_last_30_days'          => new sfWidgetFormFilterInput(),
      'property_nr_internal_last_30_days'          => new sfWidgetFormFilterInput(),
      'last_processed_cdr_timestamp'               => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'info_timestamp'                             => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
    ));

    $this->setValidators(array(
      'internal_name'                              => new sfValidatorPass(array('required' => false)),
      'instance_code'                              => new sfValidatorPass(array('required' => false)),
      'ar_organization_unit_id'                    => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArOrganizationUnit', 'column' => 'id')),
      'application_version'                        => new sfValidatorPass(array('required' => false)),
      'nr_of_critical_errors'                      => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'nr_of_important_errors'                     => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'nr_of_warning_errors'                       => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'nr_of_extensions'                           => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'nr_of_unspecified_extensions'               => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'property_nr_errors_outgoing_previous_month' => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'property_nr_errors_incoming_previous_month' => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'property_nr_errors_internal_previous_month' => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'property_nr_errors_outgoing_last_30_days'   => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'property_nr_errors_incoming_last_30_days'   => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'property_nr_errors_internal_last_30_days'   => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'property_nr_outgoing_previous_month'        => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'property_nr_incoming_previous_month'        => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'property_nr_internal_previous_month'        => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'property_nr_outgoing_last_30_days'          => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'property_nr_incoming_last_30_days'          => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'property_nr_internal_last_30_days'          => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'last_processed_cdr_timestamp'               => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'info_timestamp'                             => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
    ));

    $this->widgetSchema->setNameFormat('ar_instance_status_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArInstanceStatus';
  }

  public function getFields()
  {
    return array(
      'id'                                         => 'Number',
      'internal_name'                              => 'Text',
      'instance_code'                              => 'Text',
      'ar_organization_unit_id'                    => 'ForeignKey',
      'application_version'                        => 'Text',
      'nr_of_critical_errors'                      => 'Number',
      'nr_of_important_errors'                     => 'Number',
      'nr_of_warning_errors'                       => 'Number',
      'nr_of_extensions'                           => 'Number',
      'nr_of_unspecified_extensions'               => 'Number',
      'property_nr_errors_outgoing_previous_month' => 'Number',
      'property_nr_errors_incoming_previous_month' => 'Number',
      'property_nr_errors_internal_previous_month' => 'Number',
      'property_nr_errors_outgoing_last_30_days'   => 'Number',
      'property_nr_errors_incoming_last_30_days'   => 'Number',
      'property_nr_errors_internal_last_30_days'   => 'Number',
      'property_nr_outgoing_previous_month'        => 'Number',
      'property_nr_incoming_previous_month'        => 'Number',
      'property_nr_internal_previous_month'        => 'Number',
      'property_nr_outgoing_last_30_days'          => 'Number',
      'property_nr_incoming_last_30_days'          => 'Number',
      'property_nr_internal_last_30_days'          => 'Number',
      'last_processed_cdr_timestamp'               => 'Date',
      'info_timestamp'                             => 'Date',
    );
  }
}
