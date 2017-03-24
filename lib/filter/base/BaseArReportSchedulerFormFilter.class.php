<?php

/**
 * ArReportScheduler filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArReportSchedulerFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'is_active'                                   => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'last_execution_date'                         => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'last_from_date'                              => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'last_to_date'                                => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'ar_report_id'                                => new sfWidgetFormPropelChoice(array('model' => 'ArReport', 'add_empty' => true)),
      'ar_organization_unit_id'                     => new sfWidgetFormPropelChoice(array('model' => 'ArOrganizationUnit', 'add_empty' => true)),
      'short_description'                           => new sfWidgetFormFilterInput(),
      'additional_description'                      => new sfWidgetFormFilterInput(),
      'note'                                        => new sfWidgetFormFilterInput(),
      'produced_report_must_be_reviewed'            => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'ar_report_generation_id'                     => new sfWidgetFormPropelChoice(array('model' => 'ArReportGeneration', 'add_empty' => true)),
      'schedule_every_x_days'                       => new sfWidgetFormFilterInput(),
      'schedule_every_x_months'                     => new sfWidgetFormFilterInput(),
      'start_generation_after_x_hours'              => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'internal_name'                               => new sfWidgetFormFilterInput(),
      'ar_legal_date_generation_method_id'          => new sfWidgetFormFilterInput(),
      'days_to_add_to_legal_date_generation_method' => new sfWidgetFormFilterInput(),
      'is_yearly_legal_numeration'                  => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'generate_only_if_there_is_cost'              => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'minimum_cost'                                => new sfWidgetFormFilterInput(),
      'send_compact_report_list_to_accountant'      => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
    ));

    $this->setValidators(array(
      'is_active'                                   => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'last_execution_date'                         => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'last_from_date'                              => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'last_to_date'                                => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'ar_report_id'                                => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArReport', 'column' => 'id')),
      'ar_organization_unit_id'                     => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArOrganizationUnit', 'column' => 'id')),
      'short_description'                           => new sfValidatorPass(array('required' => false)),
      'additional_description'                      => new sfValidatorPass(array('required' => false)),
      'note'                                        => new sfValidatorPass(array('required' => false)),
      'produced_report_must_be_reviewed'            => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'ar_report_generation_id'                     => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArReportGeneration', 'column' => 'id')),
      'schedule_every_x_days'                       => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'schedule_every_x_months'                     => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'start_generation_after_x_hours'              => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'internal_name'                               => new sfValidatorPass(array('required' => false)),
      'ar_legal_date_generation_method_id'          => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'days_to_add_to_legal_date_generation_method' => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'is_yearly_legal_numeration'                  => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'generate_only_if_there_is_cost'              => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'minimum_cost'                                => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'send_compact_report_list_to_accountant'      => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
    ));

    $this->widgetSchema->setNameFormat('ar_report_scheduler_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArReportScheduler';
  }

  public function getFields()
  {
    return array(
      'id'                                          => 'Number',
      'is_active'                                   => 'Boolean',
      'last_execution_date'                         => 'Date',
      'last_from_date'                              => 'Date',
      'last_to_date'                                => 'Date',
      'ar_report_id'                                => 'ForeignKey',
      'ar_organization_unit_id'                     => 'ForeignKey',
      'short_description'                           => 'Text',
      'additional_description'                      => 'Text',
      'note'                                        => 'Text',
      'produced_report_must_be_reviewed'            => 'Boolean',
      'ar_report_generation_id'                     => 'ForeignKey',
      'schedule_every_x_days'                       => 'Number',
      'schedule_every_x_months'                     => 'Number',
      'start_generation_after_x_hours'              => 'Number',
      'internal_name'                               => 'Text',
      'ar_legal_date_generation_method_id'          => 'Number',
      'days_to_add_to_legal_date_generation_method' => 'Number',
      'is_yearly_legal_numeration'                  => 'Boolean',
      'generate_only_if_there_is_cost'              => 'Boolean',
      'minimum_cost'                                => 'Number',
      'send_compact_report_list_to_accountant'      => 'Boolean',
    );
  }
}
