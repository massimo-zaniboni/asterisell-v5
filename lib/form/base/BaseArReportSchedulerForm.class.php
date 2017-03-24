<?php

/**
 * ArReportScheduler form base class.
 *
 * @method ArReportScheduler getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArReportSchedulerForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                                          => new sfWidgetFormInputHidden(),
      'is_active'                                   => new sfWidgetFormInputCheckbox(),
      'last_execution_date'                         => new sfWidgetFormDateTime(),
      'last_from_date'                              => new sfWidgetFormDateTime(),
      'last_to_date'                                => new sfWidgetFormDateTime(),
      'ar_report_id'                                => new sfWidgetFormPropelChoice(array('model' => 'ArReport', 'add_empty' => true)),
      'ar_organization_unit_id'                     => new sfWidgetFormPropelChoice(array('model' => 'ArOrganizationUnit', 'add_empty' => true)),
      'short_description'                           => new sfWidgetFormInputText(),
      'additional_description'                      => new sfWidgetFormInputText(),
      'note'                                        => new sfWidgetFormInputText(),
      'produced_report_must_be_reviewed'            => new sfWidgetFormInputCheckbox(),
      'ar_report_generation_id'                     => new sfWidgetFormPropelChoice(array('model' => 'ArReportGeneration', 'add_empty' => true)),
      'schedule_every_x_days'                       => new sfWidgetFormInputText(),
      'schedule_every_x_months'                     => new sfWidgetFormInputText(),
      'start_generation_after_x_hours'              => new sfWidgetFormInputText(),
      'internal_name'                               => new sfWidgetFormInputText(),
      'ar_legal_date_generation_method_id'          => new sfWidgetFormInputText(),
      'days_to_add_to_legal_date_generation_method' => new sfWidgetFormInputText(),
      'is_yearly_legal_numeration'                  => new sfWidgetFormInputCheckbox(),
      'generate_only_if_there_is_cost'              => new sfWidgetFormInputCheckbox(),
      'minimum_cost'                                => new sfWidgetFormInputText(),
      'send_compact_report_list_to_accountant'      => new sfWidgetFormInputCheckbox(),
    ));

    $this->setValidators(array(
      'id'                                          => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'is_active'                                   => new sfValidatorBoolean(array('required' => false)),
      'last_execution_date'                         => new sfValidatorDateTime(array('required' => false)),
      'last_from_date'                              => new sfValidatorDateTime(array('required' => false)),
      'last_to_date'                                => new sfValidatorDateTime(array('required' => false)),
      'ar_report_id'                                => new sfValidatorPropelChoice(array('model' => 'ArReport', 'column' => 'id', 'required' => false)),
      'ar_organization_unit_id'                     => new sfValidatorPropelChoice(array('model' => 'ArOrganizationUnit', 'column' => 'id', 'required' => false)),
      'short_description'                           => new sfValidatorString(array('max_length' => 2048, 'required' => false)),
      'additional_description'                      => new sfValidatorString(array('max_length' => 2048, 'required' => false)),
      'note'                                        => new sfValidatorString(array('max_length' => 2048, 'required' => false)),
      'produced_report_must_be_reviewed'            => new sfValidatorBoolean(),
      'ar_report_generation_id'                     => new sfValidatorPropelChoice(array('model' => 'ArReportGeneration', 'column' => 'id', 'required' => false)),
      'schedule_every_x_days'                       => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'schedule_every_x_months'                     => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'start_generation_after_x_hours'              => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647)),
      'internal_name'                               => new sfValidatorString(array('max_length' => 512, 'required' => false)),
      'ar_legal_date_generation_method_id'          => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'days_to_add_to_legal_date_generation_method' => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'is_yearly_legal_numeration'                  => new sfValidatorBoolean(array('required' => false)),
      'generate_only_if_there_is_cost'              => new sfValidatorBoolean(),
      'minimum_cost'                                => new sfValidatorInteger(array('min' => -9.2233720368548E+18, 'max' => 9223372036854775807, 'required' => false)),
      'send_compact_report_list_to_accountant'      => new sfValidatorBoolean(),
    ));

    $this->widgetSchema->setNameFormat('ar_report_scheduler[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArReportScheduler';
  }


}
