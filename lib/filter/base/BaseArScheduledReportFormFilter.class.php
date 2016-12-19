<?php

/**
 * ArScheduledReport filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArScheduledReportFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'php_class_serialization'                => new sfWidgetFormFilterInput(),
      'is_active'                              => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'ar_organization_unit_id'                => new sfWidgetFormPropelChoice(array('model' => 'ArOrganizationUnit', 'add_empty' => true)),
      'note'                                   => new sfWidgetFormFilterInput(),
      'ar_user_id'                             => new sfWidgetFormPropelChoice(array('model' => 'ArUser', 'add_empty' => true)),
      'produced_report_short_description'      => new sfWidgetFormFilterInput(),
      'produced_report_additional_description' => new sfWidgetFormFilterInput(),
    ));

    $this->setValidators(array(
      'php_class_serialization'                => new sfValidatorPass(array('required' => false)),
      'is_active'                              => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'ar_organization_unit_id'                => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArOrganizationUnit', 'column' => 'id')),
      'note'                                   => new sfValidatorPass(array('required' => false)),
      'ar_user_id'                             => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArUser', 'column' => 'id')),
      'produced_report_short_description'      => new sfValidatorPass(array('required' => false)),
      'produced_report_additional_description' => new sfValidatorPass(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_scheduled_report_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArScheduledReport';
  }

  public function getFields()
  {
    return array(
      'id'                                     => 'Number',
      'php_class_serialization'                => 'Text',
      'is_active'                              => 'Boolean',
      'ar_organization_unit_id'                => 'ForeignKey',
      'note'                                   => 'Text',
      'ar_user_id'                             => 'ForeignKey',
      'produced_report_short_description'      => 'Text',
      'produced_report_additional_description' => 'Text',
    );
  }
}
