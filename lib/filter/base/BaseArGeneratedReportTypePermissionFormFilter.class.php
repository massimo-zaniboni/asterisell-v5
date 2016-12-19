<?php

/**
 * ArGeneratedReportTypePermission filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArGeneratedReportTypePermissionFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_generated_report_type_id' => new sfWidgetFormPropelChoice(array('model' => 'ArGeneratedReportType', 'add_empty' => true)),
      'ar_permission_id'            => new sfWidgetFormPropelChoice(array('model' => 'ArPermission', 'add_empty' => true)),
    ));

    $this->setValidators(array(
      'ar_generated_report_type_id' => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArGeneratedReportType', 'column' => 'id')),
      'ar_permission_id'            => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArPermission', 'column' => 'id')),
    ));

    $this->widgetSchema->setNameFormat('ar_generated_report_type_permission_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArGeneratedReportTypePermission';
  }

  public function getFields()
  {
    return array(
      'ar_generated_report_type_id' => 'ForeignKey',
      'ar_permission_id'            => 'ForeignKey',
      'id'                          => 'Number',
    );
  }
}
