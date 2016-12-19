<?php

/**
 * ArReportNeededPermissions filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArReportNeededPermissionsFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_report_id'     => new sfWidgetFormPropelChoice(array('model' => 'ArReport', 'add_empty' => true)),
      'ar_permission_id' => new sfWidgetFormPropelChoice(array('model' => 'ArPermission', 'add_empty' => true)),
    ));

    $this->setValidators(array(
      'ar_report_id'     => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArReport', 'column' => 'id')),
      'ar_permission_id' => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArPermission', 'column' => 'id')),
    ));

    $this->widgetSchema->setNameFormat('ar_report_needed_permissions_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArReportNeededPermissions';
  }

  public function getFields()
  {
    return array(
      'id'               => 'Number',
      'ar_report_id'     => 'ForeignKey',
      'ar_permission_id' => 'ForeignKey',
    );
  }
}
