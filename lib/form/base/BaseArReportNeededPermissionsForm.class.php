<?php

/**
 * ArReportNeededPermissions form base class.
 *
 * @method ArReportNeededPermissions getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArReportNeededPermissionsForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'               => new sfWidgetFormInputHidden(),
      'ar_report_id'     => new sfWidgetFormPropelChoice(array('model' => 'ArReport', 'add_empty' => true)),
      'ar_permission_id' => new sfWidgetFormPropelChoice(array('model' => 'ArPermission', 'add_empty' => true)),
    ));

    $this->setValidators(array(
      'id'               => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'ar_report_id'     => new sfValidatorPropelChoice(array('model' => 'ArReport', 'column' => 'id', 'required' => false)),
      'ar_permission_id' => new sfValidatorPropelChoice(array('model' => 'ArPermission', 'column' => 'id', 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_report_needed_permissions[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArReportNeededPermissions';
  }


}
