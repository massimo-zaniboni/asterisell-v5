<?php

/**
 * ArReportAlsoFor form base class.
 *
 * @method ArReportAlsoFor getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArReportAlsoForForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_report_id' => new sfWidgetFormInputHidden(),
      'ar_role_id'   => new sfWidgetFormInputHidden(),
    ));

    $this->setValidators(array(
      'ar_report_id' => new sfValidatorPropelChoice(array('model' => 'ArReport', 'column' => 'id', 'required' => false)),
      'ar_role_id'   => new sfValidatorPropelChoice(array('model' => 'ArRole', 'column' => 'id', 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_report_also_for[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArReportAlsoFor';
  }


}
