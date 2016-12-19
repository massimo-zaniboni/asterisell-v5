<?php

/**
 * ArUserCanViewReport form base class.
 *
 * @method ArUserCanViewReport getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArUserCanViewReportForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_user_id'   => new sfWidgetFormInputHidden(),
      'ar_report_id' => new sfWidgetFormInputHidden(),
    ));

    $this->setValidators(array(
      'ar_user_id'   => new sfValidatorPropelChoice(array('model' => 'ArUser', 'column' => 'id', 'required' => false)),
      'ar_report_id' => new sfValidatorPropelChoice(array('model' => 'ArReport', 'column' => 'id', 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_user_can_view_report[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArUserCanViewReport';
  }


}
