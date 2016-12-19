<?php

/**
 * ArCustomRateForm form base class.
 *
 * @method ArCustomRateForm getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArCustomRateFormForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id' => new sfWidgetFormInputHidden(),
    ));

    $this->setValidators(array(
      'id' => new sfValidatorPropelChoice(array('model' => 'ArRate', 'column' => 'id', 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_custom_rate_form[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArCustomRateForm';
  }


}
