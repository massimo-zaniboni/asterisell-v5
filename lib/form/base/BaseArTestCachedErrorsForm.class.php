<?php

/**
 * ArTestCachedErrors form base class.
 *
 * @method ArTestCachedErrors getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArTestCachedErrorsForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'calldate'               => new sfWidgetFormInputHidden(),
      'destination_type'       => new sfWidgetFormInputHidden(),
      'error_destination_type' => new sfWidgetFormInputHidden(),
      'count_of_calls'         => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'calldate'               => new sfValidatorChoice(array('choices' => array($this->getObject()->getCalldate()), 'empty_value' => $this->getObject()->getCalldate(), 'required' => false)),
      'destination_type'       => new sfValidatorChoice(array('choices' => array($this->getObject()->getDestinationType()), 'empty_value' => $this->getObject()->getDestinationType(), 'required' => false)),
      'error_destination_type' => new sfValidatorChoice(array('choices' => array($this->getObject()->getErrorDestinationType()), 'empty_value' => $this->getObject()->getErrorDestinationType(), 'required' => false)),
      'count_of_calls'         => new sfValidatorInteger(array('min' => -9.2233720368548E+18, 'max' => 9223372036854775807)),
    ));

    $this->widgetSchema->setNameFormat('ar_test_cached_errors[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArTestCachedErrors';
  }


}
