<?php

/**
 * ArRateFormat form base class.
 *
 * @method ArRateFormat getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArRateFormatForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                   => new sfWidgetFormInputHidden(),
      'short_description'    => new sfWidgetFormTextarea(),
      'detailed_description' => new sfWidgetFormTextarea(),
      'internal_name'        => new sfWidgetFormInputText(),
      'order_name'           => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'id'                   => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'short_description'    => new sfValidatorString(array('required' => false)),
      'detailed_description' => new sfValidatorString(array('required' => false)),
      'internal_name'        => new sfValidatorString(array('max_length' => 255)),
      'order_name'           => new sfValidatorString(array('max_length' => 255, 'required' => false)),
    ));

    $this->validatorSchema->setPostValidator(
      new sfValidatorPropelUnique(array('model' => 'ArRateFormat', 'column' => array('internal_name')))
    );

    $this->widgetSchema->setNameFormat('ar_rate_format[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArRateFormat';
  }


}
