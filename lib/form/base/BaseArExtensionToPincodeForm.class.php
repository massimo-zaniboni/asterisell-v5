<?php

/**
 * ArExtensionToPincode form base class.
 *
 * @method ArExtensionToPincode getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArExtensionToPincodeForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'        => new sfWidgetFormInputHidden(),
      'extension' => new sfWidgetFormInputText(),
      'pincode'   => new sfWidgetFormInputText(),
      'from_date' => new sfWidgetFormDateTime(),
    ));

    $this->setValidators(array(
      'id'        => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'extension' => new sfValidatorString(array('max_length' => 255)),
      'pincode'   => new sfValidatorString(array('max_length' => 255)),
      'from_date' => new sfValidatorDateTime(),
    ));

    $this->widgetSchema->setNameFormat('ar_extension_to_pincode[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArExtensionToPincode';
  }


}
