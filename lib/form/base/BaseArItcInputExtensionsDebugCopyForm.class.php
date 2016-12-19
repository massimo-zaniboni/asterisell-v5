<?php

/**
 * ArItcInputExtensionsDebugCopy form base class.
 *
 * @method ArItcInputExtensionsDebugCopy getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArItcInputExtensionsDebugCopyForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                      => new sfWidgetFormInputHidden(),
      'last_update'             => new sfWidgetFormDateTime(),
      'extension_code'          => new sfWidgetFormInputText(),
      'dial_string'             => new sfWidgetFormInputText(),
      'name'                    => new sfWidgetFormInputText(),
      'accountcode'             => new sfWidgetFormInputText(),
      'accountcode_description' => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'id'                      => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'last_update'             => new sfValidatorDateTime(array('required' => false)),
      'extension_code'          => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
      'dial_string'             => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
      'name'                    => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
      'accountcode'             => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
      'accountcode_description' => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_itc_input_extensions_debug_copy[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArItcInputExtensionsDebugCopy';
  }


}
