<?php

/**
 * ArVoipExtensionToMove form base class.
 *
 * @method ArVoipExtensionToMove getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArVoipExtensionToMoveForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'extension' => new sfWidgetFormInputHidden(),
    ));

    $this->setValidators(array(
      'extension' => new sfValidatorChoice(array('choices' => array($this->getObject()->getExtension()), 'empty_value' => $this->getObject()->getExtension(), 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_voip_extension_to_move[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArVoipExtensionToMove';
  }


}
