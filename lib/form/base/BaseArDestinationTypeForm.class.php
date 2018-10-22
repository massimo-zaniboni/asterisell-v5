<?php

/**
 * ArDestinationType form base class.
 *
 * @method ArDestinationType getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArDestinationTypeForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'          => new sfWidgetFormInputHidden(),
      'internal_id' => new sfWidgetFormInputText(),
      'name'        => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'id'          => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'internal_id' => new sfValidatorInteger(array('min' => -32768, 'max' => 32767, 'required' => false)),
      'name'        => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_destination_type[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArDestinationType';
  }


}
