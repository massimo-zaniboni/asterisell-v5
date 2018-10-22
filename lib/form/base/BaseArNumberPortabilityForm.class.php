<?php

/**
 * ArNumberPortability form base class.
 *
 * @method ArNumberPortability getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArNumberPortabilityForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'telephone_number'        => new sfWidgetFormInputHidden(),
      'from_date'               => new sfWidgetFormInputHidden(),
      'ported_telephone_number' => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'telephone_number'        => new sfValidatorChoice(array('choices' => array($this->getObject()->getTelephoneNumber()), 'empty_value' => $this->getObject()->getTelephoneNumber(), 'required' => false)),
      'from_date'               => new sfValidatorChoice(array('choices' => array($this->getObject()->getFromDate()), 'empty_value' => $this->getObject()->getFromDate(), 'required' => false)),
      'ported_telephone_number' => new sfValidatorString(array('max_length' => 255)),
    ));

    $this->widgetSchema->setNameFormat('ar_number_portability[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArNumberPortability';
  }


}
