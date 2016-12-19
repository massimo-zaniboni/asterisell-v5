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
      'id'                           => new sfWidgetFormInputHidden(),
      'telephone_number'             => new sfWidgetFormInputText(),
      'ported_telephone_number'      => new sfWidgetFormInputText(),
      'from_date'                    => new sfWidgetFormDateTime(),
      'is_exported_to_rating_engine' => new sfWidgetFormInputCheckbox(),
    ));

    $this->setValidators(array(
      'id'                           => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'telephone_number'             => new sfValidatorString(array('max_length' => 255)),
      'ported_telephone_number'      => new sfValidatorString(array('max_length' => 255)),
      'from_date'                    => new sfValidatorDateTime(array('required' => false)),
      'is_exported_to_rating_engine' => new sfValidatorBoolean(),
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
