<?php

/**
 * ArWholesaleCarrier form base class.
 *
 * @method ArWholesaleCarrier getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArWholesaleCarrierForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'            => new sfWidgetFormInputHidden(),
      'internal_name' => new sfWidgetFormInputText(),
      'note'          => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'id'            => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'internal_name' => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'note'          => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
    ));

    $this->validatorSchema->setPostValidator(
      new sfValidatorPropelUnique(array('model' => 'ArWholesaleCarrier', 'column' => array('internal_name')))
    );

    $this->widgetSchema->setNameFormat('ar_wholesale_carrier[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArWholesaleCarrier';
  }


}
