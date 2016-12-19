<?php

/**
 * ArServicePrice form base class.
 *
 * @method ArServicePrice getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArServicePriceForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'            => new sfWidgetFormInputHidden(),
      'internal_name' => new sfWidgetFormInputText(),
      'ar_service_id' => new sfWidgetFormPropelChoice(array('model' => 'ArService', 'add_empty' => false)),
      'from_date'     => new sfWidgetFormDateTime(),
      'price'         => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'id'            => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'internal_name' => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'ar_service_id' => new sfValidatorPropelChoice(array('model' => 'ArService', 'column' => 'id')),
      'from_date'     => new sfValidatorDateTime(),
      'price'         => new sfValidatorInteger(array('min' => -9.2233720368548E+18, 'max' => 9223372036854775807)),
    ));

    $this->validatorSchema->setPostValidator(
      new sfValidatorPropelUnique(array('model' => 'ArServicePrice', 'column' => array('internal_name')))
    );

    $this->widgetSchema->setNameFormat('ar_service_price[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArServicePrice';
  }


}
