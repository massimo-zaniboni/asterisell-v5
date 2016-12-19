<?php

/**
 * ArRateSharedWithReseller form base class.
 *
 * @method ArRateSharedWithReseller getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArRateSharedWithResellerForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_rate_id'     => new sfWidgetFormPropelChoice(array('model' => 'ArRate', 'add_empty' => true)),
      'ar_reseller_id' => new sfWidgetFormPropelChoice(array('model' => 'ArReseller', 'add_empty' => true)),
      'is_exported'    => new sfWidgetFormInputCheckbox(),
      'id'             => new sfWidgetFormInputHidden(),
    ));

    $this->setValidators(array(
      'ar_rate_id'     => new sfValidatorPropelChoice(array('model' => 'ArRate', 'column' => 'id', 'required' => false)),
      'ar_reseller_id' => new sfValidatorPropelChoice(array('model' => 'ArReseller', 'column' => 'id', 'required' => false)),
      'is_exported'    => new sfValidatorBoolean(),
      'id'             => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_rate_shared_with_reseller[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArRateSharedWithReseller';
  }


}
