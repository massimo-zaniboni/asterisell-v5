<?php

/**
 * ArSourceCdr form base class.
 *
 * @method ArSourceCdr getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArSourceCdrForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'calldate'              => new sfWidgetFormInputHidden(),
      'id'                    => new sfWidgetFormInputHidden(),
      'ar_cdr_provider_id'    => new sfWidgetFormInputText(),
      'ar_physical_format_id' => new sfWidgetFormInputText(),
      'content'               => new sfWidgetFormInputText(),
      'is_hacked'             => new sfWidgetFormInputCheckbox(),
    ));

    $this->setValidators(array(
      'calldate'              => new sfValidatorChoice(array('choices' => array($this->getObject()->getCalldate()), 'empty_value' => $this->getObject()->getCalldate(), 'required' => false)),
      'id'                    => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'ar_cdr_provider_id'    => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647)),
      'ar_physical_format_id' => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647)),
      'content'               => new sfValidatorString(array('max_length' => 10000, 'required' => false)),
      'is_hacked'             => new sfValidatorBoolean(),
    ));

    $this->widgetSchema->setNameFormat('ar_source_cdr[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArSourceCdr';
  }


}
