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
      'ar_cdr_provider_id'    => new sfWidgetFormInputHidden(),
      'ar_physical_format_id' => new sfWidgetFormInputHidden(),
      'content'               => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'calldate'              => new sfValidatorChoice(array('choices' => array($this->getObject()->getCalldate()), 'empty_value' => $this->getObject()->getCalldate(), 'required' => false)),
      'id'                    => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'ar_cdr_provider_id'    => new sfValidatorChoice(array('choices' => array($this->getObject()->getArCdrProviderId()), 'empty_value' => $this->getObject()->getArCdrProviderId(), 'required' => false)),
      'ar_physical_format_id' => new sfValidatorChoice(array('choices' => array($this->getObject()->getArPhysicalFormatId()), 'empty_value' => $this->getObject()->getArPhysicalFormatId(), 'required' => false)),
      'content'               => new sfValidatorString(array('max_length' => 10000, 'required' => false)),
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
