<?php

/**
 * ArTypeOfSourceCdr form base class.
 *
 * @method ArTypeOfSourceCdr getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArTypeOfSourceCdrForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_cdr_provider_id'    => new sfWidgetFormInputHidden(),
      'ar_physical_format_id' => new sfWidgetFormInputHidden(),
    ));

    $this->setValidators(array(
      'ar_cdr_provider_id'    => new sfValidatorChoice(array('choices' => array($this->getObject()->getArCdrProviderId()), 'empty_value' => $this->getObject()->getArCdrProviderId(), 'required' => false)),
      'ar_physical_format_id' => new sfValidatorChoice(array('choices' => array($this->getObject()->getArPhysicalFormatId()), 'empty_value' => $this->getObject()->getArPhysicalFormatId(), 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_type_of_source_cdr[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArTypeOfSourceCdr';
  }


}
