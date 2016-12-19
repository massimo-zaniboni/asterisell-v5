<?php

/**
 * ArLegalDateGenerationMethod form base class.
 *
 * @method ArLegalDateGenerationMethod getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArLegalDateGenerationMethodForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'          => new sfWidgetFormInputHidden(),
      'name'        => new sfWidgetFormInputText(),
      'description' => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'id'          => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'name'        => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
      'description' => new sfValidatorString(array('max_length' => 2048, 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_legal_date_generation_method[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArLegalDateGenerationMethod';
  }


}
