<?php

/**
 * ArReseller form base class.
 *
 * @method ArReseller getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArResellerForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'            => new sfWidgetFormInputHidden(),
      'internal_name' => new sfWidgetFormInputText(),
      'name'          => new sfWidgetFormInputText(),
      'note'          => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'id'            => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'internal_name' => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'name'          => new sfValidatorString(array('max_length' => 1204)),
      'note'          => new sfValidatorString(array('max_length' => 2048, 'required' => false)),
    ));

    $this->validatorSchema->setPostValidator(
      new sfValidatorPropelUnique(array('model' => 'ArReseller', 'column' => array('internal_name')))
    );

    $this->widgetSchema->setNameFormat('ar_reseller[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArReseller';
  }


}
