<?php

/**
 * ArPhysicalFormat form base class.
 *
 * @method ArPhysicalFormat getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArPhysicalFormatForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                   => new sfWidgetFormInputHidden(),
      'ar_logical_source_id' => new sfWidgetFormPropelChoice(array('model' => 'ArLogicalSource', 'add_empty' => true)),
      'name'                 => new sfWidgetFormInputText(),
      'description'          => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'id'                   => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'ar_logical_source_id' => new sfValidatorPropelChoice(array('model' => 'ArLogicalSource', 'column' => 'id', 'required' => false)),
      'name'                 => new sfValidatorString(array('max_length' => 200, 'required' => false)),
      'description'          => new sfValidatorString(array('max_length' => 2048, 'required' => false)),
    ));

    $this->validatorSchema->setPostValidator(
      new sfValidatorPropelUnique(array('model' => 'ArPhysicalFormat', 'column' => array('ar_logical_source_id', 'name')))
    );

    $this->widgetSchema->setNameFormat('ar_physical_format[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArPhysicalFormat';
  }


}
