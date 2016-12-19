<?php

/**
 * ArCommunicationChannelType form base class.
 *
 * @method ArCommunicationChannelType getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArCommunicationChannelTypeForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'            => new sfWidgetFormInputHidden(),
      'name'          => new sfWidgetFormInputText(),
      'description'   => new sfWidgetFormInputText(),
      'internal_name' => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'id'            => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'name'          => new sfValidatorString(array('max_length' => 255)),
      'description'   => new sfValidatorString(array('max_length' => 2048, 'required' => false)),
      'internal_name' => new sfValidatorString(array('max_length' => 200, 'required' => false)),
    ));

    $this->validatorSchema->setPostValidator(
      new sfValidatorAnd(array(
        new sfValidatorPropelUnique(array('model' => 'ArCommunicationChannelType', 'column' => array('name'))),
        new sfValidatorPropelUnique(array('model' => 'ArCommunicationChannelType', 'column' => array('internal_name'))),
      ))
    );

    $this->widgetSchema->setNameFormat('ar_communication_channel_type[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArCommunicationChannelType';
  }


}
