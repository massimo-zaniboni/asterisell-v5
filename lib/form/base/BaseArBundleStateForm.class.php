<?php

/**
 * ArBundleState form base class.
 *
 * @method ArBundleState getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArBundleStateForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'        => new sfWidgetFormInputHidden(),
      'to_time'   => new sfWidgetFormDateTime(),
      'data_file' => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'id'        => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'to_time'   => new sfValidatorDateTime(),
      'data_file' => new sfValidatorPass(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_bundle_state[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArBundleState';
  }


}
