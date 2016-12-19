<?php

/**
 * ArUpgradingJob form base class.
 *
 * @method ArUpgradingJob getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArUpgradingJobForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'             => new sfWidgetFormInputHidden(),
      'code'           => new sfWidgetFormInputText(),
      'start_time'     => new sfWidgetFormDateTime(),
      'end_time'       => new sfWidgetFormDateTime(),
      'sucessful'      => new sfWidgetFormInputCheckbox(),
      'status_message' => new sfWidgetFormTextarea(),
    ));

    $this->setValidators(array(
      'id'             => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'code'           => new sfValidatorString(array('max_length' => 2048)),
      'start_time'     => new sfValidatorDateTime(array('required' => false)),
      'end_time'       => new sfValidatorDateTime(array('required' => false)),
      'sucessful'      => new sfValidatorBoolean(array('required' => false)),
      'status_message' => new sfValidatorString(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_upgrading_job[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArUpgradingJob';
  }


}
