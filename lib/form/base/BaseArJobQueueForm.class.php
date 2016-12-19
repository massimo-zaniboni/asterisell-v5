<?php

/**
 * ArJobQueue form base class.
 *
 * @method ArJobQueue getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArJobQueueForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                         => new sfWidgetFormInputHidden(),
      'is_part_of'                 => new sfWidgetFormInputText(),
      'state'                      => new sfWidgetFormInputText(),
      'created_at'                 => new sfWidgetFormDateTime(),
      'start_at'                   => new sfWidgetFormDateTime(),
      'end_at'                     => new sfWidgetFormDateTime(),
      'description'                => new sfWidgetFormInputText(),
      'php_data_job_serialization' => new sfWidgetFormTextarea(),
      'internal_name'              => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'id'                         => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'is_part_of'                 => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647)),
      'state'                      => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647)),
      'created_at'                 => new sfValidatorDateTime(array('required' => false)),
      'start_at'                   => new sfValidatorDateTime(array('required' => false)),
      'end_at'                     => new sfValidatorDateTime(array('required' => false)),
      'description'                => new sfValidatorString(array('max_length' => 12000)),
      'php_data_job_serialization' => new sfValidatorString(array('required' => false)),
      'internal_name'              => new sfValidatorString(array('max_length' => 512, 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_job_queue[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArJobQueue';
  }


}
