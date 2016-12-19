<?php

/**
 * ArProcessedFile form base class.
 *
 * @method ArProcessedFile getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArProcessedFileForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'              => new sfWidgetFormInputHidden(),
      'name'            => new sfWidgetFormInputText(),
      'checksum'        => new sfWidgetFormInputText(),
      'processing_date' => new sfWidgetFormDateTime(),
    ));

    $this->setValidators(array(
      'id'              => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'name'            => new sfValidatorString(array('max_length' => 512)),
      'checksum'        => new sfValidatorString(array('max_length' => 2048, 'required' => false)),
      'processing_date' => new sfValidatorDateTime(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_processed_file[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArProcessedFile';
  }


}
