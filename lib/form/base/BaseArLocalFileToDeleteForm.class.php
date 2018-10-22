<?php

/**
 * ArLocalFileToDelete form base class.
 *
 * @method ArLocalFileToDelete getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArLocalFileToDeleteForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'name' => new sfWidgetFormInputText(),
      'id'   => new sfWidgetFormInputHidden(),
    ));

    $this->setValidators(array(
      'name' => new sfValidatorString(array('max_length' => 1024)),
      'id'   => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_local_file_to_delete[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArLocalFileToDelete';
  }


}
