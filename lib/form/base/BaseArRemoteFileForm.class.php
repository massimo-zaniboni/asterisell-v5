<?php

/**
 * ArRemoteFile form base class.
 *
 * @method ArRemoteFile getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArRemoteFileForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                 => new sfWidgetFormInputHidden(),
      'ar_cdr_provider_id' => new sfWidgetFormPropelChoice(array('model' => 'ArCdrProvider', 'add_empty' => true)),
      'name'               => new sfWidgetFormInputText(),
      'checksum'           => new sfWidgetFormInputText(),
      'receiving_date'     => new sfWidgetFormDateTime(),
    ));

    $this->setValidators(array(
      'id'                 => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'ar_cdr_provider_id' => new sfValidatorPropelChoice(array('model' => 'ArCdrProvider', 'column' => 'id', 'required' => false)),
      'name'               => new sfValidatorString(array('max_length' => 255)),
      'checksum'           => new sfValidatorString(array('max_length' => 2048, 'required' => false)),
      'receiving_date'     => new sfValidatorDateTime(array('required' => false)),
    ));

    $this->validatorSchema->setPostValidator(
      new sfValidatorPropelUnique(array('model' => 'ArRemoteFile', 'column' => array('name')))
    );

    $this->widgetSchema->setNameFormat('ar_remote_file[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArRemoteFile';
  }


}
