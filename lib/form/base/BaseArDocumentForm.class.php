<?php

/**
 * ArDocument form base class.
 *
 * @method ArDocument getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArDocumentForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'             => new sfWidgetFormInputHidden(),
      'ar_party_id'    => new sfWidgetFormPropelChoice(array('model' => 'ArParty', 'add_empty' => true)),
      'document_name'  => new sfWidgetFormInputText(),
      'document_date'  => new sfWidgetFormDate(),
      'document'       => new sfWidgetFormInputText(),
      'file_name'      => new sfWidgetFormInputText(),
      'mime_type'      => new sfWidgetFormInputText(),
      'already_opened' => new sfWidgetFormInputCheckbox(),
    ));

    $this->setValidators(array(
      'id'             => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'ar_party_id'    => new sfValidatorPropelChoice(array('model' => 'ArParty', 'column' => 'id', 'required' => false)),
      'document_name'  => new sfValidatorString(array('max_length' => 128, 'required' => false)),
      'document_date'  => new sfValidatorDate(array('required' => false)),
      'document'       => new sfValidatorPass(array('required' => false)),
      'file_name'      => new sfValidatorString(array('max_length' => 128, 'required' => false)),
      'mime_type'      => new sfValidatorString(array('max_length' => 256, 'required' => false)),
      'already_opened' => new sfValidatorBoolean(),
    ));

    $this->widgetSchema->setNameFormat('ar_document[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArDocument';
  }


}
