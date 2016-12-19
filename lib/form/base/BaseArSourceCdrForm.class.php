<?php

/**
 * ArSourceCdr form base class.
 *
 * @method ArSourceCdr getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArSourceCdrForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                      => new sfWidgetFormInputHidden(),
      'ar_cdr_provider_id'      => new sfWidgetFormPropelChoice(array('model' => 'ArCdrProvider', 'add_empty' => true)),
      'ar_physical_format_id'   => new sfWidgetFormPropelChoice(array('model' => 'ArPhysicalFormat', 'add_empty' => true)),
      'calldate'                => new sfWidgetFormDateTime(),
      'is_imported_service_cdr' => new sfWidgetFormInputCheckbox(),
      'content'                 => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'id'                      => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'ar_cdr_provider_id'      => new sfValidatorPropelChoice(array('model' => 'ArCdrProvider', 'column' => 'id', 'required' => false)),
      'ar_physical_format_id'   => new sfValidatorPropelChoice(array('model' => 'ArPhysicalFormat', 'column' => 'id', 'required' => false)),
      'calldate'                => new sfValidatorDateTime(),
      'is_imported_service_cdr' => new sfValidatorBoolean(),
      'content'                 => new sfValidatorString(array('max_length' => 51200, 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_source_cdr[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArSourceCdr';
  }


}
