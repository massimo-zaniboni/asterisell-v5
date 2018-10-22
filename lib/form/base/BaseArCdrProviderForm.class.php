<?php

/**
 * ArCdrProvider form base class.
 *
 * @method ArCdrProvider getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArCdrProviderForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                 => new sfWidgetFormInputHidden(),
      'internal_name'      => new sfWidgetFormInputText(),
      'description'        => new sfWidgetFormInputText(),
      'last_imported_id'   => new sfWidgetFormInputText(),
      'last_imported_data' => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'id'                 => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'internal_name'      => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'description'        => new sfValidatorString(array('max_length' => 2048, 'required' => false)),
      'last_imported_id'   => new sfValidatorInteger(array('min' => -9.2233720368548E+18, 'max' => 9223372036854775807, 'required' => false)),
      'last_imported_data' => new sfValidatorString(array('max_length' => 2048, 'required' => false)),
    ));

    $this->validatorSchema->setPostValidator(
      new sfValidatorPropelUnique(array('model' => 'ArCdrProvider', 'column' => array('internal_name')))
    );

    $this->widgetSchema->setNameFormat('ar_cdr_provider[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArCdrProvider';
  }


}
