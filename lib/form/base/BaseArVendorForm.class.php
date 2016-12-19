<?php

/**
 * ArVendor form base class.
 *
 * @method ArVendor getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArVendorForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'            => new sfWidgetFormInputHidden(),
      'internal_name' => new sfWidgetFormInputText(),
      'ar_party_id'   => new sfWidgetFormPropelChoice(array('model' => 'ArParty', 'add_empty' => true)),
      'is_internal'   => new sfWidgetFormInputCheckbox(),
    ));

    $this->setValidators(array(
      'id'            => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'internal_name' => new sfValidatorString(array('max_length' => 200, 'required' => false)),
      'ar_party_id'   => new sfValidatorPropelChoice(array('model' => 'ArParty', 'column' => 'id', 'required' => false)),
      'is_internal'   => new sfValidatorBoolean(),
    ));

    $this->validatorSchema->setPostValidator(
      new sfValidatorPropelUnique(array('model' => 'ArVendor', 'column' => array('internal_name')))
    );

    $this->widgetSchema->setNameFormat('ar_vendor[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArVendor';
  }


}
