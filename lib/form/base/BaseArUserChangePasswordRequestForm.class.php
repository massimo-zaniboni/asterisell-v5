<?php

/**
 * ArUserChangePasswordRequest form base class.
 *
 * @method ArUserChangePasswordRequest getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArUserChangePasswordRequestForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'           => new sfWidgetFormInputHidden(),
      'ar_user_id'   => new sfWidgetFormPropelChoice(array('model' => 'ArUser', 'add_empty' => true)),
      'at_date'      => new sfWidgetFormDateTime(),
      'old_password' => new sfWidgetFormInputText(),
      'new_password' => new sfWidgetFormInputText(),
      'is_processed' => new sfWidgetFormInputCheckbox(),
    ));

    $this->setValidators(array(
      'id'           => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'ar_user_id'   => new sfValidatorPropelChoice(array('model' => 'ArUser', 'column' => 'id', 'required' => false)),
      'at_date'      => new sfValidatorDateTime(),
      'old_password' => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
      'new_password' => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
      'is_processed' => new sfValidatorBoolean(),
    ));

    $this->widgetSchema->setNameFormat('ar_user_change_password_request[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArUserChangePasswordRequest';
  }


}
