<?php

/**
 * ArItcOrganizations form base class.
 *
 * @method ArItcOrganizations getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArItcOrganizationsForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                      => new sfWidgetFormInputHidden(),
      'account_code'            => new sfWidgetFormInputText(),
      'definition_time'         => new sfWidgetFormDateTime(),
      'org'                     => new sfWidgetFormInputText(),
      'name'                    => new sfWidgetFormInputText(),
      'description'             => new sfWidgetFormInputText(),
      'email'                   => new sfWidgetFormInputText(),
      'parent'                  => new sfWidgetFormInputText(),
      'calculated_account_code' => new sfWidgetFormInputText(),
      'is_new'                  => new sfWidgetFormInputCheckbox(),
      'is_maybe_modified'       => new sfWidgetFormInputCheckbox(),
      'is_to_remove'            => new sfWidgetFormInputCheckbox(),
      'can_be_removed'          => new sfWidgetFormInputCheckbox(),
    ));

    $this->setValidators(array(
      'id'                      => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'account_code'            => new sfValidatorString(array('max_length' => 255)),
      'definition_time'         => new sfValidatorDateTime(array('required' => false)),
      'org'                     => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
      'name'                    => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
      'description'             => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
      'email'                   => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
      'parent'                  => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
      'calculated_account_code' => new sfValidatorString(array('max_length' => 9046, 'required' => false)),
      'is_new'                  => new sfValidatorBoolean(array('required' => false)),
      'is_maybe_modified'       => new sfValidatorBoolean(array('required' => false)),
      'is_to_remove'            => new sfValidatorBoolean(array('required' => false)),
      'can_be_removed'          => new sfValidatorBoolean(array('required' => false)),
    ));

    $this->validatorSchema->setPostValidator(
      new sfValidatorPropelUnique(array('model' => 'ArItcOrganizations', 'column' => array('account_code')))
    );

    $this->widgetSchema->setNameFormat('ar_itc_organizations[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArItcOrganizations';
  }


}
