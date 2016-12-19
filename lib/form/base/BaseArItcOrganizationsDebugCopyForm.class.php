<?php

/**
 * ArItcOrganizationsDebugCopy form base class.
 *
 * @method ArItcOrganizationsDebugCopy getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArItcOrganizationsDebugCopyForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'          => new sfWidgetFormInputHidden(),
      'last_update' => new sfWidgetFormDateTime(),
      'org'         => new sfWidgetFormInputText(),
      'name'        => new sfWidgetFormInputText(),
      'description' => new sfWidgetFormInputText(),
      'accountcode' => new sfWidgetFormInputText(),
      'parent'      => new sfWidgetFormInputText(),
      'email'       => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'id'          => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'last_update' => new sfValidatorDateTime(array('required' => false)),
      'org'         => new sfValidatorString(array('max_length' => 2048, 'required' => false)),
      'name'        => new sfValidatorString(array('max_length' => 2048, 'required' => false)),
      'description' => new sfValidatorString(array('max_length' => 2048, 'required' => false)),
      'accountcode' => new sfValidatorString(array('max_length' => 2048, 'required' => false)),
      'parent'      => new sfValidatorString(array('max_length' => 2048, 'required' => false)),
      'email'       => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_itc_organizations_debug_copy[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArItcOrganizationsDebugCopy';
  }


}
