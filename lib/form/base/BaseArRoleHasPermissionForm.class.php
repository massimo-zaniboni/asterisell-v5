<?php

/**
 * ArRoleHasPermission form base class.
 *
 * @method ArRoleHasPermission getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArRoleHasPermissionForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_permission_id' => new sfWidgetFormPropelChoice(array('model' => 'ArPermission', 'add_empty' => true)),
      'ar_role_id'       => new sfWidgetFormPropelChoice(array('model' => 'ArRole', 'add_empty' => true)),
      'id'               => new sfWidgetFormInputHidden(),
    ));

    $this->setValidators(array(
      'ar_permission_id' => new sfValidatorPropelChoice(array('model' => 'ArPermission', 'column' => 'id', 'required' => false)),
      'ar_role_id'       => new sfValidatorPropelChoice(array('model' => 'ArRole', 'column' => 'id', 'required' => false)),
      'id'               => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_role_has_permission[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArRoleHasPermission';
  }


}
