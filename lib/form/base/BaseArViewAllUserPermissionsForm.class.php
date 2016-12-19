<?php

/**
 * ArViewAllUserPermissions form base class.
 *
 * @method ArViewAllUserPermissions getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArViewAllUserPermissionsForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_user_id'       => new sfWidgetFormInputHidden(),
      'ar_permission_id' => new sfWidgetFormInputHidden(),
    ));

    $this->setValidators(array(
      'ar_user_id'       => new sfValidatorChoice(array('choices' => array($this->getObject()->getArUserId()), 'empty_value' => $this->getObject()->getArUserId(), 'required' => false)),
      'ar_permission_id' => new sfValidatorChoice(array('choices' => array($this->getObject()->getArPermissionId()), 'empty_value' => $this->getObject()->getArPermissionId(), 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_view_all_user_permissions[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArViewAllUserPermissions';
  }


}
