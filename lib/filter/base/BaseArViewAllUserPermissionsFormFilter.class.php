<?php

/**
 * ArViewAllUserPermissions filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArViewAllUserPermissionsFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
    ));

    $this->setValidators(array(
    ));

    $this->widgetSchema->setNameFormat('ar_view_all_user_permissions_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArViewAllUserPermissions';
  }

  public function getFields()
  {
    return array(
      'ar_user_id'       => 'Text',
      'ar_permission_id' => 'Text',
    );
  }
}
