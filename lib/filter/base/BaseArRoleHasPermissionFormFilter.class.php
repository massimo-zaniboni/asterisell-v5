<?php

/**
 * ArRoleHasPermission filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArRoleHasPermissionFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_permission_id' => new sfWidgetFormPropelChoice(array('model' => 'ArPermission', 'add_empty' => true)),
      'ar_role_id'       => new sfWidgetFormPropelChoice(array('model' => 'ArRole', 'add_empty' => true)),
    ));

    $this->setValidators(array(
      'ar_permission_id' => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArPermission', 'column' => 'id')),
      'ar_role_id'       => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArRole', 'column' => 'id')),
    ));

    $this->widgetSchema->setNameFormat('ar_role_has_permission_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArRoleHasPermission';
  }

  public function getFields()
  {
    return array(
      'ar_permission_id' => 'ForeignKey',
      'ar_role_id'       => 'ForeignKey',
      'id'               => 'Number',
    );
  }
}
