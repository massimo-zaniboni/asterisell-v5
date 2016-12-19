<?php

/**
 * ArUserHasPermission filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArUserHasPermissionFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_user_id'       => new sfWidgetFormPropelChoice(array('model' => 'ArUser', 'add_empty' => true)),
      'ar_permission_id' => new sfWidgetFormPropelChoice(array('model' => 'ArPermission', 'add_empty' => true)),
    ));

    $this->setValidators(array(
      'ar_user_id'       => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArUser', 'column' => 'id')),
      'ar_permission_id' => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArPermission', 'column' => 'id')),
    ));

    $this->widgetSchema->setNameFormat('ar_user_has_permission_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArUserHasPermission';
  }

  public function getFields()
  {
    return array(
      'ar_user_id'       => 'ForeignKey',
      'ar_permission_id' => 'ForeignKey',
      'id'               => 'Number',
    );
  }
}
