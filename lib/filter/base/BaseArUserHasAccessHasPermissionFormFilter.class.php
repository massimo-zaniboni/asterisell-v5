<?php

/**
 * ArUserHasAccessHasPermission filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArUserHasAccessHasPermissionFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
    ));

    $this->setValidators(array(
    ));

    $this->widgetSchema->setNameFormat('ar_user_has_access_has_permission_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArUserHasAccessHasPermission';
  }

  public function getFields()
  {
    return array(
      'ar_user_has_access_id' => 'ForeignKey',
      'ar_permission_id'      => 'ForeignKey',
    );
  }
}
