<?php

/**
 * ArUserHasAccessHasRole filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArUserHasAccessHasRoleFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
    ));

    $this->setValidators(array(
    ));

    $this->widgetSchema->setNameFormat('ar_user_has_access_has_role_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArUserHasAccessHasRole';
  }

  public function getFields()
  {
    return array(
      'ar_role_id'            => 'ForeignKey',
      'ar_user_has_access_id' => 'ForeignKey',
    );
  }
}
