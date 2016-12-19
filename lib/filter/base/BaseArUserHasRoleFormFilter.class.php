<?php

/**
 * ArUserHasRole filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArUserHasRoleFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_user_id' => new sfWidgetFormPropelChoice(array('model' => 'ArUser', 'add_empty' => true)),
      'ar_role_id' => new sfWidgetFormPropelChoice(array('model' => 'ArRole', 'add_empty' => true)),
    ));

    $this->setValidators(array(
      'ar_user_id' => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArUser', 'column' => 'id')),
      'ar_role_id' => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArRole', 'column' => 'id')),
    ));

    $this->widgetSchema->setNameFormat('ar_user_has_role_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArUserHasRole';
  }

  public function getFields()
  {
    return array(
      'ar_user_id' => 'ForeignKey',
      'ar_role_id' => 'ForeignKey',
      'id'         => 'Number',
    );
  }
}
