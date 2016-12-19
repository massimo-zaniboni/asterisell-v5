<?php

/**
 * ArUserHasRole form base class.
 *
 * @method ArUserHasRole getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArUserHasRoleForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_user_id' => new sfWidgetFormPropelChoice(array('model' => 'ArUser', 'add_empty' => true)),
      'ar_role_id' => new sfWidgetFormPropelChoice(array('model' => 'ArRole', 'add_empty' => true)),
      'id'         => new sfWidgetFormInputHidden(),
    ));

    $this->setValidators(array(
      'ar_user_id' => new sfValidatorPropelChoice(array('model' => 'ArUser', 'column' => 'id', 'required' => false)),
      'ar_role_id' => new sfValidatorPropelChoice(array('model' => 'ArRole', 'column' => 'id', 'required' => false)),
      'id'         => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_user_has_role[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArUserHasRole';
  }


}
