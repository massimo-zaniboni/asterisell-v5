<?php

/**
 * ArUserHasRelation form base class.
 *
 * @method ArUserHasRelation getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArUserHasRelationForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'               => new sfWidgetFormInputHidden(),
      'ar_user_id'       => new sfWidgetFormPropelChoice(array('model' => 'ArUser', 'add_empty' => false)),
      'ar_role_id'       => new sfWidgetFormPropelChoice(array('model' => 'ArRole', 'add_empty' => true)),
      'ar_permission_id' => new sfWidgetFormPropelChoice(array('model' => 'ArPermission', 'add_empty' => true)),
    ));

    $this->setValidators(array(
      'id'               => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'ar_user_id'       => new sfValidatorPropelChoice(array('model' => 'ArUser', 'column' => 'id')),
      'ar_role_id'       => new sfValidatorPropelChoice(array('model' => 'ArRole', 'column' => 'id', 'required' => false)),
      'ar_permission_id' => new sfValidatorPropelChoice(array('model' => 'ArPermission', 'column' => 'id', 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_user_has_relation[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArUserHasRelation';
  }


}
