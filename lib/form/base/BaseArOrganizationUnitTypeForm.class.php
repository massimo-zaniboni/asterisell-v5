<?php

/**
 * ArOrganizationUnitType form base class.
 *
 * @method ArOrganizationUnitType getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArOrganizationUnitTypeForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'            => new sfWidgetFormInputHidden(),
      'name'          => new sfWidgetFormInputText(),
      'short_code'    => new sfWidgetFormInputText(),
      'internal_name' => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'id'            => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'name'          => new sfValidatorString(array('max_length' => 200)),
      'short_code'    => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'internal_name' => new sfValidatorString(array('max_length' => 200, 'required' => false)),
    ));

    $this->validatorSchema->setPostValidator(
      new sfValidatorAnd(array(
        new sfValidatorPropelUnique(array('model' => 'ArOrganizationUnitType', 'column' => array('name'))),
        new sfValidatorPropelUnique(array('model' => 'ArOrganizationUnitType', 'column' => array('internal_name'))),
      ))
    );

    $this->widgetSchema->setNameFormat('ar_organization_unit_type[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArOrganizationUnitType';
  }


}
