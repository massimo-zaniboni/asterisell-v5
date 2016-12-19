<?php

/**
 * ArOrganizationUnitHasStructure form base class.
 *
 * @method ArOrganizationUnitHasStructure getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArOrganizationUnitHasStructureForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                             => new sfWidgetFormInputHidden(),
      'ar_organization_unit_id'        => new sfWidgetFormPropelChoice(array('model' => 'ArOrganizationUnit', 'add_empty' => true)),
      'ar_organization_unit_type_id'   => new sfWidgetFormPropelChoice(array('model' => 'ArOrganizationUnitType', 'add_empty' => true)),
      'ar_parent_organization_unit_id' => new sfWidgetFormPropelChoice(array('model' => 'ArOrganizationUnit', 'add_empty' => true)),
      'from'                           => new sfWidgetFormDateTime(),
      'exists'                         => new sfWidgetFormInputCheckbox(),
      'ar_rate_category_id'            => new sfWidgetFormPropelChoice(array('model' => 'ArRateCategory', 'add_empty' => true)),
      'ar_party_id'                    => new sfWidgetFormPropelChoice(array('model' => 'ArParty', 'add_empty' => true)),
      'extension_codes'                => new sfWidgetFormInputText(),
      'extension_name'                 => new sfWidgetFormInputText(),
      'extension_user_code'            => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'id'                             => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'ar_organization_unit_id'        => new sfValidatorPropelChoice(array('model' => 'ArOrganizationUnit', 'column' => 'id', 'required' => false)),
      'ar_organization_unit_type_id'   => new sfValidatorPropelChoice(array('model' => 'ArOrganizationUnitType', 'column' => 'id', 'required' => false)),
      'ar_parent_organization_unit_id' => new sfValidatorPropelChoice(array('model' => 'ArOrganizationUnit', 'column' => 'id', 'required' => false)),
      'from'                           => new sfValidatorDateTime(),
      'exists'                         => new sfValidatorBoolean(),
      'ar_rate_category_id'            => new sfValidatorPropelChoice(array('model' => 'ArRateCategory', 'column' => 'id', 'required' => false)),
      'ar_party_id'                    => new sfValidatorPropelChoice(array('model' => 'ArParty', 'column' => 'id', 'required' => false)),
      'extension_codes'                => new sfValidatorString(array('max_length' => 5024, 'required' => false)),
      'extension_name'                 => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
      'extension_user_code'            => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
    ));

    $this->validatorSchema->setPostValidator(
      new sfValidatorPropelUnique(array('model' => 'ArOrganizationUnitHasStructure', 'column' => array('ar_organization_unit_id', 'from')))
    );

    $this->widgetSchema->setNameFormat('ar_organization_unit_has_structure[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArOrganizationUnitHasStructure';
  }


}
