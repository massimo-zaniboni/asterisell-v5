<?php

/**
 * ArOrganizationUnitHasStructure filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArOrganizationUnitHasStructureFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_organization_unit_id'        => new sfWidgetFormPropelChoice(array('model' => 'ArOrganizationUnit', 'add_empty' => true)),
      'ar_organization_unit_type_id'   => new sfWidgetFormPropelChoice(array('model' => 'ArOrganizationUnitType', 'add_empty' => true)),
      'ar_parent_organization_unit_id' => new sfWidgetFormPropelChoice(array('model' => 'ArOrganizationUnit', 'add_empty' => true)),
      'from'                           => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate(), 'with_empty' => false)),
      'exists'                         => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'ar_rate_category_id'            => new sfWidgetFormPropelChoice(array('model' => 'ArRateCategory', 'add_empty' => true)),
      'ar_party_id'                    => new sfWidgetFormPropelChoice(array('model' => 'ArParty', 'add_empty' => true)),
      'extension_codes'                => new sfWidgetFormFilterInput(),
      'extension_name'                 => new sfWidgetFormFilterInput(),
      'extension_user_code'            => new sfWidgetFormFilterInput(),
    ));

    $this->setValidators(array(
      'ar_organization_unit_id'        => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArOrganizationUnit', 'column' => 'id')),
      'ar_organization_unit_type_id'   => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArOrganizationUnitType', 'column' => 'id')),
      'ar_parent_organization_unit_id' => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArOrganizationUnit', 'column' => 'id')),
      'from'                           => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'exists'                         => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'ar_rate_category_id'            => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArRateCategory', 'column' => 'id')),
      'ar_party_id'                    => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArParty', 'column' => 'id')),
      'extension_codes'                => new sfValidatorPass(array('required' => false)),
      'extension_name'                 => new sfValidatorPass(array('required' => false)),
      'extension_user_code'            => new sfValidatorPass(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_organization_unit_has_structure_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArOrganizationUnitHasStructure';
  }

  public function getFields()
  {
    return array(
      'id'                             => 'Number',
      'ar_organization_unit_id'        => 'ForeignKey',
      'ar_organization_unit_type_id'   => 'ForeignKey',
      'ar_parent_organization_unit_id' => 'ForeignKey',
      'from'                           => 'Date',
      'exists'                         => 'Boolean',
      'ar_rate_category_id'            => 'ForeignKey',
      'ar_party_id'                    => 'ForeignKey',
      'extension_codes'                => 'Text',
      'extension_name'                 => 'Text',
      'extension_user_code'            => 'Text',
    );
  }
}
