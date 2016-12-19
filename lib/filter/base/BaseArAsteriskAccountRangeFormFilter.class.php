<?php

/**
 * ArAsteriskAccountRange filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArAsteriskAccountRangeFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_organization_unit_id'  => new sfWidgetFormPropelChoice(array('model' => 'ArOrganizationUnit', 'add_empty' => true)),
      'system_prefix'            => new sfWidgetFormFilterInput(),
      'system_suffix'            => new sfWidgetFormFilterInput(),
      'system_start_range'       => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'system_end_range'         => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'system_leading_zero'      => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'is_delete'                => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'is_physical_delete'       => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'user_prefix'              => new sfWidgetFormFilterInput(),
      'user_suffix'              => new sfWidgetFormFilterInput(),
      'user_start_range'         => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'generate_range_for_users' => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'user_leading_zero'        => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'user_note'                => new sfWidgetFormFilterInput(),
    ));

    $this->setValidators(array(
      'ar_organization_unit_id'  => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArOrganizationUnit', 'column' => 'id')),
      'system_prefix'            => new sfValidatorPass(array('required' => false)),
      'system_suffix'            => new sfValidatorPass(array('required' => false)),
      'system_start_range'       => new sfValidatorPass(array('required' => false)),
      'system_end_range'         => new sfValidatorPass(array('required' => false)),
      'system_leading_zero'      => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'is_delete'                => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'is_physical_delete'       => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'user_prefix'              => new sfValidatorPass(array('required' => false)),
      'user_suffix'              => new sfValidatorPass(array('required' => false)),
      'user_start_range'         => new sfValidatorPass(array('required' => false)),
      'generate_range_for_users' => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'user_leading_zero'        => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'user_note'                => new sfValidatorPass(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_asterisk_account_range_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArAsteriskAccountRange';
  }

  public function getFields()
  {
    return array(
      'id'                       => 'Number',
      'ar_organization_unit_id'  => 'ForeignKey',
      'system_prefix'            => 'Text',
      'system_suffix'            => 'Text',
      'system_start_range'       => 'Text',
      'system_end_range'         => 'Text',
      'system_leading_zero'      => 'Number',
      'is_delete'                => 'Boolean',
      'is_physical_delete'       => 'Boolean',
      'user_prefix'              => 'Text',
      'user_suffix'              => 'Text',
      'user_start_range'         => 'Text',
      'generate_range_for_users' => 'Boolean',
      'user_leading_zero'        => 'Number',
      'user_note'                => 'Text',
    );
  }
}
