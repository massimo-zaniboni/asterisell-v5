<?php

/**
 * ArOrganizationUnit filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArOrganizationUnitFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'internal_name'              => new sfWidgetFormFilterInput(),
      'internal_name2'             => new sfWidgetFormFilterInput(),
      'export_code'                => new sfWidgetFormFilterInput(),
      'automatically_managed_from' => new sfWidgetFormFilterInput(array('with_empty' => false)),
    ));

    $this->setValidators(array(
      'internal_name'              => new sfValidatorPass(array('required' => false)),
      'internal_name2'             => new sfValidatorPass(array('required' => false)),
      'export_code'                => new sfValidatorPass(array('required' => false)),
      'automatically_managed_from' => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
    ));

    $this->widgetSchema->setNameFormat('ar_organization_unit_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArOrganizationUnit';
  }

  public function getFields()
  {
    return array(
      'id'                         => 'Number',
      'internal_name'              => 'Text',
      'internal_name2'             => 'Text',
      'export_code'                => 'Text',
      'automatically_managed_from' => 'Number',
    );
  }
}
