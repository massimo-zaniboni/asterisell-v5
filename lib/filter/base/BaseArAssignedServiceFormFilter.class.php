<?php

/**
 * ArAssignedService filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArAssignedServiceFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'internal_name'           => new sfWidgetFormFilterInput(),
      'external_crm_code'       => new sfWidgetFormFilterInput(),
      'from_date'               => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate(), 'with_empty' => false)),
      'ar_service_id'           => new sfWidgetFormPropelChoice(array('model' => 'ArService', 'add_empty' => true)),
      'ar_organization_unit_id' => new sfWidgetFormPropelChoice(array('model' => 'ArOrganizationUnit', 'add_empty' => true)),
      'nr_of_items'             => new sfWidgetFormFilterInput(),
      'discount'                => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'note'                    => new sfWidgetFormFilterInput(),
    ));

    $this->setValidators(array(
      'internal_name'           => new sfValidatorPass(array('required' => false)),
      'external_crm_code'       => new sfValidatorPass(array('required' => false)),
      'from_date'               => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'ar_service_id'           => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArService', 'column' => 'id')),
      'ar_organization_unit_id' => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArOrganizationUnit', 'column' => 'id')),
      'nr_of_items'             => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'discount'                => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'note'                    => new sfValidatorPass(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_assigned_service_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArAssignedService';
  }

  public function getFields()
  {
    return array(
      'id'                      => 'Number',
      'internal_name'           => 'Text',
      'external_crm_code'       => 'Text',
      'from_date'               => 'Date',
      'ar_service_id'           => 'ForeignKey',
      'ar_organization_unit_id' => 'ForeignKey',
      'nr_of_items'             => 'Number',
      'discount'                => 'Number',
      'note'                    => 'Text',
    );
  }
}
