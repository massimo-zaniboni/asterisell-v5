<?php

/**
 * ArReportIsAccessibleFromOrganizationUnit filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArReportIsAccessibleFromOrganizationUnitFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
    ));

    $this->setValidators(array(
    ));

    $this->widgetSchema->setNameFormat('ar_report_is_accessible_from_organization_unit_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArReportIsAccessibleFromOrganizationUnit';
  }

  public function getFields()
  {
    return array(
      'ar_report_id'            => 'ForeignKey',
      'ar_organization_unit_id' => 'ForeignKey',
    );
  }
}
