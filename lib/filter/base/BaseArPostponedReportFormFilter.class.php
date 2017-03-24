<?php

/**
 * ArPostponedReport filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArPostponedReportFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
    ));

    $this->setValidators(array(
    ));

    $this->widgetSchema->setNameFormat('ar_postponed_report_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArPostponedReport';
  }

  public function getFields()
  {
    return array(
      'ar_report_set_id'        => 'ForeignKey',
      'ar_organization_unit_id' => 'ForeignKey',
    );
  }
}
