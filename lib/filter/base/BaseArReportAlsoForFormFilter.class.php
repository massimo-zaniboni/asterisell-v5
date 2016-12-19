<?php

/**
 * ArReportAlsoFor filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArReportAlsoForFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
    ));

    $this->setValidators(array(
    ));

    $this->widgetSchema->setNameFormat('ar_report_also_for_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArReportAlsoFor';
  }

  public function getFields()
  {
    return array(
      'ar_report_id' => 'ForeignKey',
      'ar_role_id'   => 'ForeignKey',
    );
  }
}
