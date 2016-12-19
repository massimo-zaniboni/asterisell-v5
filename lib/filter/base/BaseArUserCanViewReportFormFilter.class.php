<?php

/**
 * ArUserCanViewReport filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArUserCanViewReportFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
    ));

    $this->setValidators(array(
    ));

    $this->widgetSchema->setNameFormat('ar_user_can_view_report_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArUserCanViewReport';
  }

  public function getFields()
  {
    return array(
      'ar_user_id'   => 'ForeignKey',
      'ar_report_id' => 'ForeignKey',
    );
  }
}
