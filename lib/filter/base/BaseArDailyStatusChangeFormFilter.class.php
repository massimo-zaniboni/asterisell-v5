<?php

/**
 * ArDailyStatusChange filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArDailyStatusChangeFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
    ));

    $this->setValidators(array(
    ));

    $this->widgetSchema->setNameFormat('ar_daily_status_change_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArDailyStatusChange';
  }

  public function getFields()
  {
    return array(
      'day'                    => 'Date',
      'is_service_cdr'         => 'Boolean',
      'ar_daily_status_job_id' => 'ForeignKey',
    );
  }
}
