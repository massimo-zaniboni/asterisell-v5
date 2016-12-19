<?php

/**
 * ArDailyStatusChange form base class.
 *
 * @method ArDailyStatusChange getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArDailyStatusChangeForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'day'                    => new sfWidgetFormInputHidden(),
      'is_service_cdr'         => new sfWidgetFormInputHidden(),
      'ar_daily_status_job_id' => new sfWidgetFormInputHidden(),
    ));

    $this->setValidators(array(
      'day'                    => new sfValidatorChoice(array('choices' => array($this->getObject()->getDay()), 'empty_value' => $this->getObject()->getDay(), 'required' => false)),
      'is_service_cdr'         => new sfValidatorChoice(array('choices' => array($this->getObject()->getIsServiceCdr()), 'empty_value' => $this->getObject()->getIsServiceCdr(), 'required' => false)),
      'ar_daily_status_job_id' => new sfValidatorPropelChoice(array('model' => 'ArDailyStatusJob', 'column' => 'id', 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_daily_status_change[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArDailyStatusChange';
  }


}
