<?php

/**
 * ArUpgradingJob filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArUpgradingJobFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'code'           => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'start_time'     => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'end_time'       => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'sucessful'      => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'status_message' => new sfWidgetFormFilterInput(),
    ));

    $this->setValidators(array(
      'code'           => new sfValidatorPass(array('required' => false)),
      'start_time'     => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'end_time'       => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'sucessful'      => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'status_message' => new sfValidatorPass(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_upgrading_job_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArUpgradingJob';
  }

  public function getFields()
  {
    return array(
      'id'             => 'Number',
      'code'           => 'Text',
      'start_time'     => 'Date',
      'end_time'       => 'Date',
      'sucessful'      => 'Boolean',
      'status_message' => 'Text',
    );
  }
}
