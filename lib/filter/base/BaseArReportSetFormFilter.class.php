<?php

/**
 * ArReportSet filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArReportSetFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_report_scheduler_id' => new sfWidgetFormPropelChoice(array('model' => 'ArReportScheduler', 'add_empty' => true)),
      'from_date'              => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate(), 'with_empty' => false)),
      'to_date'                => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate(), 'with_empty' => false)),
      'must_be_reviewed'       => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
    ));

    $this->setValidators(array(
      'ar_report_scheduler_id' => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArReportScheduler', 'column' => 'id')),
      'from_date'              => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'to_date'                => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'must_be_reviewed'       => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
    ));

    $this->widgetSchema->setNameFormat('ar_report_set_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArReportSet';
  }

  public function getFields()
  {
    return array(
      'id'                     => 'Number',
      'ar_report_scheduler_id' => 'ForeignKey',
      'from_date'              => 'Date',
      'to_date'                => 'Date',
      'must_be_reviewed'       => 'Boolean',
    );
  }
}
