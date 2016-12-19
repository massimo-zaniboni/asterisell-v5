<?php

/**
 * ArReadReport filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArReadReportFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_report_id'               => new sfWidgetFormPropelChoice(array('model' => 'ArReport', 'add_empty' => true)),
      'ar_user_id'                 => new sfWidgetFormPropelChoice(array('model' => 'ArUser', 'add_empty' => true)),
      'seen_or_received_from_user' => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'sent_to_email_at_date'      => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'email_attempts'             => new sfWidgetFormFilterInput(),
    ));

    $this->setValidators(array(
      'ar_report_id'               => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArReport', 'column' => 'id')),
      'ar_user_id'                 => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArUser', 'column' => 'id')),
      'seen_or_received_from_user' => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'sent_to_email_at_date'      => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'email_attempts'             => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
    ));

    $this->widgetSchema->setNameFormat('ar_read_report_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArReadReport';
  }

  public function getFields()
  {
    return array(
      'id'                         => 'Number',
      'ar_report_id'               => 'ForeignKey',
      'ar_user_id'                 => 'ForeignKey',
      'seen_or_received_from_user' => 'Boolean',
      'sent_to_email_at_date'      => 'Date',
      'email_attempts'             => 'Number',
    );
  }
}
