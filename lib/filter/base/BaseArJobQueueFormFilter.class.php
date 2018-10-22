<?php

/**
 * ArJobQueue filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArJobQueueFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'is_part_of'                 => new sfWidgetFormFilterInput(),
      'state'                      => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'created_at'                 => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate(), 'with_empty' => false)),
      'start_at'                   => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'end_at'                     => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'description'                => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'php_data_job_serialization' => new sfWidgetFormFilterInput(),
      'internal_name'              => new sfWidgetFormFilterInput(),
    ));

    $this->setValidators(array(
      'is_part_of'                 => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'state'                      => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'created_at'                 => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'start_at'                   => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'end_at'                     => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'description'                => new sfValidatorPass(array('required' => false)),
      'php_data_job_serialization' => new sfValidatorPass(array('required' => false)),
      'internal_name'              => new sfValidatorPass(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_job_queue_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArJobQueue';
  }

  public function getFields()
  {
    return array(
      'id'                         => 'Number',
      'is_part_of'                 => 'Number',
      'state'                      => 'Number',
      'created_at'                 => 'Date',
      'start_at'                   => 'Date',
      'end_at'                     => 'Date',
      'description'                => 'Text',
      'php_data_job_serialization' => 'Text',
      'internal_name'              => 'Text',
    );
  }
}
