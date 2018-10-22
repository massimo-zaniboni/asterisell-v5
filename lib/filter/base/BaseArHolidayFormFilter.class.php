<?php

/**
 * ArHoliday filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArHolidayFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'day_of_month' => new sfWidgetFormFilterInput(),
      'month'        => new sfWidgetFormFilterInput(),
      'year'         => new sfWidgetFormFilterInput(),
      'day_of_week'  => new sfWidgetFormFilterInput(),
      'from_hour'    => new sfWidgetFormFilterInput(),
      'from_minutes' => new sfWidgetFormFilterInput(),
      'to_hour'      => new sfWidgetFormFilterInput(),
      'to_minutes'   => new sfWidgetFormFilterInput(),
      'peak_code'    => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'name'         => new sfWidgetFormFilterInput(),
    ));

    $this->setValidators(array(
      'day_of_month' => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'month'        => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'year'         => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'day_of_week'  => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'from_hour'    => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'from_minutes' => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'to_hour'      => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'to_minutes'   => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'peak_code'    => new sfValidatorPass(array('required' => false)),
      'name'         => new sfValidatorPass(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_holiday_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArHoliday';
  }

  public function getFields()
  {
    return array(
      'id'           => 'Number',
      'day_of_month' => 'Number',
      'month'        => 'Number',
      'year'         => 'Number',
      'day_of_week'  => 'Number',
      'from_hour'    => 'Number',
      'from_minutes' => 'Number',
      'to_hour'      => 'Number',
      'to_minutes'   => 'Number',
      'peak_code'    => 'Text',
      'name'         => 'Text',
    );
  }
}
