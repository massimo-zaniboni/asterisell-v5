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
    ));

    $this->setValidators(array(
      'day_of_month' => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'month'        => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'year'         => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'day_of_week'  => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
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
    );
  }
}
