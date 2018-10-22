<?php

/**
 * ArCachedErrors filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArCachedErrorsFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'count_of_calls'         => new sfWidgetFormFilterInput(array('with_empty' => false)),
    ));

    $this->setValidators(array(
      'count_of_calls'         => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
    ));

    $this->widgetSchema->setNameFormat('ar_cached_errors_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArCachedErrors';
  }

  public function getFields()
  {
    return array(
      'calldate'               => 'Date',
      'destination_type'       => 'Number',
      'error_destination_type' => 'Number',
      'count_of_calls'         => 'Number',
    );
  }
}
