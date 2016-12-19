<?php

/**
 * ArRateFormat filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArRateFormatFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'short_description'    => new sfWidgetFormFilterInput(),
      'detailed_description' => new sfWidgetFormFilterInput(),
      'internal_name'        => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'order_name'           => new sfWidgetFormFilterInput(),
    ));

    $this->setValidators(array(
      'short_description'    => new sfValidatorPass(array('required' => false)),
      'detailed_description' => new sfValidatorPass(array('required' => false)),
      'internal_name'        => new sfValidatorPass(array('required' => false)),
      'order_name'           => new sfValidatorPass(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_rate_format_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArRateFormat';
  }

  public function getFields()
  {
    return array(
      'id'                   => 'Number',
      'short_description'    => 'Text',
      'detailed_description' => 'Text',
      'internal_name'        => 'Text',
      'order_name'           => 'Text',
    );
  }
}
