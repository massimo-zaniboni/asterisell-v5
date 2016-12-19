<?php

/**
 * ArRateSharedWithReseller filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArRateSharedWithResellerFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_rate_id'     => new sfWidgetFormPropelChoice(array('model' => 'ArRate', 'add_empty' => true)),
      'ar_reseller_id' => new sfWidgetFormPropelChoice(array('model' => 'ArReseller', 'add_empty' => true)),
      'is_exported'    => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
    ));

    $this->setValidators(array(
      'ar_rate_id'     => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArRate', 'column' => 'id')),
      'ar_reseller_id' => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArReseller', 'column' => 'id')),
      'is_exported'    => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
    ));

    $this->widgetSchema->setNameFormat('ar_rate_shared_with_reseller_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArRateSharedWithReseller';
  }

  public function getFields()
  {
    return array(
      'ar_rate_id'     => 'ForeignKey',
      'ar_reseller_id' => 'ForeignKey',
      'is_exported'    => 'Boolean',
      'id'             => 'Number',
    );
  }
}
