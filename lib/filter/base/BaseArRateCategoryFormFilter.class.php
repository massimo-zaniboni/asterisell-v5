<?php

/**
 * ArRateCategory filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArRateCategoryFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'internal_name'     => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'short_description' => new sfWidgetFormFilterInput(),
    ));

    $this->setValidators(array(
      'internal_name'     => new sfValidatorPass(array('required' => false)),
      'short_description' => new sfValidatorPass(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_rate_category_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArRateCategory';
  }

  public function getFields()
  {
    return array(
      'id'                => 'Number',
      'internal_name'     => 'Text',
      'short_description' => 'Text',
    );
  }
}
