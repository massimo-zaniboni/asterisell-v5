<?php

/**
 * ArServicePrice filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArServicePriceFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'internal_name' => new sfWidgetFormFilterInput(),
      'ar_service_id' => new sfWidgetFormPropelChoice(array('model' => 'ArService', 'add_empty' => true)),
      'from_date'     => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate(), 'with_empty' => false)),
      'price'         => new sfWidgetFormFilterInput(array('with_empty' => false)),
    ));

    $this->setValidators(array(
      'internal_name' => new sfValidatorPass(array('required' => false)),
      'ar_service_id' => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArService', 'column' => 'id')),
      'from_date'     => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'price'         => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
    ));

    $this->widgetSchema->setNameFormat('ar_service_price_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArServicePrice';
  }

  public function getFields()
  {
    return array(
      'id'            => 'Number',
      'internal_name' => 'Text',
      'ar_service_id' => 'ForeignKey',
      'from_date'     => 'Date',
      'price'         => 'Number',
    );
  }
}
