<?php

/**
 * ArWholesaleCarrier filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArWholesaleCarrierFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'internal_name' => new sfWidgetFormFilterInput(),
      'note'          => new sfWidgetFormFilterInput(),
    ));

    $this->setValidators(array(
      'internal_name' => new sfValidatorPass(array('required' => false)),
      'note'          => new sfValidatorPass(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_wholesale_carrier_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArWholesaleCarrier';
  }

  public function getFields()
  {
    return array(
      'id'            => 'Number',
      'internal_name' => 'Text',
      'note'          => 'Text',
    );
  }
}
