<?php

/**
 * ListCdrsCustomer1Slow filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseListCdrsCustomer1SlowFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
    ));

    $this->setValidators(array(
    ));

    $this->widgetSchema->setNameFormat('list_cdrs_customer1_slow_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ListCdrsCustomer1Slow';
  }

  public function getFields()
  {
    return array(
      'id' => 'Number',
    );
  }
}
