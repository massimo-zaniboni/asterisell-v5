<?php

/**
 * ListCdrsCustomer3Slow filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseListCdrsCustomer3SlowFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
    ));

    $this->setValidators(array(
    ));

    $this->widgetSchema->setNameFormat('list_cdrs_customer3_slow_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ListCdrsCustomer3Slow';
  }

  public function getFields()
  {
    return array(
      'id' => 'Number',
    );
  }
}
