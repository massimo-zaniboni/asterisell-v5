<?php

/**
 * ListCdrsCustomer3Fast filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseListCdrsCustomer3FastFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
    ));

    $this->setValidators(array(
    ));

    $this->widgetSchema->setNameFormat('list_cdrs_customer3_fast_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ListCdrsCustomer3Fast';
  }

  public function getFields()
  {
    return array(
      'id' => 'Number',
    );
  }
}
