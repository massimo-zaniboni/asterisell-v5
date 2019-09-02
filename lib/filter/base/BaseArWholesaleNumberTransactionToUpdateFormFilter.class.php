<?php

/**
 * ArWholesaleNumberTransactionToUpdate filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArWholesaleNumberTransactionToUpdateFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
    ));

    $this->setValidators(array(
    ));

    $this->widgetSchema->setNameFormat('ar_wholesale_number_transaction_to_update_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArWholesaleNumberTransactionToUpdate';
  }

  public function getFields()
  {
    return array(
      'from_date' => 'Date',
    );
  }
}
