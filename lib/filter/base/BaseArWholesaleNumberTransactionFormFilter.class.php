<?php

/**
 * ArWholesaleNumberTransaction filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArWholesaleNumberTransactionFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'count_numbers'   => new sfWidgetFormFilterInput(),
      'count_resellers' => new sfWidgetFormFilterInput(),
      'count_carriers'  => new sfWidgetFormFilterInput(),
      'reseller_codes'  => new sfWidgetFormFilterInput(),
    ));

    $this->setValidators(array(
      'count_numbers'   => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'count_resellers' => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'count_carriers'  => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'reseller_codes'  => new sfValidatorPass(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_wholesale_number_transaction_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArWholesaleNumberTransaction';
  }

  public function getFields()
  {
    return array(
      'from_date'       => 'Date',
      'count_numbers'   => 'Number',
      'count_resellers' => 'Number',
      'count_carriers'  => 'Number',
      'reseller_codes'  => 'Text',
    );
  }
}
