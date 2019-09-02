<?php

/**
 * ArWholesaleNumberTransaction form base class.
 *
 * @method ArWholesaleNumberTransaction getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArWholesaleNumberTransactionForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'from_date'       => new sfWidgetFormInputHidden(),
      'count_numbers'   => new sfWidgetFormInputText(),
      'count_resellers' => new sfWidgetFormInputText(),
      'count_carriers'  => new sfWidgetFormInputText(),
      'reseller_codes'  => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'from_date'       => new sfValidatorChoice(array('choices' => array($this->getObject()->getFromDate()), 'empty_value' => $this->getObject()->getFromDate(), 'required' => false)),
      'count_numbers'   => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'count_resellers' => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'count_carriers'  => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'reseller_codes'  => new sfValidatorString(array('max_length' => 8048, 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_wholesale_number_transaction[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArWholesaleNumberTransaction';
  }


}
