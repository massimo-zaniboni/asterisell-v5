<?php

/**
 * ArWholesaleNumberTransactionToUpdate form base class.
 *
 * @method ArWholesaleNumberTransactionToUpdate getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArWholesaleNumberTransactionToUpdateForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'from_date' => new sfWidgetFormInputHidden(),
    ));

    $this->setValidators(array(
      'from_date' => new sfValidatorChoice(array('choices' => array($this->getObject()->getFromDate()), 'empty_value' => $this->getObject()->getFromDate(), 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_wholesale_number_transaction_to_update[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArWholesaleNumberTransactionToUpdate';
  }


}
