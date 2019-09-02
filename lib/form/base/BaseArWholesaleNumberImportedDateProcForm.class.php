<?php

/**
 * ArWholesaleNumberImportedDateProc form base class.
 *
 * @method ArWholesaleNumberImportedDateProc getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArWholesaleNumberImportedDateProcForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'from_date' => new sfWidgetFormInputHidden(),
    ));

    $this->setValidators(array(
      'from_date' => new sfValidatorChoice(array('choices' => array($this->getObject()->getFromDate()), 'empty_value' => $this->getObject()->getFromDate(), 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_wholesale_number_imported_date_proc[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArWholesaleNumberImportedDateProc';
  }


}
