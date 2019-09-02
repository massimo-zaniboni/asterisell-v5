<?php

/**
 * ArWholesaleNumberImportedDateProc filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArWholesaleNumberImportedDateProcFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
    ));

    $this->setValidators(array(
    ));

    $this->widgetSchema->setNameFormat('ar_wholesale_number_imported_date_proc_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArWholesaleNumberImportedDateProc';
  }

  public function getFields()
  {
    return array(
      'from_date' => 'Date',
    );
  }
}
