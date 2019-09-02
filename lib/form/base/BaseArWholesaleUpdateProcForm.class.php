<?php

/**
 * ArWholesaleUpdateProc form base class.
 *
 * @method ArWholesaleUpdateProc getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArWholesaleUpdateProcForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'foreign_id'     => new sfWidgetFormInputHidden(),
      'csv_comment'    => new sfWidgetFormInputText(),
      'csv_last_date'  => new sfWidgetFormDateTime(),
      'csv_is_current' => new sfWidgetFormInputCheckbox(),
    ));

    $this->setValidators(array(
      'foreign_id'     => new sfValidatorChoice(array('choices' => array($this->getObject()->getForeignId()), 'empty_value' => $this->getObject()->getForeignId(), 'required' => false)),
      'csv_comment'    => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'csv_last_date'  => new sfValidatorDateTime(array('required' => false)),
      'csv_is_current' => new sfValidatorBoolean(),
    ));

    $this->widgetSchema->setNameFormat('ar_wholesale_update_proc[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArWholesaleUpdateProc';
  }


}
