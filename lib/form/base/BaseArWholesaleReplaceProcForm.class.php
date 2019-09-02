<?php

/**
 * ArWholesaleReplaceProc form base class.
 *
 * @method ArWholesaleReplaceProc getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArWholesaleReplaceProcForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'from_date'      => new sfWidgetFormInputHidden(),
      'ar_reseller_id' => new sfWidgetFormPropelChoice(array('model' => 'ArReseller', 'add_empty' => true)),
    ));

    $this->setValidators(array(
      'from_date'      => new sfValidatorChoice(array('choices' => array($this->getObject()->getFromDate()), 'empty_value' => $this->getObject()->getFromDate(), 'required' => false)),
      'ar_reseller_id' => new sfValidatorPropelChoice(array('model' => 'ArReseller', 'column' => 'id', 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_wholesale_replace_proc[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArWholesaleReplaceProc';
  }


}
