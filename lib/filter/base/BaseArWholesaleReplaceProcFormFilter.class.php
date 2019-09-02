<?php

/**
 * ArWholesaleReplaceProc filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArWholesaleReplaceProcFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_reseller_id' => new sfWidgetFormPropelChoice(array('model' => 'ArReseller', 'add_empty' => true)),
    ));

    $this->setValidators(array(
      'ar_reseller_id' => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArReseller', 'column' => 'id')),
    ));

    $this->widgetSchema->setNameFormat('ar_wholesale_replace_proc_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArWholesaleReplaceProc';
  }

  public function getFields()
  {
    return array(
      'from_date'      => 'Date',
      'ar_reseller_id' => 'ForeignKey',
    );
  }
}
