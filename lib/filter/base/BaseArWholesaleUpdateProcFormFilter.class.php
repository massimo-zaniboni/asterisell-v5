<?php

/**
 * ArWholesaleUpdateProc filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArWholesaleUpdateProcFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'csv_comment'    => new sfWidgetFormFilterInput(),
      'csv_last_date'  => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'csv_is_current' => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
    ));

    $this->setValidators(array(
      'csv_comment'    => new sfValidatorPass(array('required' => false)),
      'csv_last_date'  => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'csv_is_current' => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
    ));

    $this->widgetSchema->setNameFormat('ar_wholesale_update_proc_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArWholesaleUpdateProc';
  }

  public function getFields()
  {
    return array(
      'foreign_id'     => 'Number',
      'csv_comment'    => 'Text',
      'csv_last_date'  => 'Date',
      'csv_is_current' => 'Boolean',
    );
  }
}
