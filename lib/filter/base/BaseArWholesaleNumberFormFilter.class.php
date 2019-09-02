<?php

/**
 * ArWholesaleNumber filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArWholesaleNumberFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'telephone_number'            => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'from_date'                   => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate(), 'with_empty' => false)),
      'exists'                      => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'extension_codes'             => new sfWidgetFormFilterInput(),
      'use_default_extension_codes' => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'ar_reseller_id'              => new sfWidgetFormPropelChoice(array('model' => 'ArReseller', 'add_empty' => true)),
      'ar_wholesale_carrier_id'     => new sfWidgetFormPropelChoice(array('model' => 'ArWholesaleCarrier', 'add_empty' => true)),
      'income_price'                => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'cost_price'                  => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'csv_comment'                 => new sfWidgetFormFilterInput(),
      'csv_last_date'               => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'csv_to_delete'               => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'csv_is_current'              => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
    ));

    $this->setValidators(array(
      'telephone_number'            => new sfValidatorPass(array('required' => false)),
      'from_date'                   => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'exists'                      => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'extension_codes'             => new sfValidatorPass(array('required' => false)),
      'use_default_extension_codes' => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'ar_reseller_id'              => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArReseller', 'column' => 'id')),
      'ar_wholesale_carrier_id'     => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArWholesaleCarrier', 'column' => 'id')),
      'income_price'                => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'cost_price'                  => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'csv_comment'                 => new sfValidatorPass(array('required' => false)),
      'csv_last_date'               => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'csv_to_delete'               => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'csv_is_current'              => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
    ));

    $this->widgetSchema->setNameFormat('ar_wholesale_number_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArWholesaleNumber';
  }

  public function getFields()
  {
    return array(
      'id'                          => 'Number',
      'telephone_number'            => 'Text',
      'from_date'                   => 'Date',
      'exists'                      => 'Boolean',
      'extension_codes'             => 'Text',
      'use_default_extension_codes' => 'Boolean',
      'ar_reseller_id'              => 'ForeignKey',
      'ar_wholesale_carrier_id'     => 'ForeignKey',
      'income_price'                => 'Number',
      'cost_price'                  => 'Number',
      'csv_comment'                 => 'Text',
      'csv_last_date'               => 'Date',
      'csv_to_delete'               => 'Boolean',
      'csv_is_current'              => 'Boolean',
    );
  }
}
