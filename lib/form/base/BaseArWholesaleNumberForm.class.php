<?php

/**
 * ArWholesaleNumber form base class.
 *
 * @method ArWholesaleNumber getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArWholesaleNumberForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                          => new sfWidgetFormInputHidden(),
      'telephone_number'            => new sfWidgetFormInputText(),
      'from_date'                   => new sfWidgetFormDateTime(),
      'exists'                      => new sfWidgetFormInputCheckbox(),
      'extension_codes'             => new sfWidgetFormInputText(),
      'use_default_extension_codes' => new sfWidgetFormInputCheckbox(),
      'ar_reseller_id'              => new sfWidgetFormPropelChoice(array('model' => 'ArReseller', 'add_empty' => true)),
      'ar_wholesale_carrier_id'     => new sfWidgetFormPropelChoice(array('model' => 'ArWholesaleCarrier', 'add_empty' => true)),
      'income_price'                => new sfWidgetFormInputText(),
      'cost_price'                  => new sfWidgetFormInputText(),
      'csv_comment'                 => new sfWidgetFormInputText(),
      'csv_last_date'               => new sfWidgetFormDateTime(),
      'csv_to_delete'               => new sfWidgetFormInputCheckbox(),
      'csv_is_current'              => new sfWidgetFormInputCheckbox(),
    ));

    $this->setValidators(array(
      'id'                          => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'telephone_number'            => new sfValidatorString(array('max_length' => 255)),
      'from_date'                   => new sfValidatorDateTime(),
      'exists'                      => new sfValidatorBoolean(),
      'extension_codes'             => new sfValidatorString(array('max_length' => 5024, 'required' => false)),
      'use_default_extension_codes' => new sfValidatorBoolean(array('required' => false)),
      'ar_reseller_id'              => new sfValidatorPropelChoice(array('model' => 'ArReseller', 'column' => 'id', 'required' => false)),
      'ar_wholesale_carrier_id'     => new sfValidatorPropelChoice(array('model' => 'ArWholesaleCarrier', 'column' => 'id', 'required' => false)),
      'income_price'                => new sfValidatorInteger(array('min' => -9.2233720368548E+18, 'max' => 9223372036854775807)),
      'cost_price'                  => new sfValidatorInteger(array('min' => -9.2233720368548E+18, 'max' => 9223372036854775807)),
      'csv_comment'                 => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'csv_last_date'               => new sfValidatorDateTime(array('required' => false)),
      'csv_to_delete'               => new sfValidatorBoolean(),
      'csv_is_current'              => new sfValidatorBoolean(),
    ));

    $this->widgetSchema->setNameFormat('ar_wholesale_number[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArWholesaleNumber';
  }


}
