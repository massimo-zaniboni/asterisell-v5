<?php

/**
 * ListCdrsAdmin3Fast form base class.
 *
 * @method ListCdrsAdmin3Fast getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseListCdrsAdmin3FastForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                               => new sfWidgetFormInputHidden(),
      'destination_type'                 => new sfWidgetFormInputText(),
      'error_destination_type'           => new sfWidgetFormInputText(),
      'operator_type'                    => new sfWidgetFormInputText(),
      'ar_communication_channel_type_id' => new sfWidgetFormInputText(),
      'vendor_id'                        => new sfWidgetFormInputText(),
      'count_of_calls'                   => new sfWidgetFormInputText(),
      'billsec'                          => new sfWidgetFormInputText(),
      'income'                           => new sfWidgetFormInputText(),
      'cost'                             => new sfWidgetFormInputText(),
      'cost_saving'                      => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'id'                               => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'destination_type'                 => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647)),
      'error_destination_type'           => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647)),
      'operator_type'                    => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'ar_communication_channel_type_id' => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'vendor_id'                        => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'count_of_calls'                   => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'billsec'                          => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'income'                           => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'cost'                             => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'cost_saving'                      => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('list_cdrs_admin3_fast[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ListCdrsAdmin3Fast';
  }


}
