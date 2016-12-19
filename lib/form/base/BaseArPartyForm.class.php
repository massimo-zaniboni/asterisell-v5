<?php

/**
 * ArParty form base class.
 *
 * @method ArParty getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArPartyForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                                 => new sfWidgetFormInputHidden(),
      'name'                               => new sfWidgetFormInputText(),
      'compact_name'                       => new sfWidgetFormInputText(),
      'external_crm_code'                  => new sfWidgetFormInputText(),
      'vat'                                => new sfWidgetFormInputText(),
      'is_billable'                        => new sfWidgetFormInputCheckbox(),
      'legal_address'                      => new sfWidgetFormInputText(),
      'legal_city'                         => new sfWidgetFormInputText(),
      'legal_zipcode'                      => new sfWidgetFormInputText(),
      'legal_state_province'               => new sfWidgetFormInputText(),
      'legal_country'                      => new sfWidgetFormInputText(),
      'email'                              => new sfWidgetFormInputText(),
      'phone'                              => new sfWidgetFormInputText(),
      'phone2'                             => new sfWidgetFormInputText(),
      'fax'                                => new sfWidgetFormInputText(),
      'max_limit_30'                       => new sfWidgetFormInputText(),
      'last_email_advise_for_max_limit_30' => new sfWidgetFormDateTime(),
      'is_active'                          => new sfWidgetFormInputCheckbox(),
      'ar_reseller_id'                     => new sfWidgetFormPropelChoice(array('model' => 'ArReseller', 'add_empty' => true)),
      'migration_field_for_telephone'      => new sfWidgetFormInputText(),
      'migration_field_for_adsl'           => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'id'                                 => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'name'                               => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'compact_name'                       => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'external_crm_code'                  => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'vat'                                => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'is_billable'                        => new sfValidatorBoolean(),
      'legal_address'                      => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'legal_city'                         => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'legal_zipcode'                      => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'legal_state_province'               => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'legal_country'                      => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'email'                              => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'phone'                              => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'phone2'                             => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'fax'                                => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'max_limit_30'                       => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'last_email_advise_for_max_limit_30' => new sfValidatorDateTime(array('required' => false)),
      'is_active'                          => new sfValidatorBoolean(),
      'ar_reseller_id'                     => new sfValidatorPropelChoice(array('model' => 'ArReseller', 'column' => 'id', 'required' => false)),
      'migration_field_for_telephone'      => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'migration_field_for_adsl'           => new sfValidatorString(array('max_length' => 255, 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_party[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArParty';
  }


}
