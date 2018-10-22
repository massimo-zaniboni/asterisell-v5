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
      'note'                               => new sfWidgetFormInputText(),
      'external_crm_code'                  => new sfWidgetFormInputText(),
      'contract_number'                    => new sfWidgetFormInputText(),
      'vat'                                => new sfWidgetFormInputText(),
      'legal_registration_number'          => new sfWidgetFormInputText(),
      'is_billable'                        => new sfWidgetFormInputCheckbox(),
      'legal_address'                      => new sfWidgetFormInputText(),
      'legal_city'                         => new sfWidgetFormInputText(),
      'legal_zipcode'                      => new sfWidgetFormInputText(),
      'legal_state_province'               => new sfWidgetFormInputText(),
      'legal_country'                      => new sfWidgetFormInputText(),
      'email'                              => new sfWidgetFormInputText(),
      'contact_name'                       => new sfWidgetFormInputText(),
      'phone'                              => new sfWidgetFormInputText(),
      'phone2'                             => new sfWidgetFormInputText(),
      'fax'                                => new sfWidgetFormInputText(),
      'web_site'                           => new sfWidgetFormInputText(),
      'max_limit_30'                       => new sfWidgetFormInputText(),
      'last_email_advise_for_max_limit_30' => new sfWidgetFormDateTime(),
      'is_active'                          => new sfWidgetFormInputCheckbox(),
      'ar_reseller_id'                     => new sfWidgetFormPropelChoice(array('model' => 'ArReseller', 'add_empty' => true)),
      'migration_field_for_telephone'      => new sfWidgetFormInputText(),
      'migration_field_for_adsl'           => new sfWidgetFormInputText(),
      'payment_iban'                       => new sfWidgetFormInputText(),
      'payment_bic'                        => new sfWidgetFormInputText(),
      'payment_sepa'                       => new sfWidgetFormInputText(),
      'payment_info'                       => new sfWidgetFormInputText(),
      'ar_party_has_tag_list'              => new sfWidgetFormPropelChoice(array('multiple' => true, 'model' => 'ArTag')),
    ));

    $this->setValidators(array(
      'id'                                 => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'name'                               => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'compact_name'                       => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'note'                               => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
      'external_crm_code'                  => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'contract_number'                    => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'vat'                                => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'legal_registration_number'          => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'is_billable'                        => new sfValidatorBoolean(),
      'legal_address'                      => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'legal_city'                         => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'legal_zipcode'                      => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'legal_state_province'               => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'legal_country'                      => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'email'                              => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'contact_name'                       => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'phone'                              => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'phone2'                             => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'fax'                                => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'web_site'                           => new sfValidatorString(array('max_length' => 120, 'required' => false)),
      'max_limit_30'                       => new sfValidatorInteger(array('min' => -9.2233720368548E+18, 'max' => 9223372036854775807, 'required' => false)),
      'last_email_advise_for_max_limit_30' => new sfValidatorDateTime(array('required' => false)),
      'is_active'                          => new sfValidatorBoolean(),
      'ar_reseller_id'                     => new sfValidatorPropelChoice(array('model' => 'ArReseller', 'column' => 'id', 'required' => false)),
      'migration_field_for_telephone'      => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'migration_field_for_adsl'           => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'payment_iban'                       => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'payment_bic'                        => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'payment_sepa'                       => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'payment_info'                       => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'ar_party_has_tag_list'              => new sfValidatorPropelChoice(array('multiple' => true, 'model' => 'ArTag', 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_party[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArParty';
  }


  public function updateDefaultsFromObject()
  {
    parent::updateDefaultsFromObject();

    if (isset($this->widgetSchema['ar_party_has_tag_list']))
    {
      $values = array();
      foreach ($this->object->getArPartyHasTags() as $obj)
      {
        $values[] = $obj->getArTagId();
      }

      $this->setDefault('ar_party_has_tag_list', $values);
    }

  }

  protected function doSave($con = null)
  {
    parent::doSave($con);

    $this->saveArPartyHasTagList($con);
  }

  public function saveArPartyHasTagList($con = null)
  {
    if (!$this->isValid())
    {
      throw $this->getErrorSchema();
    }

    if (!isset($this->widgetSchema['ar_party_has_tag_list']))
    {
      // somebody has unset this widget
      return;
    }

    if (null === $con)
    {
      $con = $this->getConnection();
    }

    $c = new Criteria();
    $c->add(ArPartyHasTagPeer::AR_PARTY_ID, $this->object->getPrimaryKey());
    ArPartyHasTagPeer::doDelete($c, $con);

    $values = $this->getValue('ar_party_has_tag_list');
    if (is_array($values))
    {
      foreach ($values as $value)
      {
        $obj = new ArPartyHasTag();
        $obj->setArPartyId($this->object->getPrimaryKey());
        $obj->setArTagId($value);
        $obj->save();
      }
    }
  }

}
