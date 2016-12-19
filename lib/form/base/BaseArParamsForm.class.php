<?php

/**
 * ArParams form base class.
 *
 * @method ArParams getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArParamsForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                                                        => new sfWidgetFormInputHidden(),
      'name'                                                      => new sfWidgetFormInputText(),
      'is_default'                                                => new sfWidgetFormInputCheckbox(),
      'service_name'                                              => new sfWidgetFormInputText(),
      'service_provider_website'                                  => new sfWidgetFormInputText(),
      'service_provider_email'                                    => new sfWidgetFormInputText(),
      'vat_tax_perc'                                              => new sfWidgetFormInputText(),
      'logo_image'                                                => new sfWidgetFormInputText(),
      'slogan'                                                    => new sfWidgetFormInputText(),
      'logo_image_in_invoices'                                    => new sfWidgetFormInputText(),
      'footer'                                                    => new sfWidgetFormInputText(),
      'user_message'                                              => new sfWidgetFormInputText(),
      'legal_name'                                                => new sfWidgetFormInputText(),
      'external_crm_code'                                         => new sfWidgetFormInputText(),
      'vat'                                                       => new sfWidgetFormInputText(),
      'legal_address'                                             => new sfWidgetFormInputText(),
      'legal_website'                                             => new sfWidgetFormInputText(),
      'legal_city'                                                => new sfWidgetFormInputText(),
      'legal_zipcode'                                             => new sfWidgetFormInputText(),
      'legal_state_province'                                      => new sfWidgetFormInputText(),
      'legal_country'                                             => new sfWidgetFormInputText(),
      'legal_email'                                               => new sfWidgetFormInputText(),
      'legal_phone'                                               => new sfWidgetFormInputText(),
      'phone2'                                                    => new sfWidgetFormInputText(),
      'legal_fax'                                                 => new sfWidgetFormInputText(),
      'invoice_notes'                                             => new sfWidgetFormInputText(),
      'invoice_payment_terms'                                     => new sfWidgetFormInputText(),
      'sender_name_on_invoicing_emails'                           => new sfWidgetFormInputText(),
      'invoicing_email_address'                                   => new sfWidgetFormInputText(),
      'logo_html_color'                                           => new sfWidgetFormInputText(),
      'html_notes_on_the_login_form'                              => new sfWidgetFormInputText(),
      'official_calldate'                                         => new sfWidgetFormDateTime(),
      'scheduled_rerate_from_official_calldate'                   => new sfWidgetFormInputCheckbox(),
      'new_imported_cdrs_from_calldate'                           => new sfWidgetFormDateTime(),
      'new_imported_cdrs_to_calldate'                             => new sfWidgetFormDateTime(),
      'scheduled_rerate_from_specific_calldate'                   => new sfWidgetFormDateTime(),
      'scheduled_rerate_to_specific_calldate'                     => new sfWidgetFormDateTime(),
      'scheduled_imported_services_rerate_from_specific_calldate' => new sfWidgetFormDateTime(),
      'scheduled_imported_services_rerate_to_specific_calldate'   => new sfWidgetFormDateTime(),
      'current_count_of_rerating_failed_attempts'                 => new sfWidgetFormInputText(),
      'current_rerating_event_is_running'                         => new sfWidgetFormInputCheckbox(),
      'should_reschedule_rerate_from_official_calldate'           => new sfWidgetFormInputCheckbox(),
      'wait_for_scheduled_rerate'                                 => new sfWidgetFormInputCheckbox(),
      'clean_error_table'                                         => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'id'                                                        => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'name'                                                      => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'is_default'                                                => new sfValidatorBoolean(array('required' => false)),
      'service_name'                                              => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'service_provider_website'                                  => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'service_provider_email'                                    => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'vat_tax_perc'                                              => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647)),
      'logo_image'                                                => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'slogan'                                                    => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
      'logo_image_in_invoices'                                    => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'footer'                                                    => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'user_message'                                              => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'legal_name'                                                => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'external_crm_code'                                         => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'vat'                                                       => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'legal_address'                                             => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'legal_website'                                             => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'legal_city'                                                => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'legal_zipcode'                                             => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'legal_state_province'                                      => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'legal_country'                                             => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'legal_email'                                               => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'legal_phone'                                               => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'phone2'                                                    => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'legal_fax'                                                 => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'invoice_notes'                                             => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'invoice_payment_terms'                                     => new sfValidatorString(array('max_length' => 2048, 'required' => false)),
      'sender_name_on_invoicing_emails'                           => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'invoicing_email_address'                                   => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'logo_html_color'                                           => new sfValidatorString(array('max_length' => 12, 'required' => false)),
      'html_notes_on_the_login_form'                              => new sfValidatorString(array('max_length' => 2048, 'required' => false)),
      'official_calldate'                                         => new sfValidatorDateTime(array('required' => false)),
      'scheduled_rerate_from_official_calldate'                   => new sfValidatorBoolean(),
      'new_imported_cdrs_from_calldate'                           => new sfValidatorDateTime(array('required' => false)),
      'new_imported_cdrs_to_calldate'                             => new sfValidatorDateTime(array('required' => false)),
      'scheduled_rerate_from_specific_calldate'                   => new sfValidatorDateTime(array('required' => false)),
      'scheduled_rerate_to_specific_calldate'                     => new sfValidatorDateTime(array('required' => false)),
      'scheduled_imported_services_rerate_from_specific_calldate' => new sfValidatorDateTime(array('required' => false)),
      'scheduled_imported_services_rerate_to_specific_calldate'   => new sfValidatorDateTime(array('required' => false)),
      'current_count_of_rerating_failed_attempts'                 => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647)),
      'current_rerating_event_is_running'                         => new sfValidatorBoolean(),
      'should_reschedule_rerate_from_official_calldate'           => new sfValidatorBoolean(),
      'wait_for_scheduled_rerate'                                 => new sfValidatorBoolean(),
      'clean_error_table'                                         => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647)),
    ));

    $this->widgetSchema->setNameFormat('ar_params[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArParams';
  }


}
