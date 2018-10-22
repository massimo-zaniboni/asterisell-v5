<?php

/**
 * ArParams filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArParamsFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'name'                                            => new sfWidgetFormFilterInput(),
      'is_default'                                      => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'service_name'                                    => new sfWidgetFormFilterInput(),
      'service_provider_website'                        => new sfWidgetFormFilterInput(),
      'service_provider_email'                          => new sfWidgetFormFilterInput(),
      'vat_tax_perc'                                    => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'logo_image'                                      => new sfWidgetFormFilterInput(),
      'slogan'                                          => new sfWidgetFormFilterInput(),
      'logo_image_in_invoices'                          => new sfWidgetFormFilterInput(),
      'footer'                                          => new sfWidgetFormFilterInput(),
      'user_message'                                    => new sfWidgetFormFilterInput(),
      'legal_name'                                      => new sfWidgetFormFilterInput(),
      'external_crm_code'                               => new sfWidgetFormFilterInput(),
      'vat'                                             => new sfWidgetFormFilterInput(),
      'legal_address'                                   => new sfWidgetFormFilterInput(),
      'legal_website'                                   => new sfWidgetFormFilterInput(),
      'legal_city'                                      => new sfWidgetFormFilterInput(),
      'legal_zipcode'                                   => new sfWidgetFormFilterInput(),
      'legal_state_province'                            => new sfWidgetFormFilterInput(),
      'legal_country'                                   => new sfWidgetFormFilterInput(),
      'legal_email'                                     => new sfWidgetFormFilterInput(),
      'legal_phone'                                     => new sfWidgetFormFilterInput(),
      'phone2'                                          => new sfWidgetFormFilterInput(),
      'legal_fax'                                       => new sfWidgetFormFilterInput(),
      'invoice_notes'                                   => new sfWidgetFormFilterInput(),
      'invoice_payment_terms'                           => new sfWidgetFormFilterInput(),
      'invoice_payment_due_in_xx_days'                  => new sfWidgetFormFilterInput(),
      'sender_name_on_invoicing_emails'                 => new sfWidgetFormFilterInput(),
      'invoicing_email_address'                         => new sfWidgetFormFilterInput(),
      'logo_html_color'                                 => new sfWidgetFormFilterInput(),
      'html_notes_on_the_login_form'                    => new sfWidgetFormFilterInput(),
      'official_calldate'                               => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'scheduled_rerate_from_official_calldate'         => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'new_imported_cdrs_from_calldate'                 => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'scheduled_rerate_from_specific_calldate'         => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'current_count_of_rerating_failed_attempts'       => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'current_rerating_event_is_running'               => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'should_reschedule_rerate_from_official_calldate' => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'wait_for_scheduled_rerate'                       => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'clean_error_table'                               => new sfWidgetFormFilterInput(array('with_empty' => false)),
    ));

    $this->setValidators(array(
      'name'                                            => new sfValidatorPass(array('required' => false)),
      'is_default'                                      => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'service_name'                                    => new sfValidatorPass(array('required' => false)),
      'service_provider_website'                        => new sfValidatorPass(array('required' => false)),
      'service_provider_email'                          => new sfValidatorPass(array('required' => false)),
      'vat_tax_perc'                                    => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'logo_image'                                      => new sfValidatorPass(array('required' => false)),
      'slogan'                                          => new sfValidatorPass(array('required' => false)),
      'logo_image_in_invoices'                          => new sfValidatorPass(array('required' => false)),
      'footer'                                          => new sfValidatorPass(array('required' => false)),
      'user_message'                                    => new sfValidatorPass(array('required' => false)),
      'legal_name'                                      => new sfValidatorPass(array('required' => false)),
      'external_crm_code'                               => new sfValidatorPass(array('required' => false)),
      'vat'                                             => new sfValidatorPass(array('required' => false)),
      'legal_address'                                   => new sfValidatorPass(array('required' => false)),
      'legal_website'                                   => new sfValidatorPass(array('required' => false)),
      'legal_city'                                      => new sfValidatorPass(array('required' => false)),
      'legal_zipcode'                                   => new sfValidatorPass(array('required' => false)),
      'legal_state_province'                            => new sfValidatorPass(array('required' => false)),
      'legal_country'                                   => new sfValidatorPass(array('required' => false)),
      'legal_email'                                     => new sfValidatorPass(array('required' => false)),
      'legal_phone'                                     => new sfValidatorPass(array('required' => false)),
      'phone2'                                          => new sfValidatorPass(array('required' => false)),
      'legal_fax'                                       => new sfValidatorPass(array('required' => false)),
      'invoice_notes'                                   => new sfValidatorPass(array('required' => false)),
      'invoice_payment_terms'                           => new sfValidatorPass(array('required' => false)),
      'invoice_payment_due_in_xx_days'                  => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'sender_name_on_invoicing_emails'                 => new sfValidatorPass(array('required' => false)),
      'invoicing_email_address'                         => new sfValidatorPass(array('required' => false)),
      'logo_html_color'                                 => new sfValidatorPass(array('required' => false)),
      'html_notes_on_the_login_form'                    => new sfValidatorPass(array('required' => false)),
      'official_calldate'                               => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'scheduled_rerate_from_official_calldate'         => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'new_imported_cdrs_from_calldate'                 => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'scheduled_rerate_from_specific_calldate'         => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'current_count_of_rerating_failed_attempts'       => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'current_rerating_event_is_running'               => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'should_reschedule_rerate_from_official_calldate' => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'wait_for_scheduled_rerate'                       => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'clean_error_table'                               => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
    ));

    $this->widgetSchema->setNameFormat('ar_params_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArParams';
  }

  public function getFields()
  {
    return array(
      'id'                                              => 'Number',
      'name'                                            => 'Text',
      'is_default'                                      => 'Boolean',
      'service_name'                                    => 'Text',
      'service_provider_website'                        => 'Text',
      'service_provider_email'                          => 'Text',
      'vat_tax_perc'                                    => 'Number',
      'logo_image'                                      => 'Text',
      'slogan'                                          => 'Text',
      'logo_image_in_invoices'                          => 'Text',
      'footer'                                          => 'Text',
      'user_message'                                    => 'Text',
      'legal_name'                                      => 'Text',
      'external_crm_code'                               => 'Text',
      'vat'                                             => 'Text',
      'legal_address'                                   => 'Text',
      'legal_website'                                   => 'Text',
      'legal_city'                                      => 'Text',
      'legal_zipcode'                                   => 'Text',
      'legal_state_province'                            => 'Text',
      'legal_country'                                   => 'Text',
      'legal_email'                                     => 'Text',
      'legal_phone'                                     => 'Text',
      'phone2'                                          => 'Text',
      'legal_fax'                                       => 'Text',
      'invoice_notes'                                   => 'Text',
      'invoice_payment_terms'                           => 'Text',
      'invoice_payment_due_in_xx_days'                  => 'Number',
      'sender_name_on_invoicing_emails'                 => 'Text',
      'invoicing_email_address'                         => 'Text',
      'logo_html_color'                                 => 'Text',
      'html_notes_on_the_login_form'                    => 'Text',
      'official_calldate'                               => 'Date',
      'scheduled_rerate_from_official_calldate'         => 'Boolean',
      'new_imported_cdrs_from_calldate'                 => 'Date',
      'scheduled_rerate_from_specific_calldate'         => 'Date',
      'current_count_of_rerating_failed_attempts'       => 'Number',
      'current_rerating_event_is_running'               => 'Boolean',
      'should_reschedule_rerate_from_official_calldate' => 'Boolean',
      'wait_for_scheduled_rerate'                       => 'Boolean',
      'clean_error_table'                               => 'Number',
    );
  }
}
