<?php

/**
 * ArParty filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArPartyFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'name'                               => new sfWidgetFormFilterInput(),
      'compact_name'                       => new sfWidgetFormFilterInput(),
      'external_crm_code'                  => new sfWidgetFormFilterInput(),
      'vat'                                => new sfWidgetFormFilterInput(),
      'is_billable'                        => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'legal_address'                      => new sfWidgetFormFilterInput(),
      'legal_city'                         => new sfWidgetFormFilterInput(),
      'legal_zipcode'                      => new sfWidgetFormFilterInput(),
      'legal_state_province'               => new sfWidgetFormFilterInput(),
      'legal_country'                      => new sfWidgetFormFilterInput(),
      'email'                              => new sfWidgetFormFilterInput(),
      'phone'                              => new sfWidgetFormFilterInput(),
      'phone2'                             => new sfWidgetFormFilterInput(),
      'fax'                                => new sfWidgetFormFilterInput(),
      'max_limit_30'                       => new sfWidgetFormFilterInput(),
      'last_email_advise_for_max_limit_30' => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'is_active'                          => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'ar_reseller_id'                     => new sfWidgetFormPropelChoice(array('model' => 'ArReseller', 'add_empty' => true)),
      'migration_field_for_telephone'      => new sfWidgetFormFilterInput(),
      'migration_field_for_adsl'           => new sfWidgetFormFilterInput(),
    ));

    $this->setValidators(array(
      'name'                               => new sfValidatorPass(array('required' => false)),
      'compact_name'                       => new sfValidatorPass(array('required' => false)),
      'external_crm_code'                  => new sfValidatorPass(array('required' => false)),
      'vat'                                => new sfValidatorPass(array('required' => false)),
      'is_billable'                        => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'legal_address'                      => new sfValidatorPass(array('required' => false)),
      'legal_city'                         => new sfValidatorPass(array('required' => false)),
      'legal_zipcode'                      => new sfValidatorPass(array('required' => false)),
      'legal_state_province'               => new sfValidatorPass(array('required' => false)),
      'legal_country'                      => new sfValidatorPass(array('required' => false)),
      'email'                              => new sfValidatorPass(array('required' => false)),
      'phone'                              => new sfValidatorPass(array('required' => false)),
      'phone2'                             => new sfValidatorPass(array('required' => false)),
      'fax'                                => new sfValidatorPass(array('required' => false)),
      'max_limit_30'                       => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'last_email_advise_for_max_limit_30' => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'is_active'                          => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'ar_reseller_id'                     => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArReseller', 'column' => 'id')),
      'migration_field_for_telephone'      => new sfValidatorPass(array('required' => false)),
      'migration_field_for_adsl'           => new sfValidatorPass(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_party_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArParty';
  }

  public function getFields()
  {
    return array(
      'id'                                 => 'Number',
      'name'                               => 'Text',
      'compact_name'                       => 'Text',
      'external_crm_code'                  => 'Text',
      'vat'                                => 'Text',
      'is_billable'                        => 'Boolean',
      'legal_address'                      => 'Text',
      'legal_city'                         => 'Text',
      'legal_zipcode'                      => 'Text',
      'legal_state_province'               => 'Text',
      'legal_country'                      => 'Text',
      'email'                              => 'Text',
      'phone'                              => 'Text',
      'phone2'                             => 'Text',
      'fax'                                => 'Text',
      'max_limit_30'                       => 'Number',
      'last_email_advise_for_max_limit_30' => 'Date',
      'is_active'                          => 'Boolean',
      'ar_reseller_id'                     => 'ForeignKey',
      'migration_field_for_telephone'      => 'Text',
      'migration_field_for_adsl'           => 'Text',
    );
  }
}
