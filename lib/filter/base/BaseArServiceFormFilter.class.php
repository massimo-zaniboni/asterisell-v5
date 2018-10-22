<?php

/**
 * ArService filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArServiceFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'internal_name'                              => new sfWidgetFormFilterInput(),
      'customer_name'                              => new sfWidgetFormFilterInput(),
      'customer_description'                       => new sfWidgetFormFilterInput(),
      'vendor_name'                                => new sfWidgetFormFilterInput(),
      'vendor_description'                         => new sfWidgetFormFilterInput(),
      'external_crm_code'                          => new sfWidgetFormFilterInput(),
      'customer_price_depend_from_activation_date' => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'customer_price_change_with_price_list'      => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'is_enabled'                                 => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'is_applied_only_one_time'                   => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'schedule_timeframe'                         => new sfWidgetFormFilterInput(),
      'was_compiled'                               => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'schedule_from'                              => new sfWidgetFormFilterInput(),
      'schedule_at'                                => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
    ));

    $this->setValidators(array(
      'internal_name'                              => new sfValidatorPass(array('required' => false)),
      'customer_name'                              => new sfValidatorPass(array('required' => false)),
      'customer_description'                       => new sfValidatorPass(array('required' => false)),
      'vendor_name'                                => new sfValidatorPass(array('required' => false)),
      'vendor_description'                         => new sfValidatorPass(array('required' => false)),
      'external_crm_code'                          => new sfValidatorPass(array('required' => false)),
      'customer_price_depend_from_activation_date' => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'customer_price_change_with_price_list'      => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'is_enabled'                                 => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'is_applied_only_one_time'                   => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'schedule_timeframe'                         => new sfValidatorPass(array('required' => false)),
      'was_compiled'                               => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'schedule_from'                              => new sfValidatorPass(array('required' => false)),
      'schedule_at'                                => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
    ));

    $this->widgetSchema->setNameFormat('ar_service_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArService';
  }

  public function getFields()
  {
    return array(
      'id'                                         => 'Number',
      'internal_name'                              => 'Text',
      'customer_name'                              => 'Text',
      'customer_description'                       => 'Text',
      'vendor_name'                                => 'Text',
      'vendor_description'                         => 'Text',
      'external_crm_code'                          => 'Text',
      'customer_price_depend_from_activation_date' => 'Boolean',
      'customer_price_change_with_price_list'      => 'Boolean',
      'is_enabled'                                 => 'Boolean',
      'is_applied_only_one_time'                   => 'Boolean',
      'schedule_timeframe'                         => 'Text',
      'was_compiled'                               => 'Boolean',
      'schedule_from'                              => 'Text',
      'schedule_at'                                => 'Date',
    );
  }
}
