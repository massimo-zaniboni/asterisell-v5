<?php

/**
 * ArVendorDomain filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArVendorDomainFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'internal_name'                    => new sfWidgetFormFilterInput(),
      'ar_vendor_id'                     => new sfWidgetFormPropelChoice(array('model' => 'ArVendor', 'add_empty' => true)),
      'ar_communication_channel_type_id' => new sfWidgetFormPropelChoice(array('model' => 'ArCommunicationChannelType', 'add_empty' => true)),
      'domain'                           => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'is_prefix'                        => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'is_suffix'                        => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'from'                             => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate(), 'with_empty' => false)),
      'to'                               => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
    ));

    $this->setValidators(array(
      'internal_name'                    => new sfValidatorPass(array('required' => false)),
      'ar_vendor_id'                     => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArVendor', 'column' => 'id')),
      'ar_communication_channel_type_id' => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArCommunicationChannelType', 'column' => 'id')),
      'domain'                           => new sfValidatorPass(array('required' => false)),
      'is_prefix'                        => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'is_suffix'                        => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'from'                             => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'to'                               => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
    ));

    $this->widgetSchema->setNameFormat('ar_vendor_domain_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArVendorDomain';
  }

  public function getFields()
  {
    return array(
      'id'                               => 'Number',
      'internal_name'                    => 'Text',
      'ar_vendor_id'                     => 'ForeignKey',
      'ar_communication_channel_type_id' => 'ForeignKey',
      'domain'                           => 'Text',
      'is_prefix'                        => 'Boolean',
      'is_suffix'                        => 'Boolean',
      'from'                             => 'Date',
      'to'                               => 'Date',
    );
  }
}
