<?php

/**
 * ListCdrsAdmin3Fast filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseListCdrsAdmin3FastFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'destination_type'                 => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'error_destination_type'           => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'operator_type'                    => new sfWidgetFormFilterInput(),
      'ar_communication_channel_type_id' => new sfWidgetFormFilterInput(),
      'vendor_id'                        => new sfWidgetFormFilterInput(),
      'count_of_calls'                   => new sfWidgetFormFilterInput(),
      'billsec'                          => new sfWidgetFormFilterInput(),
      'income'                           => new sfWidgetFormFilterInput(),
      'cost'                             => new sfWidgetFormFilterInput(),
      'cost_saving'                      => new sfWidgetFormFilterInput(),
    ));

    $this->setValidators(array(
      'destination_type'                 => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'error_destination_type'           => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'operator_type'                    => new sfValidatorPass(array('required' => false)),
      'ar_communication_channel_type_id' => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'vendor_id'                        => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'count_of_calls'                   => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'billsec'                          => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'income'                           => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'cost'                             => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'cost_saving'                      => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
    ));

    $this->widgetSchema->setNameFormat('list_cdrs_admin3_fast_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ListCdrsAdmin3Fast';
  }

  public function getFields()
  {
    return array(
      'id'                               => 'Number',
      'destination_type'                 => 'Number',
      'error_destination_type'           => 'Number',
      'operator_type'                    => 'Text',
      'ar_communication_channel_type_id' => 'Number',
      'vendor_id'                        => 'Number',
      'count_of_calls'                   => 'Number',
      'billsec'                          => 'Number',
      'income'                           => 'Number',
      'cost'                             => 'Number',
      'cost_saving'                      => 'Number',
    );
  }
}
