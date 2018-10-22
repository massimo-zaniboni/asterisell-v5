<?php

/**
 * ArCachedGroupedCdr filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArCachedGroupedCdrFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'count_of_calls'                   => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'billsec'                          => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'income'                           => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'cost_saving'                      => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'cost'                             => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'id'                               => new sfWidgetFormFilterInput(array('with_empty' => false)),
    ));

    $this->setValidators(array(
      'count_of_calls'                   => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'billsec'                          => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'income'                           => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'cost_saving'                      => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'cost'                             => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'id'                               => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
    ));

    $this->widgetSchema->setNameFormat('ar_cached_grouped_cdr_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArCachedGroupedCdr';
  }

  public function getFields()
  {
    return array(
      'cached_parent_id_hierarchy'       => 'Text',
      'billable_ar_organization_unit_id' => 'Number',
      'calldate'                         => 'Date',
      'destination_type'                 => 'Number',
      'ar_communication_channel_type_id' => 'Number',
      'operator_type'                    => 'Text',
      'ar_vendor_id'                     => 'Number',
      'geographic_location'              => 'Text',
      'count_of_calls'                   => 'Number',
      'billsec'                          => 'Number',
      'income'                           => 'Number',
      'cost_saving'                      => 'Number',
      'cost'                             => 'Number',
      'id'                               => 'Number',
    );
  }
}
