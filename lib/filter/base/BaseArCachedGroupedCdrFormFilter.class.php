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
      'calldate'                   => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate(), 'with_empty' => false)),
      'cached_parent_id_hierarchy' => new sfWidgetFormFilterInput(),
      'destination_type'           => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'count_of_calls'             => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'billsec'                    => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'income'                     => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'cost_saving'                => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'cost'                       => new sfWidgetFormFilterInput(array('with_empty' => false)),
    ));

    $this->setValidators(array(
      'calldate'                   => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'cached_parent_id_hierarchy' => new sfValidatorPass(array('required' => false)),
      'destination_type'           => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'count_of_calls'             => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'billsec'                    => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'income'                     => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'cost_saving'                => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'cost'                       => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
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
      'id'                         => 'Number',
      'calldate'                   => 'Date',
      'cached_parent_id_hierarchy' => 'Text',
      'destination_type'           => 'Number',
      'count_of_calls'             => 'Number',
      'billsec'                    => 'Number',
      'income'                     => 'Number',
      'cost_saving'                => 'Number',
      'cost'                       => 'Number',
    );
  }
}
