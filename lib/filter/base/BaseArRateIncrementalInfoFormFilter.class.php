<?php

/**
 * ArRateIncrementalInfo filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArRateIncrementalInfoFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_party_id'                => new sfWidgetFormPropelChoice(array('model' => 'ArParty', 'add_empty' => true)),
      'ar_rate_id'                 => new sfWidgetFormPropelChoice(array('model' => 'ArRate', 'add_empty' => true)),
      'period'                     => new sfWidgetFormFilterInput(),
      'last_processed_ar_cdr_date' => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'last_processed_ar_cdr_id'   => new sfWidgetFormFilterInput(),
      'bundle_rate'                => new sfWidgetFormFilterInput(),
    ));

    $this->setValidators(array(
      'ar_party_id'                => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArParty', 'column' => 'id')),
      'ar_rate_id'                 => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArRate', 'column' => 'id')),
      'period'                     => new sfValidatorPass(array('required' => false)),
      'last_processed_ar_cdr_date' => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'last_processed_ar_cdr_id'   => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'bundle_rate'                => new sfValidatorPass(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_rate_incremental_info_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArRateIncrementalInfo';
  }

  public function getFields()
  {
    return array(
      'id'                         => 'Number',
      'ar_party_id'                => 'ForeignKey',
      'ar_rate_id'                 => 'ForeignKey',
      'period'                     => 'Text',
      'last_processed_ar_cdr_date' => 'Date',
      'last_processed_ar_cdr_id'   => 'Number',
      'bundle_rate'                => 'Text',
    );
  }
}
