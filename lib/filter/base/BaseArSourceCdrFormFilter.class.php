<?php

/**
 * ArSourceCdr filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArSourceCdrFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_cdr_provider_id'      => new sfWidgetFormPropelChoice(array('model' => 'ArCdrProvider', 'add_empty' => true)),
      'ar_physical_format_id'   => new sfWidgetFormPropelChoice(array('model' => 'ArPhysicalFormat', 'add_empty' => true)),
      'calldate'                => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate(), 'with_empty' => false)),
      'is_imported_service_cdr' => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'content'                 => new sfWidgetFormFilterInput(),
    ));

    $this->setValidators(array(
      'ar_cdr_provider_id'      => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArCdrProvider', 'column' => 'id')),
      'ar_physical_format_id'   => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArPhysicalFormat', 'column' => 'id')),
      'calldate'                => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'is_imported_service_cdr' => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'content'                 => new sfValidatorPass(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_source_cdr_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArSourceCdr';
  }

  public function getFields()
  {
    return array(
      'id'                      => 'Number',
      'ar_cdr_provider_id'      => 'ForeignKey',
      'ar_physical_format_id'   => 'ForeignKey',
      'calldate'                => 'Date',
      'is_imported_service_cdr' => 'Boolean',
      'content'                 => 'Text',
    );
  }
}
