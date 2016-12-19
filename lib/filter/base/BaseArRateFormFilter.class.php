<?php

/**
 * ArRate filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArRateFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_vendor_id'             => new sfWidgetFormPropelChoice(array('model' => 'ArVendor', 'add_empty' => true)),
      'ar_rate_format_id'        => new sfWidgetFormPropelChoice(array('model' => 'ArRateFormat', 'add_empty' => true)),
      'from_time'                => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate(), 'with_empty' => false)),
      'internal_name'            => new sfWidgetFormFilterInput(),
      'ar_rate_id'               => new sfWidgetFormPropelChoice(array('model' => 'ArRate', 'add_empty' => true)),
      'short_description'        => new sfWidgetFormFilterInput(),
      'note'                     => new sfWidgetFormFilterInput(),
      'is_exported_to_resellers' => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'was_compiled'             => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'source_data_file'         => new sfWidgetFormFilterInput(),
      'backup_source_data_file'  => new sfWidgetFormFilterInput(),
      'html_description'         => new sfWidgetFormFilterInput(),
    ));

    $this->setValidators(array(
      'ar_vendor_id'             => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArVendor', 'column' => 'id')),
      'ar_rate_format_id'        => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArRateFormat', 'column' => 'id')),
      'from_time'                => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'internal_name'            => new sfValidatorPass(array('required' => false)),
      'ar_rate_id'               => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArRate', 'column' => 'id')),
      'short_description'        => new sfValidatorPass(array('required' => false)),
      'note'                     => new sfValidatorPass(array('required' => false)),
      'is_exported_to_resellers' => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'was_compiled'             => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'source_data_file'         => new sfValidatorPass(array('required' => false)),
      'backup_source_data_file'  => new sfValidatorPass(array('required' => false)),
      'html_description'         => new sfValidatorPass(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_rate_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArRate';
  }

  public function getFields()
  {
    return array(
      'id'                       => 'Number',
      'ar_vendor_id'             => 'ForeignKey',
      'ar_rate_format_id'        => 'ForeignKey',
      'from_time'                => 'Date',
      'internal_name'            => 'Text',
      'ar_rate_id'               => 'ForeignKey',
      'short_description'        => 'Text',
      'note'                     => 'Text',
      'is_exported_to_resellers' => 'Boolean',
      'was_compiled'             => 'Boolean',
      'source_data_file'         => 'Text',
      'backup_source_data_file'  => 'Text',
      'html_description'         => 'Text',
    );
  }
}
