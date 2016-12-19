<?php

/**
 * ArSourceCsvFile filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArSourceCsvFileFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_cdr_provider_id'        => new sfWidgetFormPropelChoice(array('model' => 'ArCdrProvider', 'add_empty' => true)),
      'ar_physical_format_id'     => new sfWidgetFormPropelChoice(array('model' => 'ArPhysicalFormat', 'add_empty' => true)),
      'retrieved_from_server'     => new sfWidgetFormFilterInput(),
      'is_status'                 => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'is_calldate_processed'     => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'is_imported'               => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'is_active_info'            => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'min_calldate'              => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'max_calldate'              => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'name'                      => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'archive_directory'         => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'checksum'                  => new sfWidgetFormFilterInput(),
      'receiving_date'            => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'serious_processing_errors' => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'tot_lines'                 => new sfWidgetFormFilterInput(),
      'lines_with_errors'         => new sfWidgetFormFilterInput(),
    ));

    $this->setValidators(array(
      'ar_cdr_provider_id'        => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArCdrProvider', 'column' => 'id')),
      'ar_physical_format_id'     => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArPhysicalFormat', 'column' => 'id')),
      'retrieved_from_server'     => new sfValidatorPass(array('required' => false)),
      'is_status'                 => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'is_calldate_processed'     => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'is_imported'               => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'is_active_info'            => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'min_calldate'              => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'max_calldate'              => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'name'                      => new sfValidatorPass(array('required' => false)),
      'archive_directory'         => new sfValidatorPass(array('required' => false)),
      'checksum'                  => new sfValidatorPass(array('required' => false)),
      'receiving_date'            => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'serious_processing_errors' => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'tot_lines'                 => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'lines_with_errors'         => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
    ));

    $this->widgetSchema->setNameFormat('ar_source_csv_file_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArSourceCsvFile';
  }

  public function getFields()
  {
    return array(
      'id'                        => 'Number',
      'ar_cdr_provider_id'        => 'ForeignKey',
      'ar_physical_format_id'     => 'ForeignKey',
      'retrieved_from_server'     => 'Text',
      'is_status'                 => 'Boolean',
      'is_calldate_processed'     => 'Boolean',
      'is_imported'               => 'Boolean',
      'is_active_info'            => 'Boolean',
      'min_calldate'              => 'Date',
      'max_calldate'              => 'Date',
      'name'                      => 'Text',
      'archive_directory'         => 'Text',
      'checksum'                  => 'Text',
      'receiving_date'            => 'Date',
      'serious_processing_errors' => 'Boolean',
      'tot_lines'                 => 'Number',
      'lines_with_errors'         => 'Number',
    );
  }
}
