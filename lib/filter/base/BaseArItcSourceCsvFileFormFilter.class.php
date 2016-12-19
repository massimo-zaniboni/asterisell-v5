<?php

/**
 * ArItcSourceCsvFile filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArItcSourceCsvFileFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'name'                  => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'checksum'              => new sfWidgetFormFilterInput(),
      'processing_date'       => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'min_calldate'          => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'max_calldate'          => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'tot_lines'             => new sfWidgetFormFilterInput(),
      'lines_with_errors'     => new sfWidgetFormFilterInput(),
      'content_type'          => new sfWidgetFormFilterInput(),
      'retrieved_from_server' => new sfWidgetFormFilterInput(),
      'imported'              => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
    ));

    $this->setValidators(array(
      'name'                  => new sfValidatorPass(array('required' => false)),
      'checksum'              => new sfValidatorPass(array('required' => false)),
      'processing_date'       => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'min_calldate'          => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'max_calldate'          => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'tot_lines'             => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'lines_with_errors'     => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'content_type'          => new sfValidatorPass(array('required' => false)),
      'retrieved_from_server' => new sfValidatorPass(array('required' => false)),
      'imported'              => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
    ));

    $this->widgetSchema->setNameFormat('ar_itc_source_csv_file_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArItcSourceCsvFile';
  }

  public function getFields()
  {
    return array(
      'id'                    => 'Number',
      'name'                  => 'Text',
      'checksum'              => 'Text',
      'processing_date'       => 'Date',
      'min_calldate'          => 'Date',
      'max_calldate'          => 'Date',
      'tot_lines'             => 'Number',
      'lines_with_errors'     => 'Number',
      'content_type'          => 'Text',
      'retrieved_from_server' => 'Text',
      'imported'              => 'Boolean',
    );
  }
}
