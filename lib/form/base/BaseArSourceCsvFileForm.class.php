<?php

/**
 * ArSourceCsvFile form base class.
 *
 * @method ArSourceCsvFile getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArSourceCsvFileForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                        => new sfWidgetFormInputHidden(),
      'ar_cdr_provider_id'        => new sfWidgetFormPropelChoice(array('model' => 'ArCdrProvider', 'add_empty' => true)),
      'ar_physical_format_id'     => new sfWidgetFormPropelChoice(array('model' => 'ArPhysicalFormat', 'add_empty' => true)),
      'retrieved_from_server'     => new sfWidgetFormInputText(),
      'is_status'                 => new sfWidgetFormInputCheckbox(),
      'is_calldate_processed'     => new sfWidgetFormInputCheckbox(),
      'is_imported'               => new sfWidgetFormInputCheckbox(),
      'is_active_info'            => new sfWidgetFormInputCheckbox(),
      'min_calldate'              => new sfWidgetFormDateTime(),
      'max_calldate'              => new sfWidgetFormDateTime(),
      'name'                      => new sfWidgetFormInputText(),
      'archive_directory'         => new sfWidgetFormInputText(),
      'checksum'                  => new sfWidgetFormInputText(),
      'receiving_date'            => new sfWidgetFormDateTime(),
      'serious_processing_errors' => new sfWidgetFormInputCheckbox(),
      'tot_lines'                 => new sfWidgetFormInputText(),
      'lines_with_errors'         => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'id'                        => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'ar_cdr_provider_id'        => new sfValidatorPropelChoice(array('model' => 'ArCdrProvider', 'column' => 'id', 'required' => false)),
      'ar_physical_format_id'     => new sfValidatorPropelChoice(array('model' => 'ArPhysicalFormat', 'column' => 'id', 'required' => false)),
      'retrieved_from_server'     => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
      'is_status'                 => new sfValidatorBoolean(),
      'is_calldate_processed'     => new sfValidatorBoolean(),
      'is_imported'               => new sfValidatorBoolean(),
      'is_active_info'            => new sfValidatorBoolean(),
      'min_calldate'              => new sfValidatorDateTime(array('required' => false)),
      'max_calldate'              => new sfValidatorDateTime(array('required' => false)),
      'name'                      => new sfValidatorString(array('max_length' => 255)),
      'archive_directory'         => new sfValidatorString(array('max_length' => 2048)),
      'checksum'                  => new sfValidatorString(array('max_length' => 2048, 'required' => false)),
      'receiving_date'            => new sfValidatorDateTime(array('required' => false)),
      'serious_processing_errors' => new sfValidatorBoolean(),
      'tot_lines'                 => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'lines_with_errors'         => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
    ));

    $this->validatorSchema->setPostValidator(
      new sfValidatorPropelUnique(array('model' => 'ArSourceCsvFile', 'column' => array('name')))
    );

    $this->widgetSchema->setNameFormat('ar_source_csv_file[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArSourceCsvFile';
  }


}
