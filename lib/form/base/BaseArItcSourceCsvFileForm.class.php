<?php

/**
 * ArItcSourceCsvFile form base class.
 *
 * @method ArItcSourceCsvFile getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArItcSourceCsvFileForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                    => new sfWidgetFormInputHidden(),
      'name'                  => new sfWidgetFormInputText(),
      'checksum'              => new sfWidgetFormInputText(),
      'processing_date'       => new sfWidgetFormDateTime(),
      'min_calldate'          => new sfWidgetFormDateTime(),
      'max_calldate'          => new sfWidgetFormDateTime(),
      'tot_lines'             => new sfWidgetFormInputText(),
      'lines_with_errors'     => new sfWidgetFormInputText(),
      'content_type'          => new sfWidgetFormInputText(),
      'retrieved_from_server' => new sfWidgetFormInputText(),
      'imported'              => new sfWidgetFormInputCheckbox(),
    ));

    $this->setValidators(array(
      'id'                    => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'name'                  => new sfValidatorString(array('max_length' => 255)),
      'checksum'              => new sfValidatorString(array('max_length' => 2048, 'required' => false)),
      'processing_date'       => new sfValidatorDateTime(array('required' => false)),
      'min_calldate'          => new sfValidatorDateTime(array('required' => false)),
      'max_calldate'          => new sfValidatorDateTime(array('required' => false)),
      'tot_lines'             => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'lines_with_errors'     => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'content_type'          => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'retrieved_from_server' => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
      'imported'              => new sfValidatorBoolean(),
    ));

    $this->validatorSchema->setPostValidator(
      new sfValidatorPropelUnique(array('model' => 'ArItcSourceCsvFile', 'column' => array('name')))
    );

    $this->widgetSchema->setNameFormat('ar_itc_source_csv_file[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArItcSourceCsvFile';
  }


}
