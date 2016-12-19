<?php

/**
 * ArRate form base class.
 *
 * @method ArRate getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArRateForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                       => new sfWidgetFormInputHidden(),
      'ar_vendor_id'             => new sfWidgetFormPropelChoice(array('model' => 'ArVendor', 'add_empty' => true)),
      'ar_rate_format_id'        => new sfWidgetFormPropelChoice(array('model' => 'ArRateFormat', 'add_empty' => true)),
      'from_time'                => new sfWidgetFormDateTime(),
      'internal_name'            => new sfWidgetFormInputText(),
      'ar_rate_id'               => new sfWidgetFormPropelChoice(array('model' => 'ArRate', 'add_empty' => true)),
      'short_description'        => new sfWidgetFormInputText(),
      'note'                     => new sfWidgetFormTextarea(),
      'is_exported_to_resellers' => new sfWidgetFormInputCheckbox(),
      'was_compiled'             => new sfWidgetFormInputCheckbox(),
      'source_data_file'         => new sfWidgetFormInputText(),
      'backup_source_data_file'  => new sfWidgetFormInputText(),
      'html_description'         => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'id'                       => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'ar_vendor_id'             => new sfValidatorPropelChoice(array('model' => 'ArVendor', 'column' => 'id', 'required' => false)),
      'ar_rate_format_id'        => new sfValidatorPropelChoice(array('model' => 'ArRateFormat', 'column' => 'id', 'required' => false)),
      'from_time'                => new sfValidatorDateTime(),
      'internal_name'            => new sfValidatorString(array('max_length' => 512, 'required' => false)),
      'ar_rate_id'               => new sfValidatorPropelChoice(array('model' => 'ArRate', 'column' => 'id', 'required' => false)),
      'short_description'        => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
      'note'                     => new sfValidatorString(array('required' => false)),
      'is_exported_to_resellers' => new sfValidatorBoolean(),
      'was_compiled'             => new sfValidatorBoolean(),
      'source_data_file'         => new sfValidatorPass(array('required' => false)),
      'backup_source_data_file'  => new sfValidatorPass(array('required' => false)),
      'html_description'         => new sfValidatorPass(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_rate[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArRate';
  }


}
