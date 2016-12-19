<?php

/**
 * ArRateIncrementalInfo form base class.
 *
 * @method ArRateIncrementalInfo getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArRateIncrementalInfoForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                         => new sfWidgetFormInputHidden(),
      'ar_party_id'                => new sfWidgetFormPropelChoice(array('model' => 'ArParty', 'add_empty' => true)),
      'ar_rate_id'                 => new sfWidgetFormPropelChoice(array('model' => 'ArRate', 'add_empty' => true)),
      'period'                     => new sfWidgetFormInputText(),
      'last_processed_ar_cdr_date' => new sfWidgetFormDateTime(),
      'last_processed_ar_cdr_id'   => new sfWidgetFormInputText(),
      'bundle_rate'                => new sfWidgetFormTextarea(),
    ));

    $this->setValidators(array(
      'id'                         => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'ar_party_id'                => new sfValidatorPropelChoice(array('model' => 'ArParty', 'column' => 'id', 'required' => false)),
      'ar_rate_id'                 => new sfValidatorPropelChoice(array('model' => 'ArRate', 'column' => 'id', 'required' => false)),
      'period'                     => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
      'last_processed_ar_cdr_date' => new sfValidatorDateTime(array('required' => false)),
      'last_processed_ar_cdr_id'   => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'bundle_rate'                => new sfValidatorString(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_rate_incremental_info[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArRateIncrementalInfo';
  }


}
