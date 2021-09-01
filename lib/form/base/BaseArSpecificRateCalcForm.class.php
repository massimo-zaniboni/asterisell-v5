<?php

/**
 * ArSpecificRateCalc form base class.
 *
 * @method ArSpecificRateCalc getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArSpecificRateCalcForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                                      => new sfWidgetFormInputHidden(),
      'note'                                    => new sfWidgetFormTextarea(),
      'ar_rate_id'                              => new sfWidgetFormPropelChoice(array('model' => 'ArRate', 'add_empty' => true)),
      'specific_rate_name'                      => new sfWidgetFormInputText(),
      'price_category_name'                     => new sfWidgetFormInputText(),
      'mediumtext_specific_rate_in_match_all'   => new sfWidgetFormTextarea(),
      'mediumtext_specific_rate_in_match_exact' => new sfWidgetFormTextarea(),
      'mediumtext_specific_rate_out'            => new sfWidgetFormTextarea(),
      'rate_plan_out'                           => new sfWidgetFormTextarea(),
      'mediumtext_base_rate_diff'               => new sfWidgetFormTextarea(),
      'calc_info'                               => new sfWidgetFormTextarea(),
      'calc_error'                              => new sfWidgetFormTextarea(),
      'is_recalc'                               => new sfWidgetFormInputCheckbox(),
    ));

    $this->setValidators(array(
      'id'                                      => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'note'                                    => new sfValidatorString(),
      'ar_rate_id'                              => new sfValidatorPropelChoice(array('model' => 'ArRate', 'column' => 'id', 'required' => false)),
      'specific_rate_name'                      => new sfValidatorString(array('max_length' => 255)),
      'price_category_name'                     => new sfValidatorString(array('max_length' => 255)),
      'mediumtext_specific_rate_in_match_all'   => new sfValidatorString(),
      'mediumtext_specific_rate_in_match_exact' => new sfValidatorString(),
      'mediumtext_specific_rate_out'            => new sfValidatorString(),
      'rate_plan_out'                           => new sfValidatorString(),
      'mediumtext_base_rate_diff'               => new sfValidatorString(),
      'calc_info'                               => new sfValidatorString(),
      'calc_error'                              => new sfValidatorString(array('required' => false)),
      'is_recalc'                               => new sfValidatorBoolean(),
    ));

    $this->widgetSchema->setNameFormat('ar_specific_rate_calc[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArSpecificRateCalc';
  }


}
