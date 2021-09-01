<?php

/**
 * ArSpecificRateCalc filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArSpecificRateCalcFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'note'                                    => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'ar_rate_id'                              => new sfWidgetFormPropelChoice(array('model' => 'ArRate', 'add_empty' => true)),
      'specific_rate_name'                      => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'price_category_name'                     => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'mediumtext_specific_rate_in_match_all'   => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'mediumtext_specific_rate_in_match_exact' => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'mediumtext_specific_rate_out'            => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'rate_plan_out'                           => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'mediumtext_base_rate_diff'               => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'calc_info'                               => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'calc_error'                              => new sfWidgetFormFilterInput(),
      'is_recalc'                               => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
    ));

    $this->setValidators(array(
      'note'                                    => new sfValidatorPass(array('required' => false)),
      'ar_rate_id'                              => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArRate', 'column' => 'id')),
      'specific_rate_name'                      => new sfValidatorPass(array('required' => false)),
      'price_category_name'                     => new sfValidatorPass(array('required' => false)),
      'mediumtext_specific_rate_in_match_all'   => new sfValidatorPass(array('required' => false)),
      'mediumtext_specific_rate_in_match_exact' => new sfValidatorPass(array('required' => false)),
      'mediumtext_specific_rate_out'            => new sfValidatorPass(array('required' => false)),
      'rate_plan_out'                           => new sfValidatorPass(array('required' => false)),
      'mediumtext_base_rate_diff'               => new sfValidatorPass(array('required' => false)),
      'calc_info'                               => new sfValidatorPass(array('required' => false)),
      'calc_error'                              => new sfValidatorPass(array('required' => false)),
      'is_recalc'                               => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
    ));

    $this->widgetSchema->setNameFormat('ar_specific_rate_calc_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArSpecificRateCalc';
  }

  public function getFields()
  {
    return array(
      'id'                                      => 'Number',
      'note'                                    => 'Text',
      'ar_rate_id'                              => 'ForeignKey',
      'specific_rate_name'                      => 'Text',
      'price_category_name'                     => 'Text',
      'mediumtext_specific_rate_in_match_all'   => 'Text',
      'mediumtext_specific_rate_in_match_exact' => 'Text',
      'mediumtext_specific_rate_out'            => 'Text',
      'rate_plan_out'                           => 'Text',
      'mediumtext_base_rate_diff'               => 'Text',
      'calc_info'                               => 'Text',
      'calc_error'                              => 'Text',
      'is_recalc'                               => 'Boolean',
    );
  }
}
