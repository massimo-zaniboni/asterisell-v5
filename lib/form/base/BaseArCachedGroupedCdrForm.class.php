<?php

/**
 * ArCachedGroupedCdr form base class.
 *
 * @method ArCachedGroupedCdr getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArCachedGroupedCdrForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                         => new sfWidgetFormInputHidden(),
      'calldate'                   => new sfWidgetFormDate(),
      'cached_parent_id_hierarchy' => new sfWidgetFormInputText(),
      'destination_type'           => new sfWidgetFormInputText(),
      'count_of_calls'             => new sfWidgetFormInputText(),
      'billsec'                    => new sfWidgetFormInputText(),
      'income'                     => new sfWidgetFormInputText(),
      'cost_saving'                => new sfWidgetFormInputText(),
      'cost'                       => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'id'                         => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'calldate'                   => new sfValidatorDate(),
      'cached_parent_id_hierarchy' => new sfValidatorString(array('max_length' => 850, 'required' => false)),
      'destination_type'           => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647)),
      'count_of_calls'             => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647)),
      'billsec'                    => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647)),
      'income'                     => new sfValidatorInteger(array('min' => -9.2233720368548E+18, 'max' => 9223372036854775807)),
      'cost_saving'                => new sfValidatorInteger(array('min' => -9.2233720368548E+18, 'max' => 9223372036854775807)),
      'cost'                       => new sfValidatorInteger(array('min' => -9.2233720368548E+18, 'max' => 9223372036854775807)),
    ));

    $this->widgetSchema->setNameFormat('ar_cached_grouped_cdr[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArCachedGroupedCdr';
  }


}
