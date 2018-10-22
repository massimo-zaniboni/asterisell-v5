<?php

/**
 * ArHoliday form base class.
 *
 * @method ArHoliday getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArHolidayForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'           => new sfWidgetFormInputHidden(),
      'day_of_month' => new sfWidgetFormInputText(),
      'month'        => new sfWidgetFormInputText(),
      'year'         => new sfWidgetFormInputText(),
      'day_of_week'  => new sfWidgetFormInputText(),
      'from_hour'    => new sfWidgetFormInputText(),
      'from_minutes' => new sfWidgetFormInputText(),
      'to_hour'      => new sfWidgetFormInputText(),
      'to_minutes'   => new sfWidgetFormInputText(),
      'peak_code'    => new sfWidgetFormInputText(),
      'name'         => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'id'           => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'day_of_month' => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'month'        => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'year'         => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'day_of_week'  => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'from_hour'    => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'from_minutes' => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'to_hour'      => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'to_minutes'   => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'peak_code'    => new sfValidatorString(array('max_length' => 255)),
      'name'         => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_holiday[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArHoliday';
  }


}
