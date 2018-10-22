<?php

/**
 * ArTelephonePrefix form base class.
 *
 * @method ArTelephonePrefix getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArTelephonePrefixForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                               => new sfWidgetFormInputHidden(),
      'prefix'                           => new sfWidgetFormInputText(),
      'match_only_numbers_with_n_digits' => new sfWidgetFormInputText(),
      'name'                             => new sfWidgetFormInputText(),
      'geographic_location'              => new sfWidgetFormInputText(),
      'operator_type'                    => new sfWidgetFormInputText(),
      'display_priority_level'           => new sfWidgetFormInputText(),
      'rating_code'                      => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'id'                               => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'prefix'                           => new sfValidatorString(array('max_length' => 255)),
      'match_only_numbers_with_n_digits' => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'name'                             => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
      'geographic_location'              => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'operator_type'                    => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'display_priority_level'           => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647)),
      'rating_code'                      => new sfValidatorString(array('max_length' => 255)),
    ));

    $this->validatorSchema->setPostValidator(
      new sfValidatorPropelUnique(array('model' => 'ArTelephonePrefix', 'column' => array('prefix')))
    );

    $this->widgetSchema->setNameFormat('ar_telephone_prefix[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArTelephonePrefix';
  }


}
