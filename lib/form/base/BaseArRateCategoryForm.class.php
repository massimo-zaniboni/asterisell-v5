<?php

/**
 * ArRateCategory form base class.
 *
 * @method ArRateCategory getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArRateCategoryForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                => new sfWidgetFormInputHidden(),
      'internal_name'     => new sfWidgetFormInputText(),
      'short_description' => new sfWidgetFormTextarea(),
    ));

    $this->setValidators(array(
      'id'                => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'internal_name'     => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'short_description' => new sfValidatorString(array('required' => false)),
    ));

    $this->validatorSchema->setPostValidator(
      new sfValidatorPropelUnique(array('model' => 'ArRateCategory', 'column' => array('internal_name')))
    );

    $this->widgetSchema->setNameFormat('ar_rate_category[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArRateCategory';
  }


}
