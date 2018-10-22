<?php

/**
 * ArApplicationUpgrade form base class.
 *
 * @method ArApplicationUpgrade getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArApplicationUpgradeForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                => new sfWidgetFormInputHidden(),
      'upg_key'           => new sfWidgetFormInputText(),
      'upg_output'        => new sfWidgetFormTextarea(),
      'installation_date' => new sfWidgetFormDateTime(),
    ));

    $this->setValidators(array(
      'id'                => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'upg_key'           => new sfValidatorString(array('max_length' => 2048, 'required' => false)),
      'upg_output'        => new sfValidatorString(array('required' => false)),
      'installation_date' => new sfValidatorDateTime(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_application_upgrade[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArApplicationUpgrade';
  }


}
