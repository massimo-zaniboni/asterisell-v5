<?php

/**
 * ArPartyHasTag form base class.
 *
 * @method ArPartyHasTag getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArPartyHasTagForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_party_id' => new sfWidgetFormInputHidden(),
      'ar_tag_id'   => new sfWidgetFormInputHidden(),
    ));

    $this->setValidators(array(
      'ar_party_id' => new sfValidatorPropelChoice(array('model' => 'ArParty', 'column' => 'id', 'required' => false)),
      'ar_tag_id'   => new sfValidatorPropelChoice(array('model' => 'ArTag', 'column' => 'id', 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_party_has_tag[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArPartyHasTag';
  }


}
