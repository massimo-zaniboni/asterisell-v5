<?php

/**
 * ArSourceCdrToMove form base class.
 *
 * @method ArSourceCdrToMove getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArSourceCdrToMoveForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_source_cdr_id' => new sfWidgetFormInputHidden(),
    ));

    $this->setValidators(array(
      'ar_source_cdr_id' => new sfValidatorChoice(array('choices' => array($this->getObject()->getArSourceCdrId()), 'empty_value' => $this->getObject()->getArSourceCdrId(), 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_source_cdr_to_move[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArSourceCdrToMove';
  }


}
