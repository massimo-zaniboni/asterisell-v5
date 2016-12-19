<?php

/**
 * ArVoipExtensionToMove filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArVoipExtensionToMoveFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
    ));

    $this->setValidators(array(
    ));

    $this->widgetSchema->setNameFormat('ar_voip_extension_to_move_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArVoipExtensionToMove';
  }

  public function getFields()
  {
    return array(
      'extension' => 'Text',
    );
  }
}
