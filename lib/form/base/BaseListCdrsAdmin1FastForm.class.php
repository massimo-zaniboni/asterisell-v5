<?php

/**
 * ListCdrsAdmin1Fast form base class.
 *
 * @method ListCdrsAdmin1Fast getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseListCdrsAdmin1FastForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id' => new sfWidgetFormInputHidden(),
    ));

    $this->setValidators(array(
      'id' => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('list_cdrs_admin1_fast[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ListCdrsAdmin1Fast';
  }


}
