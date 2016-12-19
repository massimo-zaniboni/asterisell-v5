<?php

/**
 * ArCachedOrganizationInfo form base class.
 *
 * @method ArCachedOrganizationInfo getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArCachedOrganizationInfoForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'            => new sfWidgetFormInputHidden(),
      'internal_name' => new sfWidgetFormInputText(),
      'content'       => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'id'            => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'internal_name' => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'content'       => new sfValidatorPass(array('required' => false)),
    ));

    $this->validatorSchema->setPostValidator(
      new sfValidatorPropelUnique(array('model' => 'ArCachedOrganizationInfo', 'column' => array('internal_name')))
    );

    $this->widgetSchema->setNameFormat('ar_cached_organization_info[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArCachedOrganizationInfo';
  }


}
