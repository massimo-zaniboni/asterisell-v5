<?php

/**
 * ArOrganizationUnit form base class.
 *
 * @method ArOrganizationUnit getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArOrganizationUnitForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                         => new sfWidgetFormInputHidden(),
      'internal_name'              => new sfWidgetFormInputText(),
      'internal_name2'             => new sfWidgetFormInputText(),
      'export_code'                => new sfWidgetFormInputText(),
      'automatically_managed_from' => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'id'                         => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'internal_name'              => new sfValidatorString(array('max_length' => 200, 'required' => false)),
      'internal_name2'             => new sfValidatorString(array('max_length' => 200, 'required' => false)),
      'export_code'                => new sfValidatorString(array('max_length' => 200, 'required' => false)),
      'automatically_managed_from' => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647)),
    ));

    $this->validatorSchema->setPostValidator(
      new sfValidatorAnd(array(
        new sfValidatorPropelUnique(array('model' => 'ArOrganizationUnit', 'column' => array('internal_name'))),
        new sfValidatorPropelUnique(array('model' => 'ArOrganizationUnit', 'column' => array('internal_name2'))),
        new sfValidatorPropelUnique(array('model' => 'ArOrganizationUnit', 'column' => array('export_code'))),
      ))
    );

    $this->widgetSchema->setNameFormat('ar_organization_unit[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArOrganizationUnit';
  }


}
