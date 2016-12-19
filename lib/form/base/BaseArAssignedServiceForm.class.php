<?php

/**
 * ArAssignedService form base class.
 *
 * @method ArAssignedService getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArAssignedServiceForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                      => new sfWidgetFormInputHidden(),
      'internal_name'           => new sfWidgetFormInputText(),
      'external_crm_code'       => new sfWidgetFormInputText(),
      'from_date'               => new sfWidgetFormDateTime(),
      'ar_service_id'           => new sfWidgetFormPropelChoice(array('model' => 'ArService', 'add_empty' => false)),
      'ar_organization_unit_id' => new sfWidgetFormPropelChoice(array('model' => 'ArOrganizationUnit', 'add_empty' => false)),
      'nr_of_items'             => new sfWidgetFormInputText(),
      'discount'                => new sfWidgetFormInputText(),
      'note'                    => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'id'                      => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'internal_name'           => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'external_crm_code'       => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'from_date'               => new sfValidatorDateTime(),
      'ar_service_id'           => new sfValidatorPropelChoice(array('model' => 'ArService', 'column' => 'id')),
      'ar_organization_unit_id' => new sfValidatorPropelChoice(array('model' => 'ArOrganizationUnit', 'column' => 'id')),
      'nr_of_items'             => new sfValidatorInteger(array('min' => -9.2233720368548E+18, 'max' => 9223372036854775807, 'required' => false)),
      'discount'                => new sfValidatorInteger(array('min' => -9.2233720368548E+18, 'max' => 9223372036854775807)),
      'note'                    => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
    ));

    $this->validatorSchema->setPostValidator(
      new sfValidatorPropelUnique(array('model' => 'ArAssignedService', 'column' => array('internal_name')))
    );

    $this->widgetSchema->setNameFormat('ar_assigned_service[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArAssignedService';
  }


}
