<?php

/**
 * ArGlobalPermissions form base class.
 *
 * @method ArGlobalPermissions getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArGlobalPermissionsForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                         => new sfWidgetFormInputHidden(),
      'show_call_cost'             => new sfWidgetFormInputCheckbox(),
      'show_call_income'           => new sfWidgetFormInputCheckbox(),
      'show_outgoing_calls'        => new sfWidgetFormInputCheckbox(),
      'show_incoming_calls'        => new sfWidgetFormInputCheckbox(),
      'show_internal_calls'        => new sfWidgetFormInputCheckbox(),
      'show_voip_provider'         => new sfWidgetFormInputCheckbox(),
      'show_communication_channel' => new sfWidgetFormInputCheckbox(),
      'show_cost_saving'           => new sfWidgetFormInputCheckbox(),
    ));

    $this->setValidators(array(
      'id'                         => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'show_call_cost'             => new sfValidatorBoolean(array('required' => false)),
      'show_call_income'           => new sfValidatorBoolean(array('required' => false)),
      'show_outgoing_calls'        => new sfValidatorBoolean(array('required' => false)),
      'show_incoming_calls'        => new sfValidatorBoolean(array('required' => false)),
      'show_internal_calls'        => new sfValidatorBoolean(array('required' => false)),
      'show_voip_provider'         => new sfValidatorBoolean(array('required' => false)),
      'show_communication_channel' => new sfValidatorBoolean(array('required' => false)),
      'show_cost_saving'           => new sfValidatorBoolean(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_global_permissions[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArGlobalPermissions';
  }


}
