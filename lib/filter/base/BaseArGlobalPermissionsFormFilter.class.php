<?php

/**
 * ArGlobalPermissions filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArGlobalPermissionsFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'show_call_cost'             => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'show_call_income'           => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'show_outgoing_calls'        => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'show_incoming_calls'        => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'show_internal_calls'        => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'show_voip_provider'         => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'show_communication_channel' => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'show_cost_saving'           => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
    ));

    $this->setValidators(array(
      'show_call_cost'             => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'show_call_income'           => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'show_outgoing_calls'        => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'show_incoming_calls'        => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'show_internal_calls'        => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'show_voip_provider'         => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'show_communication_channel' => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'show_cost_saving'           => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
    ));

    $this->widgetSchema->setNameFormat('ar_global_permissions_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArGlobalPermissions';
  }

  public function getFields()
  {
    return array(
      'id'                         => 'Number',
      'show_call_cost'             => 'Boolean',
      'show_call_income'           => 'Boolean',
      'show_outgoing_calls'        => 'Boolean',
      'show_incoming_calls'        => 'Boolean',
      'show_internal_calls'        => 'Boolean',
      'show_voip_provider'         => 'Boolean',
      'show_communication_channel' => 'Boolean',
      'show_cost_saving'           => 'Boolean',
    );
  }
}
