<?php

/**
 * ArInstanceStatus form base class.
 *
 * @method ArInstanceStatus getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArInstanceStatusForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                                         => new sfWidgetFormInputHidden(),
      'internal_name'                              => new sfWidgetFormInputText(),
      'instance_code'                              => new sfWidgetFormInputText(),
      'ar_organization_unit_id'                    => new sfWidgetFormPropelChoice(array('model' => 'ArOrganizationUnit', 'add_empty' => true)),
      'application_version'                        => new sfWidgetFormInputText(),
      'nr_of_critical_errors'                      => new sfWidgetFormInputText(),
      'nr_of_important_errors'                     => new sfWidgetFormInputText(),
      'nr_of_warning_errors'                       => new sfWidgetFormInputText(),
      'nr_of_extensions'                           => new sfWidgetFormInputText(),
      'nr_of_unspecified_extensions'               => new sfWidgetFormInputText(),
      'property_nr_errors_outgoing_previous_month' => new sfWidgetFormInputText(),
      'property_nr_errors_incoming_previous_month' => new sfWidgetFormInputText(),
      'property_nr_errors_internal_previous_month' => new sfWidgetFormInputText(),
      'property_nr_errors_outgoing_last_30_days'   => new sfWidgetFormInputText(),
      'property_nr_errors_incoming_last_30_days'   => new sfWidgetFormInputText(),
      'property_nr_errors_internal_last_30_days'   => new sfWidgetFormInputText(),
      'property_nr_outgoing_previous_month'        => new sfWidgetFormInputText(),
      'property_nr_incoming_previous_month'        => new sfWidgetFormInputText(),
      'property_nr_internal_previous_month'        => new sfWidgetFormInputText(),
      'property_nr_outgoing_last_30_days'          => new sfWidgetFormInputText(),
      'property_nr_incoming_last_30_days'          => new sfWidgetFormInputText(),
      'property_nr_internal_last_30_days'          => new sfWidgetFormInputText(),
      'last_processed_cdr_timestamp'               => new sfWidgetFormDateTime(),
      'info_timestamp'                             => new sfWidgetFormDateTime(),
    ));

    $this->setValidators(array(
      'id'                                         => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'internal_name'                              => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'instance_code'                              => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
      'ar_organization_unit_id'                    => new sfValidatorPropelChoice(array('model' => 'ArOrganizationUnit', 'column' => 'id', 'required' => false)),
      'application_version'                        => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
      'nr_of_critical_errors'                      => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'nr_of_important_errors'                     => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'nr_of_warning_errors'                       => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'nr_of_extensions'                           => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'nr_of_unspecified_extensions'               => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'property_nr_errors_outgoing_previous_month' => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'property_nr_errors_incoming_previous_month' => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'property_nr_errors_internal_previous_month' => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'property_nr_errors_outgoing_last_30_days'   => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'property_nr_errors_incoming_last_30_days'   => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'property_nr_errors_internal_last_30_days'   => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'property_nr_outgoing_previous_month'        => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'property_nr_incoming_previous_month'        => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'property_nr_internal_previous_month'        => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'property_nr_outgoing_last_30_days'          => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'property_nr_incoming_last_30_days'          => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'property_nr_internal_last_30_days'          => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'last_processed_cdr_timestamp'               => new sfValidatorDateTime(array('required' => false)),
      'info_timestamp'                             => new sfValidatorDateTime(array('required' => false)),
    ));

    $this->validatorSchema->setPostValidator(
      new sfValidatorPropelUnique(array('model' => 'ArInstanceStatus', 'column' => array('internal_name')))
    );

    $this->widgetSchema->setNameFormat('ar_instance_status[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArInstanceStatus';
  }


}
