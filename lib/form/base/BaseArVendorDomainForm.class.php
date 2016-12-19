<?php

/**
 * ArVendorDomain form base class.
 *
 * @method ArVendorDomain getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArVendorDomainForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                               => new sfWidgetFormInputHidden(),
      'internal_name'                    => new sfWidgetFormInputText(),
      'ar_vendor_id'                     => new sfWidgetFormPropelChoice(array('model' => 'ArVendor', 'add_empty' => true)),
      'ar_communication_channel_type_id' => new sfWidgetFormPropelChoice(array('model' => 'ArCommunicationChannelType', 'add_empty' => true)),
      'domain'                           => new sfWidgetFormInputText(),
      'is_prefix'                        => new sfWidgetFormInputCheckbox(),
      'is_suffix'                        => new sfWidgetFormInputCheckbox(),
      'from'                             => new sfWidgetFormDateTime(),
      'to'                               => new sfWidgetFormDateTime(),
    ));

    $this->setValidators(array(
      'id'                               => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'internal_name'                    => new sfValidatorString(array('max_length' => 200, 'required' => false)),
      'ar_vendor_id'                     => new sfValidatorPropelChoice(array('model' => 'ArVendor', 'column' => 'id', 'required' => false)),
      'ar_communication_channel_type_id' => new sfValidatorPropelChoice(array('model' => 'ArCommunicationChannelType', 'column' => 'id', 'required' => false)),
      'domain'                           => new sfValidatorString(array('max_length' => 255)),
      'is_prefix'                        => new sfValidatorBoolean(),
      'is_suffix'                        => new sfValidatorBoolean(),
      'from'                             => new sfValidatorDateTime(),
      'to'                               => new sfValidatorDateTime(array('required' => false)),
    ));

    $this->validatorSchema->setPostValidator(
      new sfValidatorAnd(array(
        new sfValidatorPropelUnique(array('model' => 'ArVendorDomain', 'column' => array('internal_name'))),
        new sfValidatorPropelUnique(array('model' => 'ArVendorDomain', 'column' => array('domain'))),
      ))
    );

    $this->widgetSchema->setNameFormat('ar_vendor_domain[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArVendorDomain';
  }


}
