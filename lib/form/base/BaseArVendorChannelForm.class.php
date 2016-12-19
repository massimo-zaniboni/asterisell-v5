<?php

/**
 * ArVendorChannel form base class.
 *
 * @method ArVendorChannel getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArVendorChannelForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'             => new sfWidgetFormInputHidden(),
      'ar_vendor_id'   => new sfWidgetFormPropelChoice(array('model' => 'ArVendor', 'add_empty' => true)),
      'channel_prefix' => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'id'             => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'ar_vendor_id'   => new sfValidatorPropelChoice(array('model' => 'ArVendor', 'column' => 'id', 'required' => false)),
      'channel_prefix' => new sfValidatorString(array('max_length' => 1024)),
    ));

    $this->widgetSchema->setNameFormat('ar_vendor_channel[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArVendorChannel';
  }


}
