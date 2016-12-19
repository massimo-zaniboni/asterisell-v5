<?php

/**
 * ArVendorChannel filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArVendorChannelFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_vendor_id'   => new sfWidgetFormPropelChoice(array('model' => 'ArVendor', 'add_empty' => true)),
      'channel_prefix' => new sfWidgetFormFilterInput(array('with_empty' => false)),
    ));

    $this->setValidators(array(
      'ar_vendor_id'   => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArVendor', 'column' => 'id')),
      'channel_prefix' => new sfValidatorPass(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_vendor_channel_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArVendorChannel';
  }

  public function getFields()
  {
    return array(
      'id'             => 'Number',
      'ar_vendor_id'   => 'ForeignKey',
      'channel_prefix' => 'Text',
    );
  }
}
