<?php

/**
 * ArVendor filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArVendorFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'internal_name' => new sfWidgetFormFilterInput(),
      'ar_party_id'   => new sfWidgetFormPropelChoice(array('model' => 'ArParty', 'add_empty' => true)),
      'is_internal'   => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
    ));

    $this->setValidators(array(
      'internal_name' => new sfValidatorPass(array('required' => false)),
      'ar_party_id'   => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArParty', 'column' => 'id')),
      'is_internal'   => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
    ));

    $this->widgetSchema->setNameFormat('ar_vendor_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArVendor';
  }

  public function getFields()
  {
    return array(
      'id'            => 'Number',
      'internal_name' => 'Text',
      'ar_party_id'   => 'ForeignKey',
      'is_internal'   => 'Boolean',
    );
  }
}
