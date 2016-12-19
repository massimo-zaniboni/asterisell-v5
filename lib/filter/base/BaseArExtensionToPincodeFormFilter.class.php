<?php

/**
 * ArExtensionToPincode filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArExtensionToPincodeFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'extension' => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'pincode'   => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'from_date' => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate(), 'with_empty' => false)),
    ));

    $this->setValidators(array(
      'extension' => new sfValidatorPass(array('required' => false)),
      'pincode'   => new sfValidatorPass(array('required' => false)),
      'from_date' => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
    ));

    $this->widgetSchema->setNameFormat('ar_extension_to_pincode_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArExtensionToPincode';
  }

  public function getFields()
  {
    return array(
      'id'        => 'Number',
      'extension' => 'Text',
      'pincode'   => 'Text',
      'from_date' => 'Date',
    );
  }
}
