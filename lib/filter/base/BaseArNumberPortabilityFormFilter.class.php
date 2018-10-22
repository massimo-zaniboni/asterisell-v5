<?php

/**
 * ArNumberPortability filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArNumberPortabilityFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ported_telephone_number' => new sfWidgetFormFilterInput(array('with_empty' => false)),
    ));

    $this->setValidators(array(
      'ported_telephone_number' => new sfValidatorPass(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_number_portability_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArNumberPortability';
  }

  public function getFields()
  {
    return array(
      'telephone_number'        => 'Text',
      'from_date'               => 'Date',
      'ported_telephone_number' => 'Text',
    );
  }
}
