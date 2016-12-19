<?php

/**
 * ArTempSourceCdrToDestCdr filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArTempSourceCdrToDestCdrFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
    ));

    $this->setValidators(array(
    ));

    $this->widgetSchema->setNameFormat('ar_temp_source_cdr_to_dest_cdr_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArTempSourceCdrToDestCdr';
  }

  public function getFields()
  {
    return array(
      'id' => 'Number',
    );
  }
}
