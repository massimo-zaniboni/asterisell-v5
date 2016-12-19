<?php

/**
 * ArTypeOfSourceCdr filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArTypeOfSourceCdrFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
    ));

    $this->setValidators(array(
    ));

    $this->widgetSchema->setNameFormat('ar_type_of_source_cdr_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArTypeOfSourceCdr';
  }

  public function getFields()
  {
    return array(
      'ar_cdr_provider_id'    => 'Number',
      'ar_physical_format_id' => 'Number',
    );
  }
}
