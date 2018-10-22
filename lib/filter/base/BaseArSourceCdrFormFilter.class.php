<?php

/**
 * ArSourceCdr filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArSourceCdrFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'content'               => new sfWidgetFormFilterInput(),
    ));

    $this->setValidators(array(
      'content'               => new sfValidatorPass(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_source_cdr_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArSourceCdr';
  }

  public function getFields()
  {
    return array(
      'calldate'              => 'Date',
      'id'                    => 'Number',
      'ar_cdr_provider_id'    => 'Number',
      'ar_physical_format_id' => 'Number',
      'content'               => 'Text',
    );
  }
}
