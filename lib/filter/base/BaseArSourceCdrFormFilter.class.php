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
      'ar_cdr_provider_id'    => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'ar_physical_format_id' => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'content'               => new sfWidgetFormFilterInput(),
      'is_hacked'             => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
    ));

    $this->setValidators(array(
      'ar_cdr_provider_id'    => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'ar_physical_format_id' => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'content'               => new sfValidatorPass(array('required' => false)),
      'is_hacked'             => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
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
      'is_hacked'             => 'Boolean',
    );
  }
}
