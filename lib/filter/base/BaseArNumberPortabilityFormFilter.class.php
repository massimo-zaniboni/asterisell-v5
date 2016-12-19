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
      'telephone_number'             => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'ported_telephone_number'      => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'from_date'                    => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'is_exported_to_rating_engine' => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
    ));

    $this->setValidators(array(
      'telephone_number'             => new sfValidatorPass(array('required' => false)),
      'ported_telephone_number'      => new sfValidatorPass(array('required' => false)),
      'from_date'                    => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'is_exported_to_rating_engine' => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
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
      'id'                           => 'Number',
      'telephone_number'             => 'Text',
      'ported_telephone_number'      => 'Text',
      'from_date'                    => 'Date',
      'is_exported_to_rating_engine' => 'Boolean',
    );
  }
}
