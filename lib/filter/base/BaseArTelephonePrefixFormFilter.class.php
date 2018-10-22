<?php

/**
 * ArTelephonePrefix filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArTelephonePrefixFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'prefix'                           => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'match_only_numbers_with_n_digits' => new sfWidgetFormFilterInput(),
      'name'                             => new sfWidgetFormFilterInput(),
      'geographic_location'              => new sfWidgetFormFilterInput(),
      'operator_type'                    => new sfWidgetFormFilterInput(),
      'display_priority_level'           => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'rating_code'                      => new sfWidgetFormFilterInput(array('with_empty' => false)),
    ));

    $this->setValidators(array(
      'prefix'                           => new sfValidatorPass(array('required' => false)),
      'match_only_numbers_with_n_digits' => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'name'                             => new sfValidatorPass(array('required' => false)),
      'geographic_location'              => new sfValidatorPass(array('required' => false)),
      'operator_type'                    => new sfValidatorPass(array('required' => false)),
      'display_priority_level'           => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'rating_code'                      => new sfValidatorPass(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_telephone_prefix_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArTelephonePrefix';
  }

  public function getFields()
  {
    return array(
      'id'                               => 'Number',
      'prefix'                           => 'Text',
      'match_only_numbers_with_n_digits' => 'Number',
      'name'                             => 'Text',
      'geographic_location'              => 'Text',
      'operator_type'                    => 'Text',
      'display_priority_level'           => 'Number',
      'rating_code'                      => 'Text',
    );
  }
}
