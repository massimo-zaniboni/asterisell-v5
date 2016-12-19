<?php

/**
 * ArDestinationType filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArDestinationTypeFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'internal_id' => new sfWidgetFormFilterInput(),
      'name'        => new sfWidgetFormFilterInput(),
    ));

    $this->setValidators(array(
      'internal_id' => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'name'        => new sfValidatorPass(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_destination_type_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArDestinationType';
  }

  public function getFields()
  {
    return array(
      'id'          => 'Number',
      'internal_id' => 'Number',
      'name'        => 'Text',
    );
  }
}
