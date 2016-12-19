<?php

/**
 * ArSourceCdrToMove filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArSourceCdrToMoveFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
    ));

    $this->setValidators(array(
    ));

    $this->widgetSchema->setNameFormat('ar_source_cdr_to_move_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArSourceCdrToMove';
  }

  public function getFields()
  {
    return array(
      'ar_source_cdr_id' => 'Number',
    );
  }
}
