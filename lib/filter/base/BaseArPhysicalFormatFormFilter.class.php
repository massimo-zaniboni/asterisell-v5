<?php

/**
 * ArPhysicalFormat filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArPhysicalFormatFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_logical_source_id' => new sfWidgetFormPropelChoice(array('model' => 'ArLogicalSource', 'add_empty' => true)),
      'name'                 => new sfWidgetFormFilterInput(),
      'description'          => new sfWidgetFormFilterInput(),
    ));

    $this->setValidators(array(
      'ar_logical_source_id' => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArLogicalSource', 'column' => 'id')),
      'name'                 => new sfValidatorPass(array('required' => false)),
      'description'          => new sfValidatorPass(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_physical_format_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArPhysicalFormat';
  }

  public function getFields()
  {
    return array(
      'id'                   => 'Number',
      'ar_logical_source_id' => 'ForeignKey',
      'name'                 => 'Text',
      'description'          => 'Text',
    );
  }
}
