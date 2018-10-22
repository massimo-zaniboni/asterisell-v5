<?php

/**
 * ArCdrProvider filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArCdrProviderFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'internal_name'      => new sfWidgetFormFilterInput(),
      'description'        => new sfWidgetFormFilterInput(),
      'last_imported_id'   => new sfWidgetFormFilterInput(),
      'last_imported_data' => new sfWidgetFormFilterInput(),
    ));

    $this->setValidators(array(
      'internal_name'      => new sfValidatorPass(array('required' => false)),
      'description'        => new sfValidatorPass(array('required' => false)),
      'last_imported_id'   => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'last_imported_data' => new sfValidatorPass(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_cdr_provider_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArCdrProvider';
  }

  public function getFields()
  {
    return array(
      'id'                 => 'Number',
      'internal_name'      => 'Text',
      'description'        => 'Text',
      'last_imported_id'   => 'Number',
      'last_imported_data' => 'Text',
    );
  }
}
