<?php

/**
 * ArDatabaseVersion filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArDatabaseVersionFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'version'           => new sfWidgetFormFilterInput(),
      'installation_date' => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
    ));

    $this->setValidators(array(
      'version'           => new sfValidatorPass(array('required' => false)),
      'installation_date' => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
    ));

    $this->widgetSchema->setNameFormat('ar_database_version_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArDatabaseVersion';
  }

  public function getFields()
  {
    return array(
      'id'                => 'Number',
      'version'           => 'Text',
      'installation_date' => 'Date',
    );
  }
}
