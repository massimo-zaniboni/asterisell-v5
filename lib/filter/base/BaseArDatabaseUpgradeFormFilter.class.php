<?php

/**
 * ArDatabaseUpgrade filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArDatabaseUpgradeFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'sql_command'       => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'installation_date' => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
    ));

    $this->setValidators(array(
      'sql_command'       => new sfValidatorPass(array('required' => false)),
      'installation_date' => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
    ));

    $this->widgetSchema->setNameFormat('ar_database_upgrade_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArDatabaseUpgrade';
  }

  public function getFields()
  {
    return array(
      'id'                => 'Number',
      'sql_command'       => 'Text',
      'installation_date' => 'Date',
    );
  }
}
