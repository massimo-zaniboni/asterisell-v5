<?php

/**
 * ArOrganizationBackupOfChanges filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArOrganizationBackupOfChangesFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'backup_at_date'      => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate(), 'with_empty' => false)),
      'application_version' => new sfWidgetFormFilterInput(),
      'md5_sum'             => new sfWidgetFormFilterInput(),
      'yaml_export_at_date' => new sfWidgetFormFilterInput(),
      'sql_tables'          => new sfWidgetFormFilterInput(),
    ));

    $this->setValidators(array(
      'backup_at_date'      => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'application_version' => new sfValidatorPass(array('required' => false)),
      'md5_sum'             => new sfValidatorPass(array('required' => false)),
      'yaml_export_at_date' => new sfValidatorPass(array('required' => false)),
      'sql_tables'          => new sfValidatorPass(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_organization_backup_of_changes_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArOrganizationBackupOfChanges';
  }

  public function getFields()
  {
    return array(
      'id'                  => 'Number',
      'backup_at_date'      => 'Date',
      'application_version' => 'Text',
      'md5_sum'             => 'Text',
      'yaml_export_at_date' => 'Text',
      'sql_tables'          => 'Text',
    );
  }
}
