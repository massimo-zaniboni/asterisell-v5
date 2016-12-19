<?php

/**
 * ArOrganizationBackupOfChanges form base class.
 *
 * @method ArOrganizationBackupOfChanges getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArOrganizationBackupOfChangesForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                  => new sfWidgetFormInputHidden(),
      'backup_at_date'      => new sfWidgetFormDateTime(),
      'application_version' => new sfWidgetFormInputText(),
      'md5_sum'             => new sfWidgetFormInputText(),
      'yaml_export_at_date' => new sfWidgetFormInputText(),
      'sql_tables'          => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'id'                  => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'backup_at_date'      => new sfValidatorDateTime(),
      'application_version' => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'md5_sum'             => new sfValidatorString(array('max_length' => 2048, 'required' => false)),
      'yaml_export_at_date' => new sfValidatorPass(array('required' => false)),
      'sql_tables'          => new sfValidatorPass(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_organization_backup_of_changes[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArOrganizationBackupOfChanges';
  }


}
