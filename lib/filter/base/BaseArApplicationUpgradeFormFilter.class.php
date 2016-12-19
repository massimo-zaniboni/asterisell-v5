<?php

/**
 * ArApplicationUpgrade filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArApplicationUpgradeFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'upg_key'           => new sfWidgetFormFilterInput(),
      'upg_output'        => new sfWidgetFormFilterInput(),
      'installation_date' => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
    ));

    $this->setValidators(array(
      'upg_key'           => new sfValidatorPass(array('required' => false)),
      'upg_output'        => new sfValidatorPass(array('required' => false)),
      'installation_date' => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
    ));

    $this->widgetSchema->setNameFormat('ar_application_upgrade_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArApplicationUpgrade';
  }

  public function getFields()
  {
    return array(
      'id'                => 'Number',
      'upg_key'           => 'Text',
      'upg_output'        => 'Text',
      'installation_date' => 'Date',
    );
  }
}
