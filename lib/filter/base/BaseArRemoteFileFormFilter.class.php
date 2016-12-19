<?php

/**
 * ArRemoteFile filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArRemoteFileFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_cdr_provider_id' => new sfWidgetFormPropelChoice(array('model' => 'ArCdrProvider', 'add_empty' => true)),
      'name'               => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'checksum'           => new sfWidgetFormFilterInput(),
      'receiving_date'     => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
    ));

    $this->setValidators(array(
      'ar_cdr_provider_id' => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArCdrProvider', 'column' => 'id')),
      'name'               => new sfValidatorPass(array('required' => false)),
      'checksum'           => new sfValidatorPass(array('required' => false)),
      'receiving_date'     => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
    ));

    $this->widgetSchema->setNameFormat('ar_remote_file_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArRemoteFile';
  }

  public function getFields()
  {
    return array(
      'id'                 => 'Number',
      'ar_cdr_provider_id' => 'ForeignKey',
      'name'               => 'Text',
      'checksum'           => 'Text',
      'receiving_date'     => 'Date',
    );
  }
}
