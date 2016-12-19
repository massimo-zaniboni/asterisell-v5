<?php

/**
 * ArReportSet form base class.
 *
 * @method ArReportSet getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArReportSetForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                     => new sfWidgetFormInputHidden(),
      'ar_report_scheduler_id' => new sfWidgetFormPropelChoice(array('model' => 'ArReportScheduler', 'add_empty' => true)),
      'from_date'              => new sfWidgetFormDateTime(),
      'to_date'                => new sfWidgetFormDateTime(),
      'must_be_reviewed'       => new sfWidgetFormInputCheckbox(),
    ));

    $this->setValidators(array(
      'id'                     => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'ar_report_scheduler_id' => new sfValidatorPropelChoice(array('model' => 'ArReportScheduler', 'column' => 'id', 'required' => false)),
      'from_date'              => new sfValidatorDateTime(),
      'to_date'                => new sfValidatorDateTime(),
      'must_be_reviewed'       => new sfValidatorBoolean(),
    ));

    $this->widgetSchema->setNameFormat('ar_report_set[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArReportSet';
  }


}
