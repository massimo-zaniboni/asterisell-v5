<?php

/**
 * ArGeneratedReportContent filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArGeneratedReportContentFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_generated_report_id' => new sfWidgetFormPropelChoice(array('model' => 'ArGeneratedReport', 'add_empty' => true)),
      'line_index'             => new sfWidgetFormFilterInput(),
      'content_part'           => new sfWidgetFormFilterInput(),
    ));

    $this->setValidators(array(
      'ar_generated_report_id' => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArGeneratedReport', 'column' => 'id')),
      'line_index'             => new sfValidatorPass(array('required' => false)),
      'content_part'           => new sfValidatorPass(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_generated_report_content_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArGeneratedReportContent';
  }

  public function getFields()
  {
    return array(
      'id'                     => 'Number',
      'ar_generated_report_id' => 'ForeignKey',
      'line_index'             => 'Text',
      'content_part'           => 'Text',
    );
  }
}
