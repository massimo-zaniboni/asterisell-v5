<?php

/**
 * ArGeneratedReport filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArGeneratedReportFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_generated_report_type_id' => new sfWidgetFormPropelChoice(array('model' => 'ArGeneratedReportType', 'add_empty' => true)),
      'ar_scheduled_report_id'      => new sfWidgetFormPropelChoice(array('model' => 'ArScheduledReport', 'add_empty' => true)),
      'generation_date'             => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate(), 'with_empty' => false)),
      'cdrs_from_date'              => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate(), 'with_empty' => false)),
      'cdrs_to_date'                => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate(), 'with_empty' => false)),
      'is_draft'                    => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'ar_organization_unit_id'     => new sfWidgetFormPropelChoice(array('model' => 'ArOrganizationUnit', 'add_empty' => true)),
      'ar_user_id'                  => new sfWidgetFormPropelChoice(array('model' => 'ArUser', 'add_empty' => true)),
      'file_name'                   => new sfWidgetFormFilterInput(),
      'pdf_document'                => new sfWidgetFormFilterInput(),
    ));

    $this->setValidators(array(
      'ar_generated_report_type_id' => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArGeneratedReportType', 'column' => 'id')),
      'ar_scheduled_report_id'      => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArScheduledReport', 'column' => 'id')),
      'generation_date'             => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'cdrs_from_date'              => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'cdrs_to_date'                => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'is_draft'                    => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'ar_organization_unit_id'     => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArOrganizationUnit', 'column' => 'id')),
      'ar_user_id'                  => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArUser', 'column' => 'id')),
      'file_name'                   => new sfValidatorPass(array('required' => false)),
      'pdf_document'                => new sfValidatorPass(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_generated_report_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArGeneratedReport';
  }

  public function getFields()
  {
    return array(
      'id'                          => 'Number',
      'ar_generated_report_type_id' => 'ForeignKey',
      'ar_scheduled_report_id'      => 'ForeignKey',
      'generation_date'             => 'Date',
      'cdrs_from_date'              => 'Date',
      'cdrs_to_date'                => 'Date',
      'is_draft'                    => 'Boolean',
      'ar_organization_unit_id'     => 'ForeignKey',
      'ar_user_id'                  => 'ForeignKey',
      'file_name'                   => 'Text',
      'pdf_document'                => 'Text',
    );
  }
}
