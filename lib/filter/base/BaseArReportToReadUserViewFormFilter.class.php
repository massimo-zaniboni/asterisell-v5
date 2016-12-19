<?php

/**
 * ArReportToReadUserView filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArReportToReadUserViewFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_report_to_read_id'                   => new sfWidgetFormPropelChoice(array('model' => 'ArReportToRead', 'add_empty' => true)),
      'ar_report_id'                           => new sfWidgetFormPropelChoice(array('model' => 'ArReport', 'add_empty' => true)),
      'ar_user_id'                             => new sfWidgetFormPropelChoice(array('model' => 'ArUser', 'add_empty' => true)),
      'seen_or_received_from_user'             => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'ar_organization_unit_id'                => new sfWidgetFormPropelChoice(array('model' => 'ArOrganizationUnit', 'add_empty' => true)),
      'from_date'                              => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'to_date'                                => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'produced_report_generation_date'        => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'produced_report_short_description'      => new sfWidgetFormFilterInput(),
      'produced_report_additional_description' => new sfWidgetFormFilterInput(),
      'produced_report_already_reviewed'       => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'produced_report_is_draft'               => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
    ));

    $this->setValidators(array(
      'ar_report_to_read_id'                   => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArReportToRead', 'column' => 'id')),
      'ar_report_id'                           => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArReport', 'column' => 'id')),
      'ar_user_id'                             => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArUser', 'column' => 'id')),
      'seen_or_received_from_user'             => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'ar_organization_unit_id'                => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArOrganizationUnit', 'column' => 'id')),
      'from_date'                              => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'to_date'                                => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'produced_report_generation_date'        => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'produced_report_short_description'      => new sfValidatorPass(array('required' => false)),
      'produced_report_additional_description' => new sfValidatorPass(array('required' => false)),
      'produced_report_already_reviewed'       => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'produced_report_is_draft'               => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
    ));

    $this->widgetSchema->setNameFormat('ar_report_to_read_user_view_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArReportToReadUserView';
  }

  public function getFields()
  {
    return array(
      'id'                                     => 'Number',
      'ar_report_to_read_id'                   => 'ForeignKey',
      'ar_report_id'                           => 'ForeignKey',
      'ar_user_id'                             => 'ForeignKey',
      'seen_or_received_from_user'             => 'Boolean',
      'ar_organization_unit_id'                => 'ForeignKey',
      'from_date'                              => 'Date',
      'to_date'                                => 'Date',
      'produced_report_generation_date'        => 'Date',
      'produced_report_short_description'      => 'Text',
      'produced_report_additional_description' => 'Text',
      'produced_report_already_reviewed'       => 'Boolean',
      'produced_report_is_draft'               => 'Boolean',
    );
  }
}
