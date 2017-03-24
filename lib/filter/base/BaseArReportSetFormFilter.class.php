<?php

/**
 * ArReportSet filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArReportSetFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_report_scheduler_id'       => new sfWidgetFormPropelChoice(array('model' => 'ArReportScheduler', 'add_empty' => true)),
      'from_date'                    => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate(), 'with_empty' => false)),
      'to_date'                      => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate(), 'with_empty' => false)),
      'must_be_reviewed'             => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'postponed_fields_are_updated' => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'postponed_reports'            => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'postponed_amount'             => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'reports'                      => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'amount'                       => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'ar_postponed_report_list'     => new sfWidgetFormPropelChoice(array('model' => 'ArOrganizationUnit', 'add_empty' => true)),
    ));

    $this->setValidators(array(
      'ar_report_scheduler_id'       => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArReportScheduler', 'column' => 'id')),
      'from_date'                    => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'to_date'                      => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'must_be_reviewed'             => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'postponed_fields_are_updated' => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'postponed_reports'            => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'postponed_amount'             => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'reports'                      => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'amount'                       => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'ar_postponed_report_list'     => new sfValidatorPropelChoice(array('model' => 'ArOrganizationUnit', 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_report_set_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function addArPostponedReportListColumnCriteria(Criteria $criteria, $field, $values)
  {
    if (!is_array($values))
    {
      $values = array($values);
    }

    if (!count($values))
    {
      return;
    }

    $criteria->addJoin(ArPostponedReportPeer::AR_REPORT_SET_ID, ArReportSetPeer::ID);

    $value = array_pop($values);
    $criterion = $criteria->getNewCriterion(ArPostponedReportPeer::AR_ORGANIZATION_UNIT_ID, $value);

    foreach ($values as $value)
    {
      $criterion->addOr($criteria->getNewCriterion(ArPostponedReportPeer::AR_ORGANIZATION_UNIT_ID, $value));
    }

    $criteria->add($criterion);
  }

  public function getModelName()
  {
    return 'ArReportSet';
  }

  public function getFields()
  {
    return array(
      'id'                           => 'Number',
      'ar_report_scheduler_id'       => 'ForeignKey',
      'from_date'                    => 'Date',
      'to_date'                      => 'Date',
      'must_be_reviewed'             => 'Boolean',
      'postponed_fields_are_updated' => 'Boolean',
      'postponed_reports'            => 'Number',
      'postponed_amount'             => 'Number',
      'reports'                      => 'Number',
      'amount'                       => 'Number',
      'ar_postponed_report_list'     => 'ManyKey',
    );
  }
}
