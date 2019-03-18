<?php

/**
 * ArOrganizationUnit filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArOrganizationUnitFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'internal_name'              => new sfWidgetFormFilterInput(),
      'internal_name2'             => new sfWidgetFormFilterInput(),
      'internal_checksum1'         => new sfWidgetFormFilterInput(),
      'internal_checksum2'         => new sfWidgetFormFilterInput(),
      'internal_checksum3'         => new sfWidgetFormFilterInput(),
      'internal_checksum4'         => new sfWidgetFormFilterInput(),
      'internal_checksum5'         => new sfWidgetFormFilterInput(),
      'export_code'                => new sfWidgetFormFilterInput(),
      'automatically_managed_from' => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'ar_postponed_report_list'   => new sfWidgetFormPropelChoice(array('model' => 'ArReportSet', 'add_empty' => true)),
    ));

    $this->setValidators(array(
      'internal_name'              => new sfValidatorPass(array('required' => false)),
      'internal_name2'             => new sfValidatorPass(array('required' => false)),
      'internal_checksum1'         => new sfValidatorPass(array('required' => false)),
      'internal_checksum2'         => new sfValidatorPass(array('required' => false)),
      'internal_checksum3'         => new sfValidatorPass(array('required' => false)),
      'internal_checksum4'         => new sfValidatorPass(array('required' => false)),
      'internal_checksum5'         => new sfValidatorPass(array('required' => false)),
      'export_code'                => new sfValidatorPass(array('required' => false)),
      'automatically_managed_from' => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'ar_postponed_report_list'   => new sfValidatorPropelChoice(array('model' => 'ArReportSet', 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_organization_unit_filters[%s]');

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

    $criteria->addJoin(ArPostponedReportPeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID);

    $value = array_pop($values);
    $criterion = $criteria->getNewCriterion(ArPostponedReportPeer::AR_REPORT_SET_ID, $value);

    foreach ($values as $value)
    {
      $criterion->addOr($criteria->getNewCriterion(ArPostponedReportPeer::AR_REPORT_SET_ID, $value));
    }

    $criteria->add($criterion);
  }

  public function getModelName()
  {
    return 'ArOrganizationUnit';
  }

  public function getFields()
  {
    return array(
      'id'                         => 'Number',
      'internal_name'              => 'Text',
      'internal_name2'             => 'Text',
      'internal_checksum1'         => 'Text',
      'internal_checksum2'         => 'Text',
      'internal_checksum3'         => 'Text',
      'internal_checksum4'         => 'Text',
      'internal_checksum5'         => 'Text',
      'export_code'                => 'Text',
      'automatically_managed_from' => 'Number',
      'ar_postponed_report_list'   => 'ManyKey',
    );
  }
}
