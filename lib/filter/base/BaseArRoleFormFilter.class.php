<?php

/**
 * ArRole filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArRoleFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'name'                    => new sfWidgetFormFilterInput(),
      'power'                   => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'description'             => new sfWidgetFormFilterInput(),
      'internal_name'           => new sfWidgetFormFilterInput(),
      'ar_report_also_for_list' => new sfWidgetFormPropelChoice(array('model' => 'ArReport', 'add_empty' => true)),
    ));

    $this->setValidators(array(
      'name'                    => new sfValidatorPass(array('required' => false)),
      'power'                   => new sfValidatorSchemaFilter('text', new sfValidatorInteger(array('required' => false))),
      'description'             => new sfValidatorPass(array('required' => false)),
      'internal_name'           => new sfValidatorPass(array('required' => false)),
      'ar_report_also_for_list' => new sfValidatorPropelChoice(array('model' => 'ArReport', 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_role_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function addArReportAlsoForListColumnCriteria(Criteria $criteria, $field, $values)
  {
    if (!is_array($values))
    {
      $values = array($values);
    }

    if (!count($values))
    {
      return;
    }

    $criteria->addJoin(ArReportAlsoForPeer::AR_ROLE_ID, ArRolePeer::ID);

    $value = array_pop($values);
    $criterion = $criteria->getNewCriterion(ArReportAlsoForPeer::AR_REPORT_ID, $value);

    foreach ($values as $value)
    {
      $criterion->addOr($criteria->getNewCriterion(ArReportAlsoForPeer::AR_REPORT_ID, $value));
    }

    $criteria->add($criterion);
  }

  public function getModelName()
  {
    return 'ArRole';
  }

  public function getFields()
  {
    return array(
      'id'                      => 'Number',
      'name'                    => 'Text',
      'power'                   => 'Number',
      'description'             => 'Text',
      'internal_name'           => 'Text',
      'ar_report_also_for_list' => 'ManyKey',
    );
  }
}
