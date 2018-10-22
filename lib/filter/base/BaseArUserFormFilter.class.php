<?php

/**
 * ArUser filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArUserFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_party_id'                  => new sfWidgetFormPropelChoice(array('model' => 'ArParty', 'add_empty' => true)),
      'ar_organization_unit_id'      => new sfWidgetFormPropelChoice(array('model' => 'ArOrganizationUnit', 'add_empty' => true)),
      'login'                        => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'password'                     => new sfWidgetFormFilterInput(),
      'clear_password_to_import'     => new sfWidgetFormFilterInput(),
      'is_enabled'                   => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'is_root_admin'                => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'ar_user_can_view_report_list' => new sfWidgetFormPropelChoice(array('model' => 'ArReport', 'add_empty' => true)),
    ));

    $this->setValidators(array(
      'ar_party_id'                  => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArParty', 'column' => 'id')),
      'ar_organization_unit_id'      => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArOrganizationUnit', 'column' => 'id')),
      'login'                        => new sfValidatorPass(array('required' => false)),
      'password'                     => new sfValidatorPass(array('required' => false)),
      'clear_password_to_import'     => new sfValidatorPass(array('required' => false)),
      'is_enabled'                   => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'is_root_admin'                => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'ar_user_can_view_report_list' => new sfValidatorPropelChoice(array('model' => 'ArReport', 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_user_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function addArUserCanViewReportListColumnCriteria(Criteria $criteria, $field, $values)
  {
    if (!is_array($values))
    {
      $values = array($values);
    }

    if (!count($values))
    {
      return;
    }

    $criteria->addJoin(ArUserCanViewReportPeer::AR_USER_ID, ArUserPeer::ID);

    $value = array_pop($values);
    $criterion = $criteria->getNewCriterion(ArUserCanViewReportPeer::AR_REPORT_ID, $value);

    foreach ($values as $value)
    {
      $criterion->addOr($criteria->getNewCriterion(ArUserCanViewReportPeer::AR_REPORT_ID, $value));
    }

    $criteria->add($criterion);
  }

  public function getModelName()
  {
    return 'ArUser';
  }

  public function getFields()
  {
    return array(
      'id'                           => 'Number',
      'ar_party_id'                  => 'ForeignKey',
      'ar_organization_unit_id'      => 'ForeignKey',
      'login'                        => 'Text',
      'password'                     => 'Text',
      'clear_password_to_import'     => 'Text',
      'is_enabled'                   => 'Boolean',
      'is_root_admin'                => 'Boolean',
      'ar_user_can_view_report_list' => 'ManyKey',
    );
  }
}
