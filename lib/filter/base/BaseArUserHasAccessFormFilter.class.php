<?php

/**
 * ArUserHasAccess filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArUserHasAccessFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_user_id'                             => new sfWidgetFormPropelChoice(array('model' => 'ArUser', 'add_empty' => true)),
      'ar_organization_unit_id'                => new sfWidgetFormPropelChoice(array('model' => 'ArOrganizationUnit', 'add_empty' => true)),
      'ar_role_id'                             => new sfWidgetFormPropelChoice(array('model' => 'ArRole', 'add_empty' => true)),
      'ar_permission_id'                       => new sfWidgetFormPropelChoice(array('model' => 'ArPermission', 'add_empty' => true)),
      'from'                                   => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'to'                                     => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'ar_user_has_access_has_permission_list' => new sfWidgetFormPropelChoice(array('model' => 'ArPermission', 'add_empty' => true)),
      'ar_user_has_access_has_role_list'       => new sfWidgetFormPropelChoice(array('model' => 'ArRole', 'add_empty' => true)),
    ));

    $this->setValidators(array(
      'ar_user_id'                             => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArUser', 'column' => 'id')),
      'ar_organization_unit_id'                => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArOrganizationUnit', 'column' => 'id')),
      'ar_role_id'                             => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArRole', 'column' => 'id')),
      'ar_permission_id'                       => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArPermission', 'column' => 'id')),
      'from'                                   => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'to'                                     => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'ar_user_has_access_has_permission_list' => new sfValidatorPropelChoice(array('model' => 'ArPermission', 'required' => false)),
      'ar_user_has_access_has_role_list'       => new sfValidatorPropelChoice(array('model' => 'ArRole', 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_user_has_access_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function addArUserHasAccessHasPermissionListColumnCriteria(Criteria $criteria, $field, $values)
  {
    if (!is_array($values))
    {
      $values = array($values);
    }

    if (!count($values))
    {
      return;
    }

    $criteria->addJoin(ArUserHasAccessHasPermissionPeer::AR_USER_HAS_ACCESS_ID, ArUserHasAccessPeer::ID);

    $value = array_pop($values);
    $criterion = $criteria->getNewCriterion(ArUserHasAccessHasPermissionPeer::AR_PERMISSION_ID, $value);

    foreach ($values as $value)
    {
      $criterion->addOr($criteria->getNewCriterion(ArUserHasAccessHasPermissionPeer::AR_PERMISSION_ID, $value));
    }

    $criteria->add($criterion);
  }

  public function addArUserHasAccessHasRoleListColumnCriteria(Criteria $criteria, $field, $values)
  {
    if (!is_array($values))
    {
      $values = array($values);
    }

    if (!count($values))
    {
      return;
    }

    $criteria->addJoin(ArUserHasAccessHasRolePeer::AR_USER_HAS_ACCESS_ID, ArUserHasAccessPeer::ID);

    $value = array_pop($values);
    $criterion = $criteria->getNewCriterion(ArUserHasAccessHasRolePeer::AR_ROLE_ID, $value);

    foreach ($values as $value)
    {
      $criterion->addOr($criteria->getNewCriterion(ArUserHasAccessHasRolePeer::AR_ROLE_ID, $value));
    }

    $criteria->add($criterion);
  }

  public function getModelName()
  {
    return 'ArUserHasAccess';
  }

  public function getFields()
  {
    return array(
      'id'                                     => 'Number',
      'ar_user_id'                             => 'ForeignKey',
      'ar_organization_unit_id'                => 'ForeignKey',
      'ar_role_id'                             => 'ForeignKey',
      'ar_permission_id'                       => 'ForeignKey',
      'from'                                   => 'Date',
      'to'                                     => 'Date',
      'ar_user_has_access_has_permission_list' => 'ManyKey',
      'ar_user_has_access_has_role_list'       => 'ManyKey',
    );
  }
}
