<?php

/**
 * ArItcOrganizations filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArItcOrganizationsFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'account_code'            => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'definition_time'         => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'org'                     => new sfWidgetFormFilterInput(),
      'name'                    => new sfWidgetFormFilterInput(),
      'description'             => new sfWidgetFormFilterInput(),
      'email'                   => new sfWidgetFormFilterInput(),
      'parent'                  => new sfWidgetFormFilterInput(),
      'calculated_account_code' => new sfWidgetFormFilterInput(),
      'is_new'                  => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'is_maybe_modified'       => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'is_to_remove'            => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'can_be_removed'          => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
    ));

    $this->setValidators(array(
      'account_code'            => new sfValidatorPass(array('required' => false)),
      'definition_time'         => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'org'                     => new sfValidatorPass(array('required' => false)),
      'name'                    => new sfValidatorPass(array('required' => false)),
      'description'             => new sfValidatorPass(array('required' => false)),
      'email'                   => new sfValidatorPass(array('required' => false)),
      'parent'                  => new sfValidatorPass(array('required' => false)),
      'calculated_account_code' => new sfValidatorPass(array('required' => false)),
      'is_new'                  => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'is_maybe_modified'       => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'is_to_remove'            => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'can_be_removed'          => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
    ));

    $this->widgetSchema->setNameFormat('ar_itc_organizations_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArItcOrganizations';
  }

  public function getFields()
  {
    return array(
      'id'                      => 'Number',
      'account_code'            => 'Text',
      'definition_time'         => 'Date',
      'org'                     => 'Text',
      'name'                    => 'Text',
      'description'             => 'Text',
      'email'                   => 'Text',
      'parent'                  => 'Text',
      'calculated_account_code' => 'Text',
      'is_new'                  => 'Boolean',
      'is_maybe_modified'       => 'Boolean',
      'is_to_remove'            => 'Boolean',
      'can_be_removed'          => 'Boolean',
    );
  }
}
