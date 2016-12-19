<?php

/**
 * ArUserChangePasswordRequest filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArUserChangePasswordRequestFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_user_id'   => new sfWidgetFormPropelChoice(array('model' => 'ArUser', 'add_empty' => true)),
      'at_date'      => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate(), 'with_empty' => false)),
      'old_password' => new sfWidgetFormFilterInput(),
      'new_password' => new sfWidgetFormFilterInput(),
      'is_processed' => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
    ));

    $this->setValidators(array(
      'ar_user_id'   => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArUser', 'column' => 'id')),
      'at_date'      => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'old_password' => new sfValidatorPass(array('required' => false)),
      'new_password' => new sfValidatorPass(array('required' => false)),
      'is_processed' => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
    ));

    $this->widgetSchema->setNameFormat('ar_user_change_password_request_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArUserChangePasswordRequest';
  }

  public function getFields()
  {
    return array(
      'id'           => 'Number',
      'ar_user_id'   => 'ForeignKey',
      'at_date'      => 'Date',
      'old_password' => 'Text',
      'new_password' => 'Text',
      'is_processed' => 'Boolean',
    );
  }
}
