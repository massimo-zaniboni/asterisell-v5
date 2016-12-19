<?php

/**
 * ArAsteriskAccount filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArAsteriskAccountFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'name'                => new sfWidgetFormFilterInput(),
      'account_code'        => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'ar_office_id'        => new sfWidgetFormPropelChoice(array('model' => 'ArOffice', 'add_empty' => true)),
      'is_active'           => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
      'ar_rate_category_id' => new sfWidgetFormPropelChoice(array('model' => 'ArRateCategory', 'add_empty' => true)),
    ));

    $this->setValidators(array(
      'name'                => new sfValidatorPass(array('required' => false)),
      'account_code'        => new sfValidatorPass(array('required' => false)),
      'ar_office_id'        => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArOffice', 'column' => 'id')),
      'is_active'           => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
      'ar_rate_category_id' => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArRateCategory', 'column' => 'id')),
    ));

    $this->widgetSchema->setNameFormat('ar_asterisk_account_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArAsteriskAccount';
  }

  public function getFields()
  {
    return array(
      'id'                  => 'Number',
      'name'                => 'Text',
      'account_code'        => 'Text',
      'ar_office_id'        => 'ForeignKey',
      'is_active'           => 'Boolean',
      'ar_rate_category_id' => 'ForeignKey',
    );
  }
}
