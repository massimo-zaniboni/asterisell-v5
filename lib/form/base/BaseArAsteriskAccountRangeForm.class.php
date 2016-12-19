<?php

/**
 * ArAsteriskAccountRange form base class.
 *
 * @method ArAsteriskAccountRange getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArAsteriskAccountRangeForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                       => new sfWidgetFormInputHidden(),
      'ar_organization_unit_id'  => new sfWidgetFormPropelChoice(array('model' => 'ArOrganizationUnit', 'add_empty' => true)),
      'system_prefix'            => new sfWidgetFormInputText(),
      'system_suffix'            => new sfWidgetFormInputText(),
      'system_start_range'       => new sfWidgetFormInputText(),
      'system_end_range'         => new sfWidgetFormInputText(),
      'system_leading_zero'      => new sfWidgetFormInputText(),
      'is_delete'                => new sfWidgetFormInputCheckbox(),
      'is_physical_delete'       => new sfWidgetFormInputCheckbox(),
      'user_prefix'              => new sfWidgetFormInputText(),
      'user_suffix'              => new sfWidgetFormInputText(),
      'user_start_range'         => new sfWidgetFormInputText(),
      'generate_range_for_users' => new sfWidgetFormInputCheckbox(),
      'user_leading_zero'        => new sfWidgetFormInputText(),
      'user_note'                => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'id'                       => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'ar_organization_unit_id'  => new sfValidatorPropelChoice(array('model' => 'ArOrganizationUnit', 'column' => 'id', 'required' => false)),
      'system_prefix'            => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'system_suffix'            => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'system_start_range'       => new sfValidatorString(array('max_length' => 18)),
      'system_end_range'         => new sfValidatorString(array('max_length' => 18)),
      'system_leading_zero'      => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647)),
      'is_delete'                => new sfValidatorBoolean(),
      'is_physical_delete'       => new sfValidatorBoolean(),
      'user_prefix'              => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'user_suffix'              => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'user_start_range'         => new sfValidatorString(array('max_length' => 18)),
      'generate_range_for_users' => new sfValidatorBoolean(),
      'user_leading_zero'        => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647)),
      'user_note'                => new sfValidatorString(array('max_length' => 6048, 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_asterisk_account_range[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArAsteriskAccountRange';
  }


}
