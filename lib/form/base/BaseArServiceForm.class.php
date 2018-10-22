<?php

/**
 * ArService form base class.
 *
 * @method ArService getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArServiceForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                                         => new sfWidgetFormInputHidden(),
      'internal_name'                              => new sfWidgetFormInputText(),
      'customer_name'                              => new sfWidgetFormInputText(),
      'customer_description'                       => new sfWidgetFormInputText(),
      'vendor_name'                                => new sfWidgetFormInputText(),
      'vendor_description'                         => new sfWidgetFormInputText(),
      'external_crm_code'                          => new sfWidgetFormInputText(),
      'customer_price_depend_from_activation_date' => new sfWidgetFormInputCheckbox(),
      'customer_price_change_with_price_list'      => new sfWidgetFormInputCheckbox(),
      'is_enabled'                                 => new sfWidgetFormInputCheckbox(),
      'is_applied_only_one_time'                   => new sfWidgetFormInputCheckbox(),
      'schedule_timeframe'                         => new sfWidgetFormInputText(),
      'was_compiled'                               => new sfWidgetFormInputCheckbox(),
      'schedule_from'                              => new sfWidgetFormInputText(),
      'schedule_at'                                => new sfWidgetFormTime(),
    ));

    $this->setValidators(array(
      'id'                                         => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'internal_name'                              => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'customer_name'                              => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'customer_description'                       => new sfValidatorString(array('max_length' => 2048, 'required' => false)),
      'vendor_name'                                => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'vendor_description'                         => new sfValidatorString(array('max_length' => 2048, 'required' => false)),
      'external_crm_code'                          => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'customer_price_depend_from_activation_date' => new sfValidatorBoolean(),
      'customer_price_change_with_price_list'      => new sfValidatorBoolean(),
      'is_enabled'                                 => new sfValidatorBoolean(),
      'is_applied_only_one_time'                   => new sfValidatorBoolean(),
      'schedule_timeframe'                         => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'was_compiled'                               => new sfValidatorBoolean(),
      'schedule_from'                              => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'schedule_at'                                => new sfValidatorTime(array('required' => false)),
    ));

    $this->validatorSchema->setPostValidator(
      new sfValidatorPropelUnique(array('model' => 'ArService', 'column' => array('internal_name')))
    );

    $this->widgetSchema->setNameFormat('ar_service[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArService';
  }


}
