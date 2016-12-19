<?php

/**
 * ArReadReport form base class.
 *
 * @method ArReadReport getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArReadReportForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                         => new sfWidgetFormInputHidden(),
      'ar_report_id'               => new sfWidgetFormPropelChoice(array('model' => 'ArReport', 'add_empty' => true)),
      'ar_user_id'                 => new sfWidgetFormPropelChoice(array('model' => 'ArUser', 'add_empty' => true)),
      'seen_or_received_from_user' => new sfWidgetFormInputCheckbox(),
      'sent_to_email_at_date'      => new sfWidgetFormDateTime(),
      'email_attempts'             => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'id'                         => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'ar_report_id'               => new sfValidatorPropelChoice(array('model' => 'ArReport', 'column' => 'id', 'required' => false)),
      'ar_user_id'                 => new sfValidatorPropelChoice(array('model' => 'ArUser', 'column' => 'id', 'required' => false)),
      'seen_or_received_from_user' => new sfValidatorBoolean(),
      'sent_to_email_at_date'      => new sfValidatorDateTime(array('required' => false)),
      'email_attempts'             => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_read_report[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArReadReport';
  }


}
