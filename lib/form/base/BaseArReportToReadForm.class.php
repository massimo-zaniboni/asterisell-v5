<?php

/**
 * ArReportToRead form base class.
 *
 * @method ArReportToRead getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArReportToReadForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                         => new sfWidgetFormInputHidden(),
      'ar_report_id'               => new sfWidgetFormPropelChoice(array('model' => 'ArReport', 'add_empty' => false)),
      'ar_user_id'                 => new sfWidgetFormPropelChoice(array('model' => 'ArUser', 'add_empty' => true)),
      'seen_or_received_from_user' => new sfWidgetFormInputCheckbox(),
      'must_be_sent_to_email'      => new sfWidgetFormInputCheckbox(),
      'sent_to_email_at_date'      => new sfWidgetFormDateTime(),
    ));

    $this->setValidators(array(
      'id'                         => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'ar_report_id'               => new sfValidatorPropelChoice(array('model' => 'ArReport', 'column' => 'id')),
      'ar_user_id'                 => new sfValidatorPropelChoice(array('model' => 'ArUser', 'column' => 'id', 'required' => false)),
      'seen_or_received_from_user' => new sfValidatorBoolean(),
      'must_be_sent_to_email'      => new sfValidatorBoolean(),
      'sent_to_email_at_date'      => new sfValidatorDateTime(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_report_to_read[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArReportToRead';
  }


}
