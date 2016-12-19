<?php

/**
 * ArReportToReadUserView form base class.
 *
 * @method ArReportToReadUserView getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArReportToReadUserViewForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                                     => new sfWidgetFormInputHidden(),
      'ar_report_to_read_id'                   => new sfWidgetFormPropelChoice(array('model' => 'ArReportToRead', 'add_empty' => true)),
      'ar_report_id'                           => new sfWidgetFormPropelChoice(array('model' => 'ArReport', 'add_empty' => true)),
      'ar_user_id'                             => new sfWidgetFormPropelChoice(array('model' => 'ArUser', 'add_empty' => true)),
      'seen_or_received_from_user'             => new sfWidgetFormInputCheckbox(),
      'ar_organization_unit_id'                => new sfWidgetFormPropelChoice(array('model' => 'ArOrganizationUnit', 'add_empty' => true)),
      'from_date'                              => new sfWidgetFormDateTime(),
      'to_date'                                => new sfWidgetFormDateTime(),
      'produced_report_generation_date'        => new sfWidgetFormDateTime(),
      'produced_report_short_description'      => new sfWidgetFormInputText(),
      'produced_report_additional_description' => new sfWidgetFormInputText(),
      'produced_report_already_reviewed'       => new sfWidgetFormInputCheckbox(),
      'produced_report_is_draft'               => new sfWidgetFormInputCheckbox(),
    ));

    $this->setValidators(array(
      'id'                                     => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'ar_report_to_read_id'                   => new sfValidatorPropelChoice(array('model' => 'ArReportToRead', 'column' => 'id', 'required' => false)),
      'ar_report_id'                           => new sfValidatorPropelChoice(array('model' => 'ArReport', 'column' => 'id', 'required' => false)),
      'ar_user_id'                             => new sfValidatorPropelChoice(array('model' => 'ArUser', 'column' => 'id', 'required' => false)),
      'seen_or_received_from_user'             => new sfValidatorBoolean(),
      'ar_organization_unit_id'                => new sfValidatorPropelChoice(array('model' => 'ArOrganizationUnit', 'column' => 'id', 'required' => false)),
      'from_date'                              => new sfValidatorDateTime(array('required' => false)),
      'to_date'                                => new sfValidatorDateTime(array('required' => false)),
      'produced_report_generation_date'        => new sfValidatorDateTime(array('required' => false)),
      'produced_report_short_description'      => new sfValidatorString(array('max_length' => 2048, 'required' => false)),
      'produced_report_additional_description' => new sfValidatorString(array('max_length' => 2048, 'required' => false)),
      'produced_report_already_reviewed'       => new sfValidatorBoolean(),
      'produced_report_is_draft'               => new sfValidatorBoolean(),
    ));

    $this->widgetSchema->setNameFormat('ar_report_to_read_user_view[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArReportToReadUserView';
  }


}
