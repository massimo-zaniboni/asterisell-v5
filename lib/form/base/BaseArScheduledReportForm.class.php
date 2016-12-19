<?php

/**
 * ArScheduledReport form base class.
 *
 * @method ArScheduledReport getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArScheduledReportForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                                     => new sfWidgetFormInputHidden(),
      'php_class_serialization'                => new sfWidgetFormTextarea(),
      'is_active'                              => new sfWidgetFormInputCheckbox(),
      'ar_organization_unit_id'                => new sfWidgetFormPropelChoice(array('model' => 'ArOrganizationUnit', 'add_empty' => true)),
      'note'                                   => new sfWidgetFormInputText(),
      'ar_user_id'                             => new sfWidgetFormPropelChoice(array('model' => 'ArUser', 'add_empty' => true)),
      'produced_report_short_description'      => new sfWidgetFormInputText(),
      'produced_report_additional_description' => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'id'                                     => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'php_class_serialization'                => new sfValidatorString(array('required' => false)),
      'is_active'                              => new sfValidatorBoolean(array('required' => false)),
      'ar_organization_unit_id'                => new sfValidatorPropelChoice(array('model' => 'ArOrganizationUnit', 'column' => 'id', 'required' => false)),
      'note'                                   => new sfValidatorString(array('max_length' => 2048, 'required' => false)),
      'ar_user_id'                             => new sfValidatorPropelChoice(array('model' => 'ArUser', 'column' => 'id', 'required' => false)),
      'produced_report_short_description'      => new sfValidatorString(array('max_length' => 2048, 'required' => false)),
      'produced_report_additional_description' => new sfValidatorString(array('max_length' => 2048, 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_scheduled_report[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArScheduledReport';
  }


}
