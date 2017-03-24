<?php

/**
 * ArPostponedReport form base class.
 *
 * @method ArPostponedReport getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArPostponedReportForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_report_set_id'        => new sfWidgetFormInputHidden(),
      'ar_organization_unit_id' => new sfWidgetFormInputHidden(),
    ));

    $this->setValidators(array(
      'ar_report_set_id'        => new sfValidatorPropelChoice(array('model' => 'ArReportSet', 'column' => 'id', 'required' => false)),
      'ar_organization_unit_id' => new sfValidatorPropelChoice(array('model' => 'ArOrganizationUnit', 'column' => 'id', 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_postponed_report[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArPostponedReport';
  }


}
