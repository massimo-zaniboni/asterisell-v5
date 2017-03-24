<?php

/**
 * ArPostponedReportTmp form base class.
 *
 * @method ArPostponedReportTmp getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArPostponedReportTmpForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_organization_unit_id' => new sfWidgetFormInputHidden(),
      'from_date'               => new sfWidgetFormDateTime(),
      'is_billed'               => new sfWidgetFormInputCheckbox(),
      'is_processed'            => new sfWidgetFormInputCheckbox(),
    ));

    $this->setValidators(array(
      'ar_organization_unit_id' => new sfValidatorPropelChoice(array('model' => 'ArOrganizationUnit', 'column' => 'id', 'required' => false)),
      'from_date'               => new sfValidatorDateTime(),
      'is_billed'               => new sfValidatorBoolean(),
      'is_processed'            => new sfValidatorBoolean(),
    ));

    $this->widgetSchema->setNameFormat('ar_postponed_report_tmp[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArPostponedReportTmp';
  }


}
