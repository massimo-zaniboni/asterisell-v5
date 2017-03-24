<?php

/**
 * ArReportSet form base class.
 *
 * @method ArReportSet getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArReportSetForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                           => new sfWidgetFormInputHidden(),
      'ar_report_scheduler_id'       => new sfWidgetFormPropelChoice(array('model' => 'ArReportScheduler', 'add_empty' => true)),
      'from_date'                    => new sfWidgetFormDateTime(),
      'to_date'                      => new sfWidgetFormDateTime(),
      'must_be_reviewed'             => new sfWidgetFormInputCheckbox(),
      'postponed_fields_are_updated' => new sfWidgetFormInputCheckbox(),
      'postponed_reports'            => new sfWidgetFormInputText(),
      'postponed_amount'             => new sfWidgetFormInputText(),
      'reports'                      => new sfWidgetFormInputText(),
      'amount'                       => new sfWidgetFormInputText(),
      'ar_postponed_report_list'     => new sfWidgetFormPropelChoice(array('multiple' => true, 'model' => 'ArOrganizationUnit')),
    ));

    $this->setValidators(array(
      'id'                           => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'ar_report_scheduler_id'       => new sfValidatorPropelChoice(array('model' => 'ArReportScheduler', 'column' => 'id', 'required' => false)),
      'from_date'                    => new sfValidatorDateTime(),
      'to_date'                      => new sfValidatorDateTime(),
      'must_be_reviewed'             => new sfValidatorBoolean(),
      'postponed_fields_are_updated' => new sfValidatorBoolean(),
      'postponed_reports'            => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647)),
      'postponed_amount'             => new sfValidatorInteger(array('min' => -9.2233720368548E+18, 'max' => 9223372036854775807)),
      'reports'                      => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647)),
      'amount'                       => new sfValidatorInteger(array('min' => -9.2233720368548E+18, 'max' => 9223372036854775807)),
      'ar_postponed_report_list'     => new sfValidatorPropelChoice(array('multiple' => true, 'model' => 'ArOrganizationUnit', 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_report_set[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArReportSet';
  }


  public function updateDefaultsFromObject()
  {
    parent::updateDefaultsFromObject();

    if (isset($this->widgetSchema['ar_postponed_report_list']))
    {
      $values = array();
      foreach ($this->object->getArPostponedReports() as $obj)
      {
        $values[] = $obj->getArOrganizationUnitId();
      }

      $this->setDefault('ar_postponed_report_list', $values);
    }

  }

  protected function doSave($con = null)
  {
    parent::doSave($con);

    $this->saveArPostponedReportList($con);
  }

  public function saveArPostponedReportList($con = null)
  {
    if (!$this->isValid())
    {
      throw $this->getErrorSchema();
    }

    if (!isset($this->widgetSchema['ar_postponed_report_list']))
    {
      // somebody has unset this widget
      return;
    }

    if (null === $con)
    {
      $con = $this->getConnection();
    }

    $c = new Criteria();
    $c->add(ArPostponedReportPeer::AR_REPORT_SET_ID, $this->object->getPrimaryKey());
    ArPostponedReportPeer::doDelete($c, $con);

    $values = $this->getValue('ar_postponed_report_list');
    if (is_array($values))
    {
      foreach ($values as $value)
      {
        $obj = new ArPostponedReport();
        $obj->setArReportSetId($this->object->getPrimaryKey());
        $obj->setArOrganizationUnitId($value);
        $obj->save();
      }
    }
  }

}
