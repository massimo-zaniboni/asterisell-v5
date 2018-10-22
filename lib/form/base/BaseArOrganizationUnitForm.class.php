<?php

/**
 * ArOrganizationUnit form base class.
 *
 * @method ArOrganizationUnit getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArOrganizationUnitForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                         => new sfWidgetFormInputHidden(),
      'internal_name'              => new sfWidgetFormInputText(),
      'internal_name2'             => new sfWidgetFormInputText(),
      'internal_checksum1'         => new sfWidgetFormInputText(),
      'internal_checksum2'         => new sfWidgetFormInputText(),
      'internal_checksum3'         => new sfWidgetFormInputText(),
      'internal_checksum4'         => new sfWidgetFormInputText(),
      'export_code'                => new sfWidgetFormInputText(),
      'automatically_managed_from' => new sfWidgetFormInputText(),
      'ar_postponed_report_list'   => new sfWidgetFormPropelChoice(array('multiple' => true, 'model' => 'ArReportSet')),
    ));

    $this->setValidators(array(
      'id'                         => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'internal_name'              => new sfValidatorString(array('max_length' => 200, 'required' => false)),
      'internal_name2'             => new sfValidatorString(array('max_length' => 200, 'required' => false)),
      'internal_checksum1'         => new sfValidatorString(array('max_length' => 200, 'required' => false)),
      'internal_checksum2'         => new sfValidatorString(array('max_length' => 200, 'required' => false)),
      'internal_checksum3'         => new sfValidatorString(array('max_length' => 200, 'required' => false)),
      'internal_checksum4'         => new sfValidatorString(array('max_length' => 200, 'required' => false)),
      'export_code'                => new sfValidatorString(array('max_length' => 200, 'required' => false)),
      'automatically_managed_from' => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647)),
      'ar_postponed_report_list'   => new sfValidatorPropelChoice(array('multiple' => true, 'model' => 'ArReportSet', 'required' => false)),
    ));

    $this->validatorSchema->setPostValidator(
      new sfValidatorAnd(array(
        new sfValidatorPropelUnique(array('model' => 'ArOrganizationUnit', 'column' => array('internal_name'))),
        new sfValidatorPropelUnique(array('model' => 'ArOrganizationUnit', 'column' => array('internal_name2'))),
        new sfValidatorPropelUnique(array('model' => 'ArOrganizationUnit', 'column' => array('export_code'))),
      ))
    );

    $this->widgetSchema->setNameFormat('ar_organization_unit[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArOrganizationUnit';
  }


  public function updateDefaultsFromObject()
  {
    parent::updateDefaultsFromObject();

    if (isset($this->widgetSchema['ar_postponed_report_list']))
    {
      $values = array();
      foreach ($this->object->getArPostponedReports() as $obj)
      {
        $values[] = $obj->getArReportSetId();
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
    $c->add(ArPostponedReportPeer::AR_ORGANIZATION_UNIT_ID, $this->object->getPrimaryKey());
    ArPostponedReportPeer::doDelete($c, $con);

    $values = $this->getValue('ar_postponed_report_list');
    if (is_array($values))
    {
      foreach ($values as $value)
      {
        $obj = new ArPostponedReport();
        $obj->setArOrganizationUnitId($this->object->getPrimaryKey());
        $obj->setArReportSetId($value);
        $obj->save();
      }
    }
  }

}
