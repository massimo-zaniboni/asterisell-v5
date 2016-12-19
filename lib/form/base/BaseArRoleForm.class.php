<?php

/**
 * ArRole form base class.
 *
 * @method ArRole getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArRoleForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                      => new sfWidgetFormInputHidden(),
      'name'                    => new sfWidgetFormInputText(),
      'power'                   => new sfWidgetFormInputText(),
      'description'             => new sfWidgetFormTextarea(),
      'internal_name'           => new sfWidgetFormInputText(),
      'ar_report_also_for_list' => new sfWidgetFormPropelChoice(array('multiple' => true, 'model' => 'ArReport')),
    ));

    $this->setValidators(array(
      'id'                      => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'name'                    => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
      'power'                   => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647)),
      'description'             => new sfValidatorString(array('required' => false)),
      'internal_name'           => new sfValidatorString(array('max_length' => 200, 'required' => false)),
      'ar_report_also_for_list' => new sfValidatorPropelChoice(array('multiple' => true, 'model' => 'ArReport', 'required' => false)),
    ));

    $this->validatorSchema->setPostValidator(
      new sfValidatorPropelUnique(array('model' => 'ArRole', 'column' => array('internal_name')))
    );

    $this->widgetSchema->setNameFormat('ar_role[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArRole';
  }


  public function updateDefaultsFromObject()
  {
    parent::updateDefaultsFromObject();

    if (isset($this->widgetSchema['ar_report_also_for_list']))
    {
      $values = array();
      foreach ($this->object->getArReportAlsoFors() as $obj)
      {
        $values[] = $obj->getArReportId();
      }

      $this->setDefault('ar_report_also_for_list', $values);
    }

  }

  protected function doSave($con = null)
  {
    parent::doSave($con);

    $this->saveArReportAlsoForList($con);
  }

  public function saveArReportAlsoForList($con = null)
  {
    if (!$this->isValid())
    {
      throw $this->getErrorSchema();
    }

    if (!isset($this->widgetSchema['ar_report_also_for_list']))
    {
      // somebody has unset this widget
      return;
    }

    if (null === $con)
    {
      $con = $this->getConnection();
    }

    $c = new Criteria();
    $c->add(ArReportAlsoForPeer::AR_ROLE_ID, $this->object->getPrimaryKey());
    ArReportAlsoForPeer::doDelete($c, $con);

    $values = $this->getValue('ar_report_also_for_list');
    if (is_array($values))
    {
      foreach ($values as $value)
      {
        $obj = new ArReportAlsoFor();
        $obj->setArRoleId($this->object->getPrimaryKey());
        $obj->setArReportId($value);
        $obj->save();
      }
    }
  }

}
