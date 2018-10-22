<?php

/**
 * ArUser form base class.
 *
 * @method ArUser getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArUserForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                           => new sfWidgetFormInputHidden(),
      'ar_party_id'                  => new sfWidgetFormPropelChoice(array('model' => 'ArParty', 'add_empty' => true)),
      'ar_organization_unit_id'      => new sfWidgetFormPropelChoice(array('model' => 'ArOrganizationUnit', 'add_empty' => true)),
      'login'                        => new sfWidgetFormInputText(),
      'password'                     => new sfWidgetFormInputText(),
      'clear_password_to_import'     => new sfWidgetFormInputText(),
      'is_enabled'                   => new sfWidgetFormInputCheckbox(),
      'is_root_admin'                => new sfWidgetFormInputCheckbox(),
      'ar_user_can_view_report_list' => new sfWidgetFormPropelChoice(array('multiple' => true, 'model' => 'ArReport')),
    ));

    $this->setValidators(array(
      'id'                           => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'ar_party_id'                  => new sfValidatorPropelChoice(array('model' => 'ArParty', 'column' => 'id', 'required' => false)),
      'ar_organization_unit_id'      => new sfValidatorPropelChoice(array('model' => 'ArOrganizationUnit', 'column' => 'id', 'required' => false)),
      'login'                        => new sfValidatorString(array('max_length' => 255)),
      'password'                     => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
      'clear_password_to_import'     => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'is_enabled'                   => new sfValidatorBoolean(),
      'is_root_admin'                => new sfValidatorBoolean(),
      'ar_user_can_view_report_list' => new sfValidatorPropelChoice(array('multiple' => true, 'model' => 'ArReport', 'required' => false)),
    ));

    $this->validatorSchema->setPostValidator(
      new sfValidatorPropelUnique(array('model' => 'ArUser', 'column' => array('login')))
    );

    $this->widgetSchema->setNameFormat('ar_user[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArUser';
  }


  public function updateDefaultsFromObject()
  {
    parent::updateDefaultsFromObject();

    if (isset($this->widgetSchema['ar_user_can_view_report_list']))
    {
      $values = array();
      foreach ($this->object->getArUserCanViewReports() as $obj)
      {
        $values[] = $obj->getArReportId();
      }

      $this->setDefault('ar_user_can_view_report_list', $values);
    }

  }

  protected function doSave($con = null)
  {
    parent::doSave($con);

    $this->saveArUserCanViewReportList($con);
  }

  public function saveArUserCanViewReportList($con = null)
  {
    if (!$this->isValid())
    {
      throw $this->getErrorSchema();
    }

    if (!isset($this->widgetSchema['ar_user_can_view_report_list']))
    {
      // somebody has unset this widget
      return;
    }

    if (null === $con)
    {
      $con = $this->getConnection();
    }

    $c = new Criteria();
    $c->add(ArUserCanViewReportPeer::AR_USER_ID, $this->object->getPrimaryKey());
    ArUserCanViewReportPeer::doDelete($c, $con);

    $values = $this->getValue('ar_user_can_view_report_list');
    if (is_array($values))
    {
      foreach ($values as $value)
      {
        $obj = new ArUserCanViewReport();
        $obj->setArUserId($this->object->getPrimaryKey());
        $obj->setArReportId($value);
        $obj->save();
      }
    }
  }

}
