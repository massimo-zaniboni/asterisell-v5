<?php

/**
 * ArReport form base class.
 *
 * @method ArReport getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArReportForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                                          => new sfWidgetFormInputHidden(),
      'is_template'                                 => new sfWidgetFormInputCheckbox(),
      'ar_report_set_id'                            => new sfWidgetFormPropelChoice(array('model' => 'ArReportSet', 'add_empty' => true)),
      'about_ar_report_set_id'                      => new sfWidgetFormPropelChoice(array('model' => 'ArReportSet', 'add_empty' => true)),
      'ar_organization_unit_id'                     => new sfWidgetFormPropelChoice(array('model' => 'ArOrganizationUnit', 'add_empty' => true)),
      'ar_user_id'                                  => new sfWidgetFormPropelChoice(array('model' => 'ArUser', 'add_empty' => true)),
      'ar_vendor_id'                                => new sfWidgetFormPropelChoice(array('model' => 'ArVendor', 'add_empty' => true)),
      'ar_tag_id'                                   => new sfWidgetFormPropelChoice(array('model' => 'ArTag', 'add_empty' => true)),
      'from_date'                                   => new sfWidgetFormDateTime(),
      'to_date'                                     => new sfWidgetFormDateTime(),
      'param_show_masked_telephone_numbers'         => new sfWidgetFormInputCheckbox(),
      'param_show_call_cost'                        => new sfWidgetFormInputCheckbox(),
      'param_show_call_income'                      => new sfWidgetFormInputCheckbox(),
      'param_show_also_outgoing_calls'              => new sfWidgetFormInputCheckbox(),
      'param_show_also_system_calls'                => new sfWidgetFormInputCheckbox(),
      'param_show_also_incoming_calls'              => new sfWidgetFormInputCheckbox(),
      'param_show_also_internal_calls'              => new sfWidgetFormInputCheckbox(),
      'param_show_call_details'                     => new sfWidgetFormInputCheckbox(),
      'param_show_voip_provider'                    => new sfWidgetFormInputCheckbox(),
      'param_show_communication_channel'            => new sfWidgetFormInputCheckbox(),
      'param_show_geographic_location'              => new sfWidgetFormInputCheckbox(),
      'param_show_connection_type'                  => new sfWidgetFormInputCheckbox(),
      'param_show_cost_saving'                      => new sfWidgetFormInputCheckbox(),
      'param_is_legal'                              => new sfWidgetFormInputCheckbox(),
      'param_expand_to_level'                       => new sfWidgetFormInputText(),
      'ar_report_order_of_children_id'              => new sfWidgetFormPropelChoice(array('model' => 'ArReportOrderOfChildren', 'add_empty' => true)),
      'php_class_name'                              => new sfWidgetFormInputText(),
      'produced_report_generation_date'             => new sfWidgetFormDateTime(),
      'report_name'                                 => new sfWidgetFormInputText(),
      'produced_report_short_description'           => new sfWidgetFormInputText(),
      'produced_report_additional_description'      => new sfWidgetFormInputText(),
      'produced_report_already_reviewed'            => new sfWidgetFormInputCheckbox(),
      'produced_report_is_draft'                    => new sfWidgetFormInputCheckbox(),
      'produced_report_must_be_regenerated'         => new sfWidgetFormInputCheckbox(),
      'produced_report_mime_type'                   => new sfWidgetFormInputText(),
      'produced_report_file_type_suffix'            => new sfWidgetFormInputText(),
      'produced_report_document'                    => new sfWidgetFormInputText(),
      'produced_report_document_checksum'           => new sfWidgetFormInputText(),
      'report_mail_subject'                         => new sfWidgetFormInputText(),
      'report_mail_body'                            => new sfWidgetFormInputText(),
      'report_attachment_file_name'                 => new sfWidgetFormInputText(),
      'report_attachment_file_name_add_report_date' => new sfWidgetFormInputCheckbox(),
      'internal_name'                               => new sfWidgetFormInputText(),
      'cached_parent_id_hierarchy'                  => new sfWidgetFormInputText(),
      'legal_nr_prefix'                             => new sfWidgetFormInputText(),
      'legal_consecutive_nr'                        => new sfWidgetFormInputText(),
      'legal_date'                                  => new sfWidgetFormDate(),
      'legal_sender_name'                           => new sfWidgetFormInputText(),
      'legal_sender_vat'                            => new sfWidgetFormInputText(),
      'legal_sender_address'                        => new sfWidgetFormInputText(),
      'legal_receiver_name'                         => new sfWidgetFormInputText(),
      'legal_receiver_vat'                          => new sfWidgetFormInputText(),
      'legal_receiver_address'                      => new sfWidgetFormInputText(),
      'total_without_tax'                           => new sfWidgetFormInputText(),
      'tax'                                         => new sfWidgetFormInputText(),
      'applied_vat'                                 => new sfWidgetFormInputText(),
      'total_with_tax'                              => new sfWidgetFormInputText(),
      'ar_report_also_for_list'                     => new sfWidgetFormPropelChoice(array('multiple' => true, 'model' => 'ArRole')),
      'ar_user_can_view_report_list'                => new sfWidgetFormPropelChoice(array('multiple' => true, 'model' => 'ArUser')),
    ));

    $this->setValidators(array(
      'id'                                          => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'is_template'                                 => new sfValidatorBoolean(),
      'ar_report_set_id'                            => new sfValidatorPropelChoice(array('model' => 'ArReportSet', 'column' => 'id', 'required' => false)),
      'about_ar_report_set_id'                      => new sfValidatorPropelChoice(array('model' => 'ArReportSet', 'column' => 'id', 'required' => false)),
      'ar_organization_unit_id'                     => new sfValidatorPropelChoice(array('model' => 'ArOrganizationUnit', 'column' => 'id', 'required' => false)),
      'ar_user_id'                                  => new sfValidatorPropelChoice(array('model' => 'ArUser', 'column' => 'id', 'required' => false)),
      'ar_vendor_id'                                => new sfValidatorPropelChoice(array('model' => 'ArVendor', 'column' => 'id', 'required' => false)),
      'ar_tag_id'                                   => new sfValidatorPropelChoice(array('model' => 'ArTag', 'column' => 'id', 'required' => false)),
      'from_date'                                   => new sfValidatorDateTime(array('required' => false)),
      'to_date'                                     => new sfValidatorDateTime(array('required' => false)),
      'param_show_masked_telephone_numbers'         => new sfValidatorBoolean(),
      'param_show_call_cost'                        => new sfValidatorBoolean(),
      'param_show_call_income'                      => new sfValidatorBoolean(),
      'param_show_also_outgoing_calls'              => new sfValidatorBoolean(),
      'param_show_also_system_calls'                => new sfValidatorBoolean(),
      'param_show_also_incoming_calls'              => new sfValidatorBoolean(),
      'param_show_also_internal_calls'              => new sfValidatorBoolean(),
      'param_show_call_details'                     => new sfValidatorBoolean(),
      'param_show_voip_provider'                    => new sfValidatorBoolean(),
      'param_show_communication_channel'            => new sfValidatorBoolean(),
      'param_show_geographic_location'              => new sfValidatorBoolean(),
      'param_show_connection_type'                  => new sfValidatorBoolean(),
      'param_show_cost_saving'                      => new sfValidatorBoolean(),
      'param_is_legal'                              => new sfValidatorBoolean(),
      'param_expand_to_level'                       => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647)),
      'ar_report_order_of_children_id'              => new sfValidatorPropelChoice(array('model' => 'ArReportOrderOfChildren', 'column' => 'id', 'required' => false)),
      'php_class_name'                              => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
      'produced_report_generation_date'             => new sfValidatorDateTime(array('required' => false)),
      'report_name'                                 => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
      'produced_report_short_description'           => new sfValidatorString(array('max_length' => 2048, 'required' => false)),
      'produced_report_additional_description'      => new sfValidatorString(array('max_length' => 2048, 'required' => false)),
      'produced_report_already_reviewed'            => new sfValidatorBoolean(),
      'produced_report_is_draft'                    => new sfValidatorBoolean(),
      'produced_report_must_be_regenerated'         => new sfValidatorBoolean(),
      'produced_report_mime_type'                   => new sfValidatorString(array('max_length' => 128)),
      'produced_report_file_type_suffix'            => new sfValidatorString(array('max_length' => 25)),
      'produced_report_document'                    => new sfValidatorPass(array('required' => false)),
      'produced_report_document_checksum'           => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
      'report_mail_subject'                         => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
      'report_mail_body'                            => new sfValidatorString(array('max_length' => 5024, 'required' => false)),
      'report_attachment_file_name'                 => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'report_attachment_file_name_add_report_date' => new sfValidatorBoolean(),
      'internal_name'                               => new sfValidatorString(array('max_length' => 512, 'required' => false)),
      'cached_parent_id_hierarchy'                  => new sfValidatorString(array('max_length' => 2024, 'required' => false)),
      'legal_nr_prefix'                             => new sfValidatorString(array('max_length' => 255)),
      'legal_consecutive_nr'                        => new sfValidatorInteger(array('min' => -2147483648, 'max' => 2147483647, 'required' => false)),
      'legal_date'                                  => new sfValidatorDate(array('required' => false)),
      'legal_sender_name'                           => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'legal_sender_vat'                            => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'legal_sender_address'                        => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
      'legal_receiver_name'                         => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'legal_receiver_vat'                          => new sfValidatorString(array('max_length' => 255, 'required' => false)),
      'legal_receiver_address'                      => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
      'total_without_tax'                           => new sfValidatorInteger(array('min' => -9.2233720368548E+18, 'max' => 9223372036854775807, 'required' => false)),
      'tax'                                         => new sfValidatorInteger(array('min' => -9.2233720368548E+18, 'max' => 9223372036854775807, 'required' => false)),
      'applied_vat'                                 => new sfValidatorInteger(array('min' => -9.2233720368548E+18, 'max' => 9223372036854775807, 'required' => false)),
      'total_with_tax'                              => new sfValidatorInteger(array('min' => -9.2233720368548E+18, 'max' => 9223372036854775807, 'required' => false)),
      'ar_report_also_for_list'                     => new sfValidatorPropelChoice(array('multiple' => true, 'model' => 'ArRole', 'required' => false)),
      'ar_user_can_view_report_list'                => new sfValidatorPropelChoice(array('multiple' => true, 'model' => 'ArUser', 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_report[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArReport';
  }


  public function updateDefaultsFromObject()
  {
    parent::updateDefaultsFromObject();

    if (isset($this->widgetSchema['ar_report_also_for_list']))
    {
      $values = array();
      foreach ($this->object->getArReportAlsoFors() as $obj)
      {
        $values[] = $obj->getArRoleId();
      }

      $this->setDefault('ar_report_also_for_list', $values);
    }

    if (isset($this->widgetSchema['ar_user_can_view_report_list']))
    {
      $values = array();
      foreach ($this->object->getArUserCanViewReports() as $obj)
      {
        $values[] = $obj->getArUserId();
      }

      $this->setDefault('ar_user_can_view_report_list', $values);
    }

  }

  protected function doSave($con = null)
  {
    parent::doSave($con);

    $this->saveArReportAlsoForList($con);
    $this->saveArUserCanViewReportList($con);
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
    $c->add(ArReportAlsoForPeer::AR_REPORT_ID, $this->object->getPrimaryKey());
    ArReportAlsoForPeer::doDelete($c, $con);

    $values = $this->getValue('ar_report_also_for_list');
    if (is_array($values))
    {
      foreach ($values as $value)
      {
        $obj = new ArReportAlsoFor();
        $obj->setArReportId($this->object->getPrimaryKey());
        $obj->setArRoleId($value);
        $obj->save();
      }
    }
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
    $c->add(ArUserCanViewReportPeer::AR_REPORT_ID, $this->object->getPrimaryKey());
    ArUserCanViewReportPeer::doDelete($c, $con);

    $values = $this->getValue('ar_user_can_view_report_list');
    if (is_array($values))
    {
      foreach ($values as $value)
      {
        $obj = new ArUserCanViewReport();
        $obj->setArReportId($this->object->getPrimaryKey());
        $obj->setArUserId($value);
        $obj->save();
      }
    }
  }

}
