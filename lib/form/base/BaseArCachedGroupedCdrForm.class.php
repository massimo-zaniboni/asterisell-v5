<?php

/**
 * ArCachedGroupedCdr form base class.
 *
 * @method ArCachedGroupedCdr getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArCachedGroupedCdrForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'cached_parent_id_hierarchy'       => new sfWidgetFormInputHidden(),
      'billable_ar_organization_unit_id' => new sfWidgetFormInputHidden(),
      'calldate'                         => new sfWidgetFormInputHidden(),
      'destination_type'                 => new sfWidgetFormInputHidden(),
      'ar_communication_channel_type_id' => new sfWidgetFormInputHidden(),
      'operator_type'                    => new sfWidgetFormInputHidden(),
      'ar_vendor_id'                     => new sfWidgetFormInputHidden(),
      'geographic_location'              => new sfWidgetFormInputHidden(),
      'count_of_calls'                   => new sfWidgetFormInputText(),
      'billsec'                          => new sfWidgetFormInputText(),
      'income'                           => new sfWidgetFormInputText(),
      'cost_saving'                      => new sfWidgetFormInputText(),
      'cost'                             => new sfWidgetFormInputText(),
      'count_of_records'                 => new sfWidgetFormInputText(),
      'id'                               => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'cached_parent_id_hierarchy'       => new sfValidatorChoice(array('choices' => array($this->getObject()->getCachedParentIdHierarchy()), 'empty_value' => $this->getObject()->getCachedParentIdHierarchy(), 'required' => false)),
      'billable_ar_organization_unit_id' => new sfValidatorChoice(array('choices' => array($this->getObject()->getBillableArOrganizationUnitId()), 'empty_value' => $this->getObject()->getBillableArOrganizationUnitId(), 'required' => false)),
      'calldate'                         => new sfValidatorChoice(array('choices' => array($this->getObject()->getCalldate()), 'empty_value' => $this->getObject()->getCalldate(), 'required' => false)),
      'destination_type'                 => new sfValidatorChoice(array('choices' => array($this->getObject()->getDestinationType()), 'empty_value' => $this->getObject()->getDestinationType(), 'required' => false)),
      'ar_communication_channel_type_id' => new sfValidatorChoice(array('choices' => array($this->getObject()->getArCommunicationChannelTypeId()), 'empty_value' => $this->getObject()->getArCommunicationChannelTypeId(), 'required' => false)),
      'operator_type'                    => new sfValidatorChoice(array('choices' => array($this->getObject()->getOperatorType()), 'empty_value' => $this->getObject()->getOperatorType(), 'required' => false)),
      'ar_vendor_id'                     => new sfValidatorChoice(array('choices' => array($this->getObject()->getArVendorId()), 'empty_value' => $this->getObject()->getArVendorId(), 'required' => false)),
      'geographic_location'              => new sfValidatorChoice(array('choices' => array($this->getObject()->getGeographicLocation()), 'empty_value' => $this->getObject()->getGeographicLocation(), 'required' => false)),
      'count_of_calls'                   => new sfValidatorInteger(array('min' => -9.2233720368548E+18, 'max' => 9223372036854775807)),
      'billsec'                          => new sfValidatorInteger(array('min' => -9.2233720368548E+18, 'max' => 9223372036854775807)),
      'income'                           => new sfValidatorInteger(array('min' => -9.2233720368548E+18, 'max' => 9223372036854775807)),
      'cost_saving'                      => new sfValidatorInteger(array('min' => -9.2233720368548E+18, 'max' => 9223372036854775807)),
      'cost'                             => new sfValidatorInteger(array('min' => -9.2233720368548E+18, 'max' => 9223372036854775807)),
      'count_of_records'                 => new sfValidatorInteger(array('min' => -9.2233720368548E+18, 'max' => 9223372036854775807)),
      'id'                               => new sfValidatorInteger(array('min' => -32768, 'max' => 32767)),
    ));

    $this->widgetSchema->setNameFormat('ar_cached_grouped_cdr[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArCachedGroupedCdr';
  }


}
