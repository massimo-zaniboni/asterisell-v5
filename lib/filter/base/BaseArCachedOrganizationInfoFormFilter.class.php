<?php

/**
 * ArCachedOrganizationInfo filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArCachedOrganizationInfoFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'internal_name' => new sfWidgetFormFilterInput(),
      'content'       => new sfWidgetFormFilterInput(),
    ));

    $this->setValidators(array(
      'internal_name' => new sfValidatorPass(array('required' => false)),
      'content'       => new sfValidatorPass(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_cached_organization_info_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArCachedOrganizationInfo';
  }

  public function getFields()
  {
    return array(
      'id'            => 'Number',
      'internal_name' => 'Text',
      'content'       => 'Text',
    );
  }
}
