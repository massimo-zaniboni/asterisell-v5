<?php

/**
 * ListCdrsAdmin1Fast filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseListCdrsAdmin1FastFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
    ));

    $this->setValidators(array(
    ));

    $this->widgetSchema->setNameFormat('list_cdrs_admin1_fast_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ListCdrsAdmin1Fast';
  }

  public function getFields()
  {
    return array(
      'id' => 'Number',
    );
  }
}
