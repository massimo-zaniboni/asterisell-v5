<?php

/**
 * ArPartyHasTag filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArPartyHasTagFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
    ));

    $this->setValidators(array(
    ));

    $this->widgetSchema->setNameFormat('ar_party_has_tag_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArPartyHasTag';
  }

  public function getFields()
  {
    return array(
      'ar_party_id' => 'ForeignKey',
      'ar_tag_id'   => 'ForeignKey',
    );
  }
}
