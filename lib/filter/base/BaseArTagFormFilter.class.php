<?php

/**
 * ArTag filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArTagFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'internal_name'         => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'note_for_admin'        => new sfWidgetFormFilterInput(),
      'name_for_customer'     => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'note_for_customer'     => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'ar_party_has_tag_list' => new sfWidgetFormPropelChoice(array('model' => 'ArParty', 'add_empty' => true)),
    ));

    $this->setValidators(array(
      'internal_name'         => new sfValidatorPass(array('required' => false)),
      'note_for_admin'        => new sfValidatorPass(array('required' => false)),
      'name_for_customer'     => new sfValidatorPass(array('required' => false)),
      'note_for_customer'     => new sfValidatorPass(array('required' => false)),
      'ar_party_has_tag_list' => new sfValidatorPropelChoice(array('model' => 'ArParty', 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_tag_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function addArPartyHasTagListColumnCriteria(Criteria $criteria, $field, $values)
  {
    if (!is_array($values))
    {
      $values = array($values);
    }

    if (!count($values))
    {
      return;
    }

    $criteria->addJoin(ArPartyHasTagPeer::AR_TAG_ID, ArTagPeer::ID);

    $value = array_pop($values);
    $criterion = $criteria->getNewCriterion(ArPartyHasTagPeer::AR_PARTY_ID, $value);

    foreach ($values as $value)
    {
      $criterion->addOr($criteria->getNewCriterion(ArPartyHasTagPeer::AR_PARTY_ID, $value));
    }

    $criteria->add($criterion);
  }

  public function getModelName()
  {
    return 'ArTag';
  }

  public function getFields()
  {
    return array(
      'id'                    => 'Number',
      'internal_name'         => 'Text',
      'note_for_admin'        => 'Text',
      'name_for_customer'     => 'Text',
      'note_for_customer'     => 'Text',
      'ar_party_has_tag_list' => 'ManyKey',
    );
  }
}
