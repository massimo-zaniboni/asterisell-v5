<?php

/**
 * ArTag form base class.
 *
 * @method ArTag getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArTagForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                    => new sfWidgetFormInputHidden(),
      'internal_name'         => new sfWidgetFormInputText(),
      'note_for_admin'        => new sfWidgetFormInputText(),
      'name_for_customer'     => new sfWidgetFormInputText(),
      'note_for_customer'     => new sfWidgetFormInputText(),
      'ar_party_has_tag_list' => new sfWidgetFormPropelChoice(array('multiple' => true, 'model' => 'ArParty')),
    ));

    $this->setValidators(array(
      'id'                    => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'internal_name'         => new sfValidatorString(array('max_length' => 255)),
      'note_for_admin'        => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
      'name_for_customer'     => new sfValidatorString(array('max_length' => 512)),
      'note_for_customer'     => new sfValidatorString(array('max_length' => 1204)),
      'ar_party_has_tag_list' => new sfValidatorPropelChoice(array('multiple' => true, 'model' => 'ArParty', 'required' => false)),
    ));

    $this->validatorSchema->setPostValidator(
      new sfValidatorPropelUnique(array('model' => 'ArTag', 'column' => array('internal_name')))
    );

    $this->widgetSchema->setNameFormat('ar_tag[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArTag';
  }


  public function updateDefaultsFromObject()
  {
    parent::updateDefaultsFromObject();

    if (isset($this->widgetSchema['ar_party_has_tag_list']))
    {
      $values = array();
      foreach ($this->object->getArPartyHasTags() as $obj)
      {
        $values[] = $obj->getArPartyId();
      }

      $this->setDefault('ar_party_has_tag_list', $values);
    }

  }

  protected function doSave($con = null)
  {
    parent::doSave($con);

    $this->saveArPartyHasTagList($con);
  }

  public function saveArPartyHasTagList($con = null)
  {
    if (!$this->isValid())
    {
      throw $this->getErrorSchema();
    }

    if (!isset($this->widgetSchema['ar_party_has_tag_list']))
    {
      // somebody has unset this widget
      return;
    }

    if (null === $con)
    {
      $con = $this->getConnection();
    }

    $c = new Criteria();
    $c->add(ArPartyHasTagPeer::AR_TAG_ID, $this->object->getPrimaryKey());
    ArPartyHasTagPeer::doDelete($c, $con);

    $values = $this->getValue('ar_party_has_tag_list');
    if (is_array($values))
    {
      foreach ($values as $value)
      {
        $obj = new ArPartyHasTag();
        $obj->setArTagId($this->object->getPrimaryKey());
        $obj->setArPartyId($value);
        $obj->save();
      }
    }
  }

}
