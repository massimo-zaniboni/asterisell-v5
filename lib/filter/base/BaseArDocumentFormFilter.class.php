<?php

/**
 * ArDocument filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArDocumentFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_party_id'    => new sfWidgetFormPropelChoice(array('model' => 'ArParty', 'add_empty' => true)),
      'document_name'  => new sfWidgetFormFilterInput(),
      'document_date'  => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'document'       => new sfWidgetFormFilterInput(),
      'file_name'      => new sfWidgetFormFilterInput(),
      'mime_type'      => new sfWidgetFormFilterInput(),
      'already_opened' => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
    ));

    $this->setValidators(array(
      'ar_party_id'    => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArParty', 'column' => 'id')),
      'document_name'  => new sfValidatorPass(array('required' => false)),
      'document_date'  => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'document'       => new sfValidatorPass(array('required' => false)),
      'file_name'      => new sfValidatorPass(array('required' => false)),
      'mime_type'      => new sfValidatorPass(array('required' => false)),
      'already_opened' => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
    ));

    $this->widgetSchema->setNameFormat('ar_document_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArDocument';
  }

  public function getFields()
  {
    return array(
      'id'             => 'Number',
      'ar_party_id'    => 'ForeignKey',
      'document_name'  => 'Text',
      'document_date'  => 'Date',
      'document'       => 'Text',
      'file_name'      => 'Text',
      'mime_type'      => 'Text',
      'already_opened' => 'Boolean',
    );
  }
}
