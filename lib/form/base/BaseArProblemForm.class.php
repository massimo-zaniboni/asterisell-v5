<?php

/**
 * ArProblem form base class.
 *
 * @method ArProblem getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArProblemForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                      => new sfWidgetFormInputHidden(),
      'ar_problem_type_id'      => new sfWidgetFormPropelChoice(array('model' => 'ArProblemType', 'add_empty' => true)),
      'created_at'              => new sfWidgetFormDateTime(),
      'duplication_key'         => new sfWidgetFormInputText(),
      'garbage_collection_key'  => new sfWidgetFormInputText(),
      'garbage_collection_from' => new sfWidgetFormDateTime(),
      'garbage_collection_to'   => new sfWidgetFormDateTime(),
      'description'             => new sfWidgetFormTextarea(),
      'effect'                  => new sfWidgetFormTextarea(),
      'proposed_solution'       => new sfWidgetFormTextarea(),
      'signaled_to_admin'       => new sfWidgetFormInputCheckbox(),
    ));

    $this->setValidators(array(
      'id'                      => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'ar_problem_type_id'      => new sfValidatorPropelChoice(array('model' => 'ArProblemType', 'column' => 'id', 'required' => false)),
      'created_at'              => new sfValidatorDateTime(array('required' => false)),
      'duplication_key'         => new sfValidatorString(array('max_length' => 255)),
      'garbage_collection_key'  => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
      'garbage_collection_from' => new sfValidatorDateTime(array('required' => false)),
      'garbage_collection_to'   => new sfValidatorDateTime(array('required' => false)),
      'description'             => new sfValidatorString(array('required' => false)),
      'effect'                  => new sfValidatorString(array('required' => false)),
      'proposed_solution'       => new sfValidatorString(array('required' => false)),
      'signaled_to_admin'       => new sfValidatorBoolean(),
    ));

    $this->validatorSchema->setPostValidator(
      new sfValidatorPropelUnique(array('model' => 'ArProblem', 'column' => array('duplication_key')))
    );

    $this->widgetSchema->setNameFormat('ar_problem[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArProblem';
  }


}
